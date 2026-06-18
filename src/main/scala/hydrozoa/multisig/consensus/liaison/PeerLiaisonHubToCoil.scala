package hydrozoa.multisig.consensus.liaison

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.{OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol.*
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{CoilAckSequencer, SlowConsensusActor, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import hydrozoa.multisig.persistence.recovery.{LaneIncomingCursors, LaneOutgoingBacking}
import hydrozoa.multisig.persistence.{JournalKey, JournalValue, Persistence, WriteBatch}

/** A hub head peer's liaison toward one coil peer it serves (§5.5 of `design/coil-network.md`)
  * [doc-ref] — the mirror of [[PeerLiaisonCoilToHub]].
  *
  * Asymmetric: it **serves the full population** down to the coil peer (block + stack spines,
  * per-head-peer request / soft-ack / head-hard-ack lanes, per-hub coil-hard-ack lanes — all outbox
  * lanes appended by `CoilRelay` / the hub's producers) and **pulls the coil peer's own hard-ack**,
  * which it routes to the hub's [[SlowConsensusActor]] (for quorum) and to [[CoilAckSequencer]]
  * (which re-sequences it onto the hub's `HubHardAckLane`).
  *
  * Built by composition: a [[Server]] over the population outbox lanes + a [[Puller]] over the one
  * inbound own-hard-ack lane. No fat base; appended artifacts are routed to the right per-author /
  * per-hub lane by their embedded author.
  */
abstract class PeerLiaisonHubToCoil(
    config: PeerLiaisonHubToCoil.Config,
    coil: CoilPeerNumber,
    pendingConnections: HeadMultisigRegimeManager.PendingConnections |
        PeerLiaisonHubToCoil.Connections,
    tracer: ContraTracer[IO, PeerLiaisonEvent],
    persistence: Persistence[IO]
) extends Actor[IO, LiaisonProtocol.HubToCoilRequest] {

    // `config` is a `CardanoNetwork.Section`; expose it as a given so the inbound-lane `WriteBatch`
    // codec in `persistInbound` picks it up.
    private given CardanoNetwork.Section = config

    /** Resolve connections — projected from the shared regime `Connections` (remote coil handle
      * from the in-process `remoteCoilLiaisons` map) or supplied directly.
      */
    private def resolveConnections: IO[PeerLiaisonHubToCoil.Connections] =
        pendingConnections match {
            case shared: HeadMultisigRegimeManager.PendingConnections =>
                shared.get.flatMap(s =>
                    s.coilAckSequencer.fold(
                      IO.raiseError(
                        java.lang.Error("Hub→coil liaison requires a CoilAckSequencer.")
                      )
                    )(seq =>
                        IO.pure(
                          PeerLiaisonHubToCoil.Connections(
                            slowConsensusActor = s.slowConsensusActor,
                            coilAckSequencer = seq,
                            remote = s.remoteCoilLiaisons(coil)
                          )
                        )
                    )
                )
            case own: PeerLiaisonHubToCoil.Connections => IO.pure(own)
        }

    private val headPeerNums: List[HeadPeerNumber] = config.headPeerNums.toList
    private val hubNums: List[HeadPeerNumber] = config.coilPeers.hubHeadPeerNumbers

    // ---- Outbox lanes (the population we serve to the coil peer) ---------------------------------
    // Each lane is backed by the journal the hub already persists as a head-mesh member (own-produced
    // or inbound-replicated), so a reply hot-loads entries below the in-memory outbox floor and
    // preStart restores only the high-water; the hub serves every author (no own-led filter).
    private val backend = persistence.backend
    private val blockBacking = LaneOutgoingBacking.block(backend, _ => true)
    private val stackBacking = LaneOutgoingBacking.stack(backend, _ => true)
    private val requestBackings =
        headPeerNums.map(h => h -> LaneOutgoingBacking.request(backend, h)).toMap
    private val softAckBackings =
        headPeerNums.map(h => h -> LaneOutgoingBacking.softAck(backend, h)).toMap
    private val headHardAckBackings =
        headPeerNums.map(h => h -> LaneOutgoingBacking.hardAck(backend, PeerId.Head(h))).toMap
    private val coilHardAckBackings =
        hubNums.map(h => h -> LaneOutgoingBacking.hubHardAck(backend, h)).toMap

    private val blockLane =
        LaneOutbound.contiguous[BlockBrief.Next, BlockNumber](
          _.blockNum,
          BlockNumber(1),
          _.increment,
          backfill = blockBacking.backfill
        )
    private val stackLane =
        LaneOutbound.contiguous[StackBrief, StackNumber](
          _.stackNum,
          StackNumber(1),
          _.increment,
          backfill = stackBacking.backfill
        )
    private val requestLanes: Map[HeadPeerNumber, LaneOutbound[UserRequestWithId, RequestNumber]] =
        headPeerNums.map { h =>
            h -> LaneOutbound.contiguous[UserRequestWithId, RequestNumber](
              _.requestId.requestNum,
              RequestNumber.zero,
              _.increment,
              config.peerLiaisonMaxRequestsPerBatch,
              backfill = requestBackings(h).backfill
            )
        }.toMap
    private val softAckLanes: Map[HeadPeerNumber, LaneOutbound[SoftAck, SoftAckNumber]] =
        headPeerNums.map { h =>
            h -> LaneOutbound.contiguous[SoftAck, SoftAckNumber](
              _.ackNum,
              SoftAckNumber.zero.increment,
              _.increment,
              backfill = softAckBackings(h).backfill
            )
        }.toMap
    private val headHardAckLanes: Map[HeadPeerNumber, LaneOutbound[HardAck, HardAckNumber]] =
        headPeerNums.map { h =>
            h -> LaneOutbound.contiguous[HardAck, HardAckNumber](
              _.hardAckNum,
              HardAckNumber.zero,
              _.increment,
              backfill = headHardAckBackings(h).backfill
            )
        }.toMap
    private val coilHardAckLanes
        : Map[HeadPeerNumber, LaneOutbound[HardAckWithId, HubHardAckNumber]] =
        hubNums.map { h =>
            h -> LaneOutbound.contiguous[HardAckWithId, HubHardAckNumber](
              _.seqNum,
              HubHardAckNumber.zero,
              _.increment,
              backfill = coilHardAckBackings(h).backfill
            )
        }.toMap

    // ---- Inbound lane (the coil peer's own hard-ack we pull) ------------------------------------
    private val ownHardAckLane =
        LaneInbound.contiguous[HardAck, HardAckNumber](
          _.hardAckNum,
          HardAckNumber.zero,
          _.increment
        )

    // ---- Connections ----------------------------------------------------------------------------
    private val connections = Ref.unsafe[IO, Option[PeerLiaisonHubToCoil.Connections]](None)

    private def getConnections: IO[PeerLiaisonHubToCoil.Connections] =
        connections.get.flatMap(
          _.fold(IO.raiseError(java.lang.Error("Hub→coil liaison missing its connections.")))(
            IO.pure
          )
        )

    // ---- Serve half (population) ----------------------------------------------------------------
    private def serve(get: Population.Get): IO[Server.Served[Population.New]] =
        for {
            blockR <- blockLane.reply(get.block)
            stackR <- stackLane.reply(get.stack)
            reqR <- requestLanes.toList.traverse { case (h, l) =>
                l.reply(get.requests(h)).map(h -> _)
            }
            saR <- softAckLanes.toList.traverse { case (h, l) =>
                l.reply(get.softAcks(h)).map(h -> _)
            }
            hhR <- headHardAckLanes.toList.traverse { case (h, l) =>
                l.reply(get.headHardAcks(h)).map(h -> _)
            }
            chR <- coilHardAckLanes.toList.traverse { case (h, l) =>
                l.reply(get.coilHardAcks(h)).map(h -> _)
            }
        } yield {
            val firstOob =
                LaneOutbound
                    .firstOutOfBounds("block" -> blockR, "stack" -> stackR)
                    .orElse(LaneOutbound.firstOutOfBounds(reqR.map { case (h, r) =>
                        s"request[$h]" -> r
                    }*))
                    .orElse(LaneOutbound.firstOutOfBounds(saR.map { case (h, r) =>
                        s"softAck[$h]" -> r
                    }*))
                    .orElse(LaneOutbound.firstOutOfBounds(hhR.map { case (h, r) =>
                        s"headHardAck[$h]" -> r
                    }*))
                    .orElse(LaneOutbound.firstOutOfBounds(chR.map { case (h, r) =>
                        s"coilHardAck[$h]" -> r
                    }*))
            firstOob match
                case Some(detail) => Server.Served.OutOfBounds(detail)
                case None =>
                    def items[T](r: LaneOutbound.Reply[T]): List[T] = r match
                        case LaneOutbound.Items(xs)            => xs
                        case LaneOutbound.OutOfBounds(_, _, _) => Nil
                    val block = items(blockR).headOption
                    val stack = items(stackR).headOption
                    val requests = reqR.map { case (h, r) => h -> items(r) }.toMap
                    val softAcks = saR.map { case (h, r) => h -> items(r).headOption }.toMap
                    val headHardAcks = hhR.map { case (h, r) => h -> items(r).headOption }.toMap
                    val coilHardAcks = chR.map { case (h, r) => h -> items(r).headOption }.toMap
                    val anyContent =
                        block.nonEmpty || stack.nonEmpty || requests.values.exists(_.nonEmpty) ||
                            softAcks.values.exists(_.nonEmpty) || headHardAcks.values.exists(
                              _.nonEmpty
                            ) ||
                            coilHardAcks.values.exists(_.nonEmpty)
                    if !anyContent then Server.Served.Empty
                    else
                        Server.Served.Reply(
                          Population.New(
                            get.batchNum,
                            block,
                            stack,
                            requests,
                            softAcks,
                            headHardAcks,
                            coilHardAcks
                          )
                        )
        }

    private val server =
        new Server[Population.Get, Population.New]("Population.Get", serve)(n =>
            getConnections.flatMap(_.remote ! n)
        )

    /** Route an artifact relayed to this hub onto its outbox lane, keyed by embedded author. */
    private def appendArtifact(
        artifact: BlockBrief.Next | StackBrief | UserRequestWithId | SoftAck | HardAck |
            HardAckWithId
    ): IO[Unit] = artifact match {
        case b: BlockBrief.Next => blockLane.append(b)
        case s: StackBrief      => stackLane.append(s)
        case r: UserRequestWithId =>
            requestLanes.get(r.requestId.peerNum).traverse_(_.append(r))
        case sa: SoftAck => softAckLanes.get(sa.peerNum).traverse_(_.append(sa))
        case ha: HardAck =>
            ha.peerId match {
                case PeerId.Head(n) => headHardAckLanes.get(n).traverse_(_.append(ha))
                case PeerId.Coil(_) =>
                    IO.raiseError(
                      new IllegalStateException(
                        s"raw coil hard-ack appended to hub outbox: ${ha.peerId}"
                      )
                    )
            }
        case hub: HardAckWithId => coilHardAckLanes.get(hub.hubPeer).traverse_(_.append(hub))
    }

    // ---- Pull half (the coil peer's own hard-ack) -----------------------------------------------
    private val initialGet: OwnHardAck.Get = OwnHardAck.Get(BatchNumber.zero, HardAckNumber.zero)

    private def buildGet(batchNum: BatchNumber): IO[OwnHardAck.Get] =
        ownHardAckLane.cursor.map(OwnHardAck.Get(batchNum, _))

    private def accept(own: OwnHardAck.New): IO[Either[String, Unit]] =
        ownHardAckLane.cursor.flatMap { c =>
            ownHardAckLane.verify(own.hardAck.toList, c) match {
                // CR8: persist the coil peer's inbound hard-ack BEFORE advancing the cursor — we
                // can't lose it if the hub crashes before CoilAckSequencer re-sequences it.
                case Right(next) =>
                    persistInbound(own) >> ownHardAckLane.advanceTo(next).as(Right(()))
                case Left(m) => IO.pure(Left(m.toString))
            }
        }

    /** Persist the coil peer's inbound hard-ack to its `HardAck` receive lane (keyed by the BOUND
      * coil — this liaison's trust boundary — not the ack's self-reported author), receipt-stamped,
      * before the cursor advances. A missing ack is a no-op.
      */
    private def persistInbound(own: OwnHardAck.New): IO[Unit] =
        own.hardAck.traverse_ { ack =>
            persistence.arrivalStamp.flatMap { stamp =>
                persistence.write(
                  WriteBatch.start
                      .put(JournalKey.HardAck(PeerId.Coil(coil), ack.hardAckNum))(
                        JournalValue(stamp, ack)
                      )
                )
            }
        }

    /** The coil peer's own hard-ack → the hub's quorum (`SlowConsensusActor`) and
      * `CoilAckSequencer` (which re-sequences it onto this hub's `HubHardAckLane`).
      */
    private def dispatch(own: OwnHardAck.New): IO[Unit] =
        getConnections.flatMap { conn =>
            own.hardAck.traverse_(ack =>
                (conn.slowConsensusActor ! ack) >> (conn.coilAckSequencer ! ack)
            )
        }

    private val puller = new Puller[OwnHardAck.Get, OwnHardAck.New](
      initialGet = initialGet,
      buildGet = buildGet,
      accept = accept,
      dispatch = dispatch,
      numberOfBatchRequest = _.batchNum,
      numberOfBatch = _.batchNum,
      tracer = tracer
    )(g => getConnections.flatMap(_.remote ! g))

    // ---- Actor shell ----------------------------------------------------------------------------
    override def preStart: IO[Unit] = context.self ! PreStart

    override def receive: Receive[IO, HubToCoilRequest] =
        PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: HubToCoilRequest): IO[Unit] = req match {
        case PreStart            => preStartLocal
        case ResendCurrent       => puller.resend
        case get: Population.Get => server.handleGet(get)
        case own: OwnHardAck.New => puller.handleReply(own)
        case artifact @ (_: BlockBrief.Next | _: StackBrief | _: UserRequestWithId | _: SoftAck |
            _: HardAck | _: HardAckWithId) =>
            appendArtifact(artifact) >> server.afterAppend
    }

    /** Restore each population outbox lane's high-water from its backing journal, leaving the
      * queues empty. The Server half answers the coil peer's `Population.Get` by hot-loading older
      * entries from the store (`LaneOutgoingBacking.backfill`); live `CoilRelay` production
      * re-appends the tail. An empty store leaves every lane cold.
      */
    private def restoreHighWaters: IO[Unit] =
        for {
            _ <- blockBacking.highWater.flatMap(blockLane.seedHighWater)
            _ <- stackBacking.highWater.flatMap(stackLane.seedHighWater)
            _ <- requestLanes.toList.traverse_ { case (h, l) =>
                requestBackings(h).highWater.flatMap(l.seedHighWater)
            }
            _ <- softAckLanes.toList.traverse_ { case (h, l) =>
                softAckBackings(h).highWater.flatMap(l.seedHighWater)
            }
            _ <- headHardAckLanes.toList.traverse_ { case (h, l) =>
                headHardAckBackings(h).highWater.flatMap(l.seedHighWater)
            }
            _ <- coilHardAckLanes.toList.traverse_ { case (h, l) =>
                coilHardAckBackings(h).highWater.flatMap(l.seedHighWater)
            }
        } yield ()

    /** Restore the inbound coil-ack receive cursor to `next(max(coil HardAck))` (CR8 persisted each
      * ack before the cursor advanced), so we re-pull only NEW acks: the hub's `CoilAckSequencer`
      * stamps any received-but-unstamped tail from the store, and `verify` rejects a stale re-serve
      * — we must not re-receive (and re-stamp) what we already hold. A single inbound lane (the
      * coil peer's own hard-acks), so one cursor.
      */
    private def restoreInboundCursors: IO[Unit] =
        for {
            _ <- LaneIncomingCursors
                .hardAck(backend, PeerId.Coil(coil))
                .flatMap(ownHardAckLane.restoreCursor)
        } yield ()

    private def preStartLocal: IO[Unit] =
        for {
            c <- resolveConnections
            _ <- connections.set(Some(c))
            _ <- tracer.traceWith(PeerLiaisonEvent.Started)
            // Restore each lane's high-water; the Server half hot-loads older population entries
            // from the store on the coil peer's Population.Get, and live CoilRelay production
            // re-appends the tail. An empty store leaves every lane cold.
            _ <- restoreHighWaters
            // Restore the inbound coil-ack receive cursor so we re-pull only NEW acks.
            _ <- restoreInboundCursors
            _ <- puller.start
            _ <- startResendTimer
        } yield ()

    private def startResendTimer: IO[Unit] =
        (IO.sleep(
          config.peerLiaisonResendInterval
        ) >> (context.self ! ResendCurrent)).foreverM.start.void
}

object PeerLiaisonHubToCoil {
    def apply(
        config: Config,
        coil: CoilPeerNumber,
        pendingConnections: HeadMultisigRegimeManager.PendingConnections | Connections,
        tracer: ContraTracer[IO, PeerLiaisonEvent],
        persistence: Persistence[IO]
    ): IO[PeerLiaisonHubToCoil] =
        IO(new PeerLiaisonHubToCoil(config, coil, pendingConnections, tracer, persistence) {})

    type Config =
        OwnPeerPublic.Section & NodeOperationMultisigConfig.Section & HeadConfig.Bootstrap.Section

    type Handle = ActorRef[IO, LiaisonProtocol.HubToCoilRequest]

    /** The hub's quorum + relay-sequencer for the coil peer's inbound hard-ack, plus the send path
      * to the coil peer's counterpart liaison.
      */
    final case class Connections(
        slowConsensusActor: SlowConsensusActor.Handle,
        coilAckSequencer: CoilAckSequencer.Handle,
        remote: LiaisonProtocol.CoilToHubHandle
    )

}
