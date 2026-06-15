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
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HardAckWithId, HubHardAckNumber, SoftAck, SoftAckNumber}
import hydrozoa.multisig.consensus.liaison.BatchMessages.{OwnHardAck, Population}
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol.*
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{CoilAckSequencer, SlowConsensusActor, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import hydrozoa.multisig.persistence.recovery.FamilyScan
import hydrozoa.multisig.persistence.{FamilyKey, FamilyValue, Persistence, WriteBatch}

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
    pendingConnections: MultisigRegimeManager.PendingConnections | PeerLiaisonHubToCoil.Connections,
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
            case shared: MultisigRegimeManager.PendingConnections =>
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
    private val blockLane =
        LaneOutbound.contiguous[BlockBrief.Next, BlockNumber](
          _.blockNum,
          BlockNumber(1),
          _.increment
        )
    private val stackLane =
        LaneOutbound.contiguous[StackBrief, StackNumber](_.stackNum, StackNumber(1), _.increment)
    private val requestLanes: Map[HeadPeerNumber, LaneOutbound[UserRequestWithId, RequestNumber]] =
        headPeerNums.map { h =>
            h -> LaneOutbound.contiguous[UserRequestWithId, RequestNumber](
              _.requestId.requestNum,
              RequestNumber.zero,
              _.increment,
              config.peerLiaisonMaxRequestsPerBatch
            )
        }.toMap
    private val softAckLanes: Map[HeadPeerNumber, LaneOutbound[SoftAck, SoftAckNumber]] =
        headPeerNums.map { h =>
            h -> LaneOutbound.contiguous[SoftAck, SoftAckNumber](
              _.ackNum,
              SoftAckNumber.zero.increment,
              _.increment
            )
        }.toMap
    private val headHardAckLanes: Map[HeadPeerNumber, LaneOutbound[HardAck, HardAckNumber]] =
        headPeerNums.map { h =>
            h -> LaneOutbound.contiguous[HardAck, HardAckNumber](
              _.hardAckNum,
              HardAckNumber.zero,
              _.increment
            )
        }.toMap
    private val coilHardAckLanes
        : Map[HeadPeerNumber, LaneOutbound[HardAckWithId, HubHardAckNumber]] =
        hubNums.map { h =>
            h -> LaneOutbound.contiguous[HardAckWithId, HubHardAckNumber](
              _.seqNum,
              HubHardAckNumber.zero,
              _.increment
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
            val all = blockR :: stackR :: (reqR ::: saR ::: hhR ::: chR).map(_._2)
            if all.contains(LaneOutbound.OutOfBounds) then Server.Served.OutOfBounds
            else {
                def items[T](r: LaneOutbound.Reply[T]): List[T] = r match
                    case LaneOutbound.Items(xs)   => xs
                    case LaneOutbound.OutOfBounds => Nil
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
        }

    private val server =
        new Server[Population.Get, Population.New](serve)(n => getConnections.flatMap(_.remote ! n))

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

    /** Persist the coil peer's inbound hard-ack to its [[FamilyKey.CoilHardAck]] receive lane,
      * receipt- stamped, before the cursor advances. A missing ack is a no-op.
      */
    private def persistInbound(own: OwnHardAck.New): IO[Unit] =
        own.hardAck.traverse_ { ack =>
            persistence.arrivalStamp.flatMap { stamp =>
                persistence.write(
                  WriteBatch.start
                      .put(FamilyKey.CoilHardAck(coil, ack.hardAckNum))(FamilyValue(stamp, ack))
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

    /** Seed every population outbox lane from a recovered [[PeerLiaisonHubToCoil.OutboxSeed]]:
      * refill each lane's queue + high-water so the coil peer can re-pull the pre-crash prefix the
      * moment it reconnects, rather than waiting for `CoilRelay` to refill the lanes from live
      * production.
      */
    private def seedOutbox(seed: PeerLiaisonHubToCoil.OutboxSeed): IO[Unit] =
        blockLane.seed(seed.blocks) >>
            stackLane.seed(seed.stacks) >>
            requestLanes.toList.traverse_ { case (h, l) =>
                l.seed(seed.requests.getOrElse(h, Nil))
            } >>
            softAckLanes.toList.traverse_ { case (h, l) =>
                l.seed(seed.softAcks.getOrElse(h, Nil))
            } >>
            headHardAckLanes.toList.traverse_ { case (h, l) =>
                l.seed(seed.headHardAcks.getOrElse(h, Nil))
            } >>
            coilHardAckLanes.toList.traverse_ { case (h, l) =>
                l.seed(seed.coilHardAcks.getOrElse(h, Nil))
            }

    private def preStartLocal: IO[Unit] =
        for {
            c <- resolveConnections
            _ <- connections.set(Some(c))
            _ <- tracer.traceWith(PeerLiaisonEvent.Started)
            // Refill the population outbox from the store (the full population the hub already holds
            // as a head-mesh member). The Server half answers the coil peer's Population.Get from
            // these lanes; a cold outbox could not serve a catch-up until CoilRelay refilled it. An
            // empty store seeds empty lanes, so a cold boot is unaffected.
            seed <- PeerLiaisonHubToCoil.recover(persistence, headPeerNums, hubNums)
            _ <- seedOutbox(seed)
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
        pendingConnections: MultisigRegimeManager.PendingConnections | Connections,
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

    /** The hub's full-population outbox restored from the store on boot — every author's entries in
      * ascending number order. [[recover]] rebuilds it; `preStartLocal` seeds the outbox lanes from
      * it (the cold value is all empty).
      */
    final case class OutboxSeed(
        blocks: List[BlockBrief.Next],
        stacks: List[StackBrief],
        requests: Map[HeadPeerNumber, List[UserRequestWithId]],
        softAcks: Map[HeadPeerNumber, List[SoftAck]],
        headHardAcks: Map[HeadPeerNumber, List[HardAck]],
        coilHardAcks: Map[HeadPeerNumber, List[HardAckWithId]]
    )

    /** Reconstruct the hub's full-population outbox after a crash — the population it serves a coil
      * peer via `Population.Get`. Unlike a head-mesh liaison (which serves only its own
      * production), a hub serves **every** author's entries, all of which it already persists as a
      * head-mesh member (own-produced via its producers; remote via
      * `PeerLiaisonHeadToHead.persistInbound`; its own `HubHardAck` via `CoilAckSequencer`; other
      * hubs' via the mesh). So no `canLead` filter — each family is read in full from its first
      * served index. **Inbound is not restored** — the coil peer's own hard-ack is re-pulled live.
      * Pure over the store; `preStartLocal` seeds.
      */
    def recover(
        persistence: Persistence[IO],
        headPeerNums: List[HeadPeerNumber],
        hubNums: List[HeadPeerNumber]
    )(using CardanoNetwork.Section): IO[OutboxSeed] =
        val backend = persistence.backend
        val kBlock = FamilyKey.Block(BlockNumber(1))
        val kStack = FamilyKey.Stack(StackNumber(1))
        for {
            blocks <- FamilyScan
                .scan(backend, kBlock)
                .map(_.map(e => kBlock.decodeValue(e.framed).payload))
            stacks <- FamilyScan
                .scan(backend, kStack)
                .map(_.map(e => kStack.decodeValue(e.framed).payload))
            requests <- headPeerNums.traverse { h =>
                val k = FamilyKey.Request(h, RequestNumber.zero)
                FamilyScan
                    .scan(backend, k)
                    .map(es => h -> es.map(e => k.decodeValue(e.framed).payload))
            }
            softAcks <- headPeerNums.traverse { h =>
                val k = FamilyKey.SoftAck(h, SoftAckNumber.zero)
                FamilyScan
                    .scan(backend, k)
                    .map(es => h -> es.map(e => k.decodeValue(e.framed).payload))
            }
            headHardAcks <- headPeerNums.traverse { h =>
                val k = FamilyKey.HardAck(h, HardAckNumber.zero)
                FamilyScan
                    .scan(backend, k)
                    .map(es => h -> es.map(e => k.decodeValue(e.framed).payload))
            }
            coilHardAcks <- hubNums.traverse { h =>
                val k = FamilyKey.HubHardAck(h, HubHardAckNumber.zero)
                FamilyScan
                    .scan(backend, k)
                    .map(es => h -> es.map(e => k.decodeValue(e.framed).payload))
            }
        } yield OutboxSeed(
          blocks = blocks,
          stacks = stacks,
          requests = requests.toMap,
          softAcks = softAcks.toMap,
          headHardAcks = headHardAcks.toMap,
          coilHardAcks = coilHardAcks.toMap
        )
}
