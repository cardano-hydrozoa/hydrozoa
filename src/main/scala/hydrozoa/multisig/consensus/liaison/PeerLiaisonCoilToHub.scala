package hydrozoa.multisig.consensus.liaison

import cats.effect.{Fiber, IO, Ref}
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
import hydrozoa.multisig.consensus.{BlockWeaver, FastConsensusActor, SlowConsensusActor, StackComposer, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import hydrozoa.multisig.persistence.recovery.{LaneIncomingCursors, LaneOutgoingBacking}
import hydrozoa.multisig.persistence.{JournalKey, JournalValue, Persistence, WriteBatch}

/** A coil peer's single liaison toward its hub head peer (§5.5 of `docs/spec/coil-network.md`)
  * [doc-ref].
  *
  * Asymmetric: it **pulls the full population** from the hub (block + stack spines, per-head-peer
  * request / soft-ack / head-hard-ack lanes, per-hub coil-hard-ack lanes) and **serves only its own
  * hard-ack** to the hub. Built by composition — a [[Puller]] over the inbound population lanes and
  * a [[Server]] over the single outbound own-hard-ack lane — with no fat shared base and no on-wire
  * author check (each lane is keyed by its author, and signatures are verified end-to-end in the
  * local consensus actors).
  */
abstract class PeerLiaisonCoilToHub(
    config: PeerLiaisonCoilToHub.Config,
    pendingConnections: HeadMultisigRegimeManager.PendingConnections |
        PeerLiaisonCoilToHub.Connections,
    tracer: ContraTracer[IO, PeerLiaisonEvent],
    persistence: Persistence[IO]
) extends Actor[IO, LiaisonProtocol.CoilToHubRequest] {
    import PeerLiaisonCoilToHub.*

    private given env: Env = Env(config, tracer, persistence)

    // The coil→hub uplink runs only on a coil peer; its own-hard-ack outbox is keyed by this number.
    private val ownCoilPeerNumber: CoilPeerNumber = env.config.ownPeerId match {
        case PeerId.Coil(c) => c
        case PeerId.Head(_) =>
            throw new IllegalStateException("PeerLiaisonCoilToHub runs only on a coil peer")
    }

    private val headPeerNums: List[HeadPeerNumber] = env.config.headPeerNums.toList
    private val hubNums: List[HeadPeerNumber] = env.config.coilPeers.hubHeadPeerNumbers

    // ---- Lanes ----------------------------------------------------------------------------------
    // Inbound population (pulled from the hub): block + stack spines are contiguous (the hub relays
    // every item in order; block 0 / stack 0 are out-of-band bootstrap so the first is 1). The
    // per-author / per-hub journals are one contiguous Lane each.
    private val blockLane =
        LaneInbound.contiguous[BlockBrief.Next, BlockNumber](
          _.blockNum,
          BlockNumber(1),
          _.increment
        )
    private val stackLane =
        LaneInbound.contiguous[StackBrief, StackNumber](_.stackNum, StackNumber(1), _.increment)
    private val requestLanes: Map[HeadPeerNumber, LaneInbound[UserRequestWithId, RequestNumber]] =
        headPeerNums.map { h =>
            h -> LaneInbound.contiguous[UserRequestWithId, RequestNumber](
              _.requestId.requestNum,
              RequestNumber.zero,
              _.increment
            )
        }.toMap
    private val softAckLanes: Map[HeadPeerNumber, LaneInbound[SoftAck, SoftAckNumber]] =
        headPeerNums.map { h =>
            h -> LaneInbound.contiguous[SoftAck, SoftAckNumber](
              _.ackNum,
              SoftAckNumber.zero.increment,
              _.increment
            )
        }.toMap
    private val headHardAckLanes: Map[HeadPeerNumber, LaneInbound[HardAck, HardAckNumber]] =
        headPeerNums.map { h =>
            h -> LaneInbound.contiguous[HardAck, HardAckNumber](
              _.hardAckNum,
              HardAckNumber.zero,
              _.increment
            )
        }.toMap
    private val coilHardAckLanes
        : Map[HeadPeerNumber, LaneInbound[HardAckWithId, HubHardAckNumber]] =
        hubNums.map { h =>
            h -> LaneInbound.contiguous[HardAckWithId, HubHardAckNumber](
              _.seqNum,
              HubHardAckNumber.zero,
              _.increment
            )
        }.toMap

    // Outbound: this coil peer's own hard-ack, served to the hub. Backed by the own coil `HardAck`
    // journal so a reply hot-loads acks below the in-memory outbox floor (the hub re-pulls old acks
    // it missed during our crash); preStart restores only the high-water, replay re-appends the
    // in-flight tail.
    private val ownHardAckBacking =
        LaneOutgoingBacking.hardAck(env.persistence.backend, PeerId.Coil(ownCoilPeerNumber))
    private val ownHardAckLane =
        LaneOutbound.contiguous[HardAck, HardAckNumber](
          _.hardAckNum,
          HardAckNumber.zero,
          _.increment,
          backfill = ownHardAckBacking.backfill
        )

    // Handle to the resend-timer fiber ([[startResendTimer]]); cancelled in [[postStop]] so it
    // doesn't outlive the actor.
    private val resendFiber = Ref.unsafe[IO, Option[Fiber[IO, Throwable, Nothing]]](None)

    /** Resolve connections — projected from the shared regime `Connections` (the hub's `HubToCoil`
      * handle from `remoteHubLiaison`) or supplied directly.
      */
    private def resolveConnections: IO[PeerLiaisonCoilToHub.Connections] =
        HeadMultisigRegimeManager.resolveConnectionsF(pendingConnections)(s =>
            s.remoteHubLiaison.fold(
              IO.raiseError(
                IllegalStateException("Coil→hub liaison requires a hub liaison handle.")
              )
            )(hub =>
                IO.pure(
                  PeerLiaisonCoilToHub.Connections(
                    blockWeaver = s.blockWeaver,
                    consensusActor = s.consensusActor,
                    stackComposer = s.stackComposer,
                    slowConsensusActor = s.slowConsensusActor,
                    remote = hub
                  )
                )
            )
        )

    // ---- Pull half (population) -----------------------------------------------------------------
    private val initialGet: Population.Get = Population.Get(
      batchNum = BatchNumber.zero,
      block = BlockNumber(1),
      stack = StackNumber(1),
      requests = headPeerNums.map(_ -> RequestNumber.zero).toMap,
      softAcks = headPeerNums.map(_ -> SoftAckNumber.zero.increment).toMap,
      headHardAcks = headPeerNums.map(_ -> HardAckNumber.zero).toMap,
      coilHardAcks = hubNums.map(_ -> HubHardAckNumber.zero).toMap
    )

    private def buildGet(batchNum: BatchNumber): IO[Population.Get] =
        for {
            b <- blockLane.cursor
            s <- stackLane.cursor
            r <- requestLanes.toList.traverse { case (h, l) => l.cursor.map(h -> _) }.map(_.toMap)
            sa <- softAckLanes.toList.traverse { case (h, l) => l.cursor.map(h -> _) }.map(_.toMap)
            hh <- headHardAckLanes.toList
                .traverse { case (h, l) => l.cursor.map(h -> _) }
                .map(_.toMap)
            ch <- coilHardAckLanes.toList
                .traverse { case (h, l) => l.cursor.map(h -> _) }
                .map(_.toMap)
        } yield Population.Get(batchNum, b, s, r, sa, hh, ch)

    /** Verify every lane against its cursor; iff all match, advance them all (atomic). */
    private def accept(pop: Population.New): IO[Either[String, Unit]] = {
        def check[T, N](lane: LaneInbound[T, N], items: List[T]): IO[Either[String, IO[Unit]]] =
            lane.cursor.map(c =>
                lane.verify(items, c) match {
                    case Right(next) => Right(lane.advanceTo(next))
                    case Left(m)     => Left(m.toString)
                }
            )
        val checks: List[IO[Either[String, IO[Unit]]]] =
            check(blockLane, pop.block.toList) ::
                check(stackLane, pop.stack.toList) ::
                requestLanes.toList.map { case (h, l) =>
                    check(l, pop.requests.getOrElse(h, Nil))
                } :::
                softAckLanes.toList.map { case (h, l) =>
                    check(l, pop.softAcks.getOrElse(h, None).toList)
                } :::
                headHardAckLanes.toList.map { case (h, l) =>
                    check(l, pop.headHardAcks.getOrElse(h, None).toList)
                } :::
                coilHardAckLanes.toList.map { case (h, l) =>
                    check(l, pop.coilHardAcks.getOrElse(h, None).toList)
                }
        checks.sequence.flatMap { results =>
            val (lefts, advances) = results.partitionMap(identity)
            lefts.headOption match {
                case Some(reason) => IO.pure(Left(reason))
                // CR8: persist the inbound population entries BEFORE advancing the receive cursors
                // past them (write-before-advance, §4).
                case None => persistInbound(pop) >> advances.sequence_.as(Right(()))
            }
        }
    }

    /** Persist the inbound population entries carried by a [[Population.New]] before the receive
      * cursors advance past them (CR8 write-before-advance). Each entry is receipt-stamped and
      * keyed by its author. An empty batch is a no-op.
      */
    private def persistInbound(pop: Population.New): IO[Unit] =
        env.persistence.arrivalStamp.flatMap { stamp =>
            def lv[P](payload: P): JournalValue[P] = JournalValue(stamp, payload)
            val spinePuts: List[WriteBatch => WriteBatch] =
                List(
                  pop.block.map(b =>
                      (wb: WriteBatch) => wb.put(JournalKey.Block(b.blockNum))(lv(b))
                  ),
                  pop.stack.map(s =>
                      (wb: WriteBatch) => wb.put(JournalKey.Stack(s.stackNum))(lv(s))
                  )
                ).flatten
            val requestPuts: List[WriteBatch => WriteBatch] =
                pop.requests.values.flatten.toList.map(r =>
                    (wb: WriteBatch) =>
                        wb.put(JournalKey.Request(r.requestId.peerNum, r.requestId.requestNum))(
                          lv(r)
                        )
                )
            val softAckPuts: List[WriteBatch => WriteBatch] =
                pop.softAcks.values.flatten.toList.map(a =>
                    (wb: WriteBatch) => wb.put(JournalKey.SoftAck(a.peerNum, a.ackNum))(lv(a))
                )
            val headHardAckPuts: List[WriteBatch => WriteBatch] =
                pop.headHardAcks.values.flatten.toList.map(a =>
                    (wb: WriteBatch) => wb.put(JournalKey.HardAck(a.peerId, a.hardAckNum))(lv(a))
                )
            val coilHardAckPuts: List[WriteBatch => WriteBatch] =
                pop.coilHardAcks.values.flatten.toList.map(h =>
                    (wb: WriteBatch) => wb.put(JournalKey.HubHardAck(h.hubPeer, h.seqNum))(lv(h))
                )
            val full =
                (spinePuts ++ requestPuts ++ softAckPuts ++ headHardAckPuts ++ coilHardAckPuts)
                    .foldLeft(WriteBatch.start)((wb, put) => put(wb))
            IO.whenA(full.size > 0)(env.persistence.write(full))
        }

    /** Route a verified population reply to the local consensus actors. */
    private def dispatch(pop: Population.New)(using env: Env.Connected): IO[Unit] = {
        import env.connections
        for {
            _ <- pop.block.traverse_(connections.blockWeaver ! _)
            _ <- pop.stack.traverse_(connections.stackComposer ! _)
            _ <- pop.requests.values.toList.flatten.traverse_(connections.blockWeaver ! _)
            _ <- pop.softAcks.values.toList.flatten.traverse_(connections.consensusActor ! _)
            _ <- pop.headHardAcks.values.toList.flatten
                .traverse_(connections.slowConsensusActor ! _)
            _ <- pop.coilHardAcks.values.toList.flatten.traverse_(hc =>
                connections.slowConsensusActor ! hc.ack
            )
        } yield ()
    }

    // ---- Serve half (own hard-ack) --------------------------------------------------------------
    private def serve(get: OwnHardAck.Get): IO[Server.Served[OwnHardAck.New]] =
        ownHardAckLane.reply(get.hardAck).map {
            case LaneOutbound.OutOfBounds(asked, bound, lastAppended) =>
                Server.Served.OutOfBounds(
                  s"lane 'ownHardAck' (asked=$asked bound=$bound lastAppended=$lastAppended)"
                )
            case LaneOutbound.Items(Nil) => Server.Served.Empty
            case LaneOutbound.Items(items) =>
                Server.Served.Reply(OwnHardAck.New(get.batchNum, items.headOption))
        }

    /** The pull / serve engines, wired to the hub over the post-barrier `connections`. Built once
      * in [[initializeConnections]] (both hold single-outstanding-request state that must persist
      * across messages) and threaded through the reactive handler.
      */
    private final class Engines(using env: Env.Connected) {
        val puller: Puller[Population.Get, Population.New] =
            new Puller[Population.Get, Population.New](
              initialGet = initialGet,
              buildGet = buildGet,
              accept = accept,
              dispatch = pop => dispatch(pop),
              numberOfBatchRequest = _.batchNum,
              numberOfBatch = _.batchNum,
              tracer = env.tracer
            )(g => env.connections.remote ! g)

        val server: Server[OwnHardAck.Get, OwnHardAck.New] =
            new Server[OwnHardAck.Get, OwnHardAck.New]("OwnHardAck.Get", serve)(n =>
                env.connections.remote ! n
            )
    }

    // ---- Actor shell ----------------------------------------------------------------------------
    override def preStart: IO[Unit] = context.self ! PreStart

    override def receive: Receive[IO, CoilToHubRequest] =
        PartialFunction.fromFunction {
            case PreStart =>
                for {
                    // Suspends on the start barrier, so connections are in place before any real
                    // message is processed.
                    connected <- initializeConnections
                    engines = new Engines(using connected)
                    _ <- preStartLocal(engines)
                    _ <- context.become(PartialFunction.fromFunction(receiveConnected(engines)))
                } yield ()
            case x =>
                IO.raiseError(RuntimeException(s"Unexpected message received before PreStart: $x"))
        }

    private def receiveConnected(engines: Engines)(req: CoilToHubRequest): IO[Unit] = req match {
        case PreStart            => IO.raiseError(RuntimeException("Unexpected duplicate PreStart"))
        case ResendCurrent       => engines.puller.resend
        case pop: Population.New => engines.puller.handleReply(pop)
        case get: OwnHardAck.Get => engines.server.handleGet(get)
        case ack: HardAck        => ownHardAckLane.append(ack) >> engines.server.afterAppend
    }

    private def initializeConnections: IO[Env.Connected] =
        resolveConnections.map(summon[Env].connected)

    private def preStartLocal(engines: Engines): IO[Unit] =
        for {
            _ <- env.tracer.traceWith(PeerLiaisonEvent.Started)
            // Restore only the own-hard-ack high-water; the lane serves older acks from the own
            // coil HardAck journal on demand (the Server half answers the hub's OwnHardAck.Get) and
            // replay re-appends the in-flight tail. An empty store leaves the lane cold.
            highWater <- ownHardAckBacking.highWater
            _ <- ownHardAckLane.seedHighWater(highWater)
            // Restore each inbound population cursor to next(max received), so on reconnect we pull
            // only NEW entries — verify rejects a stale re-serve, which would otherwise re-dispatch
            // to the consensus actors that ReplayActor already re-fed (CR8 persisted each inbound
            // entry before its cursor advanced).
            _ <- restoreInboundCursors
            _ <- engines.puller.start
            _ <- startResendTimer
        } yield ()

    /** Restore the inbound population lanes' receive cursors to `next(max(persisted journal))` (the
      * full population the coil peer pulls from the hub). An empty store leaves a lane at its cold
      * initial cursor.
      */
    private def restoreInboundCursors: IO[Unit] =
        val backend = env.persistence.backend
        for {
            _ <- LaneIncomingCursors.block(backend).flatMap(blockLane.restoreCursor)
            _ <- LaneIncomingCursors.stack(backend).flatMap(stackLane.restoreCursor)
            _ <- requestLanes.toList.traverse_ { case (h, l) =>
                LaneIncomingCursors.request(backend, h).flatMap(l.restoreCursor)
            }
            _ <- softAckLanes.toList.traverse_ { case (h, l) =>
                LaneIncomingCursors.softAck(backend, h).flatMap(l.restoreCursor)
            }
            _ <- headHardAckLanes.toList.traverse_ { case (h, l) =>
                LaneIncomingCursors.hardAck(backend, PeerId.Head(h)).flatMap(l.restoreCursor)
            }
            _ <- coilHardAckLanes.toList.traverse_ { case (h, l) =>
                LaneIncomingCursors.hubHardAck(backend, h).flatMap(l.restoreCursor)
            }
        } yield ()

    private def startResendTimer: IO[Unit] =
        (IO.sleep(
          env.config.peerLiaisonResendInterval
        ) >> (context.self ! ResendCurrent)).foreverM.start
            .flatMap(fib => resendFiber.set(Some(fib)))

    /** Cancel the resend-timer fiber so it stops pinging `self` once the actor has stopped — e.g.
      * when fallback tears down the multisig regime and this liaison — instead of leaking a fiber
      * that keeps delivering `ResendCurrent` to a dead actor (dead letters).
      */
    override def postStop: IO[Unit] =
        resendFiber.getAndSet(None).flatMap(_.fold(IO.unit)(_.cancel))
}

object PeerLiaisonCoilToHub {
    def apply(
        config: Config,
        pendingConnections: HeadMultisigRegimeManager.PendingConnections | Connections,
        tracer: ContraTracer[IO, PeerLiaisonEvent],
        persistence: Persistence[IO]
    ): IO[PeerLiaisonCoilToHub] =
        IO(new PeerLiaisonCoilToHub(config, pendingConnections, tracer, persistence) {})

    type Config =
        OwnPeerPublic.Section & NodeOperationMultisigConfig.Section & HeadConfig.Bootstrap.Section

    type Handle = ActorRef[IO, LiaisonProtocol.CoilToHubRequest]

    private final case class Env(
        config: Config,
        tracer: ContraTracer[IO, PeerLiaisonEvent],
        persistence: Persistence[IO]
    ) extends CardanoNetwork.Section {
        def cardanoNetwork: CardanoNetwork = config.cardanoNetwork
        def connected(connections: Connections): Env.Connected = Env.Connected(this, connections)
    }

    private object Env {

        final class Connected(env: Env, val connections: Connections) {
            export env.*
        }
    }

    /** The local actors a verified population reply routes to, plus the send path to the hub's
      * counterpart liaison.
      */
    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        consensusActor: FastConsensusActor.Handle,
        stackComposer: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        remote: LiaisonProtocol.HubToCoilHandle
    )
}
