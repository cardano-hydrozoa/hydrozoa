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
import hydrozoa.multisig.consensus.liaison.BatchMessages.Mesh
import hydrozoa.multisig.consensus.liaison.LiaisonProtocol.*
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber, PeerId}
import hydrozoa.multisig.consensus.{BlockWeaver, CoilRelay, FastConsensusActor, SlowConsensusActor, StackComposer, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import hydrozoa.multisig.persistence.recovery.OutboxBacking
import hydrozoa.multisig.persistence.{FamilyKey, FamilyValue, Persistence, WriteBatch}

/** A head peer's mesh liaison toward one other head peer (§5.5 of `design/coil-network.md`)
  * [doc-ref].
  *
  * Symmetric and full-duplex: each side serves its **own** production and pulls the remote head
  * peer's. Six bidirectional [[LaneBidirectional]] lanes — block + stack briefs are **sparse**
  * (only the round-robin leader emits, so the successor is the leader schedule), the request /
  * soft-ack / head-hard-ack / `HubHardAck` lanes are contiguous. Built by composition (a [[Puller]]
  * over the inbound side of the lanes + a [[Server]] over their outboxes), with no fat base.
  */
abstract class PeerLiaisonHeadToHead(
    config: PeerLiaisonHeadToHead.Config,
    remoteHead: HeadPeerId,
    pendingConnections: MultisigRegimeManager.PendingConnections |
        PeerLiaisonHeadToHead.Connections,
    tracer: ContraTracer[IO, PeerLiaisonEvent],
    persistence: Persistence[IO]
) extends Actor[IO, LiaisonProtocol.HeadToHeadRequest] {

    // `config` is a `CardanoNetwork.Section`; expose it as a given so the inbound-lane `WriteBatch`
    // codecs in `persistInbound` pick it up.
    private given CardanoNetwork.Section = config

    // The mesh liaison runs only on head peers; the satellite backings are keyed by this head peer
    // number.
    private val ownNum: HeadPeerNumber = config.ownPeerId match {
        case PeerId.Head(n) => n
        case PeerId.Coil(_) =>
            throw new IllegalStateException("PeerLiaisonHeadToHead runs only on a head peer")
    }

    /** Resolve this liaison's connections — either projected from the shared regime `Connections`
      * (the remote handle from the in-process `remoteHeadLiaisons` map) or supplied directly.
      */
    private def resolveConnections: IO[PeerLiaisonHeadToHead.Connections] =
        pendingConnections match {
            case shared: MultisigRegimeManager.PendingConnections =>
                shared.get.map(s =>
                    PeerLiaisonHeadToHead.Connections(
                      blockWeaver = s.blockWeaver,
                      consensusActor = s.consensusActor,
                      stackComposer = s.stackComposer,
                      slowConsensusActor = s.slowConsensusActor,
                      remote = s.remoteHeadLiaisons(remoteHead.peerNum),
                      coilRelay = s.coilRelay
                    )
                )
            case own: PeerLiaisonHeadToHead.Connections => IO.pure(own)
        }

    // ---- Lanes (bidirectional: outbox = our production, cursor = the remote head peer's next) ----
    // Each outbound side is backed by the family this peer's own production lives in, so a reply
    // hot-loads entries below the in-memory outbox floor and preStart restores only the high-water.
    // The spines carry every leader's brief, so their backings keep only the ones THIS peer leads;
    // the satellites are this peer's own author (CF per author).
    private val backend = persistence.backend
    private val blockBacking = OutboxBacking.block(backend, b => config.canLeadFast(b.blockNum))
    private val stackBacking = OutboxBacking.stack(backend, s => config.canLeadSlow(s.stackNum))
    private val requestBacking = OutboxBacking.request(backend, ownNum)
    private val softAckBacking = OutboxBacking.softAck(backend, ownNum)
    private val hardAckBacking = OutboxBacking.hardAck(backend, ownNum)
    private val hubHardAckBacking = OutboxBacking.hubHardAck(backend, ownNum)

    private val blockLane = LaneBidirectional.sparse[BlockBrief.Next, BlockNumber](
      numberOf = _.blockNum,
      zero = BlockNumber.zero,
      outboundNext = config.nextOwnLeaderBlock,
      inboundNext = after => Some(remoteHead.nextLeaderBlock(after)),
      load = blockBacking.load
    )
    private val stackLane = LaneBidirectional.sparse[StackBrief, StackNumber](
      numberOf = _.stackNum,
      zero = StackNumber.zero,
      outboundNext = config.nextOwnSlowLeaderStack,
      inboundNext = after => Some(remoteHead.nextSlowLeaderStack(after)),
      load = stackBacking.load
    )
    private val requestLane = LaneBidirectional.contiguous[UserRequestWithId, RequestNumber](
      _.requestId.requestNum,
      RequestNumber.zero,
      _.increment,
      config.peerLiaisonMaxRequestsPerBatch,
      load = requestBacking.load
    )
    private val softAckLane =
        LaneBidirectional.contiguous[SoftAck, SoftAckNumber](
          _.ackNum,
          SoftAckNumber.zero.increment,
          _.increment,
          load = softAckBacking.load
        )
    private val hardAckLane =
        LaneBidirectional.contiguous[HardAck, HardAckNumber](
          _.hardAckNum,
          HardAckNumber.zero,
          _.increment,
          load = hardAckBacking.load
        )
    private val hubHardAckLane =
        LaneBidirectional.contiguous[HardAckWithId, HubHardAckNumber](
          _.seqNum,
          HubHardAckNumber.zero,
          _.increment,
          load = hubHardAckBacking.load
        )

    // ---- Connections ----------------------------------------------------------------------------
    private val connections = Ref.unsafe[IO, Option[PeerLiaisonHeadToHead.Connections]](None)

    private def getConnections: IO[PeerLiaisonHeadToHead.Connections] =
        connections.get.flatMap(
          _.fold(IO.raiseError(java.lang.Error("Head↔head liaison missing its connections.")))(
            IO.pure
          )
        )

    // ---- Pull half (the remote head peer's production) ------------------------------------------
    private val initialGet: Mesh.Get = Mesh.Get(
      batchNum = BatchNumber.zero,
      block = blockInitialCursor,
      stack = stackInitialCursor,
      request = RequestNumber.zero,
      softAck = SoftAckNumber.zero.increment,
      headHardAck = HardAckNumber.zero,
      hubHardAck = HubHardAckNumber.zero
    )

    private def blockInitialCursor: BlockNumber =
        remoteHead.nextLeaderBlock(BlockNumber.zero)
    private def stackInitialCursor: StackNumber =
        remoteHead.nextSlowLeaderStack(StackNumber.zero)

    private def buildGet(batchNum: BatchNumber): IO[Mesh.Get] =
        for {
            b <- blockLane.cursor
            s <- stackLane.cursor
            r <- requestLane.cursor
            sa <- softAckLane.cursor
            hh <- hardAckLane.cursor
            hub <- hubHardAckLane.cursor
        } yield Mesh.Get(batchNum, b, s, r, sa, hh, hub)

    private def accept(m: Mesh.New): IO[Either[String, Unit]] = {
        def check[T, N](
            lane: LaneBidirectional[T, N],
            items: List[T]
        ): IO[Either[String, IO[Unit]]] =
            lane.cursor.map(c =>
                lane.verify(items, c) match {
                    case Right(next) => Right(lane.advanceTo(next))
                    case Left(mm)    => Left(mm.toString)
                }
            )
        List(
          check(blockLane, m.block.toList),
          check(stackLane, m.stack.toList),
          check(requestLane, m.requests),
          check(softAckLane, m.softAck.toList),
          check(hardAckLane, m.headHardAck.toList),
          check(hubHardAckLane, m.hubHardAck.toList)
        ).sequence.flatMap { results =>
            val (lefts, advances) = results.partitionMap(identity)
            lefts.headOption match {
                case Some(reason) => IO.pure(Left(reason))
                // CR8: persist the inbound remote lane entries BEFORE advancing the receive cursors
                // past them (write-before-advance, §4).
                case None => persistInbound(m) >> advances.sequence_.as(Right(()))
            }
        }
    }

    /** Persist the inbound remote lane entries carried by a [[Mesh.New]] before the receive cursor
      * advances past them (CR8 write-before-advance). Each entry is receipt-stamped and keyed by
      * its author. An empty batch is a no-op.
      */
    private def persistInbound(m: Mesh.New): IO[Unit] =
        persistence.arrivalStamp.flatMap { stamp =>
            def lv[P](payload: P): FamilyValue[P] = FamilyValue(stamp, payload)
            val puts: List[WriteBatch => WriteBatch] =
                List(
                  m.block.map(b => (wb: WriteBatch) => wb.put(FamilyKey.Block(b.blockNum))(lv(b))),
                  m.stack.map(s => (wb: WriteBatch) => wb.put(FamilyKey.Stack(s.stackNum))(lv(s))),
                  m.softAck.map(a =>
                      (wb: WriteBatch) => wb.put(FamilyKey.SoftAck(a.peerNum, a.ackNum))(lv(a))
                  ),
                  m.headHardAck.flatMap(a =>
                      a.peerId match {
                          case PeerId.Head(n) =>
                              Some((wb: WriteBatch) =>
                                  wb.put(FamilyKey.HardAck(n, a.hardAckNum))(lv(a))
                              )
                          case PeerId.Coil(_) => None
                      }
                  ),
                  m.hubHardAck.map(h =>
                      (wb: WriteBatch) => wb.put(FamilyKey.HubHardAck(h.hubPeer, h.seqNum))(lv(h))
                  )
                ).flatten ++ m.requests.map(r =>
                    (wb: WriteBatch) =>
                        wb.put(FamilyKey.Request(r.requestId.peerNum, r.requestId.requestNum))(
                          lv(r)
                        )
                )
            val full = puts.foldLeft(WriteBatch.start)((wb, put) => put(wb))
            IO.whenA(full.size > 0)(persistence.write(full))
        }

    private def dispatch(m: Mesh.New): IO[Unit] =
        getConnections.flatMap { conn =>
            for {
                _ <- m.block.traverse_(conn.blockWeaver ! _)
                _ <- m.stack.traverse_(conn.stackComposer ! _)
                _ <- m.requests.traverse_(conn.blockWeaver ! _)
                _ <- m.softAck.traverse_(conn.consensusActor ! _)
                _ <- m.headHardAck.traverse_(conn.slowConsensusActor ! _)
                _ <- m.hubHardAck.traverse_(hc => conn.slowConsensusActor ! hc.ack)
                // On a hub, forward this remote head peer's whole production to CoilRelay so its
                // coil peers hear the full population — including the block/stack briefs this remote
                // leads. The mesh liaisons relay remote-led briefs; the hub's own JointLedger /
                // StackComposer relay only their own-led briefs (see those sites), so every brief
                // reaches CoilRelay exactly once. They arrive in spine order — CoilRelay needs no
                // reordering; see its doc for the proof.
                _ <- conn.coilRelay.traverse_ { cr =>
                    m.block.traverse_(cr ! _) >>
                        m.stack.traverse_(cr ! _) >>
                        m.requests.traverse_(cr ! _) >>
                        m.softAck.traverse_(cr ! _) >>
                        m.headHardAck.traverse_(cr ! _) >>
                        m.hubHardAck.traverse_(cr ! _)
                }
            } yield ()
        }

    private val puller = new Puller[Mesh.Get, Mesh.New](
      initialGet = initialGet,
      buildGet = buildGet,
      accept = accept,
      dispatch = dispatch,
      numberOfBatchRequest = _.batchNum,
      numberOfBatch = _.batchNum,
      tracer = tracer
    )(g => getConnections.flatMap(_.remote ! g))

    // ---- Serve half (our own production) --------------------------------------------------------
    private def serve(get: Mesh.Get): IO[Server.Served[Mesh.New]] =
        for {
            blockR <- blockLane.reply(get.block)
            stackR <- stackLane.reply(get.stack)
            reqR <- requestLane.reply(get.request)
            saR <- softAckLane.reply(get.softAck)
            hhR <- hardAckLane.reply(get.headHardAck)
            hubR <- hubHardAckLane.reply(get.hubHardAck)
        } yield {
            val all = List(blockR, stackR, reqR, saR, hhR, hubR)
            if all.contains(LaneOutbound.OutOfBounds) then Server.Served.OutOfBounds
            else {
                def items[T](r: LaneOutbound.Reply[T]): List[T] = r match
                    case LaneOutbound.Items(xs)   => xs
                    case LaneOutbound.OutOfBounds => Nil
                if all.forall(items(_).isEmpty) then Server.Served.Empty
                else
                    Server.Served.Reply(
                      Mesh.New(
                        get.batchNum,
                        items(blockR).headOption,
                        items(stackR).headOption,
                        items(reqR),
                        items(saR).headOption,
                        items(hhR).headOption,
                        items(hubR).headOption
                      )
                    )
            }
        }

    private val server =
        new Server[Mesh.Get, Mesh.New](serve)(n => getConnections.flatMap(_.remote ! n))

    /** Append our own production (single-author = us) onto the matching outbox lane. */
    private def appendArtifact(
        artifact: BlockBrief.Next | StackBrief | UserRequestWithId | SoftAck | HardAck |
            HardAckWithId
    ): IO[Unit] = artifact match {
        case b: BlockBrief.Next   => blockLane.append(b)
        case s: StackBrief        => stackLane.append(s)
        case r: UserRequestWithId => requestLane.append(r)
        case sa: SoftAck          => softAckLane.append(sa)
        case ha: HardAck          => hardAckLane.append(ha)
        case hub: HardAckWithId   => hubHardAckLane.append(hub)
    }

    /** Restore each outbound lane's high-water from its backing family, leaving the queues empty
      * (R3): the Server half hot-loads older own-produced entries from the store on the remote's
      * `Mesh.Get`, and replay / live production re-appends the tail. The hub-hard-ack lane restores
      * on a **hub** head peer (its own `HubHardAck` is persisted by `CoilAckSequencer`); it is cold
      * on a non-hub.
      */
    private def restoreHighWaters: IO[Unit] =
        blockBacking.highWater.flatMap(blockLane.seedHighWaterOutbox) >>
            stackBacking.highWater.flatMap(stackLane.seedHighWaterOutbox) >>
            requestBacking.highWater.flatMap(requestLane.seedHighWaterOutbox) >>
            softAckBacking.highWater.flatMap(softAckLane.seedHighWaterOutbox) >>
            hardAckBacking.highWater.flatMap(hardAckLane.seedHighWaterOutbox) >>
            hubHardAckBacking.highWater.flatMap(hubHardAckLane.seedHighWaterOutbox)

    /** Restore each lane's inbound receive cursor to `next(max received from the remote)`, so on
      * reconnect we pull only NEW entries — a stale re-serve is rejected by `verify` (which would
      * otherwise re-dispatch to the consensus actors that `ReplayActor` already re-fed, CR8). The
      * satellites read the **remote**'s own-keyed family; the sparse spines read the overall family
      * max (the inbound lane's leader-schedule successor picks the remote's next-led number). An
      * empty store leaves a lane at its cold initial cursor.
      */
    private def restoreInboundCursors: IO[Unit] =
        val remoteNum = remoteHead.peerNum
        OutboxBacking.block(backend, _ => true).highWater.flatMap(blockLane.restoreInbound) >>
            OutboxBacking.stack(backend, _ => true).highWater.flatMap(stackLane.restoreInbound) >>
            OutboxBacking.request(backend, remoteNum).highWater.flatMap(requestLane.restoreInbound) >>
            OutboxBacking.softAck(backend, remoteNum).highWater.flatMap(softAckLane.restoreInbound) >>
            OutboxBacking.hardAck(backend, remoteNum).highWater.flatMap(hardAckLane.restoreInbound) >>
            OutboxBacking
                .hubHardAck(backend, remoteNum)
                .highWater
                .flatMap(hubHardAckLane.restoreInbound)

    // ---- Actor shell ----------------------------------------------------------------------------
    override def preStart: IO[Unit] = context.self ! PreStart

    override def receive: Receive[IO, HeadToHeadRequest] =
        PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: HeadToHeadRequest): IO[Unit] = req match {
        case PreStart      => preStartLocal
        case ResendCurrent => puller.resend
        case get: Mesh.Get => server.handleGet(get)
        case m: Mesh.New   => puller.handleReply(m)
        case artifact @ (_: BlockBrief.Next | _: StackBrief | _: UserRequestWithId | _: SoftAck |
            _: HardAck | _: HardAckWithId) =>
            appendArtifact(artifact) >> server.afterAppend
    }

    private def preStartLocal: IO[Unit] =
        for {
            c <- resolveConnections
            _ <- connections.set(Some(c))
            _ <- tracer.traceWith(PeerLiaisonEvent.Started)
            // Restore each lane's own-produced high-water; the Server half hot-loads older entries
            // from the store on the remote's Mesh.Get, and replay / live production re-appends the
            // tail. An empty store leaves every lane cold.
            _ <- restoreHighWaters
            // Restore the inbound receive cursors so we re-pull only NEW remote entries.
            _ <- restoreInboundCursors
            _ <- puller.start
            _ <- startResendTimer
        } yield ()

    private def startResendTimer: IO[Unit] =
        (IO.sleep(
          config.peerLiaisonResendInterval
        ) >> (context.self ! ResendCurrent)).foreverM.start.void
}

object PeerLiaisonHeadToHead {
    def apply(
        config: Config,
        remoteHead: HeadPeerId,
        pendingConnections: MultisigRegimeManager.PendingConnections | Connections,
        tracer: ContraTracer[IO, PeerLiaisonEvent],
        persistence: Persistence[IO]
    ): IO[PeerLiaisonHeadToHead] =
        IO(
          new PeerLiaisonHeadToHead(config, remoteHead, pendingConnections, tracer, persistence) {}
        )

    type Config =
        OwnPeerPublic.Section & NodeOperationMultisigConfig.Section & HeadConfig.Bootstrap.Section

    type Handle = ActorRef[IO, LiaisonProtocol.HeadToHeadRequest]

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        consensusActor: FastConsensusActor.Handle,
        stackComposer: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        remote: LiaisonProtocol.HeadToHeadHandle,
        /** Present only on a hub head peer: this remote head peer's satellites are forwarded here
          * so the hub's coil peers hear the whole population. `None` on non-hub head peers.
          */
        coilRelay: Option[CoilRelay.Handle] = None
    )

}
