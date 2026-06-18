package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager
import hydrozoa.multisig.consensus.CoilAckSequencer.*
import hydrozoa.multisig.consensus.CoilAckSequencerEvent.SequencedCoilAck
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HardAckWithId, HubHardAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.persistence.{Cf, JournalKey, JournalValue, Persistence, StoreKey, WriteBatch}
import java.nio.ByteBuffer

/** The hub-side relay sequencer for coil peer hard-acks — analogous to the request sequencer
  * ([[RequestSequencer]]).
  *
  * A hub head peer's [[hydrozoa.multisig.consensus.liaison.PeerLiaisonHubToCoil]]s hand it the coil
  * peer hard-acks they receive — each **exactly once**: the liaison's batch protocol is
  * next-expected, so a payload is dispatched only when it advances the receive cursor, and on
  * recovery that cursor is restored to the last durably received ack (so a coil never re-serves an
  * ack the hub already holds, and a stale re-serve is rejected by `verify`). It stamps each with a
  * monotonic hub-local [[HubHardAckNumber]] and fans the resulting [[HardAckWithId]] out to all the
  * hub's [[hydrozoa.multisig.consensus.liaison.PeerLiaisonHeadToHead]]s, which carry it on the
  * contiguous `HubHardAck` journal (§5.3 of `design/coil-network.md`) [doc-ref] — to the head-peer
  * mesh and onward to coil peers. The sequence number is transport ordering only; the embedded ack
  * is verified end-to-end by each receiving `SlowConsensusActor`.
  *
  * **Persistence / recovery** (`persistence-and-crash-recovery.md` §6 CoilAckSequencer). Each
  * sequenced `HardAckWithId` is written to its own-hub `HubHardAck` journal in the **same atomic
  * batch** as the per-coil-peer **stamped-high-water** mark (`StoreKey.CoilStampMark`), **before**
  * it fans out (CR4 write-before-send): a re-stamp would equivocate on the `HubHardAck` spine (two
  * `HubHardAckNumber`s for one coil ack). The receive-cursor advance (in the liaison, CR8) and the
  * stamp are separate writes, so a crash between them can leave a coil ack **durably received** (in
  * the coil's `HardAck` receive copy, persisted by the liaison) but **unstamped** — and never
  * re-served. On boot the sequencer restores only its state — `nextSeq = max(HubHardAck where hub ==
  * own) + 1` and the per-coil-peer marks from `CoilStampMark`. The unstamped gap is **replayed by
  * the `ReplayActor`**: using each coil's mark as the floor, it scans the coil's `HardAck` tail
  * above it and re-feeds those acks through the normal `HardAck` path here, closing the gap from a
  * store read rather than from a re-delivery that cannot come. No idempotency index: the restored
  * receive cursor makes re-delivery impossible, so a scalar mark per coil peer suffices.
  */
trait CoilAckSequencer(
    config: Config,
    persistence: Persistence[IO],
    pendingConnections: HeadMultisigRegimeManager.PendingConnections | CoilAckSequencer.Connections,
    tracer: ContraTracer[IO, CoilAckSequencerEvent]
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[CoilAckSequencer.Connections]](None)
    private val state = State()

    // `config` is a `CardanoNetwork.Section`; expose it as a given so the `WriteBatch.put` codecs
    // in `persistStamp` pick it up.
    private given CardanoNetwork.Section = config

    // The sequencer runs on a hub, so its own id is the hub each stamped ack is scoped to.
    private val hubPeerNum: HeadPeerNumber = config.ownPeerId match {
        case PeerId.Head(n) => n
        case PeerId.Coil(_) =>
            throw new IllegalStateException("CoilAckSequencer runs only on a hub head peer")
    }

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error("Coil ack sequencer is missing its connections to other actors.")
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: HeadMultisigRegimeManager.PendingConnections =>
            x.get.flatMap(c =>
                connections.set(
                  Some(Connections(liaisons = c.headPeerLiaisons, coilRelay = c.coilRelay))
                )
            )
        case x: CoilAckSequencer.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] = context.self ! CoilAckSequencer.PreStart

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case CoilAckSequencer.PreStart =>
            // Restore the counter + per-coil-peer marks, then wire connections. The received-but-
            // unstamped coil-ack gap is re-fed by the ReplayActor through the HardAck path below
            // (using CoilStampMark as the floor), so the sequencer itself does not scan.
            for {
                recovered <- CoilAckSequencer.recover(persistence, hubPeerNum)
                _ <- state.seed(recovered)
                _ <- initializeConnections
            } yield ()
        case ack: HardAck =>
            ack.peerId match {
                case PeerId.Head(_) =>
                    IO.raiseError(
                      new IllegalStateException(
                        s"CoilAckSequencer received a head peer hard-ack: ${ack.peerId}"
                      )
                    )
                case PeerId.Coil(coilNum) => stamp(coilNum, ack).flatMap(fanOut)
            }
    }

    /** Stamp one coil ack: assign the next `seqNum`, persist the `HardAckWithId` + the bumped
      * per-coil-peer mark in one atomic batch (CR4), advance the in-memory state, and return the
      * stamped ack for fan-out. A crash before [[fanOut]] is safe — the head mesh re-pulls the
      * durable `HubHardAck`.
      */
    private def stamp(coilNum: CoilPeerNumber, ack: HardAck): IO[HardAckWithId] =
        for {
            seq <- state.nextSeq
            marks <- state.marks
            newMarks = marks.updated(coilNum, ack.hardAckNum)
            hubAck = HardAckWithId(hubPeer = hubPeerNum, seqNum = seq, ack = ack)
            _ <- persistStamp(seq, hubAck, newMarks)
            _ <- state.commit(seq, newMarks)
            _ <- tracer.traceWith(SequencedCoilAck(coilNum, ack.hardAckNum, seq))
        } yield hubAck

    /** Persist a newly-sequenced relay ack to this hub's own `HubHardAck` journal **and** the
      * per-coil-peer stamped-high-water mark in one atomic `WriteBatch` — CR4 write-before-send
      * (durable before it fans out), and the mark stays consistent with the spine across a crash.
      */
    private def persistStamp(
        seq: HubHardAckNumber,
        hubAck: HardAckWithId,
        newMarks: Map[CoilPeerNumber, HardAckNumber]
    ): IO[Unit] =
        persistence.arrivalStamp.flatMap(stamp =>
            persistence.write(
              WriteBatch.start
                  .put(JournalKey.HubHardAck(hubPeerNum, seq))(JournalValue(stamp, hubAck))
                  .put(StoreKey.CoilStampMark)(newMarks)
            )
        )

    /** Fan a stamped relay ack out to the head-peer mesh and (on a hub) the coil relay. */
    private def fanOut(hubAck: HardAckWithId): IO[Unit] =
        getConnections.flatMap(conn =>
            (conn.liaisons ! hubAck).parallel >> conn.coilRelay.traverse_(_ ! hubAck)
        )

    private final class State {
        private val nextSeqRef = Ref.unsafe[IO, HubHardAckNumber](HubHardAckNumber.zero)
        private val marksRef =
            Ref.unsafe[IO, Map[CoilPeerNumber, HardAckNumber]](Map.empty)

        /** Seed the counter + per-coil-peer marks from recovery (boot only). */
        def seed(recovered: Recovered): IO[Unit] =
            nextSeqRef.set(recovered.nextSeq) >> marksRef.set(recovered.marks)

        def nextSeq: IO[HubHardAckNumber] = nextSeqRef.get
        def marks: IO[Map[CoilPeerNumber, HardAckNumber]] = marksRef.get

        /** Commit a stamp: advance the counter past `seq` and record the bumped marks. */
        def commit(seq: HubHardAckNumber, marks: Map[CoilPeerNumber, HardAckNumber]): IO[Unit] =
            nextSeqRef.set(seq.increment) >> marksRef.set(marks)
    }
}

object CoilAckSequencer {
    def apply(
        config: Config,
        persistence: Persistence[IO],
        pendingConnections: HeadMultisigRegimeManager.PendingConnections,
        tracer: ContraTracer[IO, CoilAckSequencerEvent]
    ): IO[CoilAckSequencer] =
        IO(new CoilAckSequencer(config, persistence, pendingConnections, tracer) {})

    // `& CardanoNetwork.Section`: the `WriteBatch.put` codecs in `persistStamp` need it (same
    // shape as `RequestSequencer.Config`).
    type Config = OwnPeerPublic.Section & CardanoNetwork.Section

    /** The sequencer's recoverable state: the next sequence number to assign and the per-coil-peer
      * stamped-high-water marks, derived from this hub's own `HubHardAck` journal and the
      * `CoilStampMark` blob (§6).
      */
    final case class Recovered(
        nextSeq: HubHardAckNumber,
        marks: Map[CoilPeerNumber, HardAckNumber]
    )

    /** Derive [[Recovered]]: `nextSeq = max(HubHardAck seqNum) + 1` (empty → `zero`, the last key
      * of the own-hub CF), and the per-coil-peer marks from the `CoilStampMark` singleton (empty →
      * no marks). Both are cheap reads — no full-journal scan.
      */
    def recover(persistence: Persistence[IO], hub: HeadPeerNumber): IO[Recovered] =
        for {
            lastSeq <- persistence.backend.lastKey(Cf.HubHardAck(hub))
            nextSeq = lastSeq.fold(HubHardAckNumber.zero)(b => decodeSeq(b).increment)
            marks <- persistence.get(StoreKey.CoilStampMark).map(_.getOrElse(Map.empty))
        } yield Recovered(nextSeq, marks)

    /** Decode a `HubHardAckNumber` from a per-author `[seqNum:4]` key (the CF is the hub, §7.1). */
    private def decodeSeq(bytes: Array[Byte]): HubHardAckNumber =
        if bytes.length != 4 then
            throw new IllegalArgumentException(
              s"HubHardAck key expected 4 bytes, got ${bytes.length}"
            )
        HubHardAckNumber(ByteBuffer.wrap(bytes).getInt)

    final case class Connections(
        liaisons: List[liaison.PeerLiaisonHeadToHead.Handle],
        coilRelay: Option[CoilRelay.Handle] = None
    )

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | HardAck

    case object PreStart
}
