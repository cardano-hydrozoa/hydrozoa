package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.CoilAckSequencer.*
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HardAckWithId, HubHardAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.persistence.recovery.FamilyScan
import hydrozoa.multisig.persistence.{Cf, FamilyKey, FamilyValue, Markers, Persistence, StoreKey, WriteBatch}
import java.nio.ByteBuffer
import org.typelevel.log4cats.Logger

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
  * contiguous `HubHardAck` family (§5.3 of `design/coil-network.md`) [doc-ref] — to the head-peer
  * mesh and onward to coil peers. The sequence number is transport ordering only; the embedded ack
  * is verified end-to-end by each receiving `SlowConsensusActor`.
  *
  * **Persistence / recovery** (`persistence-and-crash-recovery.md` §6 CoilAckSequencer). Each
  * sequenced `HardAckWithId` is written to its own-hub `HubHardAck` family in the **same atomic
  * batch** as the per-coil **stamped-high-water** mark (`StoreKey.CoilStampMark`), **before** it
  * fans out (CR4 write-before-send): a re-stamp would equivocate on the `HubHardAck` spine (two
  * `HubHardAckNumber`s for one coil ack). The receive-cursor advance (in the liaison, CR8) and the
  * stamp are separate writes, so a crash between them can leave a coil ack **durably received**
  * (`CoilHardAck`, persisted by the liaison) but **unstamped** — and never re-served. On boot the
  * sequencer therefore (1) derives `nextSeq = max(HubHardAck where hub == own) + 1` and the per-coil
  * marks from `CoilStampMark`, then (2) **loads and stamps** the `CoilHardAck` tail above each mark
  * — closing that gap from the store rather than waiting on a re-delivery that cannot come. No
  * idempotency index: the restored receive cursor makes re-delivery impossible, so a scalar mark
  * per coil suffices.
  */
trait CoilAckSequencer(
    config: Config,
    persistence: Persistence[IO],
    pendingConnections: MultisigRegimeManager.PendingConnections | CoilAckSequencer.Connections
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

    private given logger: Logger[IO] = Logging.loggerIO(s"CoilAckSequencer.${config.ownPeerLabel}")

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error("Coil ack sequencer is missing its connections to other actors.")
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
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
            // Restore the counter + per-coil marks, wire connections, then stamp any coil acks a
            // crash left durably received but unstamped — load-and-stamp from the store, since the
            // restored receive cursor means they will never be re-served.
            for {
                recovered <- CoilAckSequencer.recover(persistence, hubPeerNum)
                _ <- state.seed(recovered)
                _ <- initializeConnections
                _ <- stampGap
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
      * per-coil mark in one atomic batch (CR4), advance the in-memory state, and return the stamped
      * ack for fan-out. A crash before [[fanOut]] is safe — the head mesh re-pulls the durable
      * `HubHardAck`.
      */
    private def stamp(coilNum: CoilPeerNumber, ack: HardAck): IO[HardAckWithId] =
        for {
            seq <- state.nextSeq
            marks <- state.marks
            newMarks = marks.updated(coilNum, ack.hardAckNum)
            hubAck = HardAckWithId(hubPeer = hubPeerNum, seqNum = seq, ack = ack)
            _ <- persistStamp(seq, hubAck, newMarks)
            _ <- state.commit(seq, newMarks)
        } yield hubAck

    /** Persist a newly-sequenced relay ack to this hub's own `HubHardAck` family **and** the
      * per-coil stamped-high-water mark in one atomic `WriteBatch` — CR4 write-before-send (durable
      * before it fans out), and the mark stays consistent with the spine across a crash.
      */
    private def persistStamp(
        seq: HubHardAckNumber,
        hubAck: HardAckWithId,
        newMarks: Map[CoilPeerNumber, HardAckNumber]
    ): IO[Unit] =
        persistence.arrivalStamp.flatMap(stamp =>
            persistence.write(
              WriteBatch.start
                  .put(FamilyKey.HubHardAck(hubPeerNum, seq))(FamilyValue(stamp, hubAck))
                  .put(StoreKey.CoilStampMark)(newMarks)
            )
        )

    /** Fan a stamped relay ack out to the head-peer mesh and (on a hub) the coil relay. */
    private def fanOut(hubAck: HardAckWithId): IO[Unit] =
        getConnections.flatMap(conn =>
            logger.debug(
              s"sequenced coil ${hubAck.ack.hardAckNum} as seq ${hubAck.seqNum}"
            ) >> (conn.liaisons ! hubAck).parallel >> conn.coilRelay.traverse_(_ ! hubAck)
        )

    /** Stamp every coil's received-but-unstamped tail. For each coil the hub serves, the gap is the
      * `CoilHardAck` entries above its stamped mark; they are durable (the liaison persisted them
      * before advancing its receive cursor, CR8) but the crash skipped stamping, and the restored
      * cursor means they will not be re-served. Runs after connections are wired so the stamped acks
      * fan out exactly as a live ack would.
      */
    private def stampGap: IO[Unit] =
        config.coilPeers.coilPeerNumbers.traverse_ { coilNum =>
            for {
                marks <- state.marks
                from = marks.get(coilNum).fold(HardAckNumber.zero)(_.increment)
                gap <- loadCoilHardAcksFrom(coilNum, from)
                _ <- gap.traverse_(ack => stamp(coilNum, ack).flatMap(fanOut))
            } yield ()
        }

    /** The coil's durably-received hard-acks with number `>= from`, ascending — the `CoilHardAck`
      * family the liaison wrote on receipt.
      */
    private def loadCoilHardAcksFrom(coilNum: CoilPeerNumber, from: HardAckNumber): IO[List[HardAck]] =
        val k = FamilyKey.CoilHardAck(coilNum, from)
        FamilyScan.scan(persistence.backend, k).map(_.map(e => k.decodeValue(e.framed).payload))

    private final class State {
        private val nextSeqRef = Ref.unsafe[IO, HubHardAckNumber](HubHardAckNumber.zero)
        private val marksRef =
            Ref.unsafe[IO, Map[CoilPeerNumber, HardAckNumber]](Map.empty)

        /** Seed the counter + per-coil marks from recovery (boot only). */
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
        pendingConnections: MultisigRegimeManager.PendingConnections
    ): IO[CoilAckSequencer] =
        IO(new CoilAckSequencer(config, persistence, pendingConnections) {})

    // `& HeadConfig.Bootstrap.Section`: for the coil peer list (`coilPeers`) the gap stamper
    // iterates, and the `CardanoNetwork.Section` the persist codecs need (Bootstrap extends it).
    type Config = OwnPeerPublic.Section & HeadConfig.Bootstrap.Section

    /** The sequencer's recoverable state: the next sequence number to assign and the per-coil
      * stamped-high-water marks, derived from this hub's own `HubHardAck` family and the
      * `CoilStampMark` blob (§6).
      */
    final case class Recovered(
        nextSeq: HubHardAckNumber,
        marks: Map[CoilPeerNumber, HardAckNumber]
    )

    /** Derive [[Recovered]]: `nextSeq = max(HubHardAck seqNum) + 1` (empty → `zero`, the last key of
      * the own-hub CF), and the per-coil marks from the `CoilStampMark` singleton (empty → no
      * marks). Both are cheap reads — no full-family scan.
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
