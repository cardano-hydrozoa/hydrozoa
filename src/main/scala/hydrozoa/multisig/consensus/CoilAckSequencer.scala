package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.CoilAckSequencer.*
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HardAckWithId, HubHardAckNumber}
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import hydrozoa.multisig.persistence.{Cf, LaneKey, LaneValue, Persistence}
import java.nio.ByteBuffer
import org.typelevel.log4cats.Logger

/** The hub-side relay sequencer for coil peer hard-acks — analogous to the request sequencer
  * ([[RequestSequencer]]).
  *
  * A hub head peer's [[PeerLiaisonHubToCoil]]s hand it the coil peer hard-acks they receive — each
  * exactly once, since the liaison's batch protocol dispatches a payload only when it advances the
  * cursor. It stamps each with a monotonic hub-local [[HubHardAckNumber]] and fans the resulting
  * [[HardAckWithId]] out to all the hub's [[PeerLiaisonHeadToHead]]s, which carry it on the
  * contiguous `HubHardAck` family (§5.3 of `design/coil-network.md`) [doc-ref] — to the head-peer
  * mesh and onward to coil peers. The sequence number is transport ordering only; the embedded ack
  * is verified end-to-end by each receiving `SlowConsensusActor`.
  *
  * **Persistence / recovery** (`persistence-and-crash-recovery.md` §6 CoilAckSequencer). Each
  * sequenced `HardAckWithId` is written to its own-hub `HubHardAck` family **before** it fans out
  * (CR4 write-before-send): a re-stamp would equivocate on the `HubHardAck` spine (two
  * `HubHardAckNumber`s for one coil ack). On boot the sequencer derives `nextSeq = max(HubHardAck
  * where hub == own) + 1` (no stored counter, like `RequestSequencer`'s `max(own Request) + 1`) and
  * rebuilds a per-coil idempotency index `(coil, hardAckNum) → seq` from the own-hub `HubHardAck`
  * values, so a coil ack a liaison re-delivers after a restart is **re-emitted under its original
  * `seqNum`**, never re-stamped.
  */
trait CoilAckSequencer(
    config: Config,
    persistence: Persistence[IO],
    pendingConnections: MultisigRegimeManager.PendingConnections | CoilAckSequencer.Connections
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[CoilAckSequencer.Connections]](None)
    private val state = State()

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
            // Restore the counter + idempotency index from this hub's own HubHardAck family before
            // accepting any coil acks, then wire up connections.
            CoilAckSequencer
                .recover(persistence, hubPeerNum)
                .flatMap(state.seed) >> initializeConnections
        case ack: HardAck =>
            ack.peerId match {
                case PeerId.Head(_) =>
                    IO.raiseError(
                      new IllegalStateException(
                        s"CoilAckSequencer received a head peer hard-ack: ${ack.peerId}"
                      )
                    )
                case PeerId.Coil(coilNum) =>
                    for {
                        conn <- getConnections
                        // Idempotency: a coil ack a liaison re-delivers after a restart keeps its
                        // original seqNum — never re-stamped (would equivocate on the HubHardAck
                        // spine). A first-seen ack is stamped, persisted (CR4), and indexed before
                        // it leaves.
                        hubAck <- state.sequence(coilNum, ack)(seq => persistOwnHubAck(seq, ack))
                        _ <- logger.debug(
                          s"sequenced coil $coilNum ack ${ack.hardAckNum} as seq ${hubAck.seqNum}"
                        ) >> (conn.liaisons ! hubAck).parallel >> conn.coilRelay.traverse_(
                          _ ! hubAck
                        )
                    } yield ()
            }
    }

    /** Persist a newly-sequenced relay ack to this hub's own `HubHardAck` family, stamped at
      * creation — CR4 write-before-send (it must be durable before it fans out).
      */
    private def persistOwnHubAck(seq: HubHardAckNumber, ack: HardAck): IO[HardAckWithId] =
        val hubAck = HardAckWithId(hubPeer = hubPeerNum, seqNum = seq, ack = ack)
        persistence.arrivalStamp.flatMap(stamp =>
            persistence
                .put(LaneKey.HubHardAck(hubPeerNum, seq))(LaneValue(stamp, hubAck))
                .as(hubAck)
        )

    private final class State {
        private val nextSeq = Ref.unsafe[IO, HubHardAckNumber](HubHardAckNumber.zero)
        private val index =
            Ref.unsafe[IO, Map[(CoilPeerNumber, HardAckNumber), HubHardAckNumber]](Map.empty)

        /** Seed the counter + idempotency index from recovery (boot only). */
        def seed(recovered: Recovered): IO[Unit] =
            nextSeq.set(recovered.nextSeq) >> index.set(recovered.index)

        /** Sequence a coil ack idempotently: re-emit an already-seen `(coil, hardAckNum)` under its
          * original `seqNum`; otherwise assign the next `seqNum`, run `persist` (CR4) before
          * recording it, and index it.
          */
        def sequence(coilNum: CoilPeerNumber, ack: HardAck)(
            persist: HubHardAckNumber => IO[HardAckWithId]
        ): IO[HardAckWithId] =
            val key = (coilNum, ack.hardAckNum)
            index.get.flatMap(_.get(key) match {
                case Some(seq) =>
                    IO.pure(HardAckWithId(hubPeer = hubPeerNum, seqNum = seq, ack = ack))
                case None =>
                    for {
                        seq <- nextSeq.getAndUpdate(_.increment)
                        hubAck <- persist(seq)
                        _ <- index.update(_.updated(key, seq))
                    } yield hubAck
            })
    }
}

object CoilAckSequencer {
    def apply(
        config: Config,
        persistence: Persistence[IO],
        pendingConnections: MultisigRegimeManager.PendingConnections
    ): IO[CoilAckSequencer] =
        IO(new CoilAckSequencer(config, persistence, pendingConnections) {})

    type Config = OwnPeerPublic.Section

    /** The sequencer's recoverable state: the next sequence number to assign and the per-coil
      * idempotency index, both derived from this hub's own `HubHardAck` family (§6).
      */
    final case class Recovered(
        nextSeq: HubHardAckNumber,
        index: Map[(CoilPeerNumber, HardAckNumber), HubHardAckNumber]
    )

    /** Derive [[Recovered]] from this hub's own `HubHardAck` family: `nextSeq = max(seqNum) + 1`
      * (empty → `zero`), and the idempotency index by reading the contiguous `[0, max]` entries and
      * keying each by its embedded coil ack `(coil, hardAckNum)`. The family is gap-free, so the
      * range read is dense.
      */
    def recover(persistence: Persistence[IO], hub: HeadPeerNumber): IO[Recovered] =
        persistence.backend
            .lastKeyWithPrefix(Cf.HubHardAck, hubPrefix(hub))
            .flatMap {
                case None => IO.pure(Recovered(HubHardAckNumber.zero, Map.empty))
                case Some(lastKeyBytes) =>
                    val last = decodeSeq(lastKeyBytes)
                    (0 to last).toList
                        .traverse { i =>
                            val seq = HubHardAckNumber(i)
                            persistence.getOrFail(LaneKey.HubHardAck(hub, seq)).map { value =>
                                indexEntry(value.payload, seq)
                            }
                        }
                        .map(entries => Recovered(last.increment, entries.toMap))
            }

    /** The idempotency-index entry for a stored relay ack: key it by the embedded coil ack's
      * `(coil, hardAckNum)`, value its assigned `seqNum`. A head-authored ack here is store
      * corruption (only coil acks are re-sequenced).
      */
    private def indexEntry(
        hubAck: HardAckWithId,
        seq: HubHardAckNumber
    ): ((CoilPeerNumber, HardAckNumber), HubHardAckNumber) =
        hubAck.ack.peerId match {
            case PeerId.Coil(coil) => (coil, hubAck.ack.hardAckNum) -> seq
            case PeerId.Head(_) =>
                throw new IllegalStateException(
                  s"HubHardAck holds a head-authored ack: ${hubAck.ack.peerId}"
                )
        }

    /** The own-hub scan prefix `[hub:1]` for the `HubHardAck` family (mirrors the family's
      * `[hub:1][seqNum:4]` key layout, §7.1).
      */
    private def hubPrefix(hub: HeadPeerNumber): Array[Byte] =
        Array(((hub: Int) & 0xff).toByte)

    /** Decode a `HubHardAckNumber` from a `[hub:1][seqNum:4]` key — skip the hub byte. */
    private def decodeSeq(bytes: Array[Byte]): HubHardAckNumber =
        if bytes.length != 1 + 4 then
            throw new IllegalArgumentException(
              s"HubHardAck key expected ${1 + 4} bytes, got ${bytes.length}"
            )
        HubHardAckNumber(ByteBuffer.wrap(bytes, 1, 4).getInt)

    final case class Connections(
        liaisons: List[liaison.PeerLiaisonHeadToHead.Handle],
        coilRelay: Option[CoilRelay.Handle] = None
    )

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | HardAck

    case object PreStart
}
