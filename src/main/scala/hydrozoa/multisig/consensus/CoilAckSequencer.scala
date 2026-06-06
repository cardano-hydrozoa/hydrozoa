package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.CoilAckSequencer.*
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckWithId, HubHardAckNumber}
import hydrozoa.multisig.consensus.peer.PeerId
import org.typelevel.log4cats.Logger

/** The hub-side relay sequencer for coil peer hard-acks — analogous to the request sequencer
  * ([[EventSequencer]]).
  *
  * A hub head peer's [[PeerLiaisonHubToCoil]]s hand it the coil peer hard-acks they receive — each
  * exactly once, since the liaison's batch protocol dispatches a payload only when it advances the
  * cursor. It stamps each with a monotonic hub-local [[HubHardAckNumber]] and fans the resulting
  * [[HardAckWithId]] out to all the hub's [[PeerLiaisonHeadToHead]]s, which carry it on the
  * contiguous `HubHardAckLane` (§8 of `design/coil-network.md`) — to the head-peer mesh and onward
  * to coil peers. The sequence number is transport ordering only; the embedded ack is verified
  * end-to-end by each receiving `SlowConsensusActor`.
  *
  * In-memory only for now: the durable per-coil receive log + index CF — which on crash recovery
  * lets the sequencer resume its counter without re-stamping acks the liaisons replay — is deferred
  * to the coil-persistence workstream.
  */
trait CoilAckSequencer(
    config: Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | CoilAckSequencer.Connections
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[CoilAckSequencer.Connections]](None)
    private val state = State()

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
            x.get.flatMap(c => connections.set(Some(Connections(liaisons = c.headPeerLiaisons))))
        case x: CoilAckSequencer.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] = context.self ! CoilAckSequencer.PreStart

    override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] = req match {
        case CoilAckSequencer.PreStart => initializeConnections
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
                        seq <- state.nextSeqNum
                        hubAck = HardAckWithId(seq, ack)
                        _ <- logger.debug(
                          s"sequenced coil $coilNum ack ${ack.hardAckNum} as seq $seq"
                        ) >> (conn.liaisons ! hubAck).parallel
                    } yield ()
            }
    }

    private final class State {
        private val nextSeq = Ref.unsafe[IO, HubHardAckNumber](HubHardAckNumber.zero)

        /** The next hub-local sequence number, advancing the counter. */
        def nextSeqNum: IO[HubHardAckNumber] = nextSeq.getAndUpdate(_.increment)
    }
}

object CoilAckSequencer {
    def apply(
        config: Config,
        pendingConnections: MultisigRegimeManager.PendingConnections
    ): IO[CoilAckSequencer] =
        IO(new CoilAckSequencer(config, pendingConnections) {})

    type Config = OwnPeerPublic.Section

    final case class Connections(liaisons: List[PeerLiaisonHeadToHead.Handle])

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | HardAck

    case object PreStart
}
