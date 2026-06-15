package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.CoilAckSequencer.*
import hydrozoa.multisig.consensus.CoilAckSequencerEvent.SequencedCoilAck
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckWithId, HubHardAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}

/** The hub-side relay sequencer for coil peer hard-acks — analogous to the request sequencer
  * ([[RequestSequencer]]).
  *
  * A hub head peer's [[PeerLiaisonHubToCoil]]s hand it the coil peer hard-acks they receive — each
  * exactly once, since the liaison's batch protocol dispatches a payload only when it advances the
  * cursor. It stamps each with a monotonic hub-local [[HubHardAckNumber]] and fans the resulting
  * [[HardAckWithId]] out to all the hub's [[PeerLiaisonHeadToHead]]s, which carry it on the
  * contiguous `HubHardAckLane` (§5.3 of `design/coil-network.md`) [doc-ref] — to the head-peer mesh
  * and onward to coil peers. The sequence number is transport ordering only; the embedded ack is
  * verified end-to-end by each receiving `SlowConsensusActor`.
  *
  * In-memory only for now: the durable per-coil receive log + index CF — which on crash recovery
  * lets the sequencer resume its counter without re-stamping acks the liaisons replay — is deferred
  * to the coil-persistence workstream.
  */
trait CoilAckSequencer(
    config: Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | CoilAckSequencer.Connections,
    tracer: ContraTracer[IO, CoilAckSequencerEvent]
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[CoilAckSequencer.Connections]](None)
    private val state = State()

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
                        hubAck = HardAckWithId(hubPeer = hubPeerNum, seqNum = seq, ack = ack)
                        // To the head-peer mesh (other heads) and to CoilRelay (this hub's coil
                        // peers), both carrying it on a `HubHardAckLane`.
                        _ <- tracer.traceWith(
                          SequencedCoilAck(coilNum, ack.hardAckNum, seq)
                        ) >> (conn.liaisons ! hubAck).parallel >> conn.coilRelay.traverse_(
                          _ ! hubAck
                        )
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
        pendingConnections: MultisigRegimeManager.PendingConnections,
        tracer: ContraTracer[IO, CoilAckSequencerEvent]
    ): IO[CoilAckSequencer] =
        IO(new CoilAckSequencer(config, pendingConnections, tracer) {})

    type Config = OwnPeerPublic.Section

    final case class Connections(
        liaisons: List[liaison.PeerLiaisonHeadToHead.Handle],
        coilRelay: Option[CoilRelay.Handle] = None
    )

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | HardAck

    case object PreStart
}
