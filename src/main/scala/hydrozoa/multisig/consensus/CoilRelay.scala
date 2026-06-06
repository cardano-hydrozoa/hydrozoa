package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckWithId, SoftAck}
import hydrozoa.multisig.consensus.liaison.PeerLiaisonHubToCoil
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.stack.StackBrief

/** The hub-side fan-out that distributes the whole population stream down to a hub's coil peers
  * (§8.3 of `design/coil-network.md`).
  *
  * **Stateless.** It holds no buffer and no cursors — it just forwards each artifact it is handed
  * to **every** [[PeerLiaisonHubToCoil]] the hub runs, each of which appends it to its own per-lane
  * outbox (D-coil-5: per-liaison outboxes). It exists so the core consensus actors are not relay
  * taps: an actor's job ends when its cell saturates, but a relay must keep forwarding, so relaying
  * lives here instead. It is fed by:
  *   - the hub's own producers (`JointLedger` / `StackComposer` / `EventSequencer` /
  *     `FastConsensusActor` / `SlowConsensusActor` / `CoilAckSequencer`), each sending only its
  *     **own** output — one extra recipient, no received-traffic relay; and
  *   - the hub's mesh [[PeerLiaisonHeadToHead]]s, forwarding **other** head peers' inbound
  *     artifacts.
  */
abstract class CoilRelay(
    pendingConnections: MultisigRegimeManager.PendingConnections | CoilRelay.Connections
) extends Actor[IO, CoilRelay.Request] {
    private val connections = Ref.unsafe[IO, Option[CoilRelay.Connections]](None)

    private def getConnections: IO[CoilRelay.Connections] =
        connections.get.flatMap(
          _.fold(IO.raiseError(java.lang.Error("CoilRelay missing its connections.")))(IO.pure)
        )

    private def resolveConnections: IO[CoilRelay.Connections] = pendingConnections match {
        case shared: MultisigRegimeManager.PendingConnections =>
            shared.get.map(s => CoilRelay.Connections(coilPeerLiaisons = s.coilPeerLiaisons))
        case own: CoilRelay.Connections => IO.pure(own)
    }

    override def preStart: IO[Unit] = context.self ! CoilRelay.PreStart

    override def receive: Receive[IO, CoilRelay.Request] =
        PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: CoilRelay.Request): IO[Unit] = req match {
        case CoilRelay.PreStart => resolveConnections.flatMap(c => connections.set(Some(c)))
        case artifact: CoilRelay.Artifact =>
            getConnections.flatMap(c => (c.coilPeerLiaisons ! artifact).parallel)
    }
}

object CoilRelay {
    def apply(
        pendingConnections: MultisigRegimeManager.PendingConnections | Connections
    ): IO[CoilRelay] =
        IO(new CoilRelay(pendingConnections) {})

    case object PreStart

    /** The population artifacts a relay forwards. Each is also a member of
      * [[LiaisonProtocol.HubToCoilRequest]], so it can be sent straight to a
      * `PeerLiaisonHubToCoil`.
      */
    type Artifact =
        BlockBrief.Next | StackBrief | UserRequestWithId | SoftAck | HardAck | HardAckWithId

    type Request = PreStart.type | Artifact

    type Handle = ActorRef[IO, Request]

    final case class Connections(coilPeerLiaisons: List[PeerLiaisonHubToCoil.Handle])
}
