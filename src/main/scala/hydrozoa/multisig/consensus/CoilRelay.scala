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
  * '''Stateless — no buffer, no cursors, no reordering.''' It forwards each artifact it is handed
  * to '''every''' [[PeerLiaisonHubToCoil]] the hub runs; each liaison appends it to its own
  * per-lane outbox (D-coil-5: per-liaison outboxes). It exists so the core consensus actors are not
  * relay taps: an actor's job ends when its cell saturates, but a relay must keep forwarding, so
  * relaying lives here.
  *
  * ==Who feeds it==
  *
  * Every population artifact reaches this relay '''exactly once''', from whichever side owns it:
  *   - '''Own production''' — the hub's own producers (`JointLedger` / `StackComposer` /
  *     `FastConsensusActor` / `SlowConsensusActor` / `RequestSequencer` / `CoilAckSequencer`) each
  *     send only their '''own''' output (own-led briefs, own acks/requests). Not a received-traffic
  *     relay — just one extra broadcast target alongside the head mesh.
  *   - '''Other heads' output''' — the hub's mesh [[PeerLiaisonHeadToHead]]s forward what they
  *     receive from other head peers, including the block/stack briefs those heads lead.
  *
  * So a block/stack brief is relayed by the mesh liaison of the head that led it, or by the hub's
  * own JL/SC if the hub led it — never both.
  *
  * ==Why no reordering is needed==
  *
  * The block/stack lanes on each [[PeerLiaisonHubToCoil]] are '''contiguous''', so they must be fed
  * in ascending spine order. They are — not because this relay sorts anything, but because of an
  * end-to-end consensus invariant. The hub is a head peer, and a leader cannot produce artifact N+1
  * until artifact N is confirmed by '''all''' head peers:
  *   - Blocks: a leader seals block K only on `Block.SoftConfirmed(K-1)` ([[BlockWeaver]]
  *     `Leader.AwaitingConfirmation`), and soft-confirmation requires every head peer's ack
  *     ([[FastConsensusActor]]: a cell saturates iff `acks.keySet == headPeerVKeys`).
  *   - Stacks: a leader closes stack N+1 only once stack N is hard-confirmed ([[StackComposer]]
  *     `tryProgress` gates on `previousStackHardConfirmed`), and hard-confirmation requires every
  *     head peer ([[SlowConsensusActor]]: `AllOf(head)` ∧ a coil quorum).
  *
  * Since the hub is in every all-head quorum, it must have received (and acked) brief N before
  * brief N+1 can be produced anywhere — so it always holds brief N before brief N+1 exists, and
  * relays them in that order. This relay's FIFO mailbox and the contiguous outbox preserve it. The
  * contiguous `LaneOutbound.append` is the backstop: if the invariant were ever violated it raises
  * `AppendOutOfOrder` (a loud crash, never silent corruption) rather than appending out of order.
  *
  * '''Three invariants keep this true — don't break them:'''
  *   1. Confirmation is '''all-head''' (soft: `acks.keySet == headPeerVKeys`; hard: `AllOf(head)`).
  *      A `< nHead` threshold would let a hub be skipped and receive N+1 before N — then a reorder
  *      buffer here would be required.
  *   2. A leader gates the next artifact on the prior's confirmation (no pipelining past one).
  *   3. Every brief fed here is one the hub itself received or produced (mesh liaison for
  *      remote-led, own JL/SC for own-led) — don't add a feeder that forwards a brief the hub
  *      hasn't itself seen.
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
