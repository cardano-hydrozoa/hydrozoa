package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastSyntax.*
import hydrozoa.multisig.HeadMultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckWithId, SoftAck}
import hydrozoa.multisig.consensus.liaison.PeerLiaisonHubToCoil
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.stack.StackBrief

/** The hub-side fan-out that distributes the whole population stream down to a hub's coil peers
  * (§5.4 of `docs/spec/coil-network.md`) [doc-ref].
  *
  * '''Stateless — no buffer, no cursors, no reordering.''' It forwards each artifact it is handed
  * to '''every''' [[PeerLiaisonHubToCoil]] the hub runs; each liaison appends it to its own
  * per-lane outbox (per-liaison outboxes). It exists so the core consensus actors are not relay
  * taps: an actor's job ends when its cell saturates, but a relay must keep forwarding, so relaying
  * lives here.
  *
  * ==Who feeds it==
  *
  * Every population artifact reaches this relay '''exactly once'''. The per-author artifacts
  * (requests, soft-acks, hard-acks) are forwarded by whichever side owns them — the hub's own
  * producers (`FastConsensusActor` / `SlowConsensusActor` / `RequestSequencer` /
  * `CoilAckSequencer`) for its own output, the hub's mesh [[PeerLiaisonHeadToHead]]s for other
  * heads' — and each lands on a '''per-author''' lane, so every such lane has exactly one feeder.
  *
  * The block and stack briefs are different: their lanes are '''contiguous''' and every brief —
  * own-led AND remote-led — is fed by a '''single''' hub-local actor:
  *   - blocks by [[JointLedger]], which applies every block serially in spine order (its own when
  *     it leads, a reproduction when it follows);
  *   - stacks by [[StackComposer]], which closes every stack serially in spine order (single-flight
  *     on `previousStackHardConfirmed`).
  * A remote-led brief reaches that single feeder through the mesh (`dispatch` → `BlockWeaver` →
  * `JointLedger`, `dispatch` → `StackComposer`) and is relayed from there — never directly from the
  * mesh liaison.
  *
  * ==Why no reordering is needed==
  *
  * A contiguous lane must be fed in ascending spine order. It is — because a single actor feeds
  * each brief lane and emits from '''one''' fiber, so its `!` sends keep their order
  * (`ActorCell.sendMessage` enqueues without suspending). The relay's FIFO mailbox and the
  * contiguous outbox preserve that order downstream.
  *
  * That the feeder emits in spine order in the first place rests on an all-head confirmation
  * invariant: a leader cannot produce brief N+1 until brief N is confirmed by '''all''' head peers,
  * so the hub applies/closes N before N+1 exists:
  *   - Blocks: a leader seals block K only on `Block.SoftConfirmed(K-1)` ([[BlockWeaver]]
  *     `Leader.AwaitingConfirmation`), and soft-confirmation requires every head peer's ack
  *     ([[FastConsensusActor]]: a cell saturates iff `acks.keySet == headPeerVKeys`).
  *   - Stacks: a leader closes stack N+1 only once stack N is hard-confirmed ([[StackComposer]]
  *     `tryProgress` gates on `previousStackHardConfirmed`), and hard-confirmation requires every
  *     head peer ([[SlowConsensusActor]]: `AllOf(head)` ∧ a coil quorum).
  *
  * An earlier form of this proof argued only that the hub '''holds''' brief N before N+1 — true,
  * but insufficient. With two feeders (mesh liaison for remote-led, own producer for own-led),
  * held-at-the-node did not imply enqueued-at-the-relay: an own-led N+1 could overtake a remote-led
  * N whose forwarding fiber was descheduled, raising `AppendOutOfOrder` (the t3 hang). The single
  * feeder closes that gap — order at the node IS order at the relay. The contiguous
  * `LaneOutbound.append` remains the backstop: if the invariant were ever violated it raises
  * `AppendOutOfOrder` (a loud crash, never silent corruption) rather than appending out of order.
  *
  * '''Invariants that keep this true — don't break them:'''
  *   1. One feeder per brief lane: [[JointLedger]] for blocks, [[StackComposer]] for stacks, for
  *      BOTH own-led and remote-led. Never forward a block/stack brief to this relay from the mesh
  *      [[PeerLiaisonHeadToHead]] (or any second actor) — that reintroduces the race.
  *   2. Confirmation is '''all-head''' (soft: `acks.keySet == headPeerVKeys`; hard: `AllOf(head)`),
  *      and a leader gates the next brief on the prior's confirmation (no pipelining past one) — so
  *      the single feeder observes briefs in spine order to begin with.
  */
abstract class CoilRelay(
    pendingConnections: HeadMultisigRegimeManager.PendingConnections | CoilRelay.Connections
) extends Actor[IO, CoilRelay.Request] {
    private val connections = Ref.unsafe[IO, Option[CoilRelay.Connections]](None)

    private def getConnections: IO[CoilRelay.Connections] =
        connections.get.flatMap(
          _.fold(IO.raiseError(IllegalStateException("CoilRelay missing its connections.")))(
            IO.pure
          )
        )

    private def resolveConnections: IO[CoilRelay.Connections] = pendingConnections match {
        case shared: HeadMultisigRegimeManager.PendingConnections =>
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
        pendingConnections: HeadMultisigRegimeManager.PendingConnections | Connections
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
