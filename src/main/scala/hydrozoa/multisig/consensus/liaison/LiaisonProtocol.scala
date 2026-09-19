package hydrozoa.multisig.consensus.liaison

import cats.effect.IO
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckWithId, SoftAck}
import hydrozoa.multisig.consensus.{HardConfirmedHighWater, SoftConfirmedHighWater, UserRequestWithId}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.stack.StackBrief

/** The message protocol shared by the three liaison actors (§5.5 of `docs/spec/coil-network.md`)
  * [doc-ref].
  *
  * Each actor's `Request` and `Handle` live here rather than in its own companion to cut a compile
  * cycle between the two hub↔coil liaisons. The cycle is mutual: a [[PeerLiaisonHubToCoil]] sends
  * to its coil peer, so it holds a `CoilToHubHandle`; a [[PeerLiaisonCoilToHub]] sends to its hub,
  * so it holds a `HubToCoilHandle`. If those `Handle` aliases lived in the actors' own companions,
  * typing `PeerLiaisonHubToCoil` would force resolving `PeerLiaisonCoilToHub.Handle` (a companion
  * MEMBER, not just the class type) — which forces typing `PeerLiaisonCoilToHub`, which needs
  * `PeerLiaisonHubToCoil.Handle` back: a genuine cycle that mutual companion-member access does not
  * survive.
  *
  * Hoisting the `Request` unions and `Handle` aliases here breaks it. Both actors now depend on
  * this object, and this object depends on NEITHER — a `Handle` is just `ActorRef[IO, <a union of
  * plain payload types>]`, with no reference back to the actor classes. So `LiaisonProtocol` types
  * first, then each liaison types independently against these aliases; the actor bodies still talk
  * to each other, but only through types owned by a third party neither needs compiled first. Each
  * companion re-exposes its own alias (e.g. `PeerLiaisonHubToCoil.Handle`) for ergonomics, but the
  * canonical definition is here, so resolution never bounces between the two companions.
  *
  * A liaison receives, besides the control ticks: the **batch messages** of its two link halves
  * (one pull, one serve), and the **artifacts** local actors hand it to append to an outbox lane.
  * The appended artifact carries its own author, so the actor routes it to the right per-author
  * lane by inspecting the payload — no separate author argument.
  */
object LiaisonProtocol {

    /** Start tick — sent to self at `preStart` to wire connections and open the pull chain. */
    case object PreStart

    /** Retransmit tick — periodic self-message that re-sends the outstanding pull (self-heals the
      * chain after a wire-level loss).
      */
    case object ResendCurrent

    type Control = PreStart.type | ResendCurrent.type

    /** Head ↔ head: serves + pulls one head peer's own production (the [[BatchMessages.Mesh]]
      * shape), and accepts that head peer's artifacts to append to its outbox.
      */
    type HeadToHeadRequest =
        Control | BatchMessages.Mesh.Get | BatchMessages.Mesh.New | SoftConfirmedHighWater |
            (BlockBrief.Next | StackBrief | UserRequestWithId | SoftAck | HardAck | HardAckWithId)

    /** Hub → coil: serves the full population pull ([[BatchMessages.Population.Get]] →
      * [[BatchMessages.Population.New]]) and pulls the coil peer's own hard-ack
      * ([[BatchMessages.OwnHardAck]]). Accepts population artifacts (from `CoilRelay`) to append.
      *
      * [[BatchMessages.Join.Connected]] arrives from the transport, not the wire: the hub side of
      * the link turns an accepted handshake into it so the start-point decision is made where the
      * store is, not in the transport.
      */
    type HubRequestServed =
        Control | BatchMessages.Join.Connected | BatchMessages.Population.Get |
            BatchMessages.OwnHardAck.New |
            (BlockBrief.Next | StackBrief | UserRequestWithId | SoftAck | HardAck | HardAckWithId)

    /** Coil → hub: pulls the full population ([[BatchMessages.Population.New]]) and serves the
      * hub's pull of this coil peer's own hard-ack ([[BatchMessages.OwnHardAck.Get]]). Accepts only
      * its own `HardAck` to append.
      *
      * [[BatchMessages.Join.Offer]] and [[BatchMessages.Join.NoOffer]] are here because this union
      * is also the element type of [[CoilToHubHandle]], which is the **hub's** remote-facing handle
      * — the hub sends the answer by `remote ! offer`, and the transport puts it on the wire. They
      * are in the vocabulary for the send side, not because this actor wants them.
      *
      * On the receive side the transport takes the real answer at boot, before these actors exist,
      * so one reaching the actor is a late redial and can only be declined. Removing the two cases
      * means removing them from this union, which breaks the hub's send: separating "what a coil
      * liaison accepts" from "what a hub may send a coil" is the fix, and it is a refactor of these
      * handle types rather than a tidy-up.
      *
      * It also accepts the two local confirmation notifications its pull ceilings are anchored on
      * (design/liaison-backpressure.md).
      */
    type CoilRequestServed =
        Control | BatchMessages.Join.Offer | BatchMessages.Join.NoOffer |
            BatchMessages.Population.New | BatchMessages.OwnHardAck.Get | HardAck |
            SoftConfirmedHighWater | HardConfirmedHighWater

    type HeadToHeadHandle = ActorRef[IO, HeadToHeadRequest]
    type HubToCoilHandle = ActorRef[IO, HubRequestServed]
    type CoilToHubHandle = ActorRef[IO, CoilRequestServed]
}
