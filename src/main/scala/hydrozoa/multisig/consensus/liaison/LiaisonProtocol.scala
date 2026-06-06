package hydrozoa.multisig.consensus.liaison

import cats.effect.IO
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckWithId, SoftAck}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.stack.StackBrief

/** The message protocol shared by the three liaison actors (§8.5 of `design/coil-network.md`).
  *
  * Each actor's `Request` and `Handle` live here rather than in its own companion so that the
  * actors — whose remote handles reference each other (`CoilToHub` ↔ `HubToCoil`) — can be compiled
  * independently: the type aliases break the body-level cycle.
  *
  * A liaison receives, besides the control ticks: the **batch messages** of its two link halves
  * (one pull, one serve), and the **artifacts** local actors hand it to append to an outbox lane.
  * The appended artifact carries its own author, so the actor routes it to the right per-author
  * [[Lane]] by inspecting the payload — no separate author argument.
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
        Control | BatchMessages.Mesh.Get | BatchMessages.Mesh.New |
            (BlockBrief.Next | StackBrief | UserRequestWithId | SoftAck | HardAck | HardAckWithId)

    /** Hub → coil: serves the full population pull ([[BatchMessages.Population.Get]] →
      * [[BatchMessages.Population.New]]) and pulls the coil peer's own hard-ack
      * ([[BatchMessages.OwnHardAck]]). Accepts population artifacts (from `CoilRelay`) to append.
      */
    type HubToCoilRequest =
        Control | BatchMessages.Population.Get | BatchMessages.OwnHardAck.New |
            (BlockBrief.Next | StackBrief | UserRequestWithId | SoftAck | HardAck | HardAckWithId)

    /** Coil → hub: pulls the full population ([[BatchMessages.Population.New]]) and serves the
      * hub's pull of this coil peer's own hard-ack ([[BatchMessages.OwnHardAck.Get]]). Accepts only
      * its own `HardAck` to append.
      */
    type CoilToHubRequest =
        Control | BatchMessages.Population.New | BatchMessages.OwnHardAck.Get | HardAck

    type HeadToHeadHandle = ActorRef[IO, HeadToHeadRequest]
    type HubToCoilHandle = ActorRef[IO, HubToCoilRequest]
    type CoilToHubHandle = ActorRef[IO, CoilToHubRequest]
}
