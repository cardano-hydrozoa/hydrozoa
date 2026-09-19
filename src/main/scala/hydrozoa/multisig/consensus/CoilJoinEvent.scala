package hydrozoa.multisig.consensus

import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import scala.concurrent.duration.FiniteDuration

/** What a coil peer reports while settling where it starts, before its actors exist (GUM-312).
  *
  * Every one of these is once-per-boot, so all of them are worth INFO — except [[StillWaiting]],
  * which is the one an operator actually needs: a cold coil blocks until its hub answers, and
  * without a line saying so the node is indistinguishable from one that is merely slow to start.
  */
sealed trait CoilJoinEvent

object CoilJoinEvent:

    /** An offer arrived and is about to be checked and adopted.
      *
      * Adopting **discards this peer's ledger and store** — see `CoilJoin.adopt` for why there is
      * nothing to preserve. Worth seeing in a log before it happens, not only after.
      */
    final case class Adopting(startStack: StackNumber) extends CoilJoinEvent

    /** The offer verified and the store is seeded; the node boots from it. */
    final case class Adopted(
        startStack: StackNumber,
        anchorBlock: BlockNumber,
        cursorBlock: BlockNumber
    ) extends CoilJoinEvent

    /** The hub answered that it has no start point to give. The ordinary answer: `reason` says
      * whether the coil was already close enough or the head had nothing to seed from.
      */
    final case class NothingToAdopt(reason: String) extends CoilJoinEvent

    /** A coil peer with history gave up waiting and booted. It catches up over the population lanes
      * and is offered a start point again on its next reconnect, so this is a delay, not a fault.
      */
    final case class HubSilent(waited: FiniteDuration) extends CoilJoinEvent

    /** A coil peer with an empty store is still waiting for its hub, and will not boot without it.
      * Repeats until the hub answers.
      */
    case object StillWaiting extends CoilJoinEvent
