package hydrozoa.multisig.consensus

import hydrozoa.multisig.ledger.block.{BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.stack.StackNumber

/** Typed events emitted by [[StackComposer]]. Pure data; formatters in [[StackComposerEventFormat]]
  * decide how each variant is rendered to a particular sink.
  */
sealed trait StackComposerEvent

object StackComposerEvent:

    /** Stack 0 (init + fallback) bootstrapped and handed to SlowConsensusActor at startup. */
    case object InitialStackBootstrapped extends StackComposerEvent

    /** A committed block's evacuation-map snapshot: its `version` and the number of obligations the
      * map holds. Emitted per committed block as the running evacuation map is persisted, so an
      * observer can read the committed map size the head will resolve to under that version.
      */
    final case class CommittedMap(version: BlockVersion.Full, size: Int) extends StackComposerEvent

    /** A stack was closed and handed off to SlowConsensusActor. `isLeader` distinguishes the leader
      * deriving the brief vs. a follower accepting the leader's brief.
      */
    final case class StackClosed(
        stackNum: StackNumber,
        firstBlock: BlockNumber,
        lastBlock: BlockNumber,
        isLeader: Boolean
    ) extends StackComposerEvent

    /** The single-flight gate OPENED: `Stack.HardConfirmed` for the previous stack reached the
      * composer, so the next stack may close.
      *
      * Paired with [[SingleFlightGateClosed]], these make the gate's transitions observable. A
      * composer parked in `WaitingForPreviousHardConfirmation` otherwise gives no way to tell a
      * confirmation that never arrived from one that arrived and was then overwritten.
      */
    final case class PreviousStackHardConfirmed(stackNum: StackNumber) extends StackComposerEvent

    /** The single-flight gate CLOSED: a stack was closed, so the composer will not close another
      * until that one hard-confirms. Emitted from `afterClose`, i.e. after the brief and hard-acks
      * have already left this node.
      */
    final case class SingleFlightGateClosed(stackNum: StackNumber) extends StackComposerEvent

    /** The single-flight gate was opened from PERSISTENCE rather than from a delivered
      * `Stack.HardConfirmed`: the composer had been waiting past `HardConfirmReconcileAfterMs` and
      * the durable hard-confirmation marker had already advanced past the stack it was waiting on.
      * One of these means a message was lost — the node kept going, but the lane that lost it is
      * worth looking at.
      */
    final case class HardConfirmationReconciled(
        awaited: StackNumber,
        persisted: StackNumber
    ) extends StackComposerEvent

    /** Follower detected a structural inconsistency between the leader's brief and the local
      * single-flight position — unrecoverable; node will panic.
      */
    final case class StructuralDivergence(
        stackNum: StackNumber,
        leaderFirst: BlockNumber,
        leaderLast: BlockNumber,
        expectedFirst: BlockNumber
    ) extends StackComposerEvent
