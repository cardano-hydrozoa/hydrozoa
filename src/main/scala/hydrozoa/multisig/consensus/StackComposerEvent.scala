package hydrozoa.multisig.consensus

import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.stack.StackNumber

/** Typed events emitted by [[StackComposer]]. Pure data; formatters in [[StackComposerEventFormat]]
  * decide how each variant is rendered to a particular sink.
  */
sealed trait StackComposerEvent

object StackComposerEvent:

    /** Stack 0 (init + fallback) bootstrapped and handed to SlowConsensusActor at startup. */
    case object InitialStackBootstrapped extends StackComposerEvent

    /** A stack was closed and handed off to SlowConsensusActor. `isLeader` distinguishes the leader
      * deriving the brief vs. a follower accepting the leader's brief.
      */
    final case class StackClosed(
        stackNum: StackNumber,
        firstBlock: BlockNumber,
        lastBlock: BlockNumber,
        isLeader: Boolean
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
