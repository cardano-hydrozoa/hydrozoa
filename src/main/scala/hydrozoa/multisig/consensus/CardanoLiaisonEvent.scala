package hydrozoa.multisig.consensus

import hydrozoa.multisig.ledger.stack.StackNumber
import scalus.cardano.ledger.TransactionHash

/** Typed events emitted by [[CardanoLiaison]]. Pure data; formatters in
  * [[CardanoLiaisonEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait CardanoLiaisonEvent

object CardanoLiaisonEvent:

    case object TimeoutReceived extends CardanoLiaisonEvent

    final case class StackHardConfirmedReceived(stackNum: StackNumber) extends CardanoLiaisonEvent

    /** The hard-confirmed initial stack's L1 effects (settlement + fallback overrides) have been
      * registered into the submission state machine.
      */
    case object InitialStackEffectsLearned extends CardanoLiaisonEvent

    final case class InitialStackEffectsState(stateDump: String) extends CardanoLiaisonEvent

    case object MinorOnlyStackReceived extends CardanoLiaisonEvent

    /** A hard-confirmed regular stack's L1 effects have been registered. `settlements`,
      * `fallbacks`, and `rollouts` are the counts of each effect type. `hasFinalization` is true
      * when this is the head's final stack.
      */
    final case class StackEffectsLearned(
        settlements: Int,
        fallbacks: Int,
        rollouts: Int,
        hasFinalization: Boolean
    ) extends CardanoLiaisonEvent

    final case class StackEffectsState(stateDump: String) extends CardanoLiaisonEvent

    case object RunEffectsStarted extends CardanoLiaisonEvent

    final case class L1StateQueryError(err: String) extends CardanoLiaisonEvent

    final case class CurrentL1State(time: String, utxoIds: String, stateDump: String)
        extends CardanoLiaisonEvent

    final case class CriticalError(msg: String) extends CardanoLiaisonEvent

    case object NoActionsScheduled extends CardanoLiaisonEvent

    final case class TargetUtxoStatus(targetId: String, found: Boolean) extends CardanoLiaisonEvent

    /** The hard-confirmed init tx could not be submitted because its (config-baked) validity window
      * has already elapsed (`currentTime >= initializationTxEndTime`): the head can no longer be
      * initialized on the happy path. Usually means too much wall-clock passed between head-config
      * generation (which anchors the window) and stack-0 hard-confirmation — e.g. a long
      * restart/debug cycle.
      */
    final case class InitWindowElapsed(currentTime: String, endTime: String)
        extends CardanoLiaisonEvent

    final case class FinalizationTxStatus(hash: String, isKnown: String) extends CardanoLiaisonEvent

    final case class FinalizationTxQueryError(err: String) extends CardanoLiaisonEvent

    /** Some actions will be submitted to L1. `hasFallback` true means at least one is a fallback or
      * silence-period noop (logged at warn level).
      */
    final case class ActionsDispatched(msgs: List[String], hasFallback: Boolean)
        extends CardanoLiaisonEvent

    /** A `FallbackToRuleBased` action has been selected for submission: the head is flipping into
      * rule-based fallback. Distinct from `ActionsDispatched.hasFallback`, which also covers
      * `SilencePeriodNoop`. Tests can observe this to short-circuit scenarios that drift outside
      * the modeled happy-path regime.
      */
    final case class FallbackToRuleBasedDispatched(txId: TransactionHash)
        extends CardanoLiaisonEvent

    final case class TxSubmitting(txId: TransactionHash) extends CardanoLiaisonEvent

    final case class SubmissionErrors(count: Int) extends CardanoLiaisonEvent
