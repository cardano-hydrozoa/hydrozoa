package hydrozoa.multisig.consensus

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.stack.StackNumber
import scalus.cardano.ledger.{TransactionHash, TransactionInput}

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

    final case class InitialStackEffectsState(state: CardanoLiaison.State)
        extends CardanoLiaisonEvent

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

    final case class StackEffectsState(state: CardanoLiaison.State) extends CardanoLiaisonEvent

    case object RunEffectsStarted extends CardanoLiaisonEvent

    final case class L1StateQueryError(err: String) extends CardanoLiaisonEvent

    final case class CurrentL1State(
        time: QuantizedInstant,
        utxoIds: Set[TransactionInput],
        state: CardanoLiaison.State
    ) extends CardanoLiaisonEvent

    final case class CriticalError(msg: String) extends CardanoLiaisonEvent

    /** No direct action was due at the multisig address this pass. Later steps (finalization check,
      * rule-based handoff, skeleton resubmission) may still produce an action.
      */
    case object NoDirectActions extends CardanoLiaisonEvent

    final case class TargetUtxoStatus(targetId: TransactionInput, found: Boolean)
        extends CardanoLiaisonEvent

    /** The hard-confirmed init tx could not be submitted because its (config-baked) validity window
      * has already elapsed (`currentTime >= initializationTxEndTime`): the head can no longer be
      * initialized on the happy path. Usually means too much wall-clock passed between head-config
      * generation (which anchors the window) and stack-0 hard-confirmation — e.g. a long
      * restart/debug cycle.
      */
    final case class InitWindowElapsed(currentTime: QuantizedInstant, endTime: QuantizedInstant)
        extends CardanoLiaisonEvent

    final case class FinalizationTxStatus(hash: TransactionHash, isKnown: Boolean)
        extends CardanoLiaisonEvent

    final case class FinalizationTxQueryError(err: String) extends CardanoLiaisonEvent

    /** A learned backbone and the fallback for the treasury it spends do not have disjoint validity
      * windows (`happyPathTtl > fallbackValidityStart`) — a bad tx-timing config. The liaison stops
      * after emitting this.
      */
    final case class DisjointWindowViolation(
        treasuryUtxo: TransactionInput,
        happyPathTtl: QuantizedInstant,
        fallbackValidityStart: QuantizedInstant
    ) extends CardanoLiaisonEvent

    /** Whether the init tx is on L1, probed (step 4) when the target anchor is missing and no
      * rule-based treasury exists. `isKnown == false` triggers a full skeleton re-submission.
      */
    final case class InitTxStatus(hash: TransactionHash, isKnown: Boolean)
        extends CardanoLiaisonEvent

    final case class InitTxQueryError(err: String) extends CardanoLiaisonEvent

    /** The rule-based treasury beacon probe (step 3) failed — skipped this cycle, retried next
      * tick.
      */
    final case class RuleBasedTreasuryQueryError(err: String) extends CardanoLiaisonEvent

    /** Some actions will be submitted to L1. `hasFallback` true means at least one is a fallback or
      * silence-period noop (logged at warn level).
      */
    final case class ActionsDispatched(actions: List[CardanoLiaison.Action], hasFallback: Boolean)
        extends CardanoLiaisonEvent

    /** The rule-based treasury has been observed on L1 and the handoff to the rule-based regime has
      * been dispatched (step 3): the head has fallen into rule-based fallback. `txId` is the tx
      * that produced the observed treasury utxo (the fallback tx, or a later rule-based tx). Tests
      * can observe this to short-circuit scenarios that drift outside the modeled happy-path
      * regime.
      */
    final case class FallbackToRuleBasedDispatched(txId: TransactionHash)
        extends CardanoLiaisonEvent

    final case class TxSubmitting(txId: TransactionHash) extends CardanoLiaisonEvent

    final case class SubmissionErrors(count: Int) extends CardanoLiaisonEvent
