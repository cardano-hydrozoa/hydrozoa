package hydrozoa.multisig.consensus

/** Typed events emitted by [[CardanoLiaison]]. Pure data; formatters in
  * [[CardanoLiaisonEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait CardanoLiaisonEvent

object CardanoLiaisonEvent:

    /** The hard-confirmed initial stack's L1 effects (settlement + fallback overrides) have been
      * registered into the submission state machine.
      */
    case object InitialStackEffectsLearned extends CardanoLiaisonEvent

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
