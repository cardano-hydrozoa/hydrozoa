package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.ledger.stack.PartitionIndex

/** Round framing for the slow-consensus hard-ack protocol — a **consensus** concern, NOT a signing
  * one. Signing is one-shot over the flat [[StackEffectsSigningInputs]]; this only decides how
  * those signatures are *packaged and released*:
  *
  *   - **2-phase** (a settlement / finalization is present, or the initial stack): round 1 carries
  *     every effect's signature EXCEPT the unlock; round 2 carries the unlock. The unlock (the L1
  *     entry point all other effects depend on) is withheld from broadcast until local round-1
  *     confirmation, so a stalled peer can't leave the unlock actionable on L1 while the dependent
  *     effects aren't yet hard-confirmed (atomicity).
  *   - **1-phase / sole** (minor-only stack): one round over all the effect signatures.
  *
  * The round a signature lands in is a pure function of the flat inputs (which effect is the
  * unlock), so framing needs no extra peer coordination — the cross-peer determinism it relies on
  * is that of [[StackEffectsSigningInputs]] (see its "Determinism" note), not restated here.
  */
sealed trait HardAckRoundPlan

object HardAckRoundPlan {

    // Variants in chronological order: stack 0 (Initial) first, then stack 1+ (regular:
    // TwoPhase / Sole).

    /** Initial stack (stack 0): round 1 signs the locally-derived fallback; round 2 signs the
      * exogenous init tx body (the head multisig contribution) and, per peer, attaches an
      * individual `VKeyWitness` iff the init tx spends an input at that peer's own individual
      * address (see [[StackEffectsSigningInputs.spendsFromIndividualAddress]]). Handled by the same
      * 2-phase cell machinery as [[TwoPhase]].
      */
    final case class Initial(
        round1: HardAck.SigningInputs.Round1Initial,
        round2: HardAck.SigningInputs.Round2Initial
    ) extends HardAckRoundPlan

    /** 2-phase regular stack: round 1 = every effect except the unlock; round 2 = the unlock. */
    final case class TwoPhase(
        round1: HardAck.SigningInputs.Round1Regular,
        round2: HardAck.SigningInputs.Round2Regular
    ) extends HardAckRoundPlan

    /** 1-phase minor-only stack: the only round. */
    final case class Sole(sole: HardAck.SigningInputs.Sole) extends HardAckRoundPlan

    /** Frame the flat signing inputs into the round packaging.
      *
      * @throws IllegalStateException
      *   only for impossible-by-construction cases (a 2-phase stack with neither settlement nor
      *   finalization; a minor-only stack with no evac commitment).
      */
    def from(si: StackEffectsSigningInputs): HardAckRoundPlan = si match {
        case i: StackEffectsSigningInputs.Initial =>
            Initial(
              round1 = HardAck.SigningInputs.Round1Initial(fallback = i.fallback),
              round2 = HardAck.SigningInputs.Round2Initial(initTx = i.initTx)
            )

        case r: StackEffectsSigningInputs.Regular =>
            r.unlock match {
                case None =>
                    // Minor-only ⇒ exactly one TrailingMinors partition ⇒ one evac commitment.
                    val ec = r.evacCommit.getOrElse(
                      throw new IllegalStateException(
                        "HardAckRoundPlan: minor-only stack must have exactly one evac commit, " +
                            "but evacCommit is None"
                      )
                    )
                    Sole(HardAck.SigningInputs.Sole(refunds = r.refunds, evacCommit = ec))

                case Some(unlock) =>
                    // Round 1 = everything except the unlock; round 2 = the unlock.
                    val (round1Settlements, round1Finalization, unlockTx) = unlock match {
                        case StackEffectsSigningInputs.RegularUnlock.FirstSettlement =>
                            (
                              r.settlements - PartitionIndex.zero,
                              r.finalization, // signed in round 1 (settlement is the unlock)
                              r.settlements.getOrElse(
                                PartitionIndex.zero,
                                throw new IllegalStateException(
                                  "HardAckRoundPlan: FirstSettlement unlock but no settlement[0]"
                                )
                              )
                            )
                        case StackEffectsSigningInputs.RegularUnlock.Finalization =>
                            (
                              r.settlements, // empty when finalization is the unlock
                              None, // the finalization IS the round-2 unlock
                              r.finalization.getOrElse(
                                throw new IllegalStateException(
                                  "HardAckRoundPlan: Finalization unlock but no finalization"
                                )
                              )
                            )
                    }
                    TwoPhase(
                      round1 = HardAck.SigningInputs.Round1Regular(
                        settlements = round1Settlements,
                        fallbacks = r.fallbacks,
                        rollouts = r.rollouts,
                        refunds = r.refunds,
                        evacCommit = r.evacCommit,
                        finalization = round1Finalization
                      ),
                      round2 = HardAck.SigningInputs.Round2Regular(unlock = unlockTx)
                    )
            }
    }
}
