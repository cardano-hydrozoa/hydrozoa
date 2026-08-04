package hydrozoa.multisig.ledger.l2

import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout

/** The effects a successfully applied command produced, tagged per command — the command is known
  * from the [[L2LedgerResponse.Applied]] `commandNumber` it rides on. See
  * `docs/l2-ledger-command-coordination.md`.
  */
sealed trait AppliedEffects

object AppliedEffects:

    /** A `RegisterDeposit` produces no immediate ledger effects: the deposit's L2 utxos are spawned
      * only later, when an `ApplyDepositDecisions` absorbs it.
      */
    case object RegisterDeposit extends AppliedEffects

    /** An `ApplyDepositDecisions` produces the evacuation diffs of the deposits it absorbed. */
    final case class ApplyDepositDecisions(evacuationDiffs: Vector[EvacuationDiff])
        extends AppliedEffects

    /** An `ApplyTransaction` produces the evacuation diffs of the changed utxo set plus the payout
      * obligations the transaction withdrew.
      */
    final case class ApplyTransaction(
        evacuationDiffs: Vector[EvacuationDiff],
        payouts: Vector[Payout.Obligation]
    ) extends AppliedEffects

/** The ledger's **total** response to one command — the four-branch coordination contract (see
  * `docs/l2-ledger-command-coordination.md`). Every branch echoes the command number it answers, so
  * the caller correlates each response to the request it sent. There is no separate error channel:
  * a rejection, a desync, and a freeze are all response branches, not exceptions.
  */
sealed trait L2LedgerResponse:
    def commandNumber: L2CommandNumber

object L2LedgerResponse:

    /** The command applied at `commandNumber`; `effects` are typed per command (empty for
      * `RegisterDeposit`). A lost-ack resend replays this from the ledger's cache, so the command
      * still takes effect exactly once.
      */
    final case class Applied(commandNumber: L2CommandNumber, effects: AppliedEffects)
        extends L2LedgerResponse

    /** A deterministic rejection — a real verdict, not a transport failure. `reason` is a free-form
      * message for now (the doc's typed per-command `RejectReason` is a later refinement). The
      * caller invalidates a `RegisterDeposit`/`ApplyTransaction`, and panics on a rejected
      * `ApplyDepositDecisions`.
      */
    final case class Rejected(commandNumber: L2CommandNumber, reason: String)
        extends L2LedgerResponse

    /** The command number is neither fresh (`> tip + 1`) nor the cached last (`< tip`): a desync.
      * `expected` is the number the ledger wanted next (`tip + 1`); the caller derives the tip as
      * `expected − 1` and fail-stops.
      */
    final case class OutOfOrder(commandNumber: L2CommandNumber, expected: L2CommandNumber)
        extends L2LedgerResponse

    /** The reply to every command that arrives *after* a decision the ledger could not apply: the
      * ledger is **frozen**, and `wrongDecisionCommandNumber` is the `ApplyDepositDecisions` that
      * broke it. Cleared only by the caller rewinding past the freeze with `restoreTo`. The caller
      * fail-stops.
      */
    final case class LedgerFreeze(
        commandNumber: L2CommandNumber,
        wrongDecisionCommandNumber: L2CommandNumber
    ) extends L2LedgerResponse
