package hydrozoa.multisig.ledger.l2

import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout

/** The ledger's **total** response to one command — the coordination contract (see
  * `docs/l2-ledger-command-coordination.md`). Three outcome kinds: the command [[Applied]], a
  * recoverable user-request [[Rejected]], or an [[UnrecoverableError]] the caller fail-stops on.
  * [[Applied]] and [[Rejected]] each have a concrete descendant per command (their payloads differ
  * by command). Every branch echoes the command number it answers. There is no separate error
  * channel: a rejection, a desync, and a freeze are all response branches, not exceptions.
  */
sealed trait L2LedgerResponse:
    def commandNumber: L2CommandNumber

object L2LedgerResponse:

    /** The command applied — a concrete descendant per command, since the applied payload differs.
      * A lost-ack resend replays the ledger's cached response, so the command takes effect once.
      */
    sealed trait Applied extends L2LedgerResponse

    object Applied:

        /** A `RegisterDeposit` applied: no immediate ledger effects (the deposit's L2 utxos are
          * spawned only later, when an `ApplyDepositDecisions` absorbs it).
          */
        final case class RegisterDeposit(commandNumber: L2CommandNumber) extends Applied

        /** An `ApplyDepositDecisions` applied: the evacuation diffs of the deposits it absorbed. */
        final case class ApplyDepositDecisions(
            commandNumber: L2CommandNumber,
            evacuationDiffs: Vector[EvacuationDiff]
        ) extends Applied

        /** An `ApplyTransaction` applied: the evacuation diffs of the changed utxo set plus the
          * payout obligations the transaction withdrew.
          */
        final case class ApplyTransaction(
            commandNumber: L2CommandNumber,
            evacuationDiffs: Vector[EvacuationDiff],
            payouts: Vector[Payout.Obligation]
        ) extends Applied

    /** A deterministic, **recoverable** rejection of a *user* request (register / apply-tx) — a
      * real verdict, not a transport failure. A concrete descendant per command, carrying a
      * free-form `reason` the caller invalidates the request on. A deposit-decisions command has no
      * soft rejection: its only failures are coordination bugs, surfaced as [[UnrecoverableError]].
      */
    sealed trait Rejected extends L2LedgerResponse

    object Rejected:
        final case class RegisterDeposit(commandNumber: L2CommandNumber, reason: String)
            extends Rejected
        final case class ApplyTransaction(commandNumber: L2CommandNumber, reason: String)
            extends Rejected

    /** A failure the caller cannot recover from by invalidating a request: it fail-stops (or, at
      * boot, rewinds with `restoreTo`). Unifies the coordination faults once split between the
      * decision-rejection reasons and the standalone desync / freeze branches — a decision naming
      * unknown deposit compartments, a command-number desync, a frozen ledger, or any other
      * internal ledger error. Every case echoes the command number it answers.
      */
    sealed trait UnrecoverableError extends L2LedgerResponse

    object UnrecoverableError:

        /** An `ApplyDepositDecisions` named deposit compartments the ledger never registered — a
          * JointLedger bug, never a deposit-validity failure (deposits are validated at
          * registration). Carries the offending request ids.
          */
        final case class CompartmentsNotFound(
            commandNumber: L2CommandNumber,
            requestIds: List[RequestId]
        ) extends UnrecoverableError

        /** The command number is neither fresh (`> tip + 1`) nor the cached last (`< tip`): a
          * desync. `expected` is the number the ledger wanted next (`tip + 1`); the caller derives
          * the tip as `expected − 1` and fail-stops.
          */
        final case class OutOfOrder(commandNumber: L2CommandNumber, expected: L2CommandNumber)
            extends UnrecoverableError

        /** The reply to every command that arrives *after* a decision the ledger could not apply:
          * the ledger is **frozen**, and `wrongDecisionCommandNumber` is the
          * `ApplyDepositDecisions` that broke it. Cleared only by the caller rewinding past the
          * freeze with `restoreTo`.
          */
        final case class LedgerFreeze(
            commandNumber: L2CommandNumber,
            wrongDecisionCommandNumber: L2CommandNumber
        ) extends UnrecoverableError

        /** Any other internal ledger failure — e.g. an absorbed compartment that could not be
          * merged. A ledger bug, terminal.
          */
        final case class OtherError(commandNumber: L2CommandNumber, reason: String)
            extends UnrecoverableError

/** The exact responses each command can produce: its own [[L2LedgerResponse.Applied]] descendant,
  * an optional [[L2LedgerResponse.Rejected]] descendant (user requests only), and the shared
  * [[L2LedgerResponse.UnrecoverableError]] branch. Spelled out (not a type parameter) so each
  * `L2Ledger` method states precisely what it returns.
  */
type RegisterDepositResponse =
    L2LedgerResponse.Applied.RegisterDeposit | L2LedgerResponse.Rejected.RegisterDeposit |
        L2LedgerResponse.UnrecoverableError

type ApplyDepositDecisionsResponse =
    L2LedgerResponse.Applied.ApplyDepositDecisions | L2LedgerResponse.UnrecoverableError

type ApplyTransactionResponse =
    L2LedgerResponse.Applied.ApplyTransaction | L2LedgerResponse.Rejected.ApplyTransaction |
        L2LedgerResponse.UnrecoverableError
