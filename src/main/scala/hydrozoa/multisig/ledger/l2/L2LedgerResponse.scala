package hydrozoa.multisig.ledger.l2

import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout

/** The ledger's **total** response to one command — the coordination contract (see
  * `docs/l2-ledger-command-coordination.md`). Four outcome kinds; the [[Applied]] and [[Rejected]]
  * kinds each have a concrete descendant per command (their payloads differ by command). Every
  * branch echoes the command number it answers. There is no separate error channel: a rejection, a
  * desync, and a freeze are all response branches, not exceptions.
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

    /** A deterministic rejection — a real verdict, not a transport failure. A concrete descendant
      * per command, mirroring [[Applied]]: a `RegisterDeposit`/`ApplyTransaction` carries a
      * free-form `reason` the caller invalidates the request on; an `ApplyDepositDecisions` carries
      * a typed [[DepositDecisionRejectReason]] the caller panics on.
      */
    sealed trait Rejected extends L2LedgerResponse

    object Rejected:
        final case class RegisterDeposit(commandNumber: L2CommandNumber, reason: String)
            extends Rejected
        final case class ApplyTransaction(commandNumber: L2CommandNumber, reason: String)
            extends Rejected
        final case class ApplyDepositDecisions(
            commandNumber: L2CommandNumber,
            reason: DepositDecisionRejectReason
        ) extends Rejected

    /** Why the ledger rejected an `ApplyDepositDecisions` — always a coordination bug, never a
      * deposit-validity failure (deposits are validated at registration). The caller panics on
      * either for now; later it may branch (retry the recoverable one, fall back to the L1
      * rule-based regime on the terminal one).
      */
    sealed trait DepositDecisionRejectReason

    object DepositDecisionRejectReason:
        /** The decision named a deposit compartment the ledger never registered — a JointLedger
          * bug, recoverable.
          */
        final case class CompartmentNotFound(requestId: RequestId)
            extends DepositDecisionRejectReason

        /** The ledger could not merge an absorbed compartment — a ledger bug, terminal. */
        final case class InternalLedgerError(message: String) extends DepositDecisionRejectReason

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

/** The exact responses each command can produce: its own [[L2LedgerResponse.Applied]] and
  * [[L2LedgerResponse.Rejected]] descendants, plus the two shared coordination branches. Spelled
  * out (not a type parameter) so each `L2Ledger` method states precisely what it returns.
  */
type RegisterDepositResponse =
    L2LedgerResponse.Applied.RegisterDeposit | L2LedgerResponse.Rejected.RegisterDeposit |
        L2LedgerResponse.OutOfOrder | L2LedgerResponse.LedgerFreeze

type ApplyDepositDecisionsResponse =
    L2LedgerResponse.Applied.ApplyDepositDecisions |
        L2LedgerResponse.Rejected.ApplyDepositDecisions | L2LedgerResponse.OutOfOrder |
        L2LedgerResponse.LedgerFreeze

type ApplyTransactionResponse =
    L2LedgerResponse.Applied.ApplyTransaction | L2LedgerResponse.Rejected.ApplyTransaction |
        L2LedgerResponse.OutOfOrder | L2LedgerResponse.LedgerFreeze
