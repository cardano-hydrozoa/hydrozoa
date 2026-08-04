package hydrozoa.multisig.ledger.l2

import cats.Monad
import cats.data.EitherT
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout

/** Errors occurring from interaction with the L2 Ledger (i.e., as seen from the Joint Ledger)
  */
case class L2LedgerError(message: String) extends Throwable {
    override def toString: String = s"L2 ledger error: $message"
}

/** State changes accumulated via interaction with the L2 Ledger (i.e., as seen from the Joint
  * Ledger).
  *
  * @param diffs
  *   Evacuation diffs generated from [[L2LedgerCommand.ApplyDepositDecisions]]s and
  *   [[L2LedgerCommand.ApplyTransaction]]
  *
  * @param payouts
  *   Payouts generated from [[L2LedgerCommand.ApplyTransaction]]
  *
  * @param payoutRequestIds
  *   The producing request of each entry in `payouts`, in the same order (all payouts of one
  *   `ApplyTransaction` share its `requestId`). Local-only provenance for withdrawal-effect
  *   tracking; never on the wire or on-chain.
  */
final case class L2LedgerState private (
    diffs: Vector[EvacuationDiff],
    payouts: Vector[Payout.Obligation],
    payoutRequestIds: Vector[RequestId]
):
    /** Fold in an [[AppliedEffects.ApplyDepositDecisions]]'s effects: append its evacuation diffs.
      */
    def appendDecisionEffects(extraDiffs: Vector[EvacuationDiff]): L2LedgerState =
        L2LedgerState(diffs ++ extraDiffs, payouts, payoutRequestIds)

    /** Fold in an [[AppliedEffects.ApplyTransaction]]'s effects: append its diffs and payouts,
      * tagging each payout with the producing `requestId` — the ledger-agnostic withdrawal-effect
      * provenance (all payouts of one transaction share its request).
      */
    def appendTransactionEffects(
        extraDiffs: Vector[EvacuationDiff],
        extraPayouts: Vector[Payout.Obligation],
        requestId: RequestId
    ): L2LedgerState =
        L2LedgerState(
          diffs ++ extraDiffs,
          payouts ++ extraPayouts,
          payoutRequestIds ++ Vector.fill(extraPayouts.length)(requestId)
        )

object L2LedgerState:
    def empty: L2LedgerState = L2LedgerState(Vector.empty, Vector.empty, Vector.empty)

    /** Protected _specifically_ because we want to prevent arbitrary evolution from the empty
      * state. You _must_ begin with the empty state and evolve it with the `append*` methods
      * (driven by the ledger's [[L2LedgerResponse.Applied]] effects).
      */
    protected[l2] def apply(
        diffs: Vector[EvacuationDiff],
        payouts: Vector[Payout.Obligation],
        payoutRequestIds: Vector[RequestId]
    ) =
        new L2LedgerState(diffs, payouts, payoutRequestIds)

/** A trait defining an interface to interact with a black-box ledger component (i.e., via the Joint
  * Ledger). The L2Ledger and the state associated with the interactions via the interface are named
  * from the perspective of the _consumer_.
  *
  * Every implementation must be deterministic: consensus feeds each peer's replica the same ordered
  * commands, so a non-deterministic ledger diverges across peers and breaks consensus.
  *
  * Each `send*` returns a **total** [[L2LedgerResponse]]: an outcome (applied / rejected / desync /
  * freeze) is always a response branch, never a raised exception. JointLedger interprets the branch
  * — folding [[AppliedEffects]] into an [[L2LedgerState]] on `Applied`, invalidating the request on
  * a user-command `Rejected`, and fail-stopping on `OutOfOrder` / `LedgerFreeze` / a rejected
  * decision. (A `RemoteL2Ledger` may still raise on a broken *transport* — an undecodable frame or
  * a command-number mismatch — which is a protocol violation, not one of the four verdicts.)
  *
  * NOTE:
  *   - The constructor of [[L2LedgerState]] is private. The only way to construct a new state is
  *     via [[L2LedgerState.empty]], evolved with its `append*` methods.
  *   - Implementors of this trait only need to define the actual methods of sending the requests.
  *
  * @tparam F
  *   A monad in which the "transport" runs. This will be IO for most implementations (for network
  *   or unix socket access, etc), but can also be something like [[State]] for pure implementations
  */
trait L2Ledger[F[_]] {
    implicit def monadF: Monad[F]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#deposit-events
      * @return
      *   the ledger's [[L2LedgerResponse]] — [[L2LedgerResponse.Applied]] with
      *   [[AppliedEffects.RegisterDeposit]] (no effects) on success, or a
      *   [[L2LedgerResponse.Rejected]] the caller invalidates the request on.
      */
    def sendRegisterDeposit(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.RegisterDeposit
    ): F[L2LedgerResponse]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#deposit-events
      *
      * A deposit decision is not a user request, so a failure is not an ordinary verdict: deposits
      * are validated at registration, so a decision should never fail on deposit *validity* — only
      * on a coordination bug (a decision for a deposit compartment the ledger never registered, or
      * an internal merge error). Such a failure answers [[L2LedgerResponse.Rejected]] **and freezes
      * the ledger**: every subsequent command then answers [[L2LedgerResponse.LedgerFreeze]] until
      * `restoreTo` rewinds past the freeze. JointLedger panics on the rejection. See
      * `docs/l2-ledger-command-coordination.md`.
      *
      * @return
      *   [[L2LedgerResponse.Applied]] with [[AppliedEffects.ApplyDepositDecisions]] (the evacuation
      *   diffs the absorbed deposits produce), or a [[L2LedgerResponse.Rejected]] the caller panics
      *   on.
      */
    def sendApplyDepositDecisions(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyDepositDecisions
    ): F[L2LedgerResponse]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#l2-events
      * @return
      *   [[L2LedgerResponse.Applied]] with [[AppliedEffects.ApplyTransaction]] (the evacuation
      *   diffs to apply to the JointLedger's evacuation map plus the payout obligations), or a
      *   [[L2LedgerResponse.Rejected]] the caller invalidates the request on.
      */
    def sendApplyTransaction(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyTransaction
    ): F[L2LedgerResponse]

    /** Reconstruct the committed L2 state as of `commandNumber`, from the ledger's own durable
      * record (`(initial state, commandNumber)`; see `docs/l2-ledger-command-coordination.md`).
      * After this the ledger is positioned at `commandNumber`. Used only on crash-recovery boot.
      * JointLedger owns and persists the authoritative command number, so the ledger exposes no
      * read-back query. Implementations that do not persist (e.g. a remote black box that owns its
      * own recovery) may make this a no-op.
      *
      * Unlike the command path, this stays an [[EitherT]]: it is a boot-time reconstruction, not a
      * numbered command, so its failure is a plain error rather than one of the four verdicts.
      */
    def restoreTo(commandNumber: L2CommandNumber): EitherT[F, L2LedgerError, Unit]
}
