package hydrozoa.multisig.ledger.l2

import cats.*
import cats.data.*
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout

private type EF[F[_], A] = EitherT[F, L2LedgerError, A]
// See: "Kendo" from the test library
private type KEF[F[_]] = data.Kleisli[[X] =>> EF[F, X], L2LedgerState, L2LedgerState]

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
)

object L2LedgerState:
    def empty: L2LedgerState = L2LedgerState(Vector.empty, Vector.empty, Vector.empty)

    /** Protected _specifically_ because we want to prevent arbitrary evolution from the empty
      * state. You _must_ begin with the empty state and evolve it using [[applyL2LedgerCommand]].
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
  * NOTE:
  *   - The constructor of [[L2LedgerState]] is private. The only way to construct a new state is
  *     via the [[L2LedgerState.empty]] method in the companion object.
  *   - The only way to _evolve_ the state is by using the "applyXYZ" methods in the
  *     [[L2LedgerAction]] companion object. These methods are declared final and ensure that the
  *     state is properly updated (so that you can't forget to accumulate the [[EvacuationDiff]]s or
  *     [[Payout.Obligation]]s correctly)
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
      *   Either an error blob if the request could not be applied, or unit on success.
      */
    def sendRegisterDeposit(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.RegisterDeposit
    ): EitherT[F, L2LedgerError, Unit]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#deposit-events
      *
      * Infallible by construction: a deposit decision has no per-request verdict — it merges
      * already-registered, already-validated deposits, so it always applies. Its only failure modes
      * (a decision for an unregistered deposit; an absorbed output that should have been rejected
      * at registration) are JointLedger-side invariant violations, which an implementation
      * fail-stops on (a `raise` in `F`), never a `Left`. See
      * `docs/l2-ledger-command-coordination.md`.
      *
      * @return
      *   the evacuation diffs the absorbed deposits produce.
      */
    def sendApplyDepositDecisions(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyDepositDecisions
    ): F[Vector[EvacuationDiff]]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#l2-events
      * @return
      *   Either an error blob if the request could not be applied, or a vector of diffs to apply to
      *   the JointLedger's evacuation map and a vector of payout obligations.
      */
    def sendApplyTransaction(
        commandNumber: L2CommandNumber,
        req: L2LedgerCommand.ApplyTransaction
    ): EitherT[F, L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])]

    /** Reconstruct the committed L2 state as of `commandNumber`, from the ledger's own durable
      * record (`(initial state, commandNumber)`; see `docs/l2-ledger-command-coordination.md`).
      * After this the ledger is positioned at `commandNumber`. Used only on crash-recovery boot.
      * JointLedger owns and persists the authoritative command number, so the ledger exposes no
      * read-back query. Implementations that do not persist (e.g. a remote black box that owns its
      * own recovery) may make this a no-op.
      */
    def restoreTo(commandNumber: L2CommandNumber): EitherT[F, L2LedgerError, Unit]

    /** Actions (effectful endomorphisms) on the L2Ledger state. They may return an error or a new
      * state, and run effects in the base monad [[F]].
      */
    sealed trait L2LedgerAction {
        def unLedgerAction: KEF[F]
    }

    object L2LedgerAction {

        final class Real private[l2] (override val unLedgerAction: KEF[F]) extends L2LedgerAction {
            def run(state: L2LedgerState): F[Either[L2LedgerError, L2LedgerState]] =
                this.unLedgerAction.run(state).value
        }

        def fromL2LedgerCommand(
            commandNumber: L2CommandNumber,
            e: L2LedgerCommand
        ): L2LedgerAction.Real = e match {
            case e: L2LedgerCommand.RegisterDeposit => fromRegisterDeposit(commandNumber, e)
            case e: L2LedgerCommand.ApplyDepositDecisions =>
                fromApplyDepositDecisions(commandNumber, e)
            case e: L2LedgerCommand.ApplyTransaction => fromApplyTransaction(commandNumber, e)
        }

        private def fromRegisterDeposit(
            commandNumber: L2CommandNumber,
            req: L2LedgerCommand.RegisterDeposit
        ): L2LedgerAction.Real =
            L2LedgerAction.Real(
              Kleisli(ledgerState =>
                  for {
                      _ <- sendRegisterDeposit(commandNumber, req)
                  } yield ledgerState
              )
            )

        private def fromApplyDepositDecisions(
            commandNumber: L2CommandNumber,
            req: L2LedgerCommand.ApplyDepositDecisions
        ): L2LedgerAction.Real =
            L2LedgerAction.Real(
              Kleisli(ledgerState =>
                  for {
                      resDiffs <- EitherT.liftF(sendApplyDepositDecisions(commandNumber, req))
                      newState = L2LedgerState(
                        ledgerState.diffs ++ resDiffs,
                        ledgerState.payouts,
                        ledgerState.payoutRequestIds
                      )
                  } yield newState
              )
            )

        private def fromApplyTransaction(
            commandNumber: L2CommandNumber,
            req: L2LedgerCommand.ApplyTransaction
        ): L2LedgerAction.Real = L2LedgerAction.Real(
          Kleisli(ledgerState =>
              for {
                  res <- sendApplyTransaction(commandNumber, req)
                  // All of this tx's payouts share its requestId — the ledger-agnostic provenance
                  // tag for withdrawal-effect tracking (works for any L2 ledger backend).
                  newState = L2LedgerState(
                    ledgerState.diffs ++ res._1,
                    ledgerState.payouts ++ res._2,
                    ledgerState.payoutRequestIds ++ Vector.fill(res._2.length)(req.requestId)
                  )
              } yield newState
          )
        )

    }

}
