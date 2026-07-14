package hydrozoa.multisig.ledger.l2

import cats.*
import cats.data.*
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import scalus.uplc.builtin.ByteString

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
  */
final case class L2LedgerState private (
    diffs: Vector[EvacuationDiff],
    payouts: Vector[Payout.Obligation]
)

object L2LedgerState:
    def empty: L2LedgerState = L2LedgerState(Vector.empty, Vector.empty)

    /** Protected _specifically_ because we want to prevent arbitrary evolution from the empty
      * state. You _must_ begin with the empty state and evolve it using [[applyL2LedgerCommand]].
      */
    protected[l2] def apply(diffs: Vector[EvacuationDiff], payouts: Vector[Payout.Obligation]) =
        new L2LedgerState(diffs, payouts)

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
        req: L2LedgerCommand.RegisterDeposit
    ): EitherT[F, L2LedgerError, Unit]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#deposit-events
      *
      * @return
      *   Either an error blob if the request could not be applied, or a vector of evacuation diffs
      *   on success.
      */
    def sendApplyDepositDecisions(
        req: L2LedgerCommand.ApplyDepositDecisions
    ): EitherT[F, L2LedgerError, Vector[EvacuationDiff]]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#l2-events
      * @return
      *   Either an error blob if the request could not be applied, or a vector of diffs to apply to
      *   the JointLedger's evacuation map and a vector of payout obligations.
      */
    def sendApplyTransaction(
        req: L2LedgerCommand.ApplyTransaction
    ): EitherT[F, L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])]

    /** Stateless pre-RequestId screening of a transaction request (docs/l2-isomorphism.md): decide
      * whether the native L2 tx in `l2Payload` is worth assigning a RequestId and fanning out to
      * consensus — reject a malformed or replay-pinned tx before it consumes resources. A
      * transaction has no pre-screening stage: it self-authenticates through its own witnesses, so
      * this is the whole of its screening. Conservative: only definite, stateless failures;
      * stateful checks (balance, inputs, completeness) stay at submission.
      */
    def sendScreenTx(l2Payload: ByteString): EitherT[F, L2LedgerError, Unit]

    /** Stateless pre-RequestId screening of a deposit request — the ledger's stage, after
      * Hydrozoa's deposit pre-screening (COSE authentication + the accept-by check) has passed. The
      * ledger checks that the `l2Payload` is well-formed for it and consistent with the deposit's
      * reference data — for the EUTXO ledger, that `depositL2Value` covers the `l2Payload` outputs.
      */
    def sendScreenDeposit(req: L2LedgerCommand.ScreenDeposit): EitherT[F, L2LedgerError, Unit]

    /** The ledger's current commit commandNumber — the recovery anchor (§R2b). Bumped once per
      * successful state-mutating command (the "real" commands), so the consumer (JointLedger) can
      * read it right after a commit and record which commandNumber that block corresponds to.
      * Genesis is [[L2CommandNumber.zero]].
      */
    def currentCommandNumber: F[L2CommandNumber]

    /** Reconstruct the committed L2 state as of `commandNumber`, from the ledger's own durable
      * record (`(initial state, commandNumber)`; see `design/recovery-implementation-plan.md` R2b).
      * After this the ledger's [[currentCommandNumber]] equals `commandNumber`. Used only on
      * crash-recovery boot. Implementations that do not persist (e.g. a remote black box that owns
      * its own recovery) may leave this unsupported.
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

        def fromL2LedgerCommandReal(e: L2LedgerCommand.Real): L2LedgerAction.Real = e match {
            case e: L2LedgerCommand.RegisterDeposit       => fromRegisterDeposit(e)
            case e: L2LedgerCommand.ApplyDepositDecisions => fromApplyDepositDecisions(e)
            case e: L2LedgerCommand.ApplyTransaction      => fromApplyTransaction(e)
        }

        private def fromRegisterDeposit(
            req: L2LedgerCommand.RegisterDeposit
        ): L2LedgerAction.Real =
            L2LedgerAction.Real(
              Kleisli(ledgerState =>
                  for {
                      _ <- sendRegisterDeposit(req)
                  } yield ledgerState
              )
            )

        private def fromApplyDepositDecisions(
            req: L2LedgerCommand.ApplyDepositDecisions
        ): L2LedgerAction.Real =
            L2LedgerAction.Real(
              Kleisli(ledgerState =>
                  for {
                      resDiffs <- sendApplyDepositDecisions(req)
                      newState = L2LedgerState(ledgerState.diffs ++ resDiffs, ledgerState.payouts)
                  } yield newState
              )
            )

        private def fromApplyTransaction(
            req: L2LedgerCommand.ApplyTransaction
        ): L2LedgerAction.Real = L2LedgerAction.Real(
          Kleisli(ledgerState =>
              for {
                  res <- sendApplyTransaction(req)
                  newState =
                      L2LedgerState(ledgerState.diffs ++ res._1, ledgerState.payouts ++ res._2)
              } yield newState
          )
        )

    }

}
