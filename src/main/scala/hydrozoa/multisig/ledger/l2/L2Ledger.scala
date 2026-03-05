package hydrozoa.multisig.ledger.l2

import cats.*
import cats.data.*
import cats.syntax.all.*
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import monocle.Focus.focus

private type EF[F[_], A] = EitherT[F, L2LedgerError, A]
// See: "Kendo" from the test library
private type KEF[F[_]] = data.Kleisli[[X] =>> EF[F, X], L2LedgerState, L2LedgerState]

/** Errors occurring from interaction with the L2 Ledger (i.e., as seen from the Joint Ledger).
  *
  * @param bytes
  *   The parameter is [[Array[Byte]] because Hydrozoa itself does not define a codec (though
  *   frontends may).
  */
case class L2LedgerError(bytes: Array[Byte])

/** State changes accumulated via interaction with the L2 Ledger (i.e., as seen from the Joint
  * Ledger).
  * @param diffs
  *   Evacuation diffs generated from [[L2LedgerEvent.DepositEventDecisions]]s and
  *   [[L2LedgerEvent.L2Event]]
  * @param payouts
  *   Payouts generated from [[L2LedgerEvent.L2Event]]
  */
final case class L2LedgerState private (
    diffs: Vector[EvacuationDiff],
    payouts: Vector[Payout.Obligation]
)

object L2LedgerState:
    def empty: L2LedgerState = L2LedgerState(Vector.empty, Vector.empty)

    /** Protected _specifically_ because we want to prevent arbitrary evolution from the empty
      * state. You _must_ begin with the empty state and evolve it using [[applyL2LedgerEvent]].
      */
    protected[l2] def apply(diffs: Vector[EvacuationDiff], payouts: Vector[Payout.Obligation]) =
        new L2LedgerState(diffs, payouts)

/** A trait defining an interface to interact with a black-box ledger component (i.e., via the Joint
  * Ledger). The L2Ledger and the state associated with the interactions via the interface are named
  * from the perspective of the _consumer_.
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
    def sendDepositEventRegistration(
        req: L2LedgerEvent.DepositEventRegistration
    ): EitherT[F, L2LedgerError, Unit]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#deposit-events
      *
      * @return
      *   Either an error blob if the request could not be applied, or a vector of evacuation diffs
      *   on success.
      */
    def sendDepositEventDecisions(
        req: L2LedgerEvent.DepositEventDecisions
    ): EitherT[F, L2LedgerError, Vector[EvacuationDiff]]

    /** See:
      * https://gummiwormlabs.github.io/gummiworm-writing-room/gummiworm-poc/sugar-rush-overview/ledger-events#l2-events
      * @return
      *   Either an error blob if the request could not be applied, or a vector of diffs to apply to
      *   the JointLedger's evacuation map and a vector of payout obligations.
      */
    def sendL2Event(
        req: L2LedgerEvent.L2Event
    ): EitherT[F, L2LedgerError, (Vector[EvacuationDiff], Vector[Payout.Obligation])]

    /** Actions (effectful endomorphisms) on the L2Ledger state. They may return an error or a new
      * state, and run effects in the base monad [[F]].
      */
    case class L2LedgerAction private (private val unLedgerAction: KEF[F]) {
        def run(
            state: L2LedgerState
        ): F[Either[L2LedgerError, L2LedgerState]] =
            this.unLedgerAction.run(state).value
    }

    object L2LedgerAction {

        def fromL2LedgerEvent(e: L2LedgerEvent): L2LedgerAction = e match {
            case e: L2LedgerEvent.DepositEventDecisions    => applyDepositEventDecisions(e)
            case e: L2LedgerEvent.L2Event                  => applyL2Event(e)
            case e: L2LedgerEvent.DepositEventRegistration => applyDepositEventRegistration(e)
        }

        private def applyDepositEventRegistration(
            req: L2LedgerEvent.DepositEventRegistration
        ): L2LedgerAction =
            L2LedgerAction(
              Kleisli(ledgerState =>
                  for {
                      _ <- sendDepositEventRegistration(req)
                  } yield ledgerState
              )
            )

        private def applyDepositEventDecisions(
            req: L2LedgerEvent.DepositEventDecisions
        ): L2LedgerAction =
            L2LedgerAction(
              Kleisli(ledgerState =>
                  for {
                      resDiffs <- sendDepositEventDecisions(req)
                      newState = L2LedgerState(ledgerState.diffs ++ resDiffs, ledgerState.payouts)
                  } yield newState
              )
            )

        private def applyL2Event(
            req: L2LedgerEvent.L2Event
        ): L2LedgerAction = L2LedgerAction(
          Kleisli(ledgerState =>
              for {
                  res <- sendL2Event(req)
                  newState =
                      L2LedgerState(ledgerState.diffs ++ res._1, ledgerState.payouts ++ res._2)
              } yield newState
          )
        )
    }
}
