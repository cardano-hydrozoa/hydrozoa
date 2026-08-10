package hydrozoa.multisig.ledger.l2

import cats.data.EitherT
import scalus.cardano.ledger.{Coin, TransactionInput, Value}
import scalus.uplc.builtin.ByteString

/** Why the ledger rejected a request at screening — a soft, order-independent rejection the user
  * retries, distinct from the command path's typed responses. Free-form message.
  */
final case class L2ScreenError(message: String)

/** Stateless, fail-soft screening of a user request before a [[RequestId]] is assigned
  * (docs/spec/l2-isomorphism.md). Split from [[L2Ledger]] because the two concerns have opposite
  * semantics: screening is order-independent, query-like, and shares no mutable state with the
  * state-mutating command stream, so a failure is a soft rejection (the user retries) rather than
  * something that must land. Keeping it on its own trait lets it ride its own connection and keeps
  * the command-number contract on [[L2Ledger]] where it belongs.
  */
trait L2Screener[F[_]] {

    /** Stateless pre-RequestId screening of a transaction request: decide whether the native L2 tx
      * in `l2Payload` is worth assigning a RequestId and fanning out to consensus — reject a
      * malformed or replay-pinned tx before it consumes resources. A transaction has no L1
      * screening stage: it self-authenticates through its own witnesses, so this is the whole of
      * its screening. Conservative: only definite, stateless failures; stateful checks (balance,
      * inputs, completeness) stay at submission.
      */
    def screenTx(l2Payload: ByteString): EitherT[F, L2ScreenError, Unit]

    /** Stateless pre-RequestId screening of a deposit request — the ledger's stage, after
      * Hydrozoa's deposit L1 screening (the l2Payload pin check + the accept-by check) has passed.
      * The ledger checks that the `l2Payload` is well-formed for it and consistent with the
      * deposit's reference data — for the EUTXO ledger, that `depositL2Value` covers the
      * `l2Payload` outputs.
      */
    def screenDeposit(req: L2Screener.ScreenDeposit): EitherT[F, L2ScreenError, Unit]
}

object L2Screener {

    /** The deposit-screening request ([[L2Screener.screenDeposit]]):
      * [[L2LedgerCommand.RegisterDeposit]] minus the consensus-assigned `requestId` / `blockNumber`
      * / `blockCreationStartTime` — those do not exist yet at screening time. Deliberately not an
      * [[L2LedgerCommand]] (a state-mutating, command-numbered command): screening is a stateless
      * query, never logged or replayed, so it lives next to the [[L2Screener]] trait rather than
      * among the ledger commands.
      */
    final case class ScreenDeposit(
        depositId: TransactionInput,
        depositFee: Coin,
        depositL2Value: Value,
        refundDestination: Destination,
        l2Payload: ByteString
    )
}
