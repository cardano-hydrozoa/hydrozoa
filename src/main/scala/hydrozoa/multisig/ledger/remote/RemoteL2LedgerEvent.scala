package hydrozoa.multisig.ledger.remote

import hydrozoa.multisig.ledger.l2.L2CommandNumber
import org.http4s.Uri
import scala.concurrent.duration.FiniteDuration

/** Typed events emitted by [[RemoteL2Ledger]]. Pure data; formatters in
  * [[RemoteL2LedgerEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait RemoteL2LedgerEvent

object RemoteL2LedgerEvent:

    /** A WebSocket connection to the remote ledger is being established. */
    final case class Connecting(uri: Uri) extends RemoteL2LedgerEvent

    /** A WebSocket connection to the remote ledger succeeded. */
    final case class Connected(uri: Uri) extends RemoteL2LedgerEvent

    /** A request errored; reconnecting after [[backoff]] on attempt [[attempt]] (zero-indexed). */
    final case class ConnectionError(attempt: Int, backoff: FiniteDuration, cause: Throwable)
        extends RemoteL2LedgerEvent

    /** An exchange for [[commandNumber]] exceeded [[timeout]] before the remote answered, so the
      * connection was dropped and will be retried (attempt [[attempt]], reconnecting after
      * [[backoff]]). Distinct from [[ConnectionError]]: the transport did not fail, the response
      * was simply not delivered in time — the guard against a receive stall.
      */
    final case class ExchangeTimedOut(
        commandNumber: L2CommandNumber,
        timeout: FiniteDuration,
        attempt: Int,
        backoff: FiniteDuration
    ) extends RemoteL2LedgerEvent

    /** A [[RemoteL2Ledger.restoreTo]] exchange for [[commandNumber]] exceeded [[timeout]]. Unlike
      * [[ExchangeTimedOut]] this is terminal — retrying a restore livelocks it.
      */
    final case class RestoreTimedOut(
        commandNumber: L2CommandNumber,
        timeout: FiniteDuration
    ) extends RemoteL2LedgerEvent

    /** A reconnection attempt during error recovery itself failed; will retry. */
    final case class ReconnectionAttemptFailed(cause: Throwable) extends RemoteL2LedgerEvent

    /** Sending a JSON-encoded request to the remote ledger. */
    final case class Sending(message: String) extends RemoteL2LedgerEvent

    /** Received a raw JSON response from the remote ledger. */
    final case class Received(message: String) extends RemoteL2LedgerEvent
