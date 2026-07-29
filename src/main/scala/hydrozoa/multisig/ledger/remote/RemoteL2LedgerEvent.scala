package hydrozoa.multisig.ledger.remote

import org.http4s.Uri

/** Typed events emitted by [[RemoteL2Ledger]]. Pure data; formatters in
  * [[RemoteL2LedgerEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait RemoteL2LedgerEvent

object RemoteL2LedgerEvent:

    /** A WebSocket connection to the remote ledger is being established. */
    final case class Connecting(uri: Uri) extends RemoteL2LedgerEvent

    /** A WebSocket connection to the remote ledger succeeded. */
    final case class Connected(uri: Uri) extends RemoteL2LedgerEvent

    /** The remote ledger could not be reached; the request fails with a retryable error. */
    final case class Unavailable(uri: Uri, cause: Throwable) extends RemoteL2LedgerEvent

    /** Sending a JSON-encoded request to the remote ledger. */
    final case class Sending(message: String) extends RemoteL2LedgerEvent

    /** Received a raw JSON response from the remote ledger. */
    final case class Received(message: String) extends RemoteL2LedgerEvent
