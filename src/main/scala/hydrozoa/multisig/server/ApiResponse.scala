package hydrozoa.multisig.server

import hydrozoa.multisig.ledger.event.LedgerEventId

/** Response types for the HTTP API */
object ApiResponse {

    /** Response when an event is successfully submitted */
    final case class EventSubmitted(
        eventId: LedgerEventId
    )

    /** Error response */
    final case class Error(
        error: String
    )
}
