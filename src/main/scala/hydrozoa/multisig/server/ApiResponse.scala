package hydrozoa.multisig.server

import hydrozoa.multisig.ledger.event.RequestId

/** Response types for the HTTP API */
object ApiResponse {

    /** Response when a request is successfully accepted. */
    final case class RequestAccepted(
        requestId: RequestId
    )

    /** Error response */
    final case class Error(
        error: String
    )
}
