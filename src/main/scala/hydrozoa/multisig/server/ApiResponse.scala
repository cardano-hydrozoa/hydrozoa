package hydrozoa.multisig.server

import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.multisig.ledger.event.LedgerEventId
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, TransactionInput}

/** Response types for the HTTP API */
object ApiResponse {

    /** Response when a request is successfully accepted. */
    final case class RequestAccepted(
        requestId: LedgerEventId
    )

    /** Error response */
    final case class Error(
        error: String
    )

    /** Head id (to use in the request AND in the deposit tx's metadata) Head address (to send the
      * deposit utxo to) Multisig regime utxo id (should be referenced in the deposit tx) Submission
      * duration (seconds) to calculate ttl of the deposit tx: deposit_tx.ttl = request.validity_end
      * + submission_duration (ttl is mandatory) Refund start offset to calculate the validity start
      * of the post-dated refund (it's needed in the deposit utxo datum):
      * deposit_utxo.datum.refund_instructions.refund_start = request.validity_end +
      * refund_start_offset maxNonPlutusTxFee to check whether deposit utxo value is big enough to
      * cover minAda in the refunded utxo and the refund tx fee
      */
    final case class HeadInfo(
        headId: HeadId,
        headAddress: ShelleyAddress,
        multisigRegimeUtxo: TransactionInput,
        submissionDurationSeconds: Long,
        refundStartOffsetSeconds: Long,
        currentTimePosixSeconds: Long,
        maxNonPlutusTxFee: Coin
    )
}
