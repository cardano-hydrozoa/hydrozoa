package hydrozoa.multisig.server

import hydrozoa.config.head.HeadConfig
import hydrozoa.multisig.NodeStatus
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.l2.{L2TxKind, L2TxSummary}
import io.bullet.borer.Cbor
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import java.time.Instant
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{DatumOption, TransactionInput, TransactionOutput, Value}
import scalus.uplc.builtin.ByteString

/** Flat data-transfer objects for the HTTP API responses.
  *
  * These exist so the endpoints have a single self-documenting contract: every field is a
  * primitive, string, list, or map, so both the circe JSON codecs and the tapir OpenAPI schemas
  * derive from them automatically — no hand-written encoder can drift from the generated schema.
  * The handlers map the domain types onto these; the string forms match the project's CIP-0116
  * conventions (bech32 addresses, hex hashes, decimal-string coins/quantities).
  */
object ApiDto {

    /** `{ "status": "ok" }` — the liveness body. */
    final case class HealthResponse(status: String)
    given Codec[HealthResponse] = deriveCodec

    /** `{ "status": "<lifecycle>" }` — the readiness diagnostic body for `GET /ready`. The verdict
      * itself is the HTTP status (200 vs 503); this is only for diagnostics.
      */
    final case class ReadinessResponse(status: String)
    given Codec[ReadinessResponse] = deriveCodec

    /** Map the node lifecycle status to its readiness body (kebab-case label). */
    def mkReadinessResponse(status: NodeStatus): ReadinessResponse =
        ReadinessResponse(nodeStatusLabel(status))

    private def nodeStatusLabel(status: NodeStatus): String = status match
        case NodeStatus.Initializing         => "initializing"
        case NodeStatus.Active               => "active"
        case NodeStatus.Finalized            => "finalized"
        case NodeStatus.HandedOffToRuleBased => "handed-off-to-rule-based"

    /** `{ "status": "success", "message": ... }` — the finalize-trigger body. */
    final case class FinalizeResponse(status: String, message: String)
    given Codec[FinalizeResponse] = deriveCodec

    /** `{ "requestId": <i64> }` — a write request's assigned id, packed as the SugarRush i64 form.
      */
    final case class RequestAcceptedResponse(requestId: Long)
    given Codec[RequestAcceptedResponse] = deriveCodec

    /** Map an accepted write request's id to its i64 response body. */
    def mkRequestAcceptedResponse(id: RequestId): RequestAcceptedResponse =
        RequestAcceptedResponse(id.asI64)

    /** `{ "error": ... }` — the error body used across the API. */
    final case class ErrorResponse(error: String)
    given Codec[ErrorResponse] = deriveCodec

    /** A request id in object form, `{ headPeerNumber, requestNumber }`. */
    final case class RequestIdView(headPeerNumber: Int, requestNumber: Long)

    /** Map a request id to its object-form view. */
    def mkRequestIdView(id: RequestId): RequestIdView =
        val (headPeerNumber, requestNumber) = id.convert
        RequestIdView(headPeerNumber, requestNumber)

    /** One entry of the recent-transactions feed. */
    final case class L2TxSummaryView(
        requestId: RequestIdView,
        blockNumber: Int,
        kind: String
    )
    given Codec[RequestIdView] = deriveCodec
    given Codec[L2TxSummaryView] = deriveCodec

    /** Map an applied-command summary to its recent-transactions feed entry. */
    def mkL2TxSummaryView(summary: L2TxSummary): L2TxSummaryView =
        L2TxSummaryView(
          requestId = mkRequestIdView(summary.requestId),
          blockNumber = summary.blockNumber.convert,
          kind = kindName(summary.kind)
        )

    private def kindName(kind: L2TxKind): String = kind match
        case L2TxKind.Transaction       => "transaction"
        case L2TxKind.DepositRegistered => "depositRegistered"
        case L2TxKind.DepositAbsorbed   => "depositAbsorbed"
        case L2TxKind.DepositRefunded   => "depositRefunded"

    /** One row of the block listing: number, fast-cycle leader (absent for the initial block, which
      * is config, not woven), and block type.
      */
    final case class BlockSummaryView(number: Int, leader: Option[Int], blockType: String)
    given Codec[BlockSummaryView] = deriveCodec

    /** A block's confirmation from this node's viewpoint: `PROPOSED`, `SOFT`, or `HARD`, with the
      * node-local confirmation moments (ISO-8601) where reached.
      */
    final case class BlockConfirmationView(
        status: String,
        softConfirmedAt: Option[String],
        hardConfirmedAt: Option[String]
    )
    given Codec[BlockConfirmationView] = deriveCodec

    /** The block-details body: the listing row plus the confirmation status. */
    final case class BlockDetailsView(
        number: Int,
        leader: Option[Int],
        blockType: String,
        confirmation: BlockConfirmationView
    )
    given Codec[BlockDetailsView] = deriveCodec

    /** Map a brief to its listing row; `nHeadPeers` fixes the round-robin leader. */
    def mkBlockSummaryView(brief: BlockBrief.Next, nHeadPeers: Int): BlockSummaryView =
        BlockSummaryView(
          number = brief.blockNum.convert,
          leader = Some(brief.blockNum.convert % nHeadPeers),
          blockType = blockTypeName(brief)
        )

    /** The synthesized listing row for the initial block (block 0 is config, not a spine entry).
      */
    def mkInitialBlockSummaryView: BlockSummaryView =
        BlockSummaryView(number = 0, leader = None, blockType = "initial")

    /** Assemble the confirmation view from the optional node-local confirmation moments. */
    def mkBlockConfirmationView(
        softConfirmedAt: Option[Instant],
        hardConfirmedAt: Option[Instant]
    ): BlockConfirmationView =
        val status =
            if hardConfirmedAt.isDefined then "HARD"
            else if softConfirmedAt.isDefined then "SOFT"
            else "PROPOSED"
        BlockConfirmationView(
          status = status,
          softConfirmedAt = softConfirmedAt.map(_.toString),
          hardConfirmedAt = hardConfirmedAt.map(_.toString)
        )

    private def blockTypeName(brief: BlockBrief.Next): String = brief match
        case _: BlockBrief.Minor => "minor"
        case _: BlockBrief.Major => "major"
        case _: BlockBrief.Final => "final"

    /** One row of the request listing: the opaque request id (the packed i64, the same value the
      * submit response returns), the author peer number, and the request type.
      */
    final case class RequestSummaryView(requestId: Long, peerNumber: Int, requestType: String)
    given Codec[RequestSummaryView] = deriveCodec

    /** A request's lifecycle status from this node's viewpoint: `UNPROCESSED`, `LOCALLY_PROCESSED`,
      * `SOFT_CONFIRMED`, or `HARD_CONFIRMED`, with the fields each stage adds. `relatedEffects` is
      * always absent for now (per-request effect tracking is a separate change).
      */
    final case class RequestStatusView(
        status: String,
        blockNumber: Option[Int],
        validity: Option[String],
        softConfirmedAt: Option[String],
        hardConfirmedAt: Option[String],
        relatedEffects: Option[List[Int]]
    )
    given Codec[RequestStatusView] = deriveCodec

    /** The request-details body: opaque id (echoed from the path), author peer, type, receive time
      * (the node-local wall clock derived from the request's arrival stamp), and the lifecycle
      * status.
      */
    final case class RequestDetailsView(
        requestId: Long,
        peerNumber: Int,
        requestType: String,
        receivedAt: Option[String],
        status: RequestStatusView
    )
    given Codec[RequestDetailsView] = deriveCodec

    /** The request type discriminator, matching the submit-body field names. */
    def requestTypeName(request: UserRequestWithId): String = request match
        case _: UserRequestWithId.DepositRequest     => "deposit"
        case _: UserRequestWithId.TransactionRequest => "transaction"

    /** Map a request to its listing row. */
    def mkRequestSummaryView(request: UserRequestWithId): RequestSummaryView =
        RequestSummaryView(
          requestId = request.requestId.asI64,
          peerNumber = request.requestId.peerNum.convert,
          requestType = requestTypeName(request)
        )

    private def validityName(validity: ValidityFlag): String = validity match
        case ValidityFlag.Valid   => "valid"
        case ValidityFlag.Invalid => "invalid"

    /** Assemble the request-status view from its resolved lifecycle stages: the processing block
      * and verdict (absent while unprocessed) and the node-local confirmation moments.
      */
    def mkRequestStatusView(
        block: Option[(Int, ValidityFlag)],
        softConfirmedAt: Option[Instant],
        hardConfirmedAt: Option[Instant]
    ): RequestStatusView =
        val status = block match
            case None => "UNPROCESSED"
            case Some(_) =>
                if hardConfirmedAt.isDefined then "HARD_CONFIRMED"
                else if softConfirmedAt.isDefined then "SOFT_CONFIRMED"
                else "LOCALLY_PROCESSED"
        RequestStatusView(
          status = status,
          blockNumber = block.map(_._1),
          validity = block.map((_, v) => validityName(v)),
          softConfirmedAt = softConfirmedAt.map(_.toString),
          hardConfirmedAt = hardConfirmedAt.map(_.toString),
          relatedEffects = None
        )

    /** Map a request, its derived receive time, and its resolved status to the details body. */
    def mkRequestDetailsView(
        request: UserRequestWithId,
        receivedAt: Option[Instant],
        status: RequestStatusView
    ): RequestDetailsView =
        RequestDetailsView(
          requestId = request.requestId.asI64,
          peerNumber = request.requestId.peerNum.convert,
          requestType = requestTypeName(request),
          receivedAt = receivedAt.map(_.toString),
          status = status
        )

    /** A utxo reference, `{ transaction_id, index }` (CIP-0116). */
    final case class TxInputView(transaction_id: String, index: Int)

    /** A Cardano value: ADA in `coin`, native assets nested policy → asset → quantity. */
    final case class ValueView(coin: String, assets: Map[String, Map[String, String]])

    /** An output's datum: at most one of `inline` (CBOR hex) or `hash` (hex); absent when none. */
    final case class DatumView(inline: Option[String], hash: Option[String])

    /** One L2 output: address, value, and (optional) datum. */
    final case class L2OutputView(address: String, value: ValueView, datum: Option[DatumView])

    /** One L2 utxo: its input reference and the output it holds. */
    final case class L2UtxoView(input: TxInputView, output: L2OutputView)

    given Codec[TxInputView] = deriveCodec
    given Codec[ValueView] = deriveCodec
    given Codec[DatumView] = deriveCodec
    given Codec[L2OutputView] = deriveCodec
    given Codec[L2UtxoView] = deriveCodec

    /** Map an L2 utxo (its input reference and output) to its API view. */
    def mkL2UtxoView(input: TransactionInput, output: TransactionOutput): L2UtxoView =
        L2UtxoView(mkTxInputView(input), mkL2OutputView(output))

    private def mkTxInputView(input: TransactionInput): TxInputView =
        TxInputView(input.transactionId.toHex, input.index.toInt)

    private def mkL2OutputView(output: TransactionOutput): L2OutputView =
        L2OutputView(addressBech32(output.address), mkValueView(output.value), mkDatumView(output))

    private def addressBech32(address: Address): String =
        address match
            case shelley: ShelleyAddress => shelley.toBech32.getOrElse(shelley.toHex)
            case other                   => other.toString

    private def mkValueView(value: Value): ValueView =
        val assets = value.assets.assets.map { (policyId, byName) =>
            policyId.toHex -> byName.map { (assetName, quantity) =>
                assetName.bytes.toHex -> java.lang.Long.toUnsignedString(quantity)
            }.toMap
        }.toMap
        ValueView(java.lang.Long.toUnsignedString(value.coin.value), assets)

    private def mkDatumView(output: TransactionOutput): Option[DatumView] =
        output match
            case TransactionOutput.Shelley(_, _, datumHash) =>
                datumHash.map(hash => DatumView(inline = None, hash = Some(hash.toHex)))
            case TransactionOutput.Babbage(_, _, datumOption, _) =>
                datumOption.map {
                    case DatumOption.Inline(data) =>
                        DatumView(
                          inline = Some(ByteString.fromArray(Cbor.encode(data).toByteArray).toHex),
                          hash = None
                        )
                    case DatumOption.Hash(hash) =>
                        DatumView(inline = None, hash = Some(hash.toHex))
                }

    /** The head-parameters body. All hashes/ids are hex; the address is bech32; coins are decimal
      * strings; the beacon token is `policyIdHex.assetNameHex`.
      */
    final case class HeadInfoResponse(
        headId: String,
        headAddress: String,
        multisigRegimeUtxo: TxInputView,
        treasuryBeaconToken: String,
        submissionDurationSeconds: Long,
        absorptionStartOffsetSeconds: Long,
        refundStartOffsetSeconds: Long,
        currentTimePosixSeconds: Long,
        maxNonPlutusTxFee: String
    )
    given Codec[HeadInfoResponse] = deriveCodec

    /** Map the head config plus the current node time to the head-parameters response. */
    def mkHeadInfoResponse(
        headConfig: HeadConfig,
        currentTimePosixSeconds: Long
    ): HeadInfoResponse =
        HeadInfoResponse(
          headId = headConfig.headId.bytes.toHex,
          headAddress = headConfig.headMultisigAddress.toBech32
              .getOrElse(headConfig.headMultisigAddress.toHex),
          multisigRegimeUtxo = mkTxInputView(headConfig.multisigRegimeUtxo.input),
          treasuryBeaconToken =
              s"${headConfig.headMultisigScript.policyId.toHex}.${headConfig.headTokenNames.treasuryTokenName.bytes.toHex}",
          submissionDurationSeconds =
              headConfig.txTiming.depositSubmissionDuration.finiteDuration.toSeconds,
          absorptionStartOffsetSeconds =
              headConfig.txTiming.absorptionStartOffsetDuration.finiteDuration.toSeconds,
          refundStartOffsetSeconds =
              headConfig.txTiming.refundStartOffsetDuration.finiteDuration.toSeconds,
          currentTimePosixSeconds = currentTimePosixSeconds,
          maxNonPlutusTxFee = java.lang.Long.toUnsignedString(headConfig.maxNonPlutusTxFee.value)
        )
}
