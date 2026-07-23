package hydrozoa.multisig.server

import hydrozoa.config.head.HeadConfig
import hydrozoa.multisig.NodeStatus
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.l2.{L2TxKind, L2TxSummary}
import hydrozoa.multisig.persistence.DepositDecision
import io.bullet.borer.Cbor
import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder}
import java.time.Instant
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{DatumOption, TransactionInput, TransactionOutput, Value}
import scalus.uplc.builtin.ByteString
import sttp.tapir.Schema

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
        case L2TxKind.DepositRejected   => "depositRejected"

    /** One row of the block listing: number, fast-cycle leader (absent for the initial block, which
      * is config, not woven), and block type.
      */
    final case class BlockSummaryView(number: Int, leader: Option[Int], blockType: String)
    given Codec[BlockSummaryView] = deriveCodec

    /** A block's confirmation from this node's viewpoint: `PROPOSED`, `SOFT`, or `HARD`, with the
      * confirmation moments (ISO-8601) where reached. Each time records a **local event at this
      * peer** — when it produced the confirmation — so different peers report different times for
      * the same block. This is by design.
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

    /** One L1 effect a request became: its `l1TxId` (hex — the same value the `/head/effects/{id}`
      * and block-effects queries use) and its kind (`settlement`, `finalization`, `sec`, `refund`).
      */
    final case class EffectRefView(l1TxId: String, kind: String)
    given Codec[EffectRefView] = deriveCodec
    given Schema[EffectRefView] = Schema.derived

    /** Map a resolved effect to its request-facing reference. */
    def mkEffectRefView(effect: ResolvedEffect): EffectRefView =
        EffectRefView(effect.l1TxId.toHex, effectKindName(effect.kind))

    /** A request's lifecycle status from this node's viewpoint, as a ladder where each stage adds
      * the fields of the one below plus its own: `UNPROCESSED` → `LOCALLY_PROCESSED` (block,
      * verdict) → `SOFT_CONFIRMED` (soft time) → `HARD_CONFIRMED` (hard time, effects). The ladder
      * is modeled as traits that extend one another; the concrete statuses are the final case
      * classes below. Each confirmation time records a **local event at this peer** — different
      * peers report different times for the same request, by design. `relatedEffects` — the L1
      * effects the request became (a transaction's settlement/SEC/finalization carriers, a
      * deposit's post-dated refund) — is present once those effects are hard-confirmed.
      *
      * The wire form is flat (a `status` string plus the accumulated fields, nulls dropped): the
      * codec / schema round-trip through the private [[RequestStatusView.Shape]].
      */
    sealed trait RequestStatusView:
        def status: String

    /** `LOCALLY_PROCESSED` and up: the request landed in a block with a validity verdict. */
    sealed trait LocallyProcessedStatus extends RequestStatusView:
        def blockNumber: Int
        def validity: String

    /** `SOFT_CONFIRMED` and up: this peer soft-confirmed the block. The time is optional because a
      * hard-confirmed request that (exceptionally) has no soft-confirmation record still sits on
      * the `HardConfirmed` rung, which inherits this field.
      */
    sealed trait SoftConfirmedStatus extends LocallyProcessedStatus:
        def softConfirmedAt: Option[String]

    /** `HARD_CONFIRMED`: this peer hard-confirmed the covering stack; effects are resolvable. */
    sealed trait HardConfirmedStatus extends SoftConfirmedStatus:
        def hardConfirmedAt: Option[String]
        def relatedEffects: List[EffectRefView]

    object RequestStatusView:
        case object Unprocessed extends RequestStatusView:
            val status = "UNPROCESSED"

        final case class LocallyProcessed(blockNumber: Int, validity: String)
            extends LocallyProcessedStatus:
            val status = "LOCALLY_PROCESSED"

        final case class SoftConfirmed(
            blockNumber: Int,
            validity: String,
            softConfirmedAt: Option[String]
        ) extends SoftConfirmedStatus:
            val status = "SOFT_CONFIRMED"

        final case class HardConfirmed(
            blockNumber: Int,
            validity: String,
            softConfirmedAt: Option[String],
            hardConfirmedAt: Option[String],
            relatedEffects: List[EffectRefView]
        ) extends HardConfirmedStatus:
            val status = "HARD_CONFIRMED"

        /** The flat serialization shape — one object, the status string plus every ladder field as
          * an option; the drop-null printer omits the absent ones.
          */
        private final case class Shape(
            status: String,
            blockNumber: Option[Int],
            validity: Option[String],
            softConfirmedAt: Option[String],
            hardConfirmedAt: Option[String],
            relatedEffects: Option[List[EffectRefView]]
        )
        private object Shape:
            given Codec[Shape] = deriveCodec
            given Schema[Shape] = Schema.derived

        private def toShape(v: RequestStatusView): Shape = v match
            case Unprocessed => Shape("UNPROCESSED", None, None, None, None, None)
            case LocallyProcessed(b, v) =>
                Shape("LOCALLY_PROCESSED", Some(b), Some(v), None, None, None)
            case SoftConfirmed(b, v, s) =>
                Shape("SOFT_CONFIRMED", Some(b), Some(v), s, None, None)
            case HardConfirmed(b, v, s, h, effects) =>
                Shape(
                  "HARD_CONFIRMED",
                  Some(b),
                  Some(v),
                  s,
                  h,
                  Option.when(effects.nonEmpty)(effects)
                )

        private def fromShape(s: Shape): RequestStatusView = s.status match
            case "LOCALLY_PROCESSED" =>
                LocallyProcessed(s.blockNumber.getOrElse(0), s.validity.getOrElse(""))
            case "SOFT_CONFIRMED" =>
                SoftConfirmed(
                  s.blockNumber.getOrElse(0),
                  s.validity.getOrElse(""),
                  s.softConfirmedAt
                )
            case "HARD_CONFIRMED" =>
                HardConfirmed(
                  s.blockNumber.getOrElse(0),
                  s.validity.getOrElse(""),
                  s.softConfirmedAt,
                  s.hardConfirmedAt,
                  s.relatedEffects.getOrElse(Nil)
                )
            case _ => Unprocessed

        given Codec[RequestStatusView] = Codec.from(
          summon[Decoder[Shape]].map(fromShape),
          summon[Encoder[Shape]].contramap(toShape)
        )
        given Schema[RequestStatusView] =
            summon[Schema[Shape]]
                .map(s => Some(fromShape(s)))(toShape)
                .name(Schema.SName("RequestStatusView"))

    /** A valid deposit's decision status, a ladder parallel to [[RequestStatusView]] but tracking
      * the absorb-or-reject decision: `UNDECIDED` → `LOCAL_DECISION` (deciding block + `ABSORBED` /
      * `REJECTED`) → `SOFT_CONFIRMED` (soft time) → `HARD_CONFIRMED` (hard time, and — only for an
      * absorbed deposit — the absorbing settlement's `l1TxId`). Times are of the **deciding**
      * block. Present only for valid deposit requests; absent for transactions and invalid
      * deposits.
      */
    sealed trait DepositDecisionStatusView:
        def status: String

    /** `LOCAL_DECISION` and up: a block decided the deposit. */
    sealed trait LocalDecisionStatus extends DepositDecisionStatusView:
        def blockNumber: Int
        def decision: String

    /** `SOFT_CONFIRMED` and up: this peer soft-confirmed the deciding block (time optional for the
      * same reason as [[SoftConfirmedStatus]]).
      */
    sealed trait DecisionSoftConfirmedStatus extends LocalDecisionStatus:
        def softConfirmedAt: Option[String]

    /** `HARD_CONFIRMED`: this peer hard-confirmed the deciding block's stack. */
    sealed trait DecisionHardConfirmedStatus extends DecisionSoftConfirmedStatus:
        def hardConfirmedAt: Option[String]
        def settlementEffect: Option[String]

    object DepositDecisionStatusView:
        case object Undecided extends DepositDecisionStatusView:
            val status = "UNDECIDED"

        final case class LocalDecision(blockNumber: Int, decision: String)
            extends LocalDecisionStatus:
            val status = "LOCAL_DECISION"

        final case class SoftConfirmed(
            blockNumber: Int,
            decision: String,
            softConfirmedAt: Option[String]
        ) extends DecisionSoftConfirmedStatus:
            val status = "SOFT_CONFIRMED"

        final case class HardConfirmed(
            blockNumber: Int,
            decision: String,
            softConfirmedAt: Option[String],
            hardConfirmedAt: Option[String],
            settlementEffect: Option[String]
        ) extends DecisionHardConfirmedStatus:
            val status = "HARD_CONFIRMED"

        private final case class Shape(
            status: String,
            blockNumber: Option[Int],
            decision: Option[String],
            softConfirmedAt: Option[String],
            hardConfirmedAt: Option[String],
            settlementEffect: Option[String]
        )
        private object Shape:
            given Codec[Shape] = deriveCodec
            given Schema[Shape] = Schema.derived

        private def toShape(v: DepositDecisionStatusView): Shape = v match
            case Undecided => Shape("UNDECIDED", None, None, None, None, None)
            case LocalDecision(b, d) =>
                Shape("LOCAL_DECISION", Some(b), Some(d), None, None, None)
            case SoftConfirmed(b, d, s) =>
                Shape("SOFT_CONFIRMED", Some(b), Some(d), s, None, None)
            case HardConfirmed(b, d, s, h, settlement) =>
                Shape("HARD_CONFIRMED", Some(b), Some(d), s, h, settlement)

        private def fromShape(s: Shape): DepositDecisionStatusView = s.status match
            case "LOCAL_DECISION" =>
                LocalDecision(s.blockNumber.getOrElse(0), s.decision.getOrElse(""))
            case "SOFT_CONFIRMED" =>
                SoftConfirmed(
                  s.blockNumber.getOrElse(0),
                  s.decision.getOrElse(""),
                  s.softConfirmedAt
                )
            case "HARD_CONFIRMED" =>
                HardConfirmed(
                  s.blockNumber.getOrElse(0),
                  s.decision.getOrElse(""),
                  s.softConfirmedAt,
                  s.hardConfirmedAt,
                  s.settlementEffect
                )
            case _ => Undecided

        given Codec[DepositDecisionStatusView] = Codec.from(
          summon[Decoder[Shape]].map(fromShape),
          summon[Encoder[Shape]].contramap(toShape)
        )
        given Schema[DepositDecisionStatusView] =
            summon[Schema[Shape]]
                .map(s => Some(fromShape(s)))(toShape)
                .name(Schema.SName("DepositDecisionStatusView"))

    /** The request-details body: opaque id (echoed from the path), author peer, type, receive time
      * (when this peer received the request — a local event, so different peers report different
      * times for the same request, by design), the lifecycle status, and — for valid deposit
      * requests only — the absorb/reject decision status.
      */
    final case class RequestDetailsView(
        requestId: Long,
        peerNumber: Int,
        requestType: String,
        receivedAt: String,
        status: RequestStatusView,
        decisionStatus: Option[DepositDecisionStatusView]
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

    /** Assemble the request-status ladder from its resolved lifecycle stages: the processing block
      * and verdict (absent while unprocessed), the node-local confirmation moments (each present
      * iff this peer holds that confirmation record — `wallClockOf` is total), and the L1 effects
      * the request became (present once hard-confirmed).
      */
    def mkRequestStatus(
        block: Option[(Int, ValidityFlag)],
        softConfirmedAt: Option[Instant],
        hardConfirmedAt: Option[Instant],
        relatedEffects: List[EffectRefView]
    ): RequestStatusView =
        block match
            case None => RequestStatusView.Unprocessed
            case Some((blockNumber, v)) =>
                val validity = validityName(v)
                if hardConfirmedAt.isDefined then
                    RequestStatusView.HardConfirmed(
                      blockNumber,
                      validity,
                      softConfirmedAt.map(_.toString),
                      hardConfirmedAt.map(_.toString),
                      relatedEffects
                    )
                else if softConfirmedAt.isDefined then
                    RequestStatusView.SoftConfirmed(
                      blockNumber,
                      validity,
                      softConfirmedAt.map(_.toString)
                    )
                else RequestStatusView.LocallyProcessed(blockNumber, validity)

    /** The `ABSORBED` / `REJECTED` label of a deposit decision. */
    def decisionName(decision: DepositDecision): String = decision match
        case _: DepositDecision.Absorbed => "ABSORBED"
        case _: DepositDecision.Rejected => "REJECTED"

    /** Assemble the deposit decision-status ladder: undecided (no decision row yet), else the
      * deciding block + `ABSORBED`/`REJECTED`, promoted by the deciding block's node-local
      * confirmation moments. The absorbing settlement's `l1TxId` (`ABSORBED`, hard-confirmed only)
      * rides on the hard-confirmed rung.
      */
    def mkDecisionStatus(
        decided: Option[(Int, String)],
        softConfirmedAt: Option[Instant],
        hardConfirmedAt: Option[Instant],
        settlementEffect: Option[String]
    ): DepositDecisionStatusView =
        decided match
            case None => DepositDecisionStatusView.Undecided
            case Some((blockNumber, decision)) =>
                if hardConfirmedAt.isDefined then
                    DepositDecisionStatusView.HardConfirmed(
                      blockNumber,
                      decision,
                      softConfirmedAt.map(_.toString),
                      hardConfirmedAt.map(_.toString),
                      settlementEffect
                    )
                else if softConfirmedAt.isDefined then
                    DepositDecisionStatusView.SoftConfirmed(
                      blockNumber,
                      decision,
                      softConfirmedAt.map(_.toString)
                    )
                else DepositDecisionStatusView.LocalDecision(blockNumber, decision)

    /** Map a request, its derived receive time, its lifecycle status, and — for valid deposits —
      * its decision status to the details body.
      */
    def mkRequestDetailsView(
        request: UserRequestWithId,
        receivedAt: Instant,
        status: RequestStatusView,
        decisionStatus: Option[DepositDecisionStatusView]
    ): RequestDetailsView =
        RequestDetailsView(
          requestId = request.requestId.asI64,
          peerNumber = request.requestId.peerNum.convert,
          requestType = requestTypeName(request),
          receivedAt = receivedAt.toString,
          status = status,
          decisionStatus = decisionStatus
        )

    /** A block's L1 effects as `l1TxId`s, grouped by kind — the fields present depend on the block
      * type (INITIAL: initialization + fallback; MINOR: sec? + refunds; MAJOR: settlement +
      * fallback + rollouts + refunds; FINAL: finalization + rollouts). Absent kinds are omitted.
      */
    final case class BlockEffectsView(
        blockType: String,
        initialization: Option[String],
        settlement: Option[String],
        finalization: Option[String],
        fallback: Option[String],
        sec: Option[String],
        rollouts: List[String],
        refunds: List[String]
    )
    given Codec[BlockEffectsView] = deriveCodec

    /** One L1 effect in full. Every effect has an `l1TxId`, `kind`, and `blockNumber`. A real tx
      * effect carries its `txCbor` (hex); a standalone evacuation commitment (kind `sec`) carries
      * its serialized on-chain bytes (whose hash is the synthetic `l1TxId`) and the hard-ack
      * signatures split into `headSignatures` then `coilSignatures`.
      */
    final case class EffectView(
        l1TxId: String,
        kind: String,
        blockNumber: Int,
        txCbor: Option[String],
        secOnchainSerialized: Option[String],
        headSignatures: Option[List[String]],
        coilSignatures: Option[List[String]]
    )
    given Codec[EffectView] = deriveCodec

    /** The effect-kind discriminator string (matches the sub-resource path segments). */
    def effectKindName(kind: EffectKind): String = kind match
        case EffectKind.Initialization => "initialization"
        case EffectKind.Settlement     => "settlement"
        case EffectKind.Fallback       => "fallback"
        case EffectKind.Rollout        => "rollout"
        case EffectKind.Finalization   => "finalization"
        case EffectKind.Refund         => "refund"
        case EffectKind.Sec            => "sec"

    /** Group a block's resolved effects into the by-kind listing. */
    def mkBlockEffectsView(blockType: String, effects: List[ResolvedEffect]): BlockEffectsView =
        def firstIdOf(k: EffectKind): Option[String] =
            effects.collectFirst { case e if e.kind == k => e.l1TxId.toHex }
        def idsOf(k: EffectKind): List[String] =
            effects.collect { case e if e.kind == k => e.l1TxId.toHex }
        BlockEffectsView(
          blockType = blockType,
          initialization = firstIdOf(EffectKind.Initialization),
          settlement = firstIdOf(EffectKind.Settlement),
          finalization = firstIdOf(EffectKind.Finalization),
          fallback = firstIdOf(EffectKind.Fallback),
          sec = firstIdOf(EffectKind.Sec),
          rollouts = idsOf(EffectKind.Rollout),
          refunds = idsOf(EffectKind.Refund)
        )

    /** Map a resolved effect to its full view; `nHeadPeers` splits an SEC's hard-ack signatures
      * into head (first `nHeadPeers`) then coil.
      */
    def mkEffectView(effect: ResolvedEffect, nHeadPeers: Int): EffectView = effect match
        case tx: ResolvedEffect.Tx =>
            EffectView(
              l1TxId = tx.l1TxId.toHex,
              kind = effectKindName(tx.kind),
              blockNumber = tx.blockNumber.convert,
              txCbor = Some(ByteString.fromArray(tx.tx.toCbor).toHex),
              secOnchainSerialized = None,
              headSignatures = None,
              coilSignatures = None
            )
        case sec: ResolvedEffect.Sec =>
            val sigsHex = sec.commitment.headerMultiSigned.map { sig =>
                val bytes: Array[Byte] = sig
                ByteString.fromArray(bytes).toHex
            }
            val headerBytes: Array[Byte] = sec.commitment.commitment.header
            EffectView(
              l1TxId = sec.l1TxId.toHex,
              kind = "sec",
              blockNumber = sec.blockNumber.convert,
              txCbor = None,
              secOnchainSerialized = Some(ByteString.fromArray(headerBytes).toHex),
              headSignatures = Some(sigsHex.take(nHeadPeers)),
              coilSignatures = Some(sigsHex.drop(nHeadPeers))
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
