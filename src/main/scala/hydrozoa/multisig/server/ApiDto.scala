package hydrozoa.multisig.server

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.given
import hydrozoa.multisig.NodeStatus
import hydrozoa.multisig.consensus.{UserRequest, UserRequestBody, UserRequestWithId}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockHeader}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.l2.{L2TxKind, L2TxSummary}
import hydrozoa.multisig.persistence.DepositDecision
import io.bullet.borer.Cbor
import io.circe.derivation.{Configuration as CirceConfig, ConfiguredCodec}
import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder}
import java.time.Instant
import scala.util.Try
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{DatumOption, TransactionInput, TransactionOutput, Value}
import scalus.uplc.builtin.ByteString
import sttp.tapir.generic.Configuration as TapirConfig
import sttp.tapir.{Schema, SchemaType, Validator}

/** Flat data-transfer objects for the HTTP API responses.
  *
  * These exist so the endpoints have a single self-documenting contract: every field is a
  * primitive, string, list, or map, so both the circe JSON codecs and the tapir OpenAPI schemas
  * derive from them automatically — no hand-written encoder can drift from the generated schema.
  * The handlers map the domain types onto these; the string forms match the project's CIP-0116
  * conventions (bech32 addresses, hex hashes, decimal-string coins/quantities).
  */
object ApiDto {

    /** Screaming-snake-case a PascalCase constructor name — `SoftConfirmed` -> `SOFT_CONFIRMED` —
      * so the `type` discriminator reads as the API's status vocabulary.
      */
    private def screamingSnake(name: String): String =
        name.replaceAll("([a-z0-9])([A-Z])", "$1_$2").toUpperCase

    /** Sum-type DTOs are **internally tagged**: a `type` field selects the constructor (no
      * flattening / duck typing). [[circeTag]] gives the codec that writes/reads `type`;
      * [[tapirTag]] gives the schema that renders a `oneOf` with an OpenAPI `discriminator`, so the
      * closed constructor set is expressed in the spec. Both take the SAME constructor-name
      * transform, so the codec and the schema agree on every discriminator value.
      *
      * A discriminator value derives from a variant's constructor name, but the OpenAPI schema name
      * derives from it too — so sums that share a status vocabulary (`SOFT_CONFIRMED`, ...) give
      * their variants a sum-specific prefix (`RequestSoftConfirmedView`) for a unique schema name,
      * and the transform strips the prefix so the `type` value stays the shared word. See
      * [[statusTag]].
      */
    private def circeTag(discriminatorValue: String => String): CirceConfig =
        CirceConfig.default
            .withDiscriminator("type")
            .withTransformConstructorNames(discriminatorValue)
    private def tapirTag(discriminatorValue: String => String): TapirConfig =
        TapirConfig.default
            .withDiscriminator("type")
            .copy(toDiscriminatorValue =
                sname => discriminatorValue(sname.fullName.split('.').last)
            )

    /** The name transform for a status ladder: strip the sum-specific `prefix` and the `View`
      * suffix, then screaming-snake (`BlockSoftConfirmedView` with prefix `Block` ->
      * `SOFT_CONFIRMED`).
      */
    private def statusTag(prefix: String): String => String =
        name => screamingSnake(name.stripPrefix(prefix).stripSuffix("View"))

    /** The name transform for a kind sum tagged by lowercase kind (`DepositView` -> `deposit`). */
    private def kindTag: String => String =
        name => name.stripSuffix("View").toLowerCase

    /** A closed-vocabulary string field rendered as an OpenAPI `enum`: [[enumCodec]] maps to/from
      * the wire label, [[enumSchema]] gives `type: string` + `enum: [...]`. Modeled as typed API
      * enums so the enum propagates automatically wherever the field appears — including inside the
      * discriminated status sums, where a per-field validator could not reach.
      */
    private def enumCodec[T](all: List[T])(label: T => String): Codec[T] =
        Codec.from(
          Decoder.decodeString.emap(s =>
              all.find(t => label(t) == s).toRight(s"unexpected value: $s")
          ),
          Encoder.encodeString.contramap(label)
        )
    private def enumSchema[T](all: List[T])(label: T => String): Schema[T] =
        Schema[T](SchemaType.SString()).validate(Validator.enumeration(all, t => Some(label(t))))

    /** A request's validity verdict in a block. */
    enum ValidityView(val label: String):
        case Valid extends ValidityView("valid")
        case Invalid extends ValidityView("invalid")
    object ValidityView:
        given Codec[ValidityView] = enumCodec(values.toList)(_.label)
        given Schema[ValidityView] = enumSchema(values.toList)(_.label)

    /** A deposit's absorb-or-reject decision. */
    enum DecisionView(val label: String):
        case Absorbed extends DecisionView("ABSORBED")
        case Rejected extends DecisionView("REJECTED")
    object DecisionView:
        given Codec[DecisionView] = enumCodec(values.toList)(_.label)
        given Schema[DecisionView] = enumSchema(values.toList)(_.label)

    /** An L1 effect's kind. */
    enum EffectKindView(val label: String):
        case Initialization extends EffectKindView("initialization")
        case Settlement extends EffectKindView("settlement")
        case Fallback extends EffectKindView("fallback")
        case Rollout extends EffectKindView("rollout")
        case Finalization extends EffectKindView("finalization")
        case Refund extends EffectKindView("refund")
        case Sec extends EffectKindView("sec")
    object EffectKindView:
        given Codec[EffectKindView] = enumCodec(values.toList)(_.label)
        given Schema[EffectKindView] = enumSchema(values.toList)(_.label)

    /** A block's type. */
    enum BlockTypeView(val label: String):
        case Initial extends BlockTypeView("initial")
        case Minor extends BlockTypeView("minor")
        case Major extends BlockTypeView("major")
        case Final extends BlockTypeView("final")
    object BlockTypeView:
        given Codec[BlockTypeView] = enumCodec(values.toList)(_.label)
        given Schema[BlockTypeView] = enumSchema(values.toList)(_.label)

    /** A recent-transaction feed entry's kind. */
    enum L2TxKindView(val label: String):
        case Transaction extends L2TxKindView("transaction")
        case DepositRegistered extends L2TxKindView("depositRegistered")
        case DepositAbsorbed extends L2TxKindView("depositAbsorbed")
        case DepositRejected extends L2TxKindView("depositRejected")
    object L2TxKindView:
        given Codec[L2TxKindView] = enumCodec(values.toList)(_.label)
        given Schema[L2TxKindView] = enumSchema(values.toList)(_.label)

    /** The node's readiness lifecycle status. */
    enum NodeStatusView(val label: String):
        case Initializing extends NodeStatusView("initializing")
        case Active extends NodeStatusView("active")
        case Finalized extends NodeStatusView("finalized")
        case HandedOffToRuleBased extends NodeStatusView("handed-off-to-rule-based")
    object NodeStatusView:
        given Codec[NodeStatusView] = enumCodec(values.toList)(_.label)
        given Schema[NodeStatusView] = enumSchema(values.toList)(_.label)

    /** `{ "status": "ok" }` — the liveness body. */
    final case class HealthResponse(status: String)
    given Codec[HealthResponse] = deriveCodec

    /** `{ "version": ..., "gitCommit": ..., "buildTime": ... }` — the build identity baked in at
      * compile time ([[hydrozoa.BuildInfo]]), served from `GET /version`.
      */
    final case class VersionResponse(version: String, gitCommit: String, buildTime: String)
    given Codec[VersionResponse] = deriveCodec

    /** `{ "status": "<lifecycle>" }` — the readiness diagnostic body for `GET /ready`. The verdict
      * itself is the HTTP status (200 vs 503); this is only for diagnostics.
      */
    final case class ReadinessResponse(status: NodeStatusView)
    given Codec[ReadinessResponse] = deriveCodec

    /** Map the node lifecycle status to its readiness body. */
    def mkReadinessResponse(status: NodeStatus): ReadinessResponse =
        ReadinessResponse(nodeStatusView(status))

    private def nodeStatusView(status: NodeStatus): NodeStatusView = status match
        case NodeStatus.Initializing         => NodeStatusView.Initializing
        case NodeStatus.Active               => NodeStatusView.Active
        case NodeStatus.Finalized            => NodeStatusView.Finalized
        case NodeStatus.HandedOffToRuleBased => NodeStatusView.HandedOffToRuleBased

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

    /** A deposit submission: the unsigned deposit-tx CBOR (`l1Payload`) and the serialized L2
      * outputs it spawns on absorption (`l2Payload`), both lowercase hex.
      */
    sealed trait SubmitRequestView
    object SubmitRequestView:
        // Internal tagging: a `type` field (`deposit` / `transaction`) selects the kind, with the
        // payloads (`l1Payload` deposits only, `l2Payload` all) alongside it. The transform strips
        // the `Submit` prefix and `View` suffix so the `type` value is the bare kind.
        private val submitTag: String => String =
            name => name.stripPrefix("Submit").stripSuffix("View").toLowerCase
        private given CirceConfig = circeTag(submitTag)
        private given TapirConfig = tapirTag(submitTag)

        final case class SubmitDepositView(l1Payload: String, l2Payload: String)
            extends SubmitRequestView
        final case class SubmitTransactionView(l2Payload: String) extends SubmitRequestView
        given Codec[SubmitRequestView] = ConfiguredCodec.derived
        given Schema[SubmitRequestView] = Schema.derived

    /** Decode a submit body into a domain `UserRequest` (hex payloads to bytes), or a client-facing
      * error message when a payload is not lowercase hex.
      */
    def toUserRequest(view: SubmitRequestView): Either[String, UserRequest] =
        def hex(field: String, value: String): Either[String, ByteString] =
            Try(ByteString.fromHex(value)).toEither.left.map(_ => s"$field must be lowercase hex")
        view match
            case SubmitRequestView.SubmitDepositView(l1Payload, l2Payload) =>
                for {
                    l1 <- hex("l1Payload", l1Payload)
                    l2 <- hex("l2Payload", l2Payload)
                } yield UserRequest.DepositRequest(UserRequestBody.DepositRequestBody(l1, l2))
            case SubmitRequestView.SubmitTransactionView(l2Payload) =>
                hex("l2Payload", l2Payload).map(l2 =>
                    UserRequest.TransactionRequest(UserRequestBody.TransactionRequestBody(l2))
                )

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
        kind: L2TxKindView
    )
    given Codec[RequestIdView] = deriveCodec
    given Codec[L2TxSummaryView] = deriveCodec

    /** Map an applied-command summary to its recent-transactions feed entry. */
    def mkL2TxSummaryView(summary: L2TxSummary): L2TxSummaryView =
        L2TxSummaryView(
          requestId = mkRequestIdView(summary.requestId),
          blockNumber = summary.blockNumber.convert,
          kind = kindView(summary.kind)
        )

    private def kindView(kind: L2TxKind): L2TxKindView = kind match
        case L2TxKind.Transaction       => L2TxKindView.Transaction
        case L2TxKind.DepositRegistered => L2TxKindView.DepositRegistered
        case L2TxKind.DepositAbsorbed   => L2TxKindView.DepositAbsorbed
        case L2TxKind.DepositRejected   => L2TxKindView.DepositRejected

    /** One row of the block listing: number, fast-cycle leader (absent for the initial block, which
      * is config, not woven), and block type.
      */
    final case class BlockSummaryView(number: Int, leader: Option[Int], blockType: BlockTypeView)
    given Codec[BlockSummaryView] = deriveCodec

    /** The soft-confirmation rung of the [[BlockConfirmationView]] ladder: this peer's local
      * soft-confirmation moment (if held).
      */
    trait BlockSoftConfirmedRung:
        def softConfirmedAt: Option[String]

    /** The hard-confirmation rung — the one above, adding this peer's hard-confirmation moment. */
    trait BlockHardConfirmedRung extends BlockSoftConfirmedRung:
        def hardConfirmedAt: String

    /** A block's confirmation status from this node's viewpoint, `type`-tagged and laddered:
      * `PROPOSED` (woven, not yet soft-confirmed) -> `SOFT_CONFIRMED` (+ soft time) ->
      * `HARD_CONFIRMED` (+ hard time; soft time optional, since a block may be hard-confirmed with
      * no local soft-confirmation record). Each time records a **local event at this peer**, so
      * different peers report different times for the same block. This is by design.
      */
    sealed trait BlockConfirmationView
    object BlockConfirmationView:
        private given CirceConfig = circeTag(statusTag("Block"))
        private given TapirConfig = tapirTag(statusTag("Block"))

        case object BlockProposedView extends BlockConfirmationView
        final case class BlockSoftConfirmedView(softConfirmedAt: Option[String])
            extends BlockConfirmationView,
              BlockSoftConfirmedRung
        final case class BlockHardConfirmedView(
            softConfirmedAt: Option[String],
            hardConfirmedAt: String
        ) extends BlockConfirmationView,
              BlockHardConfirmedRung
        given Codec[BlockConfirmationView] = ConfiguredCodec.derived
        given Schema[BlockConfirmationView] = Schema.derived

    /** A block header from this node's viewpoint, `type`-tagged by block type. All headers carry
      * the block number, version (`major`.`minor`) and creation window (`startTime`/`endTime`,
      * ISO-8601); `initial`, `minor` and `major` additionally carry the fallback and wakeup timing.
      * `final` carries only the common fields.
      */
    sealed trait BlockHeaderView
    object BlockHeaderView:
        // Tag with the lowercase block type (`minor` / `major` / `final` / `initial`), matching
        // `BlockDetailsView.blockType`; shadows the screaming-snake status tagging.
        private given CirceConfig = circeTag(kindTag)
        private given TapirConfig = tapirTag(kindTag)

        final case class InitialView(
            number: Int,
            versionMajor: Int,
            versionMinor: Int,
            startTime: String,
            endTime: String,
            fallbackTxStartTime: String,
            forcedMajorBlockWakeupTime: String,
            depositDecisionWakeupTime: Option[String]
        ) extends BlockHeaderView
        final case class MinorView(
            number: Int,
            versionMajor: Int,
            versionMinor: Int,
            startTime: String,
            endTime: String,
            fallbackTxStartTime: String,
            forcedMajorBlockWakeupTime: String,
            depositDecisionWakeupTime: Option[String]
        ) extends BlockHeaderView
        final case class MajorView(
            number: Int,
            versionMajor: Int,
            versionMinor: Int,
            startTime: String,
            endTime: String,
            fallbackTxStartTime: String,
            forcedMajorBlockWakeupTime: String,
            depositDecisionWakeupTime: Option[String]
        ) extends BlockHeaderView
        final case class FinalView(
            number: Int,
            versionMajor: Int,
            versionMinor: Int,
            startTime: String,
            endTime: String
        ) extends BlockHeaderView
        given Codec[BlockHeaderView] = ConfiguredCodec.derived
        given Schema[BlockHeaderView] = Schema.derived

    /** The block-details body: the listing row, the hard-confirming stack's number (absent until
      * the block is hard-confirmed), the confirmation status, and the full block header (absent for
      * the initial block, which has no persisted brief).
      */
    final case class BlockDetailsView(
        number: Int,
        leader: Option[Int],
        blockType: BlockTypeView,
        stackId: Option[Int],
        status: BlockConfirmationView,
        header: Option[BlockHeaderView]
    )
    given Codec[BlockDetailsView] = deriveCodec

    /** One of a block's transactions: the opaque request id woven into the block and the validity
      * verdict it received there.
      */
    final case class BlockRequestView(requestId: Long, validity: ValidityView)
    given Codec[BlockRequestView] = deriveCodec

    /** The block-body body — the block's content: its transactions (the requests woven into it,
      * each with its verdict) and its deposit decisions (the request ids absorbed into the treasury
      * and those rejected).
      */
    final case class BlockBodyView(
        number: Int,
        blockType: BlockTypeView,
        transactions: List[BlockRequestView],
        depositsAbsorbed: List[Long],
        depositsRejected: List[Long]
    )
    given Codec[BlockBodyView] = deriveCodec

    /** Map a block's brief to its content view. */
    def mkBlockBodyView(brief: BlockBrief.Next): BlockBodyView =
        BlockBodyView(
          number = brief.blockNum.convert,
          blockType = blockTypeView(brief),
          transactions = brief.requests.map((id, v) => BlockRequestView(id.asI64, validityView(v))),
          depositsAbsorbed = brief.depositsAbsorbed.map(_.asI64),
          depositsRejected = brief.depositsRejected.map(_.asI64)
        )

    /** The initial block (block 0) has no woven content. */
    def mkInitialBlockBodyView: BlockBodyView =
        BlockBodyView(number = 0, blockType = BlockTypeView.Initial, transactions = Nil, Nil, Nil)

    /** Map a brief to its listing row; `nHeadPeers` fixes the round-robin leader. */
    def mkBlockSummaryView(brief: BlockBrief.Next, nHeadPeers: Int): BlockSummaryView =
        BlockSummaryView(
          number = brief.blockNum.convert,
          leader = Some(brief.blockNum.convert % nHeadPeers),
          blockType = blockTypeView(brief)
        )

    /** The synthesized listing row for the initial block (block 0 is config, not a spine entry).
      */
    def mkInitialBlockSummaryView: BlockSummaryView =
        BlockSummaryView(number = 0, leader = None, blockType = BlockTypeView.Initial)

    /** Assemble the confirmation rung from the node-local confirmation moments (present iff their
      * record is held — `wallClockOf` is total).
      */
    def mkBlockConfirmationView(
        softConfirmedAt: Option[Instant],
        hardConfirmedAt: Option[Instant]
    ): BlockConfirmationView =
        (softConfirmedAt, hardConfirmedAt) match
            case (soft, Some(hard)) =>
                BlockConfirmationView.BlockHardConfirmedView(soft.map(_.toString), hard.toString)
            case (Some(soft), None) =>
                BlockConfirmationView.BlockSoftConfirmedView(Some(soft.toString))
            case (None, None) => BlockConfirmationView.BlockProposedView

    private def blockTypeView(brief: BlockBrief.Next): BlockTypeView = brief match
        case _: BlockBrief.Minor => BlockTypeView.Minor
        case _: BlockBrief.Major => BlockTypeView.Major
        case _: BlockBrief.Final => BlockTypeView.Final

    /** Map a block header to its `type`-tagged view, rendering the quantized timing as ISO-8601. */
    def mkBlockHeaderView(header: BlockHeader): BlockHeaderView =
        val number = header.blockNum.convert
        val versionMajor = header.blockVersion.major.convert
        val versionMinor = header.blockVersion.minor.convert
        val startTime = header.startTime.instant.toString
        val endTime = header.endTime.instant.toString
        header match
            case h: BlockHeader.Initial =>
                BlockHeaderView.InitialView(
                  number,
                  versionMajor,
                  versionMinor,
                  startTime,
                  endTime,
                  h.fallbackTxStartTime.instant.toString,
                  h.forcedMajorBlockWakeupTime.instant.toString,
                  h.mDepositDecisionWakeupTime.map(_.convert.instant.toString)
                )
            case h: BlockHeader.Minor =>
                BlockHeaderView.MinorView(
                  number,
                  versionMajor,
                  versionMinor,
                  startTime,
                  endTime,
                  h.fallbackTxStartTime.instant.toString,
                  h.forcedMajorBlockWakeupTime.instant.toString,
                  h.mDepositDecisionWakeupTime.map(_.convert.instant.toString)
                )
            case h: BlockHeader.Major =>
                BlockHeaderView.MajorView(
                  number,
                  versionMajor,
                  versionMinor,
                  startTime,
                  endTime,
                  h.fallbackTxStartTime.instant.toString,
                  h.forcedMajorBlockWakeupTime.instant.toString,
                  h.mDepositDecisionWakeupTime.map(_.convert.instant.toString)
                )
            case _: BlockHeader.Final =>
                BlockHeaderView.FinalView(number, versionMajor, versionMinor, startTime, endTime)

    /** One row of the request listing: the opaque request id (the packed i64, the same value the
      * submit response returns), the author peer number, and the request type.
      */
    final case class RequestSummaryView(requestId: Long, peerNumber: Int, requestType: String)
    given Codec[RequestSummaryView] = deriveCodec
    // `requestType` is the closed set `deposit` / `transaction`; validate it so the schema renders an
    // OpenAPI enum, matching the submit / request-details `type` discriminator vocabulary.
    given Schema[RequestSummaryView] =
        Schema
            .derived[RequestSummaryView]
            .modify(_.requestType)(
              _.validate(Validator.enumeration(List("deposit", "transaction"), s => Some(s)))
            )

    /** One L1 effect a request became: its `l1TxId` (hex — the same value the `/head/effects/{id}`
      * and block-effects queries use) and its kind (`settlement`, `finalization`, `sec`, `refund`).
      */
    final case class EffectRefView(l1TxId: String, kind: EffectKindView)
    given Codec[EffectRefView] = deriveCodec
    given Schema[EffectRefView] = Schema.derived

    /** Map a resolved effect to its request-facing reference. */
    def mkEffectRefView(effect: ResolvedEffect): EffectRefView =
        EffectRefView(effect.l1TxId.toHex, effectKindView(effect.kind))

    /** A request's lifecycle status from this node's viewpoint, `type`-tagged, where each rung adds
      * the fields of the one below plus its own: `UNPROCESSED` -> `PROPOSED` (block, verdict) ->
      * `SOFT_CONFIRMED` (soft time) -> `HARD_CONFIRMED` (hard time; soft time optional, since a
      * request may be hard-confirmed with no local soft record; and the effects). Each confirmation
      * time records a **local event at this peer** — different peers report different times for the
      * same request, by design. `relatedEffects` — the L1 effects the request became (a
      * transaction's settlement/SEC/finalization carriers, a deposit's post-dated refund) — is
      * present once those effects are hard-confirmed.
      */
    sealed trait RequestStatusView
    object RequestStatusView:
        private given CirceConfig = circeTag(statusTag("Request"))
        private given TapirConfig = tapirTag(statusTag("Request"))

        case object RequestUnprocessedView extends RequestStatusView
        final case class RequestProposedView(blockNumber: Int, validity: ValidityView)
            extends RequestStatusView
        final case class RequestSoftConfirmedView(
            blockNumber: Int,
            validity: ValidityView,
            softConfirmedAt: String
        ) extends RequestStatusView
        final case class RequestHardConfirmedView(
            blockNumber: Int,
            validity: ValidityView,
            softConfirmedAt: Option[String],
            hardConfirmedAt: String,
            relatedEffects: List[EffectRefView]
        ) extends RequestStatusView
        given Codec[RequestStatusView] = ConfiguredCodec.derived
        given Schema[RequestStatusView] = Schema.derived

    /** A valid deposit's absorb-or-reject decision status, `type`-tagged in the same vocabulary as
      * [[RequestStatusView]]: `UNPROCESSED` (no decision yet) -> `PROPOSED` (deciding block +
      * `ABSORBED` / `REJECTED`) -> `SOFT_CONFIRMED` (soft time) -> `HARD_CONFIRMED` (hard time, and
      * — only for an absorbed deposit — the absorbing settlement's `l1TxId`). Times are of the
      * **deciding** block. Present only for valid deposit requests.
      */
    sealed trait AbsorptionDecisionStatusView
    object AbsorptionDecisionStatusView:
        private given CirceConfig = circeTag(statusTag("Absorption"))
        private given TapirConfig = tapirTag(statusTag("Absorption"))

        case object AbsorptionUnprocessedView extends AbsorptionDecisionStatusView
        final case class AbsorptionProposedView(blockNumber: Int, decision: DecisionView)
            extends AbsorptionDecisionStatusView
        final case class AbsorptionSoftConfirmedView(
            blockNumber: Int,
            decision: DecisionView,
            softConfirmedAt: String
        ) extends AbsorptionDecisionStatusView
        final case class AbsorptionHardConfirmedView(
            blockNumber: Int,
            decision: DecisionView,
            softConfirmedAt: Option[String],
            hardConfirmedAt: String,
            settlementEffect: Option[String]
        ) extends AbsorptionDecisionStatusView
        given Codec[AbsorptionDecisionStatusView] = ConfiguredCodec.derived
        given Schema[AbsorptionDecisionStatusView] = Schema.derived

    /** The request-details body, `type`-tagged by request kind: `transaction` and `deposit` share
      * the opaque id (echoed from the path), author peer, receive time (when this peer received the
      * request — a local event, so different peers report different times, by design) and lifecycle
      * status; only `deposit` carries the absorb/reject decision status.
      */
    sealed trait RequestDetailsView
    object RequestDetailsView:
        // Request-kind sums tag with the lowercase kind (`transaction` / `deposit`), shadowing the
        // screaming-snake status tagging in the enclosing scope.
        private given CirceConfig = circeTag(kindTag)
        private given TapirConfig = tapirTag(kindTag)

        final case class TransactionView(
            requestId: Long,
            peerNumber: Int,
            receivedAt: String,
            status: RequestStatusView
        ) extends RequestDetailsView
        final case class DepositView(
            requestId: Long,
            peerNumber: Int,
            receivedAt: String,
            status: RequestStatusView,
            absorptionDecisionStatus: AbsorptionDecisionStatusView
        ) extends RequestDetailsView
        given Codec[RequestDetailsView] = ConfiguredCodec.derived
        given Schema[RequestDetailsView] = Schema.derived

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

    private def validityView(validity: ValidityFlag): ValidityView = validity match
        case ValidityFlag.Valid   => ValidityView.Valid
        case ValidityFlag.Invalid => ValidityView.Invalid

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
            case None => RequestStatusView.RequestUnprocessedView
            case Some((blockNumber, v)) =>
                val validity = validityView(v)
                hardConfirmedAt match
                    case Some(hard) =>
                        RequestStatusView.RequestHardConfirmedView(
                          blockNumber,
                          validity,
                          softConfirmedAt.map(_.toString),
                          hard.toString,
                          relatedEffects
                        )
                    case None =>
                        softConfirmedAt match
                            case Some(soft) =>
                                RequestStatusView.RequestSoftConfirmedView(
                                  blockNumber,
                                  validity,
                                  soft.toString
                                )
                            case None =>
                                RequestStatusView.RequestProposedView(blockNumber, validity)

    /** The `ABSORBED` / `REJECTED` label of a deposit decision. */
    def decisionView(decision: DepositDecision): DecisionView = decision match
        case _: DepositDecision.Absorbed => DecisionView.Absorbed
        case _: DepositDecision.Rejected => DecisionView.Rejected

    /** Assemble the deposit absorption-decision ladder: unprocessed (no decision row yet), else the
      * deciding block + `ABSORBED`/`REJECTED`, promoted by the deciding block's node-local
      * confirmation moments. The absorbing settlement's `l1TxId` (`ABSORBED`, hard-confirmed only)
      * rides on the hard-confirmed rung.
      */
    def mkAbsorptionDecisionStatus(
        decided: Option[(Int, DecisionView)],
        softConfirmedAt: Option[Instant],
        hardConfirmedAt: Option[Instant],
        settlementEffect: Option[String]
    ): AbsorptionDecisionStatusView =
        decided match
            case None => AbsorptionDecisionStatusView.AbsorptionUnprocessedView
            case Some((blockNumber, decision)) =>
                hardConfirmedAt match
                    case Some(hard) =>
                        AbsorptionDecisionStatusView.AbsorptionHardConfirmedView(
                          blockNumber,
                          decision,
                          softConfirmedAt.map(_.toString),
                          hard.toString,
                          settlementEffect
                        )
                    case None =>
                        softConfirmedAt match
                            case Some(soft) =>
                                AbsorptionDecisionStatusView.AbsorptionSoftConfirmedView(
                                  blockNumber,
                                  decision,
                                  soft.toString
                                )
                            case None =>
                                AbsorptionDecisionStatusView.AbsorptionProposedView(
                                  blockNumber,
                                  decision
                                )

    /** Map a request, its derived receive time, its lifecycle status, and — for deposits — its
      * absorption-decision status to the `type`-tagged details body.
      */
    def mkRequestDetailsView(
        request: UserRequestWithId,
        receivedAt: Instant,
        status: RequestStatusView,
        absorptionDecisionStatus: Option[AbsorptionDecisionStatusView]
    ): RequestDetailsView =
        val requestId = request.requestId.asI64
        val peerNumber = request.requestId.peerNum.convert
        request match
            case _: UserRequestWithId.DepositRequest =>
                RequestDetailsView.DepositView(
                  requestId,
                  peerNumber,
                  receivedAt.toString,
                  status,
                  absorptionDecisionStatus.getOrElse(
                    AbsorptionDecisionStatusView.AbsorptionUnprocessedView
                  )
                )
            case _: UserRequestWithId.TransactionRequest =>
                RequestDetailsView.TransactionView(
                  requestId,
                  peerNumber,
                  receivedAt.toString,
                  status
                )

    /** A block's L1 effects as `l1TxId`s, `type`-tagged by block type — each variant carries only
      * the effect kinds that block type produces: `initial` (initialization + fallback), `minor`
      * (an optional sec + refunds), `major` (settlement + fallback + rollouts + refunds), `final`
      * (finalization + rollouts).
      */
    sealed trait BlockEffectsView
    object BlockEffectsView:
        private val blockEffectsTag: String => String =
            name => name.stripPrefix("BlockEffects").stripSuffix("View").toLowerCase
        private given CirceConfig = circeTag(blockEffectsTag)
        private given TapirConfig = tapirTag(blockEffectsTag)

        final case class BlockEffectsInitialView(
            initialization: String,
            fallback: String
        ) extends BlockEffectsView
        final case class BlockEffectsMinorView(
            sec: Option[String],
            refunds: List[String]
        ) extends BlockEffectsView
        final case class BlockEffectsMajorView(
            settlement: String,
            fallback: String,
            rollouts: List[String],
            refunds: List[String]
        ) extends BlockEffectsView
        final case class BlockEffectsFinalView(
            finalization: String,
            rollouts: List[String]
        ) extends BlockEffectsView
        given Codec[BlockEffectsView] = ConfiguredCodec.derived
        given Schema[BlockEffectsView] = Schema.derived

    /** An L1 effect in full, `type`-tagged by kind — the `GET /head/effects/{l1TxId}` response. Its
      * `l1TxId` is the queried path, so it is not echoed. Each real-tx kind carries its `txCbor`
      * (hex); `sec` (a standalone evacuation commitment, whose synthetic hash is that l1TxId)
      * carries its serialized on-chain bytes and the hard-ack signatures split `headSignatures`
      * then `coilSignatures`.
      */
    sealed trait EffectView
    object EffectView:
        private given CirceConfig = circeTag(kindTag)
        private given TapirConfig = tapirTag(kindTag)

        final case class InitializationView(blockNumber: Int, txCbor: String) extends EffectView
        final case class SettlementView(blockNumber: Int, txCbor: String) extends EffectView
        final case class FallbackView(blockNumber: Int, txCbor: String) extends EffectView
        final case class RolloutView(blockNumber: Int, txCbor: String) extends EffectView
        final case class FinalizationView(blockNumber: Int, txCbor: String) extends EffectView
        final case class RefundView(blockNumber: Int, txCbor: String) extends EffectView
        final case class SecView(
            blockNumber: Int,
            secOnchainSerialized: String,
            headSignatures: List[String],
            coilSignatures: List[String]
        ) extends EffectView
        given Codec[EffectView] = ConfiguredCodec.derived
        given Schema[EffectView] = Schema.derived

    /** A real-tx effect at a path-fixed kind — the block-scoped by-kind endpoints' response (the
      * kind is the path, so it is not discriminated). Carries its `l1TxId` and `txCbor` (hex).
      */
    final case class TxEffectView(l1TxId: String, blockNumber: Int, txCbor: String)
    given Codec[TxEffectView] = deriveCodec

    /** A standalone evacuation commitment (SEC) effect — the block-scoped `sec` endpoint's
      * response. Carries its `l1TxId` (the synthetic hash), on-chain bytes, and split hard-ack
      * signatures.
      */
    final case class SecEffectView(
        l1TxId: String,
        blockNumber: Int,
        secOnchainSerialized: String,
        headSignatures: List[String],
        coilSignatures: List[String]
    )
    given Codec[SecEffectView] = deriveCodec

    /** The effect-kind label (matches the sub-resource path segments). */
    def effectKindView(kind: EffectKind): EffectKindView = kind match
        case EffectKind.Initialization => EffectKindView.Initialization
        case EffectKind.Settlement     => EffectKindView.Settlement
        case EffectKind.Fallback       => EffectKindView.Fallback
        case EffectKind.Rollout        => EffectKindView.Rollout
        case EffectKind.Finalization   => EffectKindView.Finalization
        case EffectKind.Refund         => EffectKindView.Refund
        case EffectKind.Sec            => EffectKindView.Sec

    /** Group a block's resolved effects into the per-block-type listing. */
    def mkBlockEffectsView(
        blockType: BlockTypeView,
        effects: List[ResolvedEffect]
    ): BlockEffectsView =
        def firstIdOf(k: EffectKind): Option[String] =
            effects.collectFirst { case e if e.kind == k => e.l1TxId.toHex }
        def idsOf(k: EffectKind): List[String] =
            effects.collect { case e if e.kind == k => e.l1TxId.toHex }
        // A hard-confirmed block of a given type always carries its type's required effects (these
        // views are only built for hard-confirmed blocks); a missing one is an invariant violation.
        def requireId(k: EffectKind): String =
            firstIdOf(k).getOrElse(
              throw new IllegalStateException(s"$blockType block missing its $k effect")
            )
        blockType match
            case BlockTypeView.Initial =>
                BlockEffectsView.BlockEffectsInitialView(
                  requireId(EffectKind.Initialization),
                  requireId(EffectKind.Fallback)
                )
            case BlockTypeView.Minor =>
                BlockEffectsView.BlockEffectsMinorView(
                  firstIdOf(EffectKind.Sec),
                  idsOf(EffectKind.Refund)
                )
            case BlockTypeView.Major =>
                BlockEffectsView.BlockEffectsMajorView(
                  requireId(EffectKind.Settlement),
                  requireId(EffectKind.Fallback),
                  idsOf(EffectKind.Rollout),
                  idsOf(EffectKind.Refund)
                )
            case BlockTypeView.Final =>
                BlockEffectsView.BlockEffectsFinalView(
                  requireId(EffectKind.Finalization),
                  idsOf(EffectKind.Rollout)
                )

    /** Map any resolved effect to the `type`-tagged by-id view (no `l1TxId` — it is the queried
      * path); `nHeadPeers` splits an SEC's hard-ack signatures into head (first `nHeadPeers`) then
      * coil.
      */
    def mkEffectView(effect: ResolvedEffect, nHeadPeers: Int): EffectView = effect match
        case tx: ResolvedEffect.Tx =>
            val blockNumber = tx.blockNumber.convert
            val cbor = txCborHex(tx)
            tx.kind match
                case EffectKind.Initialization => EffectView.InitializationView(blockNumber, cbor)
                case EffectKind.Settlement     => EffectView.SettlementView(blockNumber, cbor)
                case EffectKind.Fallback       => EffectView.FallbackView(blockNumber, cbor)
                case EffectKind.Rollout        => EffectView.RolloutView(blockNumber, cbor)
                case EffectKind.Finalization   => EffectView.FinalizationView(blockNumber, cbor)
                case EffectKind.Refund         => EffectView.RefundView(blockNumber, cbor)
                case EffectKind.Sec =>
                    throw new IllegalStateException(s"tx effect ${tx.l1TxId.toHex} tagged Sec")
        case sec: ResolvedEffect.Sec =>
            val (headSignatures, coilSignatures) = secSignatures(sec, nHeadPeers)
            EffectView.SecView(
              sec.blockNumber.convert,
              secOnchainHex(sec),
              headSignatures,
              coilSignatures
            )

    /** Map a real-tx effect to the block-scoped by-kind view (carries `l1TxId`). */
    def mkTxEffectView(tx: ResolvedEffect.Tx): TxEffectView =
        TxEffectView(tx.l1TxId.toHex, tx.blockNumber.convert, txCborHex(tx))

    /** Map an SEC effect to the block-scoped `sec` view (carries `l1TxId`). */
    def mkSecEffectView(sec: ResolvedEffect.Sec, nHeadPeers: Int): SecEffectView =
        val (headSignatures, coilSignatures) = secSignatures(sec, nHeadPeers)
        SecEffectView(
          sec.l1TxId.toHex,
          sec.blockNumber.convert,
          secOnchainHex(sec),
          headSignatures,
          coilSignatures
        )

    private def txCborHex(tx: ResolvedEffect.Tx): String =
        ByteString.fromArray(tx.tx.toCbor).toHex

    private def secOnchainHex(sec: ResolvedEffect.Sec): String =
        val headerBytes: Array[Byte] = sec.commitment.commitment.header
        ByteString.fromArray(headerBytes).toHex

    /** An SEC's hard-ack signatures (hex), split into head (first `nHeadPeers`) then coil. */
    private def secSignatures(
        sec: ResolvedEffect.Sec,
        nHeadPeers: Int
    ): (List[String], List[String]) =
        val sigsHex = sec.commitment.headerMultiSigned.map { sig =>
            val bytes: Array[Byte] = sig
            ByteString.fromArray(bytes).toHex
        }
        (sigsHex.take(nHeadPeers), sigsHex.drop(nHeadPeers))

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
