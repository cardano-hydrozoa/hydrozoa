package hydrozoa.multisig.server

import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.given
import hydrozoa.multisig.NodeStatus
import hydrozoa.multisig.consensus.UserRequestWithId
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
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.{DatumOption, TransactionInput, TransactionOutput, Value}
import scalus.uplc.builtin.ByteString
import sttp.tapir.Schema
import sttp.tapir.generic.Configuration as TapirConfig

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
      * their variants a sum-specific prefix (`RequestSoftConfirmed`) for a unique schema name, and
      * the transform strips the prefix so the `type` value stays the shared word. See
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

    /** The name transform for a status ladder: strip the sum-specific `prefix`, then
      * screaming-snake (`BlockSoftConfirmed` with prefix `Block` -> `SOFT_CONFIRMED`).
      */
    private def statusTag(prefix: String): String => String =
        name => screamingSnake(name.stripPrefix(prefix))

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

    /** A block's confirmation from this node's viewpoint, `type`-tagged: `PROPOSED` (woven, not yet
      * soft-confirmed) -> `SOFT_CONFIRMED` (+ soft time) -> `HARD_CONFIRMED` (+ hard time; soft
      * time optional, since a block may be hard-confirmed with no local soft-confirmation record).
      * Each time records a **local event at this peer**, so different peers report different times
      * for the same block. This is by design.
      */
    sealed trait BlockConfirmationView
    object BlockConfirmationView:
        private given CirceConfig = circeTag(statusTag("Block"))
        private given TapirConfig = tapirTag(statusTag("Block"))

        case object BlockProposed extends BlockConfirmationView
        final case class BlockSoftConfirmed(softConfirmedAt: String) extends BlockConfirmationView
        final case class BlockHardConfirmed(
            softConfirmedAt: Option[String],
            hardConfirmedAt: String
        ) extends BlockConfirmationView
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
        private given CirceConfig = circeTag(_.toLowerCase)
        private given TapirConfig = tapirTag(_.toLowerCase)

        final case class Initial(
            number: Int,
            versionMajor: Int,
            versionMinor: Int,
            startTime: String,
            endTime: String,
            fallbackTxStartTime: String,
            forcedMajorBlockWakeupTime: String,
            depositDecisionWakeupTime: Option[String]
        ) extends BlockHeaderView
        final case class Minor(
            number: Int,
            versionMajor: Int,
            versionMinor: Int,
            startTime: String,
            endTime: String,
            fallbackTxStartTime: String,
            forcedMajorBlockWakeupTime: String,
            depositDecisionWakeupTime: Option[String]
        ) extends BlockHeaderView
        final case class Major(
            number: Int,
            versionMajor: Int,
            versionMinor: Int,
            startTime: String,
            endTime: String,
            fallbackTxStartTime: String,
            forcedMajorBlockWakeupTime: String,
            depositDecisionWakeupTime: Option[String]
        ) extends BlockHeaderView
        final case class Final(
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
        blockType: String,
        stackId: Option[Int],
        confirmation: BlockConfirmationView,
        header: Option[BlockHeaderView]
    )
    given Codec[BlockDetailsView] = deriveCodec

    /** One of a block's transactions: the opaque request id woven into the block and the validity
      * verdict it received there.
      */
    final case class BlockRequestView(requestId: Long, validity: String)
    given Codec[BlockRequestView] = deriveCodec

    /** The block-body body — the block's content: its transactions (the requests woven into it,
      * each with its verdict) and its deposit decisions (the request ids absorbed into the treasury
      * and those rejected).
      */
    final case class BlockBodyView(
        number: Int,
        blockType: String,
        transactions: List[BlockRequestView],
        depositsAbsorbed: List[Long],
        depositsRejected: List[Long]
    )
    given Codec[BlockBodyView] = deriveCodec

    /** Map a block's brief to its content view. */
    def mkBlockBodyView(brief: BlockBrief.Next): BlockBodyView =
        BlockBodyView(
          number = brief.blockNum.convert,
          blockType = blockTypeName(brief),
          transactions = brief.requests.map((id, v) => BlockRequestView(id.asI64, validityName(v))),
          depositsAbsorbed = brief.depositsAbsorbed.map(_.asI64),
          depositsRejected = brief.depositsRejected.map(_.asI64)
        )

    /** The initial block (block 0) has no woven content. */
    def mkInitialBlockBodyView: BlockBodyView =
        BlockBodyView(number = 0, blockType = "initial", transactions = Nil, Nil, Nil)

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

    /** Assemble the confirmation rung from the node-local confirmation moments (present iff their
      * record is held — `wallClockOf` is total).
      */
    def mkBlockConfirmationView(
        softConfirmedAt: Option[Instant],
        hardConfirmedAt: Option[Instant]
    ): BlockConfirmationView =
        (softConfirmedAt, hardConfirmedAt) match
            case (soft, Some(hard)) =>
                BlockConfirmationView.BlockHardConfirmed(soft.map(_.toString), hard.toString)
            case (Some(soft), None) => BlockConfirmationView.BlockSoftConfirmed(soft.toString)
            case (None, None)       => BlockConfirmationView.BlockProposed

    private def blockTypeName(brief: BlockBrief.Next): String = brief match
        case _: BlockBrief.Minor => "minor"
        case _: BlockBrief.Major => "major"
        case _: BlockBrief.Final => "final"

    /** Map a block header to its `type`-tagged view, rendering the quantized timing as ISO-8601. */
    def mkBlockHeaderView(header: BlockHeader): BlockHeaderView =
        val number = header.blockNum.convert
        val versionMajor = header.blockVersion.major.convert
        val versionMinor = header.blockVersion.minor.convert
        val startTime = header.startTime.instant.toString
        val endTime = header.endTime.instant.toString
        header match
            case h: BlockHeader.Initial =>
                BlockHeaderView.Initial(
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
                BlockHeaderView.Minor(
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
                BlockHeaderView.Major(
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
                BlockHeaderView.Final(number, versionMajor, versionMinor, startTime, endTime)

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

        case object RequestUnprocessed extends RequestStatusView
        final case class RequestProposed(blockNumber: Int, validity: String)
            extends RequestStatusView
        final case class RequestSoftConfirmed(
            blockNumber: Int,
            validity: String,
            softConfirmedAt: String
        ) extends RequestStatusView
        final case class RequestHardConfirmed(
            blockNumber: Int,
            validity: String,
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

        case object AbsorptionUnprocessed extends AbsorptionDecisionStatusView
        final case class AbsorptionProposed(blockNumber: Int, decision: String)
            extends AbsorptionDecisionStatusView
        final case class AbsorptionSoftConfirmed(
            blockNumber: Int,
            decision: String,
            softConfirmedAt: String
        ) extends AbsorptionDecisionStatusView
        final case class AbsorptionHardConfirmed(
            blockNumber: Int,
            decision: String,
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
        private given CirceConfig = circeTag(_.toLowerCase)
        private given TapirConfig = tapirTag(_.toLowerCase)

        final case class Transaction(
            requestId: Long,
            peerNumber: Int,
            receivedAt: String,
            status: RequestStatusView
        ) extends RequestDetailsView
        final case class Deposit(
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
            case None => RequestStatusView.RequestUnprocessed
            case Some((blockNumber, v)) =>
                val validity = validityName(v)
                hardConfirmedAt match
                    case Some(hard) =>
                        RequestStatusView.RequestHardConfirmed(
                          blockNumber,
                          validity,
                          softConfirmedAt.map(_.toString),
                          hard.toString,
                          relatedEffects
                        )
                    case None =>
                        softConfirmedAt match
                            case Some(soft) =>
                                RequestStatusView.RequestSoftConfirmed(
                                  blockNumber,
                                  validity,
                                  soft.toString
                                )
                            case None =>
                                RequestStatusView.RequestProposed(blockNumber, validity)

    /** The `ABSORBED` / `REJECTED` label of a deposit decision. */
    def decisionName(decision: DepositDecision): String = decision match
        case _: DepositDecision.Absorbed => "ABSORBED"
        case _: DepositDecision.Rejected => "REJECTED"

    /** Assemble the deposit absorption-decision ladder: unprocessed (no decision row yet), else the
      * deciding block + `ABSORBED`/`REJECTED`, promoted by the deciding block's node-local
      * confirmation moments. The absorbing settlement's `l1TxId` (`ABSORBED`, hard-confirmed only)
      * rides on the hard-confirmed rung.
      */
    def mkAbsorptionDecisionStatus(
        decided: Option[(Int, String)],
        softConfirmedAt: Option[Instant],
        hardConfirmedAt: Option[Instant],
        settlementEffect: Option[String]
    ): AbsorptionDecisionStatusView =
        decided match
            case None => AbsorptionDecisionStatusView.AbsorptionUnprocessed
            case Some((blockNumber, decision)) =>
                hardConfirmedAt match
                    case Some(hard) =>
                        AbsorptionDecisionStatusView.AbsorptionHardConfirmed(
                          blockNumber,
                          decision,
                          softConfirmedAt.map(_.toString),
                          hard.toString,
                          settlementEffect
                        )
                    case None =>
                        softConfirmedAt match
                            case Some(soft) =>
                                AbsorptionDecisionStatusView.AbsorptionSoftConfirmed(
                                  blockNumber,
                                  decision,
                                  soft.toString
                                )
                            case None =>
                                AbsorptionDecisionStatusView.AbsorptionProposed(
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
                RequestDetailsView.Deposit(
                  requestId,
                  peerNumber,
                  receivedAt.toString,
                  status,
                  absorptionDecisionStatus.getOrElse(
                    AbsorptionDecisionStatusView.AbsorptionUnprocessed
                  )
                )
            case _: UserRequestWithId.TransactionRequest =>
                RequestDetailsView.Transaction(requestId, peerNumber, receivedAt.toString, status)

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
