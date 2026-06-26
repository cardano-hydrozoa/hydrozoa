package hydrozoa.timingviz

import hydrozoa.timingviz.Ids.ObjectId

/** Visual classification for a [[TrackItem]]. The renderer maps each kind to a color/icon/dash
  * style; the field is also the closest thing to a "type" the frontend reasons about.
  */
enum ItemKind:
    case Request
    case BlockCreation
    case SpineInit, SpineSettlement, SpineFinalization
    case Fallback
    case Refund
    case DepositSubmissionWindow
    case DepositMaturityWait
    case DepositAbsorptionWindow
    case DepositSilenceWindow
    case DepositRefundStart

/** One renderable entry on a track. `Bar` spans an interval; `Marker` sits at an instant. */
enum TrackItem:
    case Bar(
        id: String,
        objectId: ObjectId,
        label: String,
        kind: ItemKind,
        source: Source,
        startMs: Long,
        endMs: Long
    )
    case Marker(
        id: String,
        objectId: ObjectId,
        label: String,
        kind: ItemKind,
        source: Source,
        atMs: Long
    )

/** Tracks are grouped into named lanes for the renderer's vertical layout. */
final case class TracksFrame(
    requests: List[TrackItem],
    blocks: List[TrackItem],
    spineEffects: List[TrackItem],
    fallbacks: List[TrackItem],
    deposits: List[TrackItem],
    refunds: List[TrackItem]
)

/** Wire-format view of `TxTiming`. Durations are milliseconds. */
final case class ConfigFrame(
    minSettlementMs: Long,
    inactivityMarginMs: Long,
    silenceMs: Long,
    depositSubmissionMs: Long,
    depositMaturityMs: Long,
    depositAbsorptionMs: Long
)

/** "Field F of object T was derived from field G of object S." Frontend uses these to highlight
  * related items on hover.
  */
final case class DerivationEdge(
    targetObject: ObjectId,
    targetField: FieldKey,
    sourceObject: ObjectId,
    sourceField: FieldKey
)

/** A complete snapshot for the renderer. Sent over WebSocket on every state change. */
final case class Frame(
    nowMs: Long,
    config: ConfigFrame,
    tracks: TracksFrame,
    derivations: List[DerivationEdge]
)
