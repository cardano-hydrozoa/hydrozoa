package hydrozoa.timingviz

import hydrozoa.config.head.multisig.timing.TxTiming.{BlockTimes, RequestTimes}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.timingviz.Ids.*

final case class Interval(start: QuantizedInstant, end: QuantizedInstant)

enum BlockKind:
    case Initial, Minor, Major, Final

enum SpineKind:
    case Init, Settlement, Finalization

enum DepositStatus:
    case Pending, OnChain, Mature
    case Absorbed(by: BlockId)
    case Rejected(by: BlockId)
    case Refunded

/** A positioned object on the timeline. The `track` extension drives which visual pane it lands in;
  * see [[Track]].
  */
enum TimedObject:
    case Request(id: RequestId, validity: Interval)
    case Block(id: BlockId, creation: Interval, kind: BlockKind, forced: Boolean)
    case SpineEffect(
        id: EffectId,
        kind: SpineKind,
        validity: Interval,
        blockId: BlockId
    )
    case Fallback(
        id: EffectId,
        start: BlockTimes.FallbackTxStartTime,
        pairedEffectId: EffectId
    )
    case Deposit(
        id: DepositId,
        requestValidityEnd: RequestTimes.RequestValidityEndTime,
        status: DepositStatus
    )
    case Refund(
        id: EffectId,
        start: hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant,
        depositId: DepositId
    )

enum Track:
    case Requests, Blocks, SpineEffects, Fallbacks, Deposits, Refunds

object TimedObject:
    extension (o: TimedObject)
        def objectId: ObjectId = o match
            case Request(id, _)           => ObjectId.OfRequest(id)
            case Block(id, _, _, _)       => ObjectId.OfBlock(id)
            case SpineEffect(id, _, _, _) => ObjectId.OfEffect(id)
            case Fallback(id, _, _)       => ObjectId.OfEffect(id)
            case Deposit(id, _, _)        => ObjectId.OfDeposit(id)
            case Refund(id, _, _)         => ObjectId.OfEffect(id)

        def track: Track = o match
            case _: Request     => Track.Requests
            case _: Block       => Track.Blocks
            case _: SpineEffect => Track.SpineEffects
            case _: Fallback    => Track.Fallbacks
            case _: Deposit     => Track.Deposits
            case _: Refund      => Track.Refunds
