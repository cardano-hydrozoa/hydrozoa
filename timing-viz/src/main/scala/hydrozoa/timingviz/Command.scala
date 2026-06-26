package hydrozoa.timingviz

import hydrozoa.config.head.multisig.timing.TxTiming.{BlockTimes, RequestTimes}
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.timingviz.Ids.*

/** The six free parameters from `TxTiming`. */
enum ParameterKey:
    case MinSettlementDuration
    case InactivityMarginDuration
    case SilenceDuration
    case DepositSubmissionDuration
    case DepositMaturityDuration
    case DepositAbsorptionDuration

/** The visualizer's input alphabet. Three flavors:
  *   - clock/parameter knobs (`AdvanceClock`, `SetParameter`)
  *   - `Observe*` events from logs/tracers/WS (the canonical history of a real run)
  *   - `Hypothesize*` events from the UI in draft mode
  * Plus `Retract` for undoing either kind.
  */
enum Command:
    case AdvanceClock(to: QuantizedInstant)
    case SetParameter(key: ParameterKey, value: QuantizedFiniteDuration)

    case ObserveBlock(id: BlockId, creation: Interval, kind: BlockKind)
    case ObserveRequest(id: RequestId, validity: Interval)
    case ObserveDepositRequest(id: DepositId, end: RequestTimes.RequestValidityEndTime)
    case ObserveDepositOnChain(id: DepositId)
    case ObserveDepositAbsorbed(id: DepositId, byBlock: BlockId)
    case ObserveDepositRejected(id: DepositId, byBlock: BlockId)
    case ObserveSpineEffect(
        id: EffectId,
        kind: SpineKind,
        blockId: BlockId,
        validity: Interval
    )
    case ObserveFallback(
        id: EffectId,
        pairedEffectId: EffectId,
        start: BlockTimes.FallbackTxStartTime
    )
    case ObserveRefund(
        id: EffectId,
        depositId: DepositId,
        start: hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
    )

    case HypothesizeDeposit(id: DepositId, end: RequestTimes.RequestValidityEndTime)
    case HypothesizeBlock(id: BlockId, creation: Interval, kind: BlockKind)

    case Retract(id: ObjectId)
