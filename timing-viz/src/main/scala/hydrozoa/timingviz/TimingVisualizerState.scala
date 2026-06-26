package hydrozoa.timingviz

import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.timingviz.Ids.ObjectId

/** Everything the visualizer needs to render a frame.
  *
  *   - `config` carries the six free timing parameters (and the slot config they're quantized to).
  *   - `now` is the visualizer's clock; advanced by `AdvanceClock` commands.
  *   - `objects` is keyed by `ObjectId` so every timed thing has a single home.
  *   - `derivations` is the constraint provenance graph; populated by transitions that compute
  *     derived times from observed/parameter inputs.
  */
final case class TimingVisualizerState(
    config: TxTiming,
    now: QuantizedInstant,
    objects: Map[ObjectId, TaggedObject],
    derivations: DerivationMap
)

object TimingVisualizerState:
    def empty(config: TxTiming, now: QuantizedInstant): TimingVisualizerState =
        TimingVisualizerState(config, now, Map.empty, Map.empty)
