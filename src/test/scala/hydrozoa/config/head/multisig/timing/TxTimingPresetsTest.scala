package hydrozoa.config.head.multisig.timing

import hydrozoa.config.head.network.CardanoNetwork
import org.scalatest.funsuite.AnyFunSuite

class TxTimingPresetsTest extends AnyFunSuite:

    private val slotConfig = CardanoNetwork.Preview.slotConfig

    // Each built-in preset is routed through the validating `mk`, which requires
    // depositAbsorptionDuration > minSettlementDuration + inactivityMarginDuration and throws
    // otherwise. So merely constructing every preset is the assertion that they all satisfy it.
    test("built-in TxTiming presets satisfy the depositAbsorptionDuration invariant"):
        val presets = List(
          TxTiming.default(slotConfig),
          TxTiming.yaci(slotConfig),
          TxTiming.demo(slotConfig),
          TxTiming.testnet(slotConfig)
        )
        assert(presets.sizeIs == 4)

    // A config whose absorption window is too small must be rejected, not silently built.
    test(
      "mk rejects depositAbsorptionDuration <= minSettlementDuration + inactivityMarginDuration"
    ):
        import TxTiming.Durations.*
        import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
        import scala.concurrent.duration.DurationInt
        val bad = TxTiming.mk(
          MinSettlementDuration(1.hour.quantize(slotConfig)),
          InactivityMarginDuration(2.hours.quantize(slotConfig)),
          SilenceDuration(2.minutes.quantize(slotConfig)),
          DepositSubmissionDuration(2.minutes.quantize(slotConfig)),
          DepositMaturityDuration(3.minutes.quantize(slotConfig)),
          DepositAbsorptionDuration(3.hours.quantize(slotConfig)) // == 1h + 2h, not >
        )
        assert(bad.isLeft, s"expected Left, got $bad")
