package hydrozoa.timingviz

import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, quantize}
import hydrozoa.timingviz.Command.*
import hydrozoa.timingviz.Ids.*
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scala.concurrent.duration.DurationInt

// ScalaTest's `assert`/`succeed`/`fail` return Assertion; tests stacking multiple of them
// trip -Wvalue-discard. The fact-as-side-effect model is fundamental to ScalaTest.
@nowarn("msg=unused value of type org.scalatest.compatible.Assertion")
class TimingVisualizerTest extends AnyFunSuite:

    private val slotConfig = CardanoNetwork.Preview.slotConfig
    private val cfg = TxTiming.default(slotConfig)
    private val t0 = java.time.Instant.ofEpochMilli(1_700_000_000_000L).quantize(slotConfig)

    private def freshState: TimingVisualizerState =
        TimingVisualizerState.empty(cfg, t0)

    private def interval(offsetMin: Int, lengthMs: Int = 1000): Interval =
        Interval(t0 + offsetMin.minutes, t0 + offsetMin.minutes + lengthMs.millis)

    test("AdvanceClock forward succeeds and updates now"):
        val (s, out) = TimingVisualizer(AdvanceClock(t0 + 5.minutes)).run(freshState).value
        assert(out.result.isRight)
        assert(s.now == t0 + 5.minutes)

    test("AdvanceClock backward succeeds (scrubbing is allowed)"):
        val program = for
            _ <- TimingVisualizer(AdvanceClock(t0 + 10.minutes))
            r <- TimingVisualizer(AdvanceClock(t0 + 5.minutes))
        yield r
        val (s, out) = program.run(freshState).value
        assert(out.result.isRight)
        assert(s.now == t0 + 5.minutes)

    test("SetParameter updates the named TxTiming field"):
        val newSilence = QuantizedFiniteDuration(slotConfig, 7.minutes)
        val (s, out) =
            TimingVisualizer(SetParameter(ParameterKey.SilenceDuration, newSilence))
                .run(freshState)
                .value
        assert(out.result.isRight)
        assert(s.config.silenceDuration.convert == newSilence)

    test("ObserveBlock inserts a Block; duplicate rejected; invalid interval rejected"):
        val bid = BlockId("b1")
        val good = interval(offsetMin = 0)
        val bad = Interval(t0 + 5.minutes, t0) // end before start
        val program = for
            r1 <- TimingVisualizer(ObserveBlock(bid, good, BlockKind.Minor))
            r2 <- TimingVisualizer(ObserveBlock(bid, good, BlockKind.Minor))
            r3 <- TimingVisualizer(ObserveBlock(BlockId("b2"), bad, BlockKind.Minor))
        yield (r1, r2, r3)
        val (s, (r1, r2, r3)) = program.run(freshState).value
        assert(r1.result.isRight, s"first insert: $r1")
        assert(r2.result == Left(Rejection.DuplicateObject(ObjectId.OfBlock(bid))))
        r3.result match
            case Left(_: Rejection.InvalidInterval) => ()
            case other                              => fail(s"expected InvalidInterval, got $other")
        assert(s.objects.contains(ObjectId.OfBlock(bid)))
        assert(!s.objects.contains(ObjectId.OfBlock(BlockId("b2"))))

    test("Deposit lifecycle: request → on-chain → absorbed"):
        val did = DepositId("d1")
        val end = RequestTimes.RequestValidityEndTime(t0 + 1.hour)
        val block = BlockId("b1")
        val program = for
            _ <- TimingVisualizer(ObserveDepositRequest(did, end))
            _ <- TimingVisualizer(ObserveDepositOnChain(did))
            _ <- TimingVisualizer(ObserveDepositAbsorbed(did, block))
        yield ()
        val (s, _) = program.run(freshState).value
        val tagged = s.objects(ObjectId.OfDeposit(did))
        val deposit = tagged.timedObject.asInstanceOf[TimedObject.Deposit]
        assert(deposit.status == DepositStatus.Absorbed(block))
        assert(tagged.source == Source.Observed)

    test("HypothesizeDeposit tags as Hypothetical"):
        val did = DepositId("h1")
        val end = RequestTimes.RequestValidityEndTime(t0 + 1.hour)
        val (s, _) = TimingVisualizer(HypothesizeDeposit(did, end)).run(freshState).value
        assert(s.objects(ObjectId.OfDeposit(did)).source == Source.Hypothetical)

    test("Deposit status transition on unknown id rejects"):
        val (s, out) = TimingVisualizer(ObserveDepositOnChain(DepositId("nope")))
            .run(freshState)
            .value
        out.result match
            case Left(_: Rejection.UnknownObject) => ()
            case other                            => fail(s"expected UnknownObject, got $other")
        assert(s.objects.isEmpty)

    test("Retract removes the object and prunes provenance"):
        val bid = BlockId("b1")
        val good = interval(offsetMin = 0)
        val program = for
            _ <- TimingVisualizer(ObserveBlock(bid, good, BlockKind.Minor))
            r <- TimingVisualizer(Retract(ObjectId.OfBlock(bid)))
        yield r
        val (s, out) = program.run(freshState).value
        assert(out.result.isRight)
        assert(s.objects.isEmpty)

    test("Retract on unknown id rejects"):
        val (s, out) = TimingVisualizer(Retract(ObjectId.OfBlock(BlockId("never-seen"))))
            .run(freshState)
            .value
        assert(out.result.isLeft)
        assert(s.objects.isEmpty)
