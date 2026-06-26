package hydrozoa.timingviz

import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.timingviz.Codecs.given
import hydrozoa.timingviz.Command.*
import hydrozoa.timingviz.Ids.*
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scala.concurrent.duration.DurationInt

@nowarn("msg=unused value of type org.scalatest.compatible.Assertion")
class PresentationTest extends AnyFunSuite:

    private val network = CardanoNetwork.Preview
    private val slotConfig = network.slotConfig
    private val cfg = TxTiming.default(slotConfig)
    private val t0 = java.time.Instant.ofEpochMilli(1_700_000_000_000L).quantize(slotConfig)
    private given CardanoNetwork.Section = network

    test("Deposit expands to 4 bars + 1 marker on the deposits track"):
        val s0 = TimingVisualizerState.empty(cfg, t0)
        val did = DepositId("d1")
        val end = RequestTimes.RequestValidityEndTime(t0 + 1.hour)
        val (s, _) = TimingVisualizer(ObserveDepositRequest(did, end)).run(s0).value
        val frame = Presentation.render(s)
        val bars = frame.tracks.deposits.collect { case b: TrackItem.Bar => b }
        val markers = frame.tracks.deposits.collect { case m: TrackItem.Marker => m }
        assert(bars.size == 4, s"expected 4 bars, got ${bars.size}")
        assert(markers.size == 1, s"expected 1 marker, got ${markers.size}")
        assert(
          bars.map(_.kind).toSet == Set(
            ItemKind.DepositSubmissionWindow,
            ItemKind.DepositMaturityWait,
            ItemKind.DepositAbsorptionWindow,
            ItemKind.DepositSilenceWindow
          )
        )
        assert(markers.head.kind == ItemKind.DepositRefundStart)

    test("Block lands on the blocks track only"):
        val s0 = TimingVisualizerState.empty(cfg, t0)
        val bid = BlockId("b1")
        val (s, _) = TimingVisualizer(
          ObserveBlock(bid, Interval(t0, t0 + 1.second), BlockKind.Minor)
        ).run(s0).value
        val frame = Presentation.render(s)
        assert(frame.tracks.blocks.size == 1)
        assert(frame.tracks.requests.isEmpty)
        assert(frame.tracks.deposits.isEmpty)

    test("Frame round-trips through JSON encoding"):
        val s0 = TimingVisualizerState.empty(cfg, t0)
        val (s, _) = TimingVisualizer(
          ObserveBlock(BlockId("b1"), Interval(t0, t0 + 1.second), BlockKind.Major)
        ).run(s0).value
        val frame = Presentation.render(s)
        val json = frame.asJson.noSpaces
        assert(json.contains("\"nowMs\""))
        assert(json.contains("\"blocks\""))
        assert(json.contains("\"kind\":\"BlockCreation\""))
        assert(json.contains("Major")) // appears in the item label

    test("Command round-trips through JSON"):
        val cmd: Command = ObserveBlock(BlockId("b1"), Interval(t0, t0 + 1.second), BlockKind.Minor)
        val json = cmd.asJson.noSpaces
        val parsed = io.circe.parser.parse(json).flatMap(_.as[Command])
        parsed match
            case Right(c) => assert(c == cmd)
            case Left(e)  => fail(s"round-trip failed: $e")

    test("Source tag appears in encoded items"):
        val s0 = TimingVisualizerState.empty(cfg, t0)
        val (s, _) = TimingVisualizer(
          HypothesizeBlock(BlockId("h1"), Interval(t0, t0 + 1.second), BlockKind.Minor)
        ).run(s0).value
        val item = Presentation.render(s).tracks.blocks.head
        val src = item match
            case b: TrackItem.Bar    => b.source
            case m: TrackItem.Marker => m.source
        assert(src == Source.Hypothetical)

    test("Fallback paired to a SpineEffect emits a derivation edge"):
        val s0 = TimingVisualizerState.empty(cfg, t0)
        val spineId = EffectId("s1")
        val fallbackId = EffectId("f1")
        val blockId = BlockId("b1")
        val program = for
            _ <- TimingVisualizer(
              ObserveSpineEffect(
                spineId,
                SpineKind.Settlement,
                blockId,
                Interval(t0, t0 + 12.hours)
              )
            )
            _ <- TimingVisualizer(
              ObserveFallback(
                fallbackId,
                spineId,
                hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes
                    .FallbackTxStartTime(t0 + 13.hours)
              )
            )
        yield ()
        val (s, _) = program.run(s0).value
        val edges = Presentation.render(s).derivations
        assert(edges.size == 1, s"expected one edge, got $edges")
        val edge = edges.head
        assert(edge.targetObject == ObjectId.OfEffect(spineId))
        assert(edge.targetField == FieldKey.ValidityEnd)
        assert(edge.sourceObject == ObjectId.OfEffect(fallbackId))
        assert(edge.sourceField == FieldKey.FallbackStart)
