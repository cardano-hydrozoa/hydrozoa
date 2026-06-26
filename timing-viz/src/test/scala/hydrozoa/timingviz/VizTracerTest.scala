package hydrozoa.timingviz

import cats.effect.unsafe.implicits.global
import cats.effect.{
  IO,
  Ref
}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, FallbackTxStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.multisig.HeadMultisigRegimeManagerEvent as MRMEvent
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.JointLedgerEvent
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.nowarn
import scala.concurrent.duration.DurationInt

@nowarn("msg=unused value of type org.scalatest.compatible.Assertion")
class VizTracerTest extends AnyFunSuite:

    private val network = CardanoNetwork.Preview
    private val slotConfig = network.slotConfig
    private val t0 = java.time.Instant.ofEpochMilli(1_700_000_000_000L).quantize(slotConfig)

    test("stateless mapping: BlockCompleting → ObserveBlock (zero-duration)"):
        val captured: List[Command] = (for
            ref <- Ref.of[IO, List[Command]](Nil)
            tr = VizTracer.stateless(c => ref.update(c :: _))
            _ <- tr.traceWith(
              MRMEvent.JointLedger(
                JointLedgerEvent.BlockCompleting(
                  blockNum = BlockNumber(7),
                  endTime = BlockCreationEndTime(t0),
                  competingFallbackTxTime = FallbackTxStartTime(t0),
                  splitDescription = "test"
                )
              )
            )
            cmds <- ref.get
        yield cmds).unsafeRunSync()
        assert(captured.size == 1)
        captured.head match
            case Command.ObserveBlock(_, Interval(start, end), _) =>
                assert(start == end, "stateless mapping produces zero-duration interval")
            case other => fail(s"expected ObserveBlock, got $other")

    test("stateful mapping: BlockStarted + BlockCompleting → ObserveBlock with proper interval"):
        val startTime = t0
        val endTime = t0 + 100.millis
        val captured: List[Command] = (for
            ref <- Ref.of[IO, List[Command]](Nil)
            tr <- VizTracer.make(c => ref.update(c :: _))
            _ <- tr.traceWith(
              MRMEvent.JointLedger(
                JointLedgerEvent.BlockStarted(
                  blockNum = BlockNumber(7),
                  startTime = BlockCreationStartTime(startTime)
                )
              )
            )
            _ <- tr.traceWith(
              MRMEvent.JointLedger(
                JointLedgerEvent.BlockCompleting(
                  blockNum = BlockNumber(7),
                  endTime = BlockCreationEndTime(endTime),
                  competingFallbackTxTime = FallbackTxStartTime(endTime),
                  splitDescription = "test"
                )
              )
            )
            cmds <- ref.get
        yield cmds).unsafeRunSync()
        assert(captured.size == 1)
        captured.head match
            case Command.ObserveBlock(_, Interval(s, e), _) =>
                assert(s.instant.toEpochMilli == startTime.instant.toEpochMilli)
                assert(e.instant.toEpochMilli == endTime.instant.toEpochMilli)
            case other => fail(s"expected ObserveBlock, got $other")

    test("stateful: orphan BlockCompleting (no BlockStarted seen) falls back to zero-duration"):
        val captured: List[Command] = (for
            ref <- Ref.of[IO, List[Command]](Nil)
            tr <- VizTracer.make(c => ref.update(c :: _))
            _ <- tr.traceWith(
              MRMEvent.JointLedger(
                JointLedgerEvent.BlockCompleting(
                  blockNum = BlockNumber(99),
                  endTime = BlockCreationEndTime(t0),
                  competingFallbackTxTime = FallbackTxStartTime(t0),
                  splitDescription = "test"
                )
              )
            )
            cmds <- ref.get
        yield cmds).unsafeRunSync()
        assert(captured.size == 1)
        captured.head match
            case Command.ObserveBlock(_, Interval(s, e), _) => assert(s == e)
            case other => fail(s"expected ObserveBlock, got $other")

    test("unmapped events produce no commands"):
        val captured: List[Command] = (for
            ref <- Ref.of[IO, List[Command]](Nil)
            tr <- VizTracer.make(c => ref.update(c :: _))
            _ <- tr.traceWith(MRMEvent.StartingActors)
            _ <- tr.traceWith(MRMEvent.WatchingActors)
            cmds <- ref.get
        yield cmds).unsafeRunSync()
        assert(captured.isEmpty)
