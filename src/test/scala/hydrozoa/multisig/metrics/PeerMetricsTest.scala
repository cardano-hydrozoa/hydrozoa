package hydrozoa.multisig.metrics

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*

class PeerMetricsTest extends AnyFunSuite:

    private def fresh(peers: Vector[Int] = Vector(0, 1)): PeerMetrics =
        PeerMetrics.create(0L, peers)

    test("local accepted/rejected counters accumulate and split by cause"):
        val m = fresh()
        (1 to 5).foreach(_ => m.onLocalAccepted())
        (1 to 3).foreach(_ => m.onLocalRejected(RejectionKind.Screening))
        (1 to 7).foreach(_ => m.onLocalRejected(RejectionKind.Backpressure))
        val s = m.snapshot(1000L)
        assert(
          s.localAccepted == 5 && s.localRejScreening == 3 &&
              s.localRejBackpressure == 7 && s.uptimeSeconds == 1,
          s"snapshot: $s"
        )

    test("peer requests are counted per peer; unknown peers are dropped, not crashed"):
        val m = fresh(Vector(0, 1, 2))
        m.onPeerRequests(1, 4)
        m.onPeerRequests(1, 6)
        m.onPeerRequests(2, 3)
        m.onPeerRequests(99, 100) // unknown -> ignored
        val s = m.snapshot(0L)
        assert(
          s.peerRequests(1).total == 10 && s.peerRequests(2).total == 3 &&
              s.peerRequests(0).total == 0 && !s.peerRequests.contains(99),
          s"peerRequests: ${s.peerRequests}"
        )

    test("block stats: minor/major counts, average and max events; empty is zero"):
        val m = fresh()
        m.onBlockConfirmed(isMajor = false, events = 10)
        m.onBlockConfirmed(isMajor = false, events = 20)
        m.onBlockConfirmed(isMajor = true, events = 30)
        val b = m.snapshot(0L).blocks
        assert(
          b.minor == 2 && b.major == 1 && b.maxEvents == 30 && b.avgEvents == 20.0 &&
              fresh().snapshot(0L).blocks.avgEvents == 0.0,
          s"blocks: $b"
        )

    test("stack stats: totals, size, mean inter-stack gap, and seconds-since"):
        val m = fresh()
        m.onStackConfirmed(stackNum = 1, blocksAbsorbed = 100, nowMillis = 1000L)
        m.onStackConfirmed(stackNum = 2, blocksAbsorbed = 200, nowMillis = 4000L) // gap 3s
        m.onStackConfirmed(stackNum = 3, blocksAbsorbed = 300, nowMillis = 7000L) // gap 3s
        val s = m.snapshot(10000L).stacks
        assert(
          s.total == 3 && s.lastStackNumber == 3 && s.maxBlocksAbsorbed == 300 &&
              s.avgBlocksAbsorbed == 200.0 && s.meanInterStackGapSeconds == 3.0 &&
              s.secondsSinceLastHardConfirm == 3,
          s"stacks: $s"
        )

    test(
      "block-lifecycle timings route lead vs replay, keep top blocks by number, and gauges publish"
    ):
        val m = fresh()
        // Two led blocks and one replayed one; each closed at onBlockProduced.
        m.onLeadStart(5)
        m.onBlockProduced(5, requests = 10)
        m.onLeadStart(7)
        m.onBlockProduced(7, requests = 20)
        m.onReplayStart(6)
        m.onBlockProduced(6, requests = 3)
        // A produced block with no start opened is ignored, not crashed.
        m.onBlockProduced(999, requests = 5)
        // Soft-consensus is its own clock (cell spawned -> confirmed).
        m.onCellSpawned(5)
        m.onCellConfirmed(5)
        m.onMempoolSize(42)
        m.onSequencerLimit(limit = 3000, headroom = 12)
        val s = m.snapshot(0L)
        val bt = s.blockTimings
        assert(
          bt.lead.count == 2 && bt.replay.count == 1 && bt.softConsensus.count == 1 &&
              bt.lead.top.map(_.blockNumber).toSet == Set(5L, 7L) &&
              bt.replay.top.map(_.blockNumber) == List(6L) &&
              bt.softConsensus.avgMillisPerRequest == 0.0 &&
              s.mempoolSize == 42 && s.sequencerLimit == 3000 && s.sequencerHeadroom == 12,
          s"blockTimings=$bt mempool=${s.mempoolSize} seq=${s.sequencerLimit}/${s.sequencerHeadroom}"
        )

    test("sampler moves the EWMA load averages for local, block, and request rates after activity"):
        val program = TestControl.executeEmbed {
            for {
                m <- IO.realTime.map(t => PeerMetrics.create(t.toMillis, Vector(0, 1)))
                fiber <- m.sampler(1.second).start
                // one local request + one 2-event block per second for 5 seconds
                _ <- (1 to 5).toList.traverse_ { _ =>
                    IO {
                        m.onLocalAccepted()
                        m.onBlockConfirmed(isMajor = false, events = 2)
                    } >> IO.sleep(1.second)
                }
                _ <- IO.sleep(1.second) // let the sampler observe the last tick
                s <- IO.realTime.flatMap(t => IO(m.snapshot(t.toMillis)))
                _ <- fiber.cancel
            } yield s
        }
        val s = program.unsafeRunSync()
        assert(
          s.blocks.minor == 5 &&
              s.localRate.load1m > 0.0 && // steady local traffic registers on the EWMA
              s.blocks.blockRate.load1m > 0.0 && // ~1 block/s
              s.blocks.requestRate.load1m > s.blocks.blockRate.load1m, // 2 events per block
          s"snapshot: $s"
        )
