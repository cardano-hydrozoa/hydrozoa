package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, FallbackTxStartTime}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.consensus.StackComposer.{ReadyBlock, countMajors, takeUpToMajors}
import hydrozoa.multisig.ledger.block.{Block, BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockResult, BlockVersion}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.SlotConfig

/** The stack's Major-block cap: a stack covers at most `maxMajorBlocksPerStack` Major blocks, and
  * the overflow is held for the next stack rather than dropped.
  *
  * Minor blocks are deliberately uncapped — a run of them collapses into one partition, so they
  * cost a stack almost nothing (docs/spec/slow-consensus.md, "Partition model").
  */
class StackMajorCapTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    private val slotConfig = SlotConfig.preprod
    private val txTiming = TxTiming.default(slotConfig)
    private val now = realTimeQuantizedInstant(slotConfig).unsafeRunSync()

    private val start = BlockCreationStartTime(now)
    private val end = BlockCreationEndTime(now)
    private val fallback = FallbackTxStartTime(now)
    private val forced = txTiming.forcedMajorBlockWakeupTime(fallback)

    private def mkMajor(n: Int): ReadyBlock = {
        val brief = BlockBrief.Major(
          BlockHeader.Major(
            blockNum = BlockNumber(n),
            blockVersion = BlockVersion.Full(n, 0),
            startTime = start,
            endTime = end,
            fallbackTxStartTime = fallback,
            forcedMajorBlockWakeupTime = forced,
            mDepositDecisionWakeupTime = None
          ),
          BlockBody.Major(requests = Nil, depositsAbsorbed = Nil, depositsRejected = Nil)
        )
        ReadyBlock(
          BlockResult(brief, Nil, Nil, Nil, Nil, Nil, fallback),
          Block.SoftConfirmed.Major(brief, Nil, finalizationRequested = false)
        )
    }

    private def mkMinor(n: Int): ReadyBlock = {
        val brief = BlockBrief.Minor(
          BlockHeader.Minor(
            blockNum = BlockNumber(n),
            blockVersion = BlockVersion.Full(0, n),
            startTime = start,
            endTime = end,
            fallbackTxStartTime = fallback,
            forcedMajorBlockWakeupTime = forced,
            mDepositDecisionWakeupTime = None
          ),
          BlockBody.Minor(requests = Nil, depositsRejected = Nil)
        )
        ReadyBlock(
          BlockResult(brief, Nil, Nil, Nil, Nil, Nil, fallback),
          Block.SoftConfirmed.Minor(brief, Nil, finalizationRequested = false)
        )
    }

    /** A run of blocks, each independently Major or Minor, numbered from 1. */
    private val genRun: Gen[List[ReadyBlock]] =
        for {
            kinds <- Gen.listOfN(30, Gen.oneOf(true, false))
        } yield kinds.zipWithIndex.map { case (isMajor, i) =>
            if isMajor then mkMajor(i + 1) else mkMinor(i + 1)
        }

    private val genCap: Gen[Int] = Gen.choose(1, 10)

    test("a capped prefix never covers more majors than the cap") {
        forAll(genRun, genCap) { (run, cap) =>
            assert(countMajors(takeUpToMajors(run, cap)) <= cap)
        }
    }

    test("the capped prefix is a prefix of the run — nothing is reordered or dropped from within") {
        forAll(genRun, genCap) { (run, cap) =>
            val taken = takeUpToMajors(run, cap)
            assert(run.take(taken.length) == taken)
        }
    }

    test("truncation cuts on a major, so the held remainder opens the next stack") {
        forAll(genRun, genCap) { (run, cap) =>
            val taken = takeUpToMajors(run, cap)
            val dropped = run.drop(taken.length)
            // Cutting before the (cap+1)-th Major means the first held block is that Major — never
            // a Minor, which would needlessly split a Major's trailing run.
            whenever(dropped.nonEmpty) {
                assert(countMajors(List(dropped.head)) == 1 && countMajors(taken) == cap)
            }
        }
    }

    test("a run within the cap is taken whole") {
        forAll(genRun) { run =>
            // A cap at least as large as the run's own major count must not truncate it. Stated
            // directly rather than filtered for: with 30 blocks the discard rate would be total.
            assert(takeUpToMajors(run, countMajors(run).max(1)) == run)
        }
    }

    test("minors alone never trigger truncation") {
        forAll(genCap) { cap =>
            val minors = (1 to 50).map(mkMinor).toList
            assert(takeUpToMajors(minors, cap) == minors)
        }
    }

    test("a positive cap always admits the first major, so a stack is never empty") {
        forAll(genRun, genCap) { (run, cap) =>
            whenever(run.nonEmpty) {
                assert(takeUpToMajors(run, cap).nonEmpty)
            }
        }
    }
}
