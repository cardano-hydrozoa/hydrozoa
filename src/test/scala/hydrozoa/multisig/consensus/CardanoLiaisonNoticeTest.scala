package hydrozoa.multisig.consensus

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.FallbackTxStartTime
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.lib.logging.Level
import hydrozoa.multisig.consensus.CardanoLiaison.Action.{FallbackToRuleBased, PushForwardMultisig, SilencePeriodNoop}
import hydrozoa.multisig.consensus.CardanoLiaison.{DispatchNotice, classifyDispatch}
import hydrozoa.multisig.consensus.CardanoLiaisonEvent.ActionsDispatched
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import java.time.Instant
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.{SlotConfig, TransactionHash}

/** The liaison logs entering a silence period, and submitting a fallback, at WARN once per tx: the
  * actions are re-derived and dispatched on every tick until the fallback lands, and only the first
  * dispatch about a tx is news.
  */
class CardanoLiaisonNoticeTest extends AnyFunSuite {

    private val slotConfig = SlotConfig.mainnet
    private val t0 = Instant.parse("2026-09-27T00:00:00Z").quantize(slotConfig)

    private def txId(n: Int): TransactionHash = TransactionHash.fromHex(f"$n%064x")

    private def silence(happyPathTx: Int, tick: Int): SilencePeriodNoop =
        SilencePeriodNoop(
          currentTime = t0 + (tick * 10).seconds,
          happyPathTxTtl = t0,
          fallbackValidityStart = FallbackTxStartTime(t0 + 300.seconds),
          happyPathTxId = txId(happyPathTx)
        )

    private val routine = PushForwardMultisig(Seq.empty)

    /** A real fallback tx: the initial block's, from a generated head. */
    private val fallback: FallbackToRuleBased = FallbackToRuleBased(
      MultiNodeConfig.generateDefault
          .pureApply(Gen.Parameters.default, Seed(0L))
          .headConfig
          .initialBlock
          .effects
          .fallbackTx
    )

    test("a silence period is a first notice once, then a repeat on every later tick") {
        val (first, noticed) = classifyDispatch(Seq(silence(1, 0)), Set.empty)
        val _ = assert(first == DispatchNotice.FirstNotice)
        val repeats = (1 to 30).scanLeft((DispatchNotice.FirstNotice, noticed)) {
            case ((_, seen), tick) => classifyDispatch(Seq(silence(1, tick)), seen)
        }
        val _ = assert(repeats.tail.map(_._1).forall(_ == DispatchNotice.Repeat))
        assert(repeats.last._2 == Set(txId(1)))
    }

    test("another settlement's silence period is a first notice of its own") {
        val (_, noticed) = classifyDispatch(Seq(silence(1, 0)), Set.empty)
        val (notice, after) = classifyDispatch(Seq(silence(1, 1), silence(2, 1)), noticed)
        val _ = assert(notice == DispatchNotice.FirstNotice)
        assert(after == Set(txId(1), txId(2)))
    }

    test("a fallback's first submission is a first notice, its resubmissions repeats") {
        val (first, noticed) = classifyDispatch(Seq(fallback), Set.empty)
        val _ = assert(first == DispatchNotice.FirstNotice)
        val _ = assert(noticed == Set(fallback.tx.tx.id))
        assert(classifyDispatch(Seq(fallback), noticed) == (DispatchNotice.Repeat, noticed))
    }

    test("routine actions stay routine, alone or beside a repeat") {
        val _ =
            assert(classifyDispatch(Seq(routine), Set.empty) == (DispatchNotice.Routine, Set.empty))
        val noticed = Set(txId(1))
        assert(
          classifyDispatch(Seq(routine, silence(1, 3)), noticed) ==
              (DispatchNotice.Routine, noticed)
        )
    }

    test("the format logs a first notice at WARN, a repeat at DEBUG, routine at INFO") {
        def level(notice: DispatchNotice): Level =
            CardanoLiaisonEventFormat
                .humanFormat(HeadPeerNumber.zero)(ActionsDispatched(List(silence(1, 0)), notice))
                .level
        val _ = assert(level(DispatchNotice.FirstNotice) == Level.Warn)
        val _ = assert(level(DispatchNotice.Repeat) == Level.Debug)
        assert(level(DispatchNotice.Routine) == Level.Info)
    }
}
