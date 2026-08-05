package hydrozoa.multisig.ledger.l1.txseq

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1.txseq.SettlementTxSeq.{NoRollouts, WithRollouts}
import hydrozoa.multisig.ledger.l1.utxo.{Equity, MultisigTreasuryUtxo}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_TransactionInput
import scalus.cardano.ledger.{Coin, Transaction, TransactionInput, Value}
import scalus.uplc.builtin.ByteString
import test.Generators.Hydrozoa.genKnownValuePayoutObligationWithMinAdaEnsured
import test.TestPeersSpec

/** Pins the payout-obligation layout the withdrawal-effect tracking depends on: the settlement /
  * finalization discharges the front `[0, payoutCount)` of the ordered obligation vector, and each
  * rollout in chain order discharges the next contiguous run, ascending — so a tx's slice is the
  * running sum of the preceding `payoutCount`s (no stored `payoutOffset`). Uses enough distinct
  * payouts to force multiple rollouts.
  */
class RolloutPayoutOrderingTest extends AnyFunSuite:

    private val multiNodeConfig: MultiNodeConfig =
        MultiNodeConfig
            .generate(TestPeersSpec.default)()
            .pureApply(Gen.Parameters.default, Seed(0L))
    private val config = multiNodeConfig.headConfig
    private given CardanoNetwork.Section = config

    /** Obligation `i` — a distinct ADA value so a tx output can be mapped back to its vector index.
      */
    private def obligation(i: Int): Payout.Obligation =
        genKnownValuePayoutObligationWithMinAdaEnsured(Value(Coin(5_000_000L + i.toLong * 1000L)))
            .pureApply(Gen.Parameters.default, Seed(i.toLong))

    test(
      "settlement + rollouts discharge the payout vector as forward, ascending contiguous slices"
    ) {
        val n = 700
        val obligations: Vector[Payout.Obligation] = Vector.tabulate(n)(obligation)

        // Recover a vector index from a payout output's exact ADA value.
        val indexOfValue: Map[Long, Int] =
            obligations.zipWithIndex.map((o, i) => o.utxo.value.value.coin.value -> i).toMap

        val totalPayoutValue = obligations.foldLeft(Value.zero)((v, o) => v + o.utxo.value.value)
        val equity = Equity(Coin(5_000_000_000L)).get
        val treasury = MultisigTreasuryUtxo(
          treasuryTokenName = config.headTokenNames.treasuryTokenName,
          utxoId =
              Arbitrary.arbitrary[TransactionInput].pureApply(Gen.Parameters.default, Seed(1L)),
          address = config.headMultisigAddress,
          datum =
              MultisigTreasuryUtxo.Datum(ByteString.fromArray(Array.fill[Byte](48)(0)), BigInt(7)),
          value = totalPayoutValue + Value(equity.coin),
          equity = equity
        )

        val prevEnd = BlockCreationEndTime(java.time.Instant.now().quantize(config.slotConfig))
        val seq = SettlementTxSeq
            .Build(config)(
              kzgCommitment = ByteString.fromArray(Array.fill[Byte](48)(0)),
              majorVersionProduced = BlockVersion.Major(8),
              depositsToSpend = Nil,
              payoutObligationsRemaining = obligations,
              treasuryToSpend = treasury,
              competingFallbackValidityStart = config.txTiming.newFallbackStartTime(prevEnd),
              blockCreationEndTime = prevEnd
            )
            .result match
            case Right(s)  => s
            case Left(err) => fail(s"settlement seq build failed: $err")

        // The effect txs in offset order: the settlement's direct slice, then the rollouts in chain
        // order (`notLast :+ last`).
        val effectTxs: List[Transaction] = seq match
            case NoRollouts(settlement, _) => List(settlement.tx)
            case WithRollouts(settlement, _, rollouts) =>
                settlement.tx :: (rollouts.notLast.map(_.tx).toList :+ rollouts.last.tx)

        val _ = assert(
          effectTxs.sizeIs >= 3,
          s"expected the settlement plus >= 2 rollouts, got ${effectTxs.size} effect txs — " +
              "raise `n` so the payouts overflow into multiple rollouts"
        )

        // Each effect tx's payout outputs (those whose value is one of ours), as vector indices.
        val perTxIndices: List[List[Int]] = effectTxs.map { tx =>
            tx.body.value.outputs.toList.flatMap(o => indexOfValue.get(o.value.value.coin.value))
        }

        // Walk them in offset order accumulating `payoutCount` — exactly as `trackWithdrawals` does —
        // and assert each tx's payout set is the contiguous range starting at the running offset.
        val finalOffset = perTxIndices.foldLeft(0) { (offset, idxs) =>
            val expected = (offset until offset + idxs.size).toSet
            val _ = assert(
              idxs.toSet == expected,
              "a tx's payout slice is not the contiguous range " +
                  s"[$offset, ${offset + idxs.size}); got ${idxs.sorted.take(5)}…"
            )
            offset + idxs.size
        }
        assert(
          finalOffset == n,
          s"the effect txs discharge $finalOffset payouts across [settlement, rollouts], expected $n"
        )
    }
