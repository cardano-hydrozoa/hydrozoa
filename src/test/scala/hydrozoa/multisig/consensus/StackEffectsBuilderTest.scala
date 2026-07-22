package hydrozoa.multisig.consensus

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockResult, BlockVersion}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationKey, EvacuationMap}
import hydrozoa.multisig.ledger.l1.tx.FinalizationTx
import hydrozoa.multisig.ledger.l1.utxo.{Equity, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackPartition}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_TransactionInput
import scalus.cardano.ledger.{Coin, TransactionInput, Value}
import scalus.uplc.builtin.ByteString
import test.Generators.Hydrozoa.genKnownValuePayoutObligationWithMinAdaEnsured

/** [[StackEffectsBuilder.mkEffectsRegular]] — the Final partition (finalization). */
class StackEffectsBuilderTest extends AnyFunSuite {

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, Seed(0L))
    private val headConfig: HeadConfig = config.headConfig
    private val now = realTimeQuantizedInstant(headConfig.slotConfig).unsafeRunSync()

    /** Materialize a generator deterministically from a fixed seed. */
    private def fixed[A](gen: Gen[A], seed: Long): A =
        gen.pureApply(Gen.Parameters.default, Seed(seed))

    private def obligation(lovelace: Long, seed: Long): Payout.Obligation =
        fixed(
          genKnownValuePayoutObligationWithMinAdaEnsured(Value(Coin(lovelace)))(using config),
          seed
        )

    private val treasuryTokenValue: Value = Value.asset(
      headConfig.headMultisigScript.script.scriptHash,
      headConfig.headTokenNames.treasuryTokenName,
      1L
    )

    // A final block can process an L2 transaction that withdraws a pre-existing L2 utxo: that utxo
    // leaves the L2 active set (a `Delete` in the block's `evacuationMapDiff`) and its funds are paid
    // out on L1 as a `Payout.Obligation`. The Final branch must fold the diff into the running map
    // before draining the residual, or the withdrawn utxo is paid twice — once as a residual balance
    // (still in the pre-final map) and once as the withdrawal.
    test("finalization does not double-pay a residual utxo the final block withdrew") {
        // The pre-final running map holds one utxo, `keyX`.
        val keyX = EvacuationKey(ByteString.fromHex("dd" * 32)).get
        val oblX = obligation(5_000_000L, seed = 10)
        val initialMap =
            EvacuationMap.applyDiffs(EvacuationMap.empty, Seq(EvacuationDiff.Update(keyX, oblX)))

        // The final block withdraws `keyX`: its diff deletes it, and its funds ride out as a payout.
        val oblY = obligation(5_000_000L, seed = 20)
        val reqId = RequestId(0, 1L)
        val finalBlock = BlockResult(
          brief = BlockBrief.Final(
            BlockHeader.Final(
              blockNum = BlockNumber(1),
              blockVersion = BlockVersion.Full(1, 0),
              startTime = BlockCreationStartTime(now),
              endTime = BlockCreationEndTime(now + 1.second)
            ),
            BlockBody.Final(events = List.empty, depositsRefunded = List.empty)
          ),
          evacuationMapDiff = Seq(EvacuationDiff.Delete(keyX)),
          payoutObligations = List(oblY),
          payoutRequestIds = List(reqId),
          postDatedRefundTxs = Nil,
          absorbedDeposits = Nil,
          competingFallbackTxTime =
              headConfig.txTiming.newFallbackStartTime(BlockCreationEndTime(now + 1.second))
        )
        val partitions = StackPartition.partition(NonEmptyList.one(finalBlock))

        val treasury = MultisigTreasuryUtxo(
          treasuryTokenName = headConfig.headTokenNames.treasuryTokenName,
          utxoId = fixed(Arbitrary.arbitrary[TransactionInput], seed = 1),
          address = headConfig.headMultisigAddress,
          datum =
              MultisigTreasuryUtxo.Datum(ByteString.fromArray(Array.fill[Byte](48)(0)), BigInt(3)),
          // Generous ADA so the build succeeds even under the buggy (double-counting) path — the
          // regression then shows up as `payoutCount == 2`, not an opaque build failure.
          value = Value(Coin(2_000_000_000L)) + treasuryTokenValue,
          equity = Equity(Coin(1_000_000_000L)).get
        )

        val result = StackEffectsBuilder.mkEffectsRegular(
          config = headConfig,
          treasury = treasury,
          partitions = partitions,
          initialEvacuationMap = initialMap
        )

        val (effects, _, _, rows) = result match {
            case Right(v)  => v
            case Left(err) => fail(s"finalization build failed: $err")
        }

        val finalization = effects.partitions
            .collect { case f: PartitionEffects.Final =>
                f.finalization
            }
            .headOption
            .getOrElse(fail("no Final partition effects"))

        // Only the withdrawal is paid directly; the residual is empty because `keyX` was deleted.
        val _ = finalization match {
            case wodp: FinalizationTx.WithOnlyDirectPayouts =>
                assert(
                  wodp.payoutCount == 1,
                  s"the withdrawn utxo was double-paid: payoutCount=${wodp.payoutCount} " +
                      "(expected 1 — only the withdrawal, its residual entry deleted)"
                )
            case other =>
                fail(
                  "expected WithOnlyDirectPayouts with a single withdrawal, got " +
                      other.getClass.getSimpleName
                )
        }

        // The withdrawal is tracked to the finalization tx; the residual (None provenance) is not.
        assert(
          rows.map(_._1) == List(reqId),
          s"expected one withdrawal-effect row for $reqId, got $rows"
        )
    }
}
