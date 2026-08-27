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
import hydrozoa.multisig.ledger.joint.{EvacuationDiff, EvacuationDiffGroup, EvacuationKey, EvacuationMap}
import hydrozoa.multisig.ledger.l1.tx.{FinalizationTx, genDepositUtxo}
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

/** [[StackEffectsBuilder.mkEffectsRegular]] — the Final partition (finalization) and the
  * per-command conservation check (no account over- or under-credited by deposits, L2 transactions,
  * or withdrawals; the randomized L2-transaction properties live in
  * [[EvacuationMapConservationTest]], and the settlement builder's value-exactness property in
  * [[hydrozoa.multisig.ledger.l1.txseq.SettlementTxSeqBuilderTest]]).
  */
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
            BlockBody.Final(requests = List.empty, depositsRejected = List.empty)
          ),
          evacuationMapDiff =
              Seq(EvacuationDiffGroup.Transaction(reqId, Vector(EvacuationDiff.Delete(keyX)))),
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
          datum = MultisigTreasuryUtxo.Datum(
            ByteString.fromArray(Array.fill[Byte](48)(0)),
            BigInt(3),
            ByteString.fromArray(Array.fill[Byte](32)(0))
          ),
          // Generous ADA so the build succeeds even under the buggy (double-counting) path — the
          // regression then shows up as `payoutCount == 2`, not an opaque build failure.
          value = Value(Coin(2_000_000_000L)) + treasuryTokenValue,
          equity = Equity(Coin(1_000_000_000L)).get
        )

        val result = StackEffectsBuilder.mkEffectsRegular(
          config = headConfig,
          initialTreasury = treasury,
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

    /** Run [[StackEffectsBuilder.mkEffectsRegular]] over a single minor block carrying `diffs`,
      * against a treasury of `treasuryValue` (plus the beacon token), starting from `initialMap`
      * (the map recovered from the slow-side state at the previous stack close).
      */
    private def mkMinorStackResult(
        diffs: Seq[EvacuationDiffGroup],
        treasuryValue: Value,
        initialMap: EvacuationMap
    ) = {
        val end = BlockCreationEndTime(now + 1.second)
        val fallback = headConfig.txTiming.newFallbackStartTime(end)
        val minorBlock = BlockResult(
          brief = BlockBrief.Minor(
            BlockHeader.Minor(
              blockNum = BlockNumber(1),
              blockVersion = BlockVersion.Full(0, 1),
              startTime = BlockCreationStartTime(now),
              endTime = end,
              fallbackTxStartTime = fallback,
              forcedMajorBlockWakeupTime = headConfig.txTiming.forcedMajorBlockWakeupTime(fallback),
              mDepositDecisionWakeupTime = None
            ),
            BlockBody.Minor(requests = List.empty, depositsRejected = List.empty)
          ),
          evacuationMapDiff = diffs,
          payoutObligations = Nil,
          payoutRequestIds = Nil,
          postDatedRefundTxs = Nil,
          absorbedDeposits = Nil,
          competingFallbackTxTime = fallback
        )
        val treasury = MultisigTreasuryUtxo(
          treasuryTokenName = headConfig.headTokenNames.treasuryTokenName,
          utxoId = fixed(Arbitrary.arbitrary[TransactionInput], seed = 2),
          address = headConfig.headMultisigAddress,
          datum = MultisigTreasuryUtxo.Datum(
            ByteString.fromArray(Array.fill[Byte](48)(0)),
            BigInt(3),
            ByteString.fromArray(Array.fill[Byte](32)(0))
          ),
          value = treasuryValue + treasuryTokenValue,
          equity = Equity(Coin(5_000_000L)).get
        )
        StackEffectsBuilder.mkEffectsRegular(
          config = headConfig,
          initialTreasury = treasury,
          partitions = StackPartition.partition(NonEmptyList.one(minorBlock)),
          initialEvacuationMap = initialMap
        )
    }

    private def singletonMap(keyHexByte: String, entry: Payout.Obligation): EvacuationMap =
        EvacuationMap.applyDiffs(
          EvacuationMap.empty,
          Seq(EvacuationDiff.Update(EvacuationKey(ByteString.fromHex(keyHexByte * 32)).get, entry))
        )

    /** A major block absorbing one 10-ADA deposit whose diffs spawn `spawnedLovelace` of L2
      * outputs. Conservation requires the spawn to equal the absorbed `l2Value` exactly — an
      * under-spawn under-credits the depositor (the difference enters the treasury but never the
      * map), an over-spawn over-credits (mints L2 value).
      */
    private def mkMajorStackResult(spawnedLovelace: Long) = {
        val end = BlockCreationEndTime(now + 1.second)
        val fallback = headConfig.txTiming.newFallbackStartTime(end)
        val deposit = fixed(
          genDepositUtxo(
            headConfig,
            headAddress = Some(headConfig.headMultisigAddress),
            genDepositAmount = Gen.const(Value(Coin(10_000_000L)))
          )(),
          seed = 3
        )
        val majorBlock = BlockResult(
          brief = BlockBrief.Major(
            BlockHeader.Major(
              blockNum = BlockNumber(1),
              blockVersion = BlockVersion.Full(1, 0),
              startTime = BlockCreationStartTime(now),
              endTime = end,
              fallbackTxStartTime = fallback,
              forcedMajorBlockWakeupTime = headConfig.txTiming.forcedMajorBlockWakeupTime(fallback),
              mDepositDecisionWakeupTime = None
            ),
            BlockBody.Major(
              requests = List.empty,
              depositsAbsorbed = List.empty,
              depositsRejected = List.empty
            )
          ),
          evacuationMapDiff = Seq(
            EvacuationDiffGroup.DepositDecisions(
              Vector(
                EvacuationDiff.Update(
                  EvacuationKey(ByteString.fromHex("cc" * 32)).get,
                  obligation(spawnedLovelace, seed = 70)
                )
              )
            )
          ),
          payoutObligations = Nil,
          payoutRequestIds = Nil,
          postDatedRefundTxs = Nil,
          absorbedDeposits = List(deposit),
          competingFallbackTxTime = fallback
        )
        val treasury = MultisigTreasuryUtxo(
          treasuryTokenName = headConfig.headTokenNames.treasuryTokenName,
          utxoId = fixed(Arbitrary.arbitrary[TransactionInput], seed = 4),
          address = headConfig.headMultisigAddress,
          datum = MultisigTreasuryUtxo.Datum(
            ByteString.fromArray(Array.fill[Byte](48)(0)),
            BigInt(3),
            ByteString.fromArray(Array.fill[Byte](32)(0))
          ),
          value = Value(Coin(2_000_000_000L)) + treasuryTokenValue,
          equity = Equity(Coin(1_000_000_000L)).get
        )
        StackEffectsBuilder.mkEffectsRegular(
          config = headConfig,
          initialTreasury = treasury,
          partitions = StackPartition.partition(NonEmptyList.one(majorBlock)),
          initialEvacuationMap = EvacuationMap.empty
        )
    }

    /** A final block that deletes the running map's 5-ADA entry and pays out a withdrawal of
      * `payoutLovelace`. Conservation requires the payout to equal the deleted value exactly.
      */
    private def mkFinalWithdrawalResult(payoutLovelace: Long) = {
        val keyX = EvacuationKey(ByteString.fromHex("ab" * 32)).get
        val initialMap = EvacuationMap.applyDiffs(
          EvacuationMap.empty,
          Seq(EvacuationDiff.Update(keyX, obligation(5_000_000L, seed = 80)))
        )
        val finalBlock = BlockResult(
          brief = BlockBrief.Final(
            BlockHeader.Final(
              blockNum = BlockNumber(1),
              blockVersion = BlockVersion.Full(1, 0),
              startTime = BlockCreationStartTime(now),
              endTime = BlockCreationEndTime(now + 1.second)
            ),
            BlockBody.Final(requests = List.empty, depositsRejected = List.empty)
          ),
          evacuationMapDiff = Seq(
            EvacuationDiffGroup.Transaction(RequestId(0, 2L), Vector(EvacuationDiff.Delete(keyX)))
          ),
          payoutObligations = List(obligation(payoutLovelace, seed = 90)),
          payoutRequestIds = List(RequestId(0, 2L)),
          postDatedRefundTxs = Nil,
          absorbedDeposits = Nil,
          competingFallbackTxTime =
              headConfig.txTiming.newFallbackStartTime(BlockCreationEndTime(now + 1.second))
        )
        val treasury = MultisigTreasuryUtxo(
          treasuryTokenName = headConfig.headTokenNames.treasuryTokenName,
          utxoId = fixed(Arbitrary.arbitrary[TransactionInput], seed = 5),
          address = headConfig.headMultisigAddress,
          datum = MultisigTreasuryUtxo.Datum(
            ByteString.fromArray(Array.fill[Byte](48)(0)),
            BigInt(3),
            ByteString.fromArray(Array.fill[Byte](32)(0))
          ),
          value = Value(Coin(2_000_000_000L)) + treasuryTokenValue,
          equity = Equity(Coin(1_000_000_000L)).get
        )
        StackEffectsBuilder.mkEffectsRegular(
          config = headConfig,
          initialTreasury = treasury,
          partitions = StackPartition.partition(NonEmptyList.one(finalBlock)),
          initialEvacuationMap = initialMap
        )
    }

    test("rejects a final block whose withdrawal over-credits the withdrawn utxo") {
        // A 5-ADA utxo leaves the map but a 6-ADA payout rides out: the withdrawer is
        // over-credited at the treasury's expense.
        val result = mkFinalWithdrawalResult(payoutLovelace = 6_000_000L)
        assert(
          result.left.exists(_.isInstanceOf[StackEffectsBuilder.Error.EvacuationMapNotConserved]),
          s"over-crediting withdrawal was not rejected as a conservation break: $result"
        )
    }

    test("rejects a final block whose withdrawal under-credits the withdrawn utxo") {
        // A 5-ADA utxo leaves the map but only a 4-ADA payout rides out: the withdrawer loses
        // 1 ADA to the treasury.
        val result = mkFinalWithdrawalResult(payoutLovelace = 4_000_000L)
        assert(
          result.left.exists(_.isInstanceOf[StackEffectsBuilder.Error.EvacuationMapNotConserved]),
          s"under-crediting withdrawal was not rejected as a conservation break: $result"
        )
    }

    test("rejects a major block whose map delta under-credits the absorbed deposit") {
        // 10 ADA absorbed, only 8 ADA spawned on L2: the depositor's missing 2 ADA would sit in
        // the treasury unaccounted by the map.
        val result = mkMajorStackResult(spawnedLovelace = 8_000_000L)
        assert(
          result.left.exists(_.isInstanceOf[StackEffectsBuilder.Error.EvacuationMapNotConserved]),
          s"under-crediting major block was not rejected as a conservation break: $result"
        )
    }

    test("rejects a major block whose map delta over-credits the absorbed deposit") {
        // 10 ADA absorbed but 12 ADA spawned on L2: value minted from nowhere.
        val result = mkMajorStackResult(spawnedLovelace = 12_000_000L)
        assert(
          result.left.exists(_.isInstanceOf[StackEffectsBuilder.Error.EvacuationMapNotConserved]),
          s"over-crediting major block was not rejected as a conservation break: $result"
        )
    }

    // The double-entry identity across a REAL settlement: the settlement builder's value/equity
    // arithmetic must be exact to the lovelace. One hand-built end-to-end scenario through
    // `mkEffectsRegular`; the randomized per-builder property lives in
    // `SettlementTxSeqBuilderTest`. Exercises both boundary directions in one build — absorb
    // a 10 ADA deposit and withdraw the 20 ADA pot — through SettlementTxSeq.Build.
    test("a real settlement preserves treasury.value == map + equity + beacon") {
        val prevEnd = BlockCreationEndTime(now)
        val end = BlockCreationEndTime(now + 10.seconds)
        val competingFallback = headConfig.txTiming.newFallbackStartTime(prevEnd)
        val ownFallback = headConfig.txTiming.newFallbackStartTime(end)
        val potKey = EvacuationKey(ByteString.fromHex("ad" * 32)).get
        val initialMap = singletonMap("ad", obligation(20_000_000L, seed = 110))
        val deposit = fixed(
          genDepositUtxo(
            headConfig,
            headAddress = Some(headConfig.headMultisigAddress),
            genDepositAmount = Gen.const(Value(Coin(10_000_000L)))
          )(),
          seed = 6
        )
        val withdrawReq = RequestId(0, 7L)
        val majorBlock = BlockResult(
          brief = BlockBrief.Major(
            BlockHeader.Major(
              blockNum = BlockNumber(1),
              blockVersion = BlockVersion.Full(1, 0),
              startTime = BlockCreationStartTime(now),
              endTime = end,
              fallbackTxStartTime = ownFallback,
              forcedMajorBlockWakeupTime =
                  headConfig.txTiming.forcedMajorBlockWakeupTime(ownFallback),
              mDepositDecisionWakeupTime = None
            ),
            BlockBody.Major(
              requests = List.empty,
              depositsAbsorbed = List.empty,
              depositsRejected = List.empty
            )
          ),
          evacuationMapDiff = Seq(
            EvacuationDiffGroup.Transaction(withdrawReq, Vector(EvacuationDiff.Delete(potKey))),
            EvacuationDiffGroup.DepositDecisions(
              Vector(
                EvacuationDiff.Update(
                  EvacuationKey(ByteString.fromHex("ae" * 32)).get,
                  obligation(10_000_000L, seed = 120)
                )
              )
            )
          ),
          payoutObligations = List(obligation(20_000_000L, seed = 130)),
          payoutRequestIds = List(withdrawReq),
          postDatedRefundTxs = Nil,
          absorbedDeposits = List(deposit),
          competingFallbackTxTime = competingFallback
        )
        // Balanced going in: value (120 ADA + beacon) == map (20 ADA) + equity (100 ADA) + beacon.
        val treasury = MultisigTreasuryUtxo(
          treasuryTokenName = headConfig.headTokenNames.treasuryTokenName,
          utxoId = fixed(Arbitrary.arbitrary[TransactionInput], seed = 7),
          address = headConfig.headMultisigAddress,
          datum = MultisigTreasuryUtxo.Datum(
            ByteString.fromArray(Array.fill[Byte](48)(0)),
            BigInt(3),
            ByteString.fromArray(Array.fill[Byte](32)(0))
          ),
          value = Value(Coin(120_000_000L)) + treasuryTokenValue,
          equity = Equity(Coin(100_000_000L)).get
        )
        val result = StackEffectsBuilder.mkEffectsRegular(
          config = headConfig,
          initialTreasury = treasury,
          partitions = StackPartition.partition(NonEmptyList.one(majorBlock)),
          initialEvacuationMap = initialMap
        )
        result match {
            case Right((_, newTreasury, newMap, _)) =>
                val imbalance = newTreasury.value - newMap.totalValue -
                    Value(newTreasury.equity.coin) - treasuryTokenValue
                assert(
                  imbalance.isZero,
                  s"the settlement builder leaked value: imbalance=$imbalance " +
                      s"(treasury=${newTreasury.value}, map=${newMap.totalValue}, " +
                      s"equity=${newTreasury.equity.coin})"
                )
            case Left(err) => fail(s"settlement build failed: $err")
        }
    }

    test("accepts a minor partition and returns the folded map") {
        val result = mkMinorStackResult(
          diffs = Nil,
          treasuryValue = Value(Coin(10_000_000L)),
          initialMap = singletonMap("aa", obligation(5_000_000L, seed = 50))
        )
        result match {
            case Right((_, _, newMap, _)) =>
                assert(newMap.size == 1, s"expected the folded map to hold the entry, got $newMap")
            case Left(err) => fail(s"minor partition was rejected: $err")
        }
    }
}
