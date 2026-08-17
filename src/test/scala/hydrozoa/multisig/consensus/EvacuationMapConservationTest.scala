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
import hydrozoa.multisig.ledger.l1.utxo.{Equity, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.stack.StackPartition
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_TransactionInput
import scalus.cardano.ledger.{AssetName, Coin, TransactionInput, Value}
import scalus.uplc.builtin.ByteString
import test.Generators.Hydrozoa.genKnownValuePayoutObligationWithMinAdaEnsured

/** Conservation properties over the evacuation map: an L2 transaction (a minor block's
  * `evacuationMapDiff`) must neither over- nor under-credit any account — the map's total value is
  * exactly conserved, in the coin and in every asset (the EUTXO L2 is zero-fee, transient tokens
  * never reach the diff stream, and minors carry no deposits or withdrawals).
  *
  * The treasury fixture is double-entry balanced (value == map + equity + beacon), and the
  * conservation gate runs before the balance gate — so every perturbed scenario asserts the
  * specific `EvacuationMapNotConserved` rejection, pinning the per-command attribution. This pins
  * the sugar-rush-ledger failure mode: a per-transaction rounding error (one extra token unit
  * credited to a maker) mis-credits accounts long before any treasury-level bound would notice.
  */
object EvacuationMapConservationTest extends Properties("EvacuationMap conservation") {
    import Prop.{forAll, propBoolean}

    private val config: NodeConfig =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, Seed(0L))
    private val headConfig: HeadConfig = config.headConfig
    private val now = realTimeQuantizedInstant(headConfig.slotConfig).unsafeRunSync()

    /** Materialize a generator deterministically from a fixed seed. */
    private def fixed[A](gen: Gen[A], seed: Long): A =
        gen.pureApply(Gen.Parameters.default, Seed(seed))

    private def obligation(value: Value, seed: Long): Payout.Obligation =
        fixed(genKnownValuePayoutObligationWithMinAdaEnsured(value)(using config), seed)

    private val demoAsset = AssetName.fromHex("44454d4f") // "DEMO"

    private def demoTokens(quantity: Long, lovelace: Long): Value = Value.asset(
      headConfig.headMultisigScript.script.scriptHash,
      demoAsset,
      quantity,
      Coin(lovelace)
    )

    private val treasuryTokenValue: Value = Value.asset(
      headConfig.headMultisigScript.script.scriptHash,
      headConfig.headTokenNames.treasuryTokenName,
      1L
    )

    private def key(hexByte: String): EvacuationKey =
        EvacuationKey(ByteString.fromHex(hexByte * 32)).get

    private val potLovelace = 100_000_000L

    /** The pre-block running map: one 100-ADA pot utxo the scenario's minor block spends. */
    private val initialMap = EvacuationMap.applyDiffs(
      EvacuationMap.empty,
      Seq(EvacuationDiff.Update(key("dd"), obligation(Value(Coin(potLovelace)), seed = 10)))
    )

    /** Run `mkEffectsRegular` over a single minor block carrying `diffs`, from [[initialMap]],
      * against a double-entry balanced treasury (101 ADA + beacon == 100-ADA map + 1-ADA equity +
      * beacon).
      */
    private def mkMinorStackResult(diffs: Seq[EvacuationDiffGroup]) = {
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
          utxoId = fixed(Arbitrary.arbitrary[TransactionInput], seed = 1),
          address = headConfig.headMultisigAddress,
          datum =
              MultisigTreasuryUtxo.Datum(ByteString.fromArray(Array.fill[Byte](48)(0)), BigInt(3)),
          value = Value(Coin(potLovelace + 1_000_000L)) + treasuryTokenValue,
          equity = Equity(Coin(1_000_000L)).get
        )
        StackEffectsBuilder.mkEffectsRegular(
          config = headConfig,
          treasury = treasury,
          partitions = StackPartition.partition(NonEmptyList.one(minorBlock)),
          initialEvacuationMap = initialMap
        )
    }

    /** The scenario's diff stream — one transaction: spend the pot into two outputs at `cut`, with
      * the second output's value shifted by `perturbation` (zero = value-conserving).
      */
    private def splitDiffs(cut: Long, perturbation: Value): Seq[EvacuationDiffGroup] =
        Seq(
          EvacuationDiffGroup.Transaction(
            RequestId(0, 1L),
            Vector(
              EvacuationDiff.Delete(key("dd")),
              EvacuationDiff.Update(key("aa"), obligation(Value(Coin(cut)), seed = 20)),
              EvacuationDiff.Update(
                key("bb"),
                obligation(Value(Coin(potLovelace - cut)) + perturbation, seed = 30)
              )
            )
          )
        )

    // Cuts stay >= 5 ADA on both sides so `ensureMinAda` never bumps an output (a bump would
    // change the scenario's totals behind the property's back).
    private val genCut: Gen[Long] = Gen.choose(5_000_000L, potLovelace - 5_000_000L)

    val _ = property("a value-conserving L2 transaction is accepted") = forAll(genCut) { cut =>
        val result = mkMinorStackResult(splitDiffs(cut, Value.zero))
        result.isRight :| s"conserving split at $cut rejected: $result"
    }

    /** The scenario must be rejected as a conservation break specifically — a coverage rejection
      * would mean the treasury slack fixture failed and the property proves nothing.
      */
    private def rejectedAsNotConserved(
        result: Either[StackEffectsBuilder.Error, ?],
        label: String
    ): Prop =
        (result match {
            case Left(_: StackEffectsBuilder.Error.EvacuationMapNotConserved) => true
            case _                                                            => false
        }) :| s"$label: expected EvacuationMapNotConserved, got $result"

    val _ = property("over-crediting an account by one lovelace is rejected") = forAll(genCut) {
        cut =>
            rejectedAsNotConserved(
              mkMinorStackResult(splitDiffs(cut, Value(Coin(1L)))),
              s"one-lovelace inflation at $cut"
            )
    }

    val _ = property("under-crediting an account by one lovelace is rejected") = forAll(genCut) {
        cut =>
            rejectedAsNotConserved(
              mkMinorStackResult(splitDiffs(cut, Value(Coin(-1L)))),
              s"one-lovelace deflation at $cut"
            )
    }

    val _ = property("over-crediting an account by one token unit is rejected") = forAll(genCut) {
        cut =>
            // The sugar-rush case: the maker's output carries one extra micro-token.
            rejectedAsNotConserved(
              mkMinorStackResult(splitDiffs(cut, demoTokens(1L, 0L))),
              s"one-token inflation at $cut"
            )
    }

    val _ = property("an account vanishing without a payout is rejected") = forAll(genCut) { _ =>
        // Deletion with no replacement is solvency-safe (the map shrinks), so coverage would
        // accept it — but the pot holder was under-credited by the whole pot.
        rejectedAsNotConserved(
          mkMinorStackResult(
            Seq(
              EvacuationDiffGroup
                  .Transaction(RequestId(0, 1L), Vector(EvacuationDiff.Delete(key("dd"))))
            )
          ),
          "vanishing pot"
        )
    }

    val _ = property("compensating errors across two transactions are rejected") = forAll(genCut) {
        cut =>
            // Two transactions in one block whose errors cancel — the sugar-rush rounding shape
            // with balanced flow: a buy over-credits its maker by one lovelace, a sell
            // under-credits its maker by one. The BLOCK's aggregate delta is zero, so a per-block
            // check passes; conservation must hold per transaction.
            //
            // tx1: spend the pot into (aa: cut + 1, bb: potLovelace - cut)   — delta +1
            // tx2: spend aa into (cc: cut)                                   — delta -1
            val tx1 = EvacuationDiffGroup.Transaction(
              RequestId(0, 1L),
              Vector(
                EvacuationDiff.Delete(key("dd")),
                EvacuationDiff.Update(key("aa"), obligation(Value(Coin(cut + 1)), seed = 40)),
                EvacuationDiff.Update(
                  key("bb"),
                  obligation(Value(Coin(potLovelace - cut)), seed = 50)
                )
              )
            )
            val tx2 = EvacuationDiffGroup.Transaction(
              RequestId(0, 2L),
              Vector(
                EvacuationDiff.Delete(key("aa")),
                EvacuationDiff.Update(key("cc"), obligation(Value(Coin(cut)), seed = 60))
              )
            )
            rejectedAsNotConserved(
              mkMinorStackResult(Seq(tx1, tx2)),
              s"compensating one-lovelace errors at $cut"
            )
    }
}
