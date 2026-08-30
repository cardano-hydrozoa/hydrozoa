package hydrozoa.rulebased.evacuator

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genCollateralUtxo
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedRegimeUtxo, RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.tailrec
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.{CardanoMutator, State, UtxoEnv}
import scalus.testing.ImmutableEmulator
import test.Generators.Hydrozoa.genEvacuationMap

/** Drains an evacuation map using the sizes [[EvacuationPlan]] chose, against the real validator.
  *
  * This is the test that decides whether the planner is right. Its estimate is only a prediction
  * until a transaction built at that size passes script evaluation, and the emulator runs the same
  * `CardanoMutator` under `EvaluateAndComputeCost` that the node does — so a batch that would be
  * rejected on chain is rejected here.
  *
  * It also closes the loop on the cost model: the emulator reports the ex-units actually charged,
  * so the predicted and charged costs can be compared directly rather than trusted.
  */
class PlannedEvacuationDrainTest extends AnyFunSuite {

    private val env =
        MultiNodeConfig.generateWithCoil().pureApply(Gen.Parameters.default, Seed(0L))

    private def fixed[A](gen: Gen[A], seed: Long): A =
        gen.pureApply(Gen.Parameters.default, Seed(seed))

    private val config: NodeConfig = env.nodeConfigs.head._2
    private val ownWallet: PeerWallet = env.nodePrivateConfigs.head._2.ownWallet
    private val ownKeyHash = ownWallet.exportVerificationKey.addrKeyHash
    private val treasuryAddr = HydrozoaBlueprint.mkTreasuryAddress(env.headConfig.network)
    private val params = env.headConfig.cardanoProtocolParams

    /** Enough entries to need several transactions, so the chaining is exercised and not just a
      * single batch. Each entry costs a real on-chain KZG membership check, so this is kept modest.
      */
    private val numEvacuees = 12

    private val treasuryToken = Value.asset(
      env.headConfig.headMultisigScript.policyId,
      env.headConfig.headTokenNames.treasuryTokenName,
      1
    )
    private val fallbackTxId = fixed(Arbitrary.arbitrary[TransactionHash], 1)
    private val now = realTimeQuantizedInstant(env.headConfig.slotConfig).unsafeRunSync()

    private val evacMap: EvacuationMap = fixed(genEvacuationMap(numEvacuees)(using env), 2)

    private val resolvedDatum = Resolved(
      headMp = env.headConfig.headMultisigScript.policyId,
      evacuationActive = evacMap.kzgCommitment,
      version = (BigInt(100), BigInt(2))
    )
    private val treasuryBaseValue = Value(Coin.ada(5)) + treasuryToken + Value(Coin.ada(200))
    private val treasury = RuleBasedTreasuryUtxo(
      utxoId = TransactionInput(fallbackTxId, 0),
      treasuryOutput =
          RuleBasedTreasuryOutput(resolvedDatum, treasuryBaseValue + evacMap.totalValue)
    )

    private val collateral = fixed(genCollateralUtxo(ownKeyHash)(using env.headConfig), 4)
    private val regimeUtxo = RuleBasedRegimeUtxo(TransactionInput(fallbackTxId, 9))

    private val initialUtxos: Utxos = (
      Map(
        (treasury.utxoId, treasury.treasuryOutput.toOutput(using config)),
        (collateral.input, collateral.collateralOutput.toOutput(using env)),
        regimeUtxo.toUtxo(using env.headConfig).toTuple
      )
          ++ config.scriptReferenceUtxos.toList.map(_.toTuple)
    )

    /** What one planned step cost when it was actually evaluated. */
    private final case class Charged(batchSize: Int, steps: Long)

    /** Drain the map in planned batches, submitting each to the emulator. */
    @tailrec
    private def drainByPlan(
        remaining: EvacuationMap,
        treasury: RuleBasedTreasuryUtxo,
        collateral: CollateralUtxo,
        emulator: ImmutableEmulator,
        depth: Int,
        evacuated: List[TransactionOutput],
        charged: List[Charged]
    ): (RuleBasedTreasuryUtxo, List[TransactionOutput], List[Charged]) =
        if remaining.isEmpty then (treasury, evacuated, charged)
        else {
            val batch = BatchPlanner.nextBatch(remaining, params)

            // `evacuateesToTryNext` is the planned batch, while `allRemainingEvacuatees` stays the
            // full remainder: the membership proof is the residual commitment, so it must be taken
            // against everything still owed, not against the batch.
            val evac = EvacuationTx
                .Build(
                  inputTreasuryUtxo = treasury,
                  regimeUtxo = regimeUtxo,
                  evacuateesToTryNext = batch,
                  allRemainingEvacuatees = remaining,
                  collateralUtxo = collateral
                )
                .result(using config) match {
                case Right(e)  => e
                case Left(err) => fail(s"planned build failed at step $depth: $err")
            }

            // The planner is only correct if the size it picked survives evaluation unchanged. The
            // builder halves internally on rejection, so a smaller batch here means the plan
            // overshot and the halving quietly rescued it.
            val actualBatch = evac.evacuatedOutputs.size
            val _ = assert(
              actualBatch == batch.size,
              s"step $depth: planned ${batch.size} payouts but built $actualBatch — " +
                  "the builder had to halve, so the planner overshot"
            )

            val nextEmulator = emulator.submit(ownWallet.signTx(evac.tx)) match {
                case Right((_, e)) => e
                case Left(err)     => fail(s"planned submit failed at step $depth: $err")
            }

            val chargedSteps = evac.tx.witnessSet.redeemers
                .map(_.value.totalExUnits.steps.toLong)
                .getOrElse(0L)

            val newTreasury =
                evac.treasuryUtxoProduced.copy(utxoId = currentTreasuryInput(nextEmulator))
            val newCollateralInput = TransactionInput(evac.tx.id, 0)
            val newCollateral = CollateralUtxo
                .parse(Utxo(newCollateralInput, nextEmulator.utxos(newCollateralInput)))
                .fold(err => fail(s"parsing returned collateral at step $depth: $err"), identity)

            drainByPlan(
              remaining.removedAll(batch.evacuationMap.keySet),
              newTreasury,
              newCollateral,
              nextEmulator,
              depth + 1,
              evacuated ++ evac.evacuatedOutputs,
              charged :+ Charged(actualBatch, chargedSteps)
            )
        }

    private def bag(xs: List[TransactionOutput]): Map[TransactionOutput, Int] =
        xs.groupBy(identity).view.mapValues(_.size).toMap

    private lazy val drained = {
        val emulator0 = mkEmulator(initialUtxos, now.toSlot)
        drainByPlan(evacMap, treasury, collateral, emulator0, 0, Nil, Nil)
    }

    test("planned batches drain the map exactly, and every one validates on chain") {
        val (finalTreasury, evacuated, _) = drained

        val _ = assert(
          bag(evacuated) == bag(evacMap.outputsCooked.toList),
          "every obligation must be paid out exactly once, to its own address and value"
        )
        assert(
          finalTreasury.treasuryOutput.value == treasuryBaseValue,
          "the treasury must drain to exactly its base value"
        )
    }

    test("the planner's estimate is never exceeded by what evaluation charges") {
        val (_, _, charged) = drained
        charged.foreach { c =>
            val predicted = BatchPlanner.predictedSteps(c.batchSize)
            val _ = assert(
              c.steps <= predicted,
              s"a batch of ${c.batchSize} was charged ${c.steps} against a prediction of $predicted"
            )
            val _ = assert(
              c.steps <= params.maxTxExecutionUnits.steps.toLong,
              s"a batch of ${c.batchSize} exceeded the per-tx limit at ${c.steps}"
            )
        }
        assert(charged.nonEmpty, "nothing was drained, so nothing was checked")
    }

    test("the map drains in the number of transactions the plan predicted") {
        val (_, _, charged) = drained
        assert(charged.size == EvacuationPlan.txCount(evacMap, params))
    }

    private def mkEmulator(initialUtxos: Utxos, slot: Slot): ImmutableEmulator =
        ImmutableEmulator(
          state = State(utxos = initialUtxos),
          env = UtxoEnv(
            slot.slot,
            env.headConfig.cardanoProtocolParams,
            certState = CertState.empty,
            env.headConfig.network
          ),
          slotConfig = env.headConfig.slotConfig,
          evaluatorMode = EvaluateAndComputeCost,
          validators = Seq.empty,
          mutators = Seq(CardanoMutator)
        )

    private def currentTreasuryInput(emu: ImmutableEmulator): TransactionInput =
        emu.utxos.collectFirst { case (i, o) if o.address == treasuryAddr => i }.get
}
