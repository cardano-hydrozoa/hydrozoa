package hydrozoa.rulebased.ledger.l1.script.plutus

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.addrKeyHash
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genCollateralUtxo
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.tailrec
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.{CardanoMutator, State, UtxoEnv}
import scalus.testing.ImmutableEmulator
import scalus.uplc.builtin.bls12_381.G2Element
import test.Generators.Hydrozoa.genEvacuationMap

/** Evacuation-map drain test for the rule-based regime (test "B").
  *
  * Starting from a **Resolved** treasury whose accumulator commits to a full evacuation map `M`,
  * drains `M` to L1 one obligation at a time by **chaining** `EvacuationTx`s: each carries a KZG
  * membership proof; the produced residual treasury is re-read from the emulator and fed to the
  * next step. It asserts:
  *
  *   - **distribution == M**: the multiset of evacuated outputs equals `M.outputsCooked` (every
  *     obligation paid out exactly once, to its own address/value — no theft, no stranding);
  *   - the treasury **drains exactly**: the final residual value equals `treasuryBaseValue` (the
  *     on-chain value invariant `treasuryIn == treasuryOut + Σ evacuated`).
  *
  * Deterministic example test (seeded `MultiNodeConfig`). A single fixed evacuation order is used:
  * the drain is order-independent, so one order suffices — there are no adversarial/divergent
  * choices here, hence no `Scenario`. (A peer redirecting the residual treasury to its own address
  * or breaking evacuation liveness is a separate, genuinely adversarial scenario test.)
  *
  * Each `EvacuationTx` pays its fee from the collateral utxo (returned fee-subtracted at output
  * index 0), and the drain loop threads that returned collateral into the next step. Evacuation
  * has no time-validity bound, so no deadline/sleep is involved.
  */
class EvacuationDrainTest extends AnyFunSuite {

    /** A single, deterministic multi-peer fixture (seeded). */
    private val env =
        MultiNodeConfig.generateWithCoil().pureApply(Gen.Parameters.default, Seed(0L))

    /** Materialize a generator deterministically from a fixed seed. */
    private def fixed[A](gen: Gen[A], seed: Long): A =
        gen.pureApply(Gen.Parameters.default, Seed(seed))

    private val config: NodeConfig = env.nodeConfigs.head._2
    private val ownWallet: PeerWallet = env.nodePrivateConfigs.head._2.ownWallet
    private val ownKeyHash = ownWallet.exportVerificationKey.addrKeyHash
    private val treasuryAddr = HydrozoaBlueprint.mkTreasuryAddress(env.headConfig.network)

    // Small on purpose: each step is an on-chain KZG membership check. Larger maps belong to the
    // bounded-trace ScalaCheck Commands test.
    private val numEvacuees = 2

    private val treasuryToken = Value.asset(
      env.headConfig.headMultisigScript.policyId,
      env.headConfig.headTokenNames.treasuryTokenName,
      1
    )
    private val fallbackTxId = fixed(Arbitrary.arbitrary[TransactionHash], 1)
    private val now = realTimeQuantizedInstant(env.headConfig.slotConfig).unsafeRunSync()

    /** The full evacuation map M to drain. */
    private val evacMap: EvacuationMap = fixed(genEvacuationMap(numEvacuees)(using env), 2)

    // Resolved treasury: accumulator = full-set commitment; value = base (beacon + min-ada +
    // buffer) + M.totalValue, so draining all of M leaves exactly `treasuryBaseValue`.
    private val resolvedDatum = Resolved(
      evacuationActive = evacMap.kzgCommitment,
      version = (BigInt(100), BigInt(2)),
      setupG2 = TrustedSetup
          .takeSrsG2(EvacuationTx.Assumptions.maxEvacuationsPerTx + 1)
          .map(p2 => G2Element(p2).toCompressedByteString)
    )
    private val treasuryBaseValue = Value(Coin.ada(5)) + treasuryToken + Value(Coin.ada(200))
    private val treasury = RuleBasedTreasuryUtxo(
      utxoId = TransactionInput(fallbackTxId, 0),
      treasuryOutput =
          RuleBasedTreasuryOutput(resolvedDatum, treasuryBaseValue + evacMap.totalValue)
    )

    // One collateral utxo, threaded through the drain loop: each EvacuationTx spends it as its
    // fee-paying input and returns it (fee-subtracted) at index 0 for the next step to consume.
    private val collateral = fixed(genCollateralUtxo(ownKeyHash)(using env.headConfig), 4)

    private val initialUtxos: Utxos = (
      Map(
        (treasury.utxoId, treasury.treasuryOutput.toOutput(using config)),
        (collateral.input, collateral.collateralOutput.toOutput(using env))
      )
          ++ config.scriptReferenceUtxos.toList.map(_.toTuple)
    )

    /** Drain `remaining` one obligation at a time, in a fixed key order. Threads the emulator and
      * the produced treasury forward and accumulates the evacuated outputs; returns the final
      * (fully-drained) treasury and every output paid out.
      */
    @tailrec
    private def evacuateAll(
        remaining: EvacuationMap,
        treasury: RuleBasedTreasuryUtxo,
        collateral: CollateralUtxo,
        emulator: ImmutableEmulator,
        depth: Int,
        evacuated: List[TransactionOutput]
    ): (RuleBasedTreasuryUtxo, List[TransactionOutput]) =
        if remaining.isEmpty then (treasury, evacuated)
        else {
            val key = remaining.keys.head
            val subset = remaining.removedAll(remaining.keys.filter(_ != key))
            val evac = EvacuationTx
                .Build(
                  inputTreasuryUtxo = treasury,
                  evacuateesToTryNext = subset,
                  allRemainingEvacuatees = remaining,
                  collateralUtxo = collateral
                )
                .result(using config) match {
                case Right(e)  => e
                case Left(err) => fail(s"evacuation build failed at step $depth: $err")
            }
            val nextEmulator = emulator.submit(ownWallet.signTx(evac.tx)) match {
                case Right((_, e)) => e
                case Left(err)     => fail(s"evacuation submit failed at step $depth: $err")
            }
            // EvacuationTx hard-codes treasuryUtxoProduced.utxoId to output index 0, but the residual
            // treasury is not at index 0 (the collateral-return output precedes it), so re-read the
            // real treasury input from the emulator before chaining.
            val newTreasury =
                evac.treasuryUtxoProduced.copy(utxoId = currentTreasuryInput(nextEmulator))
            // The returned collateral (fee-subtracted) sits at output index 0 of the just-submitted
            // evacuation tx; parse it back into a CollateralUtxo for the next step.
            val newCollateralInput = TransactionInput(evac.tx.id, 0)
            val newCollateral = CollateralUtxo
                .parse(Utxo(newCollateralInput, nextEmulator.utxos(newCollateralInput)))
                .fold(
                  err => fail(s"parsing returned collateral at step $depth failed: $err"),
                  identity
                )
            evacuateAll(
              remaining.removed(key),
              newTreasury,
              newCollateral,
              nextEmulator,
              depth + 1,
              evacuated ++ evac.evacuatedOutputs
            )
        }

    private def bag(xs: List[TransactionOutput]): Map[TransactionOutput, Int] =
        xs.groupBy(identity).view.mapValues(_.size).toMap

    test("evacuation drains the full evacuation map to exactly M") {
        val emulator0 = mkEmulator(initialUtxos, now.toSlot)
        val (finalTreasury, evacuated) =
            evacuateAll(evacMap, treasury, collateral, emulator0, 0, Nil)

        val _ = assert(
          bag(evacuated) == bag(evacMap.outputsCooked.toList),
          "every obligation must be paid out exactly once, to its own address/value (distribution == M)"
        )
        val _ = assert(
          finalTreasury.treasuryOutput.value == treasuryBaseValue,
          "the treasury must drain to exactly its base value"
        )
    }

    /** Mirror the Mock execution model: single CardanoMutator under EvaluateAndComputeCost. */
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

    /** The current treasury input in the emulator (the single utxo at the treasury address). */
    private def currentTreasuryInput(emu: ImmutableEmulator): TransactionInput =
        emu.utxos.collectFirst { case (i, o) if o.address == treasuryAddr => i }.get
}
