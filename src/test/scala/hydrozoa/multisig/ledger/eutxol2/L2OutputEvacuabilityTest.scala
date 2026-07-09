package hydrozoa.multisig.ledger.eutxol2

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.{addrKeyHash, shelleyAddress}
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.evacuationKeyToData
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genCollateralUtxo
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{CardanoMutator, State, UtxoEnv}
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.serialization.cbor.Cbor
import scalus.testing.ImmutableEmulator
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.bls12_381.G2Element
import test.Generators.Hydrozoa.{genPolicyId, genPubKeyUtxo}

/** Test "C" — are L2-conformant outputs actually **L1-evacuable**?
  *
  * When a head falls into the rule-based regime, every L2 utxo must be paid out to L1 by an
  * `EvacuationTx`. So an L2 output is only safe if it can survive that round trip. The L2 ledger's
  * gatekeeper for output shape is [[L2ConformanceValidator]] `l2Validate`, but `l2Validate` is
  * **purely structural** (Babbage output, inline datum, allowed script ref) — it checks none of the
  * L1 sizing/value invariants. This suite walks the four L1-evacuability properties and pins, for
  * each, **where** in the pipeline it is (or is not) enforced:
  *
  *   - **min-ada** — gated downstream of `l2Validate` by [[Payout.Obligation]]: a below-min-ada
  *     output can never form an obligation, so it can never enter an evacuation map. SAFE.
  *   - **value size** — gated at L2-entry by `OutputsHaveTooBigValueStorageSizeValidator` (run by
  *     `Mutator.transit` next to `l2Validate`): an output whose value serializes past
  *     `maxValueSize` is rejected before it ever becomes an active L2 utxo, so it can never reach
  *     an evacuation map. SAFE.
  *   - **datum size** — **NOT gated anywhere for a single output.** `l2Validate` accepts any inline
  *     datum; the per-output validators only measure the *value*, not the datum; only the whole-tx
  *     `TransactionSizeValidator` bounds it. A datum small enough to ride in its own L2 transaction
  *     can still be too large to evacuate, because the `EvacuationTx` must also carry the residual
  *     treasury (whose Resolved datum embeds the ~maxEvac G2 trusted-setup elements) and the KZG
  *     membership proof. The output is then **stranded**. THE GAP.
  *   - **ada-only** — `l2Validate`'s `TransactionOutput` doc says "can only contain Ada", but
  *     nothing enforces it; a token-carrying output is accepted. Tokens are value-conserved, so
  *     this is a doc-vs-code mismatch rather than a safety hole.
  *
  * The two datum-size tests assert the **desired** invariants — every L2-conformant output is
  * L1-evacuable, and `EvacuationTx.Build` terminates — rather than asserting that the gap exists.
  * Both are violated today, so they are wrapped in `pendingUntilFixed`: each is reported
  * **pending** while it fails and turns into a hard failure the day the behaviour is fixed
  * (signalling that the wrapper should be removed). They reuse test "B"'s machinery (a Resolved
  * treasury committing to the map, a real `EvacuationTx.Build` + emulator submit): the evacuability
  * test measures a working *small-datum* control's serialized tx size and shows the datum delta
  * alone pushes an otherwise-identical evacuation past `maxTxSize`; the termination test runs the
  * build on a daemon Future and times out (since halving a singleton is a no-op `drop(0)`, the
  * build loops forever). The fix is tracked in #488.
  */
class L2OutputEvacuabilityTest extends AnyFunSuite {

    /** A single, deterministic multi-peer fixture (seeded) — same house pattern as tests A/B. */
    private val env =
        MultiNodeConfig.generateWithCoil().pureApply(Gen.Parameters.default, Seed(0L))

    /** Materialize a generator deterministically from a fixed seed. */
    private def fixed[A](gen: Gen[A], seed: Long): A =
        gen.pureApply(Gen.Parameters.default, Seed(seed))

    private val config: NodeConfig = env.nodeConfigs.head._2
    private val ownWallet: PeerWallet = env.nodePrivateConfigs.head._2.ownWallet
    private val ownKeyHash = ownWallet.exportVerificationKey.addrKeyHash
    private val ownAddr = ownWallet.exportVerificationKey.shelleyAddress()(using env.headConfig)
    private val params = env.headConfig.cardanoProtocolParams
    private val maxTxSize: Long = params.maxTxSize
    private val maxValueSize: Long = params.maxValueSize

    private val treasuryToken = env.headConfig.treasuryToken
    private val fallbackTxId = fixed(Arbitrary.arbitrary[TransactionHash], 1)
    private val now = realTimeQuantizedInstant(env.headConfig.slotConfig).unsafeRunSync()
    private val collateral = fixed(genCollateralUtxo(ownKeyHash)(using env.headConfig), 4)

    // Generous buffer so even a large min-ada residual treasury is funded.
    private val treasuryBaseValue = Value(Coin.ada(5)) + treasuryToken + Value(Coin.ada(500))

    private val l2OutputValidator = summon[L2ConformanceValidator[TransactionOutput]]

    /** A Babbage output to `ownAddr` with the given value and datum. */
    private def mkOutput(value: Value, datum: Option[DatumOption]): Babbage =
        Babbage(address = ownAddr, value = value, datumOption = datum, scriptRef = None)

    /** A min-ada-ensured payout obligation for the given value/datum, on this head's network. */
    private def obligationOf(value: Value, datum: Option[DatumOption]): Payout.Obligation =
        Payout
            .Obligation(KeepRaw(ensureMinAda(mkOutput(value, datum), params)), env.headConfig)
            .toOption
            .get

    /** A single-entry evacuation map keyed by a real (TransactionInput-encoded) evacuation key. */
    private def singletonMap(keySeed: Long, obligation: Payout.Obligation): EvacuationMap =
        EvacuationMap(
          TreeMap(
            TransactionInput(
              fixed(Arbitrary.arbitrary[TransactionHash], keySeed),
              0
            ).toEvacuationKey
                -> obligation
          )
        )

    /** A fresh ada-only fee utxo from `ownAddr`. */
    private def feeUtxo(seed: Long): Utxo =
        fixed(
          genPubKeyUtxo(address = ownAddr, genValue = Gen.const(Value.ada(100)))(using
            env.headConfig
          ),
          seed
        )

    // Resolved treasury committing to `m`: accumulator = full-set commitment; value = base + the
    // map's total, so the on-chain `treasuryIn == treasuryOut + Σ evacuated` invariant can hold.
    private def mkResolvedTreasury(m: EvacuationMap): RuleBasedTreasuryUtxo =
        RuleBasedTreasuryUtxo(
          utxoId = TransactionInput(fallbackTxId, 0),
          treasuryOutput = RuleBasedTreasuryOutput(
            Resolved(
              evacuationActive = m.kzgCommitment,
              version = (BigInt(100), BigInt(2)),
              setupG2 = TrustedSetup
                  .takeSrsG2(EvacuationTx.Assumptions.maxEvacuationsPerTx + 1)
                  .map(p2 => G2Element(p2).toCompressedByteString)
            ),
            treasuryBaseValue + m.totalValue
          )
        )

    /** Build one `EvacuationTx` draining all of `m` in a single tx and submit it to a fresh
      * emulator. Returns the built tx (Right) or an error string (Left). Mirrors test B's per-step
      * build, minus the Scenario fan-out (each property needs only a single deterministic attempt).
      *
      * NB: do not call this on a map whose obligations cannot fit even one-per-tx —
      * `EvacuationTx.Build.result` loops forever in that case (see the class doc).
      */
    private def evacuateAllOnce(m: EvacuationMap, fee: Utxo): Either[String, EvacuationTx] = {
        val treasury = mkResolvedTreasury(m)
        val initialUtxos: Utxos = (
          Map(
            (treasury.utxoId, treasury.treasuryOutput.toOutput(using config)),
            (collateral.input, collateral.collateralOutput.toOutput(using env)),
            fee.toTuple
          )
              ++ config.scriptReferenceUtxos.toList.map(_.toTuple)
        )
        val emulator = mkEmulator(initialUtxos, now.toSlot)
        EvacuationTx
            .Build(
              inputTreasuryUtxo = treasury,
              evacuateesToTryNext = m,
              allRemainingEvacuatees = m,
              collateralUtxo = collateral
            )
            .result(using config) match {
            case Left(e) => Left(s"build: $e")
            case Right(evac) =>
                emulator.submit(ownWallet.signTx(evac.tx)) match {
                    case Left(err) => Left(s"submit: $err")
                    case Right(_)  => Right(evac)
                }
        }
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

    test("min-ada: a below-min-ada output passes l2Validate but cannot form an obligation") {
        val out: TransactionOutput =
            mkOutput(Value(Coin(0L)), Some(Inline(ByteString.empty.toData)))

        val _ = assert(
          l2OutputValidator.l2Validate(out).isRight,
          "l2Validate is structural — it does not check min-ada"
        )
        val _ = assert(
          Payout.Obligation(KeepRaw(out), env.headConfig).isLeft,
          "the obligation gate (Payout.Obligation) must reject a below-min-ada output"
        )
        val utxos: Utxos = Map(TransactionInput(fallbackTxId, 7) -> out)
        val _ = assert(
          utxos.toEvacuationMap(env.headConfig).isLeft,
          "so it can never enter an evacuation map — gated before evacuation"
        )
    }

    test(
      "value-size: an oversized-value output passes l2Validate but is rejected by the size validator"
    ) {
        val bigValue = bigMultiAssetValue()
        val out: TransactionOutput = ensureMinAda(
          mkOutput(bigValue, Some(Inline(ByteString.empty.toData))),
          params
        )

        val _ = assert(
          Cbor.encode(out.asInstanceOf[Babbage].value).length > maxValueSize,
          s"precondition: value must serialize past maxValueSize ($maxValueSize)"
        )
        val _ = assert(
          l2OutputValidator.l2Validate(out).isRight,
          "l2Validate does not check value size"
        )
        // It clears the min-ada obligation gate, so only the size validators stand between it and an
        // evacuation map. `OutputsHaveTooBigValueStorageSizeValidator` (checked here by its exact
        // predicate) runs in `Mutator.transit` at L2-entry, so the output is rejected before it ever
        // becomes an active L2 utxo — hence it can never reach an evacuation map. SAFE.
        val _ = assert(
          Payout.Obligation(KeepRaw(out), env.headConfig).isRight,
          "min-ada is satisfied, so the obligation gate alone would let it through"
        )
    }

    // NOTE: the two datum-size tests assert the *desired* invariants (every L2-conformant output is
    // L1-evacuable; EvacuationTx.Build terminates). Both are violated today, so they're wrapped in
    // `pendingUntilFixed`: each is reported **pending** while it fails, and ScalaTest fails it loudly
    // the day it starts passing — telling us to drop the wrapper. The fix is tracked in #488.

    test("datum-size: an L2-conformant output should be L1-evacuable (#488)") {
        // Setup (must succeed): the small-datum control evacuates cleanly, giving a real evac-tx size
        // baseline; project the size of the same evacuation carrying a large (but L2-legal) datum.
        val smallObligation =
            obligationOf(Value(Coin.ada(10)), Some(Inline(ByteString.empty.toData)))
        val evac = evacuateAllOnce(singletonMap(100L, smallObligation), feeUtxo(101L)) match {
            case Right(e) => e
            case Left(err) =>
                fail(s"control (small-datum) evacuation should succeed, but failed: $err")
        }
        val baselineTxSize = Cbor.encode(evac.tx).length

        // A "large but L2-legal" inline datum: 12 KiB — well under maxTxSize, so it rides happily in
        // its own single-output L2 transaction, yet it pushes an otherwise-identical evacuation over.
        val smallOut: TransactionOutput = smallObligation.utxo.value
        val attackerDatumBytes = 12 * 1024
        val bigDatum = ByteString.fromArray(new Array[Byte](attackerDatumBytes))
        val bigOut: TransactionOutput =
            ensureMinAda(
              smallOut.asInstanceOf[Babbage].copy(datumOption = Some(Inline(bigDatum.toData))),
              params
            )
        // The delta is the datum's own contribution; the rest of the evac tx — treasury accumulator
        // datum, KZG proof, collateral — is unchanged.
        val strandedTxSize =
            baselineTxSize + (Cbor.encode(bigOut).length - Cbor.encode(smallOut).length)
        info(
          s"maxTxSize=$maxTxSize baselineEvacTx=$baselineTxSize attackerDatum=$attackerDatumBytes " +
              s"strandedEvacTx=$strandedTxSize"
        )

        // DESIRED invariant: an L2-conformant output is L1-evacuable. Currently violated — the big
        // datum passes l2Validate yet its evacuation tx exceeds maxTxSize — so this is pending. It
        // flips green either way the gap is closed: l2Validate rejects such outputs, or evacuation
        // learns to carry them.
        pendingUntilFixed {
            val conformant = l2OutputValidator.l2Validate(bigOut).isRight
            val evacuable = strandedTxSize <= maxTxSize
            val _ = assert(
              !conformant || evacuable,
              "an L2-conformant output must be L1-evacuable, but the large-datum output passes " +
                  s"l2Validate while its evacuation tx ($strandedTxSize) exceeds maxTxSize ($maxTxSize)"
            )
        }
    }

    test("datum-size: EvacuationTx.Build should terminate on a single un-evacuable output (#488)") {
        // DESIRED invariant: EvacuationTx.Build.result terminates. Today it doesn't: the over-size
        // evacuation tx trips InvalidTransactionSizeException — a `halveEvacuation` trigger — and
        // halving a singleton map is `drop(0)` (a no-op), so the builder retries the identical map
        // forever (a liveness/DoS bug; cf. the infinite-halving TODO in EvacuationTx.scala). The body
        // runs it on a daemon Future and times out, so this is pending; once Build terminates the
        // Await returns and pendingUntilFixed fails to flag the fix. (fork := true + the global EC's
        // daemon threads mean the spinning worker dies with the test JVM.)
        pendingUntilFixed {
            val bigDatum = ByteString.fromArray(new Array[Byte](12 * 1024))
            val bigObligation = obligationOf(Value(Coin.ada(50)), Some(Inline(bigDatum.toData)))
            val attempt = Future(evacuateAllOnce(singletonMap(300L, bigObligation), feeUtxo(301L)))
            val result = Await.result(attempt, Duration(12, "s"))
            val _ = assert(result.isRight, s"Build should terminate and evacuate, got: $result")
        }
    }

    test("ada-only: l2Validate accepts a token-carrying output despite its 'Ada only' doc") {
        val tokenValue = Value(Coin.ada(10)) +
            Value.asset(fixed(genPolicyId, 200L), AssetName(ByteString.fromString("L2TOKEN")), 1)
        val out: TransactionOutput =
            ensureMinAda(mkOutput(tokenValue, Some(Inline(ByteString.empty.toData))), params)

        val _ = assert(
          l2OutputValidator.l2Validate(out).isRight,
          "l2Validate's TransactionOutput doc claims 'Ada only' but does not enforce it"
        )
        // Not a safety hole: the token is value-conserved, so it forms a valid payout obligation
        // (and would be carried into the evacuation output unchanged).
        val _ = assert(
          Payout.Obligation(KeepRaw(out), env.headConfig).isRight,
          "the token-carrying output still forms a valid, conserved obligation"
        )
    }

    /** A value whose multi-asset component serializes past `maxValueSize` (one policy, many names).
      */
    private def bigMultiAssetValue(): Value = {
        val policy = fixed(genPolicyId, 99L)
        val assets = SortedMap.from(
          (0 until 400).map { i =>
              AssetName(
                ByteString.fromArray(
                  Array.tabulate(28)(j => if j < 4 then (i >> (8 * j)).toByte else 0)
                )
              ) -> 1L
          }
        )
        Value(Coin.ada(60), MultiAsset(SortedMap(policy -> assets)))
    }
}
