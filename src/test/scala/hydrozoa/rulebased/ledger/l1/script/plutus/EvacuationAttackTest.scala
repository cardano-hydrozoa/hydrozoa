package hydrozoa.rulebased.ledger.l1.script.plutus

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.{addrKeyHash, shelleyAddress}
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatumOnchain
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genCollateralUtxo
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.TreeSet
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{CardanoMutator, State, UtxoEnv}
import scalus.testing.ImmutableEmulator
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.bls12_381.G2Element
import test.Generators.Hydrozoa.{genEvacuationMap, genPubKeyUtxo}

/** Adversarial evacuation test for the rule-based regime.
  *
  * Companion to `EvacuationDrainTest` (test B, happy-path drain): it stands up the same
  * **Resolved** treasury whose accumulator commits to an evacuation map `M`, builds an
  * otherwise-valid [[EvacuationTx]] that drains one obligation, then **mutates** it and confirms
  * the `RuleBasedTreasuryScript` `Evacuate` branch rejects every variant. Each case targets a
  * specific on-chain defense:
  *   - '''steal treasury value''' — skim ADA from the residual treasury into the attacker's own
  *     (collateral-return) output → `EvacuateValueShouldBePreserved`;
  *   - '''steal the beacon''' — move the head beacon NFT off the residual treasury →
  *     `EvacuateBeaconTokenShouldBePreserved`;
  *   - '''redirect an evacuee payout''' — send an evacuated obligation to the attacker. The payout
  *     is bound to the map by the KZG membership proof (the hash covers the output), so the proof
  *     no longer validates → `EvacuateMembershipValidationFailed`;
  *   - '''corrupt the output `setupG2`''' — the documented degenerate-setup attack (corrupt the
  *     trusted setup so later evacuations pass any pairing) → `EvacuateSetupG2ShouldBePreserved`;
  *   - '''positional-parse insertion''' — the `Evacuate` branch reads outputs positionally
  *     (`change, treasury, evacuees…`); inserting an output shifts the treasury slot onto the
  *     attacker's change output, which is not at the treasury script address →
  *     `EvacuateTreasuryWrongAddress`;
  *   - '''stale accumulator''' — leave the residual treasury's accumulator at the input commitment
  *     instead of advancing it to the proof → `EvacuateOutputAccumulatorUpdated`;
  *   - '''double-drain''' (chained) — drain an obligation, then try to drain it again from the
  *     residual treasury; the value invariant makes the already-paid obligation un-payable;
  *   - '''forged KZG proof''' — forge a (valid-but-wrong) G1 commitment into both the redeemer's
  *     `proof` and the output accumulator (so the accumulator-update check passes), isolating the
  *     KZG membership check → `EvacuateMembershipValidationFailed` (soundness: a forged proof does
  *     not validate).
  *
  * Deterministic example test (seeded `MultiNodeConfig`); mirrors test B's fixture and emulator.
  */
class EvacuationAttackTest extends AnyFunSuite {

    /** A single, deterministic multi-peer fixture (seeded). */
    private val env =
        MultiNodeConfig.generateWithCoil().pureApply(Gen.Parameters.default, Seed(0L))

    private def fixed[A](gen: Gen[A], seed: Long): A =
        gen.pureApply(Gen.Parameters.default, Seed(seed))

    private val config: NodeConfig = env.nodeConfigs.head._2
    private val ownWallet: PeerWallet = env.nodePrivateConfigs.head._2.ownWallet
    private val ownKeyHash = ownWallet.exportVerificationKey.addrKeyHash
    private val ownAddr: Address =
        ownWallet.exportVerificationKey.shelleyAddress()(using env.headConfig)
    private val treasuryAddr: Address = HydrozoaBlueprint.mkTreasuryAddress(env.headConfig.network)

    private val numEvacuees = 2

    private val treasuryToken = env.headConfig.treasuryToken
    private val fallbackTxId = fixed(Arbitrary.arbitrary[TransactionHash], 1)
    private val now = realTimeQuantizedInstant(env.headConfig.slotConfig).unsafeRunSync()

    /** The full evacuation map M to drain. */
    private val evacMap: EvacuationMap = fixed(genEvacuationMap(numEvacuees)(using env), 2)

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

    // Two fee utxos: the single-tx attacks use the first; the chained double-drain test needs a
    // second for its follow-up transaction.
    private val feeUtxos: List[Utxo] =
        (0 until 2).toList.map(i =>
            fixed(
              genPubKeyUtxo(address = ownAddr, genValue = Gen.const(Value.ada(100)))(using
                env.headConfig
              ),
              20L + i
            )
        )
    private val collateral = fixed(genCollateralUtxo(ownKeyHash)(using env.headConfig), 4)

    private val initialUtxos: Utxos = (
      Map(
        (treasury.utxoId, treasury.treasuryOutput.toOutput(using config)),
        (collateral.input, collateral.collateralOutput.toOutput(using env))
      )
          ++ feeUtxos.map(_.toTuple)
          ++ config.scriptReferenceUtxos.toList.map(_.toTuple)
    )

    /** Build an EvacuationTx draining the single obligation `key` from `treasuryUtxo` (paying fee
      * from `feeUtxo`), as the `allRemaining` map sees it.
      */
    private def buildEvacuation(
        treasuryUtxo: RuleBasedTreasuryUtxo,
        key: EvacuationKey,
        allRemaining: EvacuationMap,
        feeUtxo: Utxo
    ): Either[EvacuationTx.Build.Error, EvacuationTx] = {
        val subset = allRemaining.removedAll(allRemaining.keys.filter(_ != key))
        EvacuationTx
            .Build(
              inputTreasuryUtxo = treasuryUtxo,
              evacuateesToTryNext = subset,
              allRemainingEvacuatees = allRemaining,
              collateralUtxo = collateral
            )
            .result(using config)
    }

    /** An otherwise-valid EvacuationTx draining the first obligation of `M` (unsigned). */
    private val legitTx: Transaction =
        buildEvacuation(treasury, evacMap.keys.head, evacMap, feeUtxos.head)
            .fold(e => throw new AssertionError(s"legit evacuation build failed: $e"), _.tx)

    /** The current treasury input in the emulator (the single utxo at the treasury address). */
    private def currentTreasuryInput(emu: ImmutableEmulator): TransactionInput =
        emu.utxos.collectFirst { case (i, o) if o.address == treasuryAddr => i }.get

    /** Mirror the Mock execution model: single CardanoMutator under EvaluateAndComputeCost. */
    private def mkEmulator(): ImmutableEmulator =
        ImmutableEmulator(
          state = State(utxos = initialUtxos),
          env = UtxoEnv(
            now.toSlot.slot,
            env.headConfig.cardanoProtocolParams,
            certState = CertState.empty,
            env.headConfig.network
          ),
          slotConfig = env.headConfig.slotConfig,
          evaluatorMode = EvaluateAndComputeCost,
          validators = Seq.empty,
          mutators = Seq(CardanoMutator)
        )

    private def submit(tx: Transaction) =
        mkEmulator().submit(ownWallet.signTx(tx))

    /** Assert the mutated tx is rejected, and that the rejection mentions `reason` (so it is the
      * intended script check doing the rejecting, not a phase-1 fee/balance error).
      */
    private def assertRejected(label: String, tx: Transaction, reason: String): Unit =
        submit(tx) match {
            case Right(_) =>
                fail(s"$label: expected rejection but the transaction was accepted")
            case Left(err) =>
                val _ = assert(
                  err.toString.contains(reason),
                  s"$label: rejected, but not by '$reason'. Got: ${err.toString.take(300)}"
                )
        }

    private def withBody(tx: Transaction)(f: TransactionBody => TransactionBody): Transaction =
        tx.copy(body = KeepRaw(f(tx.body.value)))

    test("sanity: the un-mutated evacuation is accepted") {
        val _ = assert(
          submit(legitTx).isRight,
          s"the legit evacuation should be accepted: ${submit(legitTx).left.map(_.toString.take(300))}"
        )
    }

    test("evacuate cannot skim value from the residual treasury") {
        val skim = Value(Coin.ada(1))
        val mutated = withBody(legitTx) { b =>
            b.copy(outputs = b.outputs.zipWithIndex.map { case (s, i) =>
                s.value match {
                    case bb: Babbage if bb.address == treasuryAddr =>
                        Sized[TransactionOutput](bb.copy(value = bb.value - skim))
                    case bb: Babbage if i == 0 => // the collateral-return / change output
                        Sized[TransactionOutput](bb.copy(value = bb.value + skim))
                    case _ => s
                }
            })
        }
        assertRejected("value-skim", mutated, "Value invariant should hold")
    }

    test("evacuate must preserve the head beacon on the residual treasury") {
        // Moving a token changes the value encoding (and thus the min fee), so bump the fee too —
        // charged to the attacker's own change output — to keep the rejection on the beacon check.
        val feeBump = Coin.ada(1)
        val mutated = withBody(legitTx) { b =>
            val outs = b.outputs.zipWithIndex.map { case (s, i) =>
                s.value match {
                    case bb: Babbage if bb.address == treasuryAddr =>
                        Sized[TransactionOutput](bb.copy(value = bb.value - treasuryToken))
                    case bb: Babbage if i == 0 =>
                        Sized[TransactionOutput](
                          bb.copy(value = bb.value + treasuryToken - Value(feeBump))
                        )
                    case _ => s
                }
            }
            b.copy(outputs = outs, fee = b.fee + feeBump)
        }
        assertRejected("beacon-steal", mutated, "Beacon token should be preserved")
    }

    test("evacuee payout cannot be redirected (bound by the membership proof)") {
        val mutated = withBody(legitTx) { b =>
            // redirect the first evacuee payout (not the change, not the treasury) to the attacker
            var redirected = false
            b.copy(outputs = b.outputs.zipWithIndex.map { case (s, i) =>
                s.value match {
                    case bb: Babbage
                        if i > 0 && bb.address != treasuryAddr && bb.address != ownAddr
                            && !redirected =>
                        redirected = true
                        Sized[TransactionOutput](bb.copy(address = ownAddr))
                    case _ => s
                }
            })
        }
        assertRejected("payout-redirect", mutated, "Evacuatees membership check failed")
    }

    test("evacuate must preserve setupG2 (no degenerate-setup downgrade)") {
        val mutated = withBody(legitTx) { b =>
            b.copy(outputs = b.outputs.map { s =>
                s.value match {
                    case bb: Babbage if bb.address == treasuryAddr =>
                        bb.datumOption match {
                            case Some(Inline(d)) =>
                                fromData[RuleBasedTreasuryDatumOnchain](d) match {
                                    case r: RuleBasedTreasuryDatumOnchain.ResolvedOnchain =>
                                        val corrupted: RuleBasedTreasuryDatumOnchain =
                                            r.copy(setupG2 = r.setupG2.dropRight(1))
                                        Sized[TransactionOutput](
                                          bb.copy(datumOption = Some(Inline(toData(corrupted))))
                                        )
                                    case _ => s
                                }
                            case _ => s
                        }
                    case _ => s
                }
            })
        }
        assertRejected(
          "setupG2-corrupt",
          mutated,
          "setupG2 in treasuryInput and treasuryOutput must match"
        )
    }

    test("evacuate positional parse: an inserted output shifts the treasury slot off the beacon") {
        val minAda = Value(Coin.ada(2))
        val feeBump = Coin.ada(2)
        val dummy: TransactionOutput =
            Babbage(address = ownAddr, value = minAda, datumOption = None, scriptRef = None)
        val mutated = withBody(legitTx) { b =>
            // pay for the inserted output + larger tx out of the attacker's own change output
            val rebalanced = b.outputs.zipWithIndex.map { case (s, i) =>
                s.value match {
                    case bb: Babbage if i == 0 =>
                        Sized[TransactionOutput](
                          bb.copy(value = bb.value - minAda - Value(feeBump))
                        )
                    case _ => s
                }
            }
            b.copy(
              outputs = Sized[TransactionOutput](dummy) +: rebalanced,
              fee = b.fee + feeBump
            )
        }
        // The shifted treasury slot lands on the attacker's change output, which is not at the
        // treasury script address, so the address pin rejects it before the beacon check.
        assertRejected(
          "positional-insert",
          mutated,
          "Treasury output must remain at the treasury script address"
        )
    }

    test("evacuate must advance the output accumulator (no replay of a drained set)") {
        // Leave the residual treasury's accumulator at the INPUT value (the commitment to the full
        // set M) instead of advancing it to the proof (the commitment to M minus the drained entry).
        // The Evacuate branch requires `outputAccumulator == proof`, so it must reject.
        val mutated = withBody(legitTx) { b =>
            b.copy(outputs = b.outputs.map { s =>
                s.value match {
                    case bb: Babbage if bb.address == treasuryAddr =>
                        bb.datumOption match {
                            case Some(Inline(d)) =>
                                fromData[RuleBasedTreasuryDatumOnchain](d) match {
                                    case r: RuleBasedTreasuryDatumOnchain.ResolvedOnchain =>
                                        val stale: RuleBasedTreasuryDatumOnchain =
                                            r.copy(evacuationActive = evacMap.kzgCommitment)
                                        Sized[TransactionOutput](
                                          bb.copy(datumOption = Some(Inline(toData(stale))))
                                        )
                                    case _ => s
                                }
                            case _ => s
                        }
                    case _ => s
                }
            })
        }
        assertRejected(
          "accumulator-not-advanced",
          mutated,
          "Accumulator in the output should be properly updated"
        )
    }

    test("treasury value-conservation prevents double-draining the same obligation") {
        val key = evacMap.keys.head
        // tx1: legitimately drain `key`.
        val evac1 = buildEvacuation(treasury, key, evacMap, feeUtxos(0))
            .fold(e => fail(s"tx1 build failed: $e"), identity)
        val emu2 = mkEmulator().submit(ownWallet.signTx(evac1.tx)) match {
            case Right((_, e)) => e
            case Left(err)     => fail(s"tx1 submit failed: $err")
        }
        val residual = evac1.treasuryUtxoProduced.copy(utxoId = currentTreasuryInput(emu2))

        // tx2: try to drain `key` AGAIN from the residual treasury. Its funds for `key` are gone, so
        // the value invariant (treasuryInput = treasuryOutput + Σ evacuated) makes the obligation
        // un-payable — the builder refuses; were a tx hand-crafted, the script's membership check
        // (against the advanced accumulator) would reject it.
        buildEvacuation(residual, key, evacMap, feeUtxos(1)) match {
            case Left(_) => () // expected: cannot re-pay a drained obligation from the residual
            case Right(evac2) =>
                emu2.submit(ownWallet.signTx(evac2.tx)) match {
                    case Right(_) => fail("double-drain of the same obligation was accepted")
                    case Left(err) =>
                        val s = err.toString
                        val _ = assert(
                          s.contains("membership") || s.contains("Evacuatees") ||
                              s.contains("Accumulator") || s.contains("Value invariant"),
                          s"double-drain rejected, but not by a treasury invariant: ${s.take(300)}"
                        )
                }
        }
    }

    test("evacuate rejects a forged KZG membership proof (soundness)") {
        // A valid G1 commitment, but NOT a valid membership proof for the drained obligation. Forge
        // it into BOTH the redeemer's `proof` and the output datum's accumulator, so the
        // accumulator-update check (output == proof) passes and the KZG membership check is what
        // must reject. The redeemer change invalidates the script-data hash, so recompute it.
        val forgedProof = evacMap.kzgCommitment
        val b0 = legitTx.body.value
        val treasuryIdx = b0.inputs.toIndexedSeq.indexOf(treasury.utxoId)

        val ws0 = legitTx.witnessSetRaw.value
        val forgedRedeemers = ws0.redeemers.toList.flatMap(_.value.toSeq).map { r =>
            if r.tag == RedeemerTag.Spend && r.index == treasuryIdx then
                fromData[TreasuryRedeemer](r.data) match {
                    case TreasuryRedeemer.Evacuate(er) =>
                        r.copy(data =
                            toData(TreasuryRedeemer.Evacuate(er.copy(proof = forgedProof)))
                        )
                    case _ => r
                }
            else r
        }
        val ws1 = ws0.copy(redeemers = Some(KeepRaw(Redeemers.from(forgedRedeemers))))

        val outs = b0.outputs.map { s =>
            s.value match {
                case bb: Babbage if bb.address == treasuryAddr =>
                    bb.datumOption match {
                        case Some(Inline(d)) =>
                            fromData[RuleBasedTreasuryDatumOnchain](d) match {
                                case r: RuleBasedTreasuryDatumOnchain.ResolvedOnchain =>
                                    val forged: RuleBasedTreasuryDatumOnchain =
                                        r.copy(evacuationActive = forgedProof)
                                    Sized[TransactionOutput](
                                      bb.copy(datumOption = Some(Inline(toData(forged))))
                                    )
                                case _ => s
                            }
                        case _ => s
                    }
                case _ => s
            }
        }

        val newHash = ScriptDataHashGenerator.computeScriptDataHash(
          ws1,
          env.headConfig.cardanoProtocolParams,
          TreeSet(Language.PlutusV3),
          ws1.redeemers,
          ws1.plutusData
        )
        val mutated = legitTx.copy(
          body = KeepRaw(b0.copy(outputs = outs, scriptDataHash = newHash)),
          witnessSetRaw = KeepRaw(ws1)
        )
        assertRejected("forged-proof", mutated, "Evacuatees membership check failed")
    }
}
