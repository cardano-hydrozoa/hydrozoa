package hydrozoa.rulebased.ledger.dapp.tx

import hydrozoa.*
import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.virtual.commitment.{KzgCommitment, TrustedSetup}
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.{RuleBasedTreasuryScript, RuleBasedTreasuryValidator}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.{ResolvedDatum, RuleBasedTreasuryDatum}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.annotation.nowarn
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString}
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.v3.TokenName
import scalus.prelude as scalus
import supranational.blst.Scalar
import test.*
import test.Generators.Hydrozoa.genAdaOnlyPubKeyUtxo

/** Generator for resolved treasury UTXO with resolved datum */
def genResolvedTreasuryUtxo(
    fallbackTxId: TransactionHash,
    headMp: PolicyId,
    beaconTokenName: TokenName,
    utxosCommitment: KzgCommitment.KzgCommitment,
    setupSize: Int
): Gen[RuleBasedTreasuryUtxo] =
    for {
        treasuryDatum <- genTreasuryResolvedDatum(headMp, utxosCommitment, setupSize)
        // Min ada for the beacon token and the datum
        coin <- Gen.const(5_000_000L)
        value = Value(Coin(coin)) + singleton(headMp, AssetName(beaconTokenName))
        outputIx <- Gen.choose(0, 5)
        txId = TransactionInput(fallbackTxId, outputIx)
        // Use the correct treasury script hash
        spp = ShelleyPaymentPart.Script(RuleBasedTreasuryScript.compiledScriptHash)
        scriptAddr = ShelleyAddress(testNetwork, spp, ShelleyDelegationPart.Null)
    } yield RuleBasedTreasuryUtxo(
      utxoId = txId,
      address = scriptAddr,
      datum = RuleBasedTreasuryDatum.Resolved(treasuryDatum),
      value = value
    )

/** Generator for resolved treasury datum */
def genTreasuryResolvedDatum(
    headMp: PolicyId,
    utxosCommitment: KzgCommitment.KzgCommitment,
    setupSize: Int
): Gen[ResolvedDatum] =
    for {
        version <- genVersion
        params <- genByteStringOfN(32)
        setup = TrustedSetup
            .takeSrsG2(setupSize)
            .map(p2 => BLS12_381_G2_Element(p2).toCompressedByteString)
    } yield ResolvedDatum(
      headMp = headMp,
      utxosActive = ByteString.fromArray(IArray.genericWrapArray(utxosCommitment).toArray),
      version = version,
      params = params,
      setup = setup
    )

def mkCommitment(withdrawals: Utxos): KzgCommitment.KzgCommitment =
    val utxoHashed = KzgCommitment.hashToScalar(withdrawals)
    // println(s"blst utxos active hashes: ${utxoHashed.map(e => BigInt.apply(e.to_bendian()))}")
    val ret = KzgCommitment.calculateCommitment(utxoHashed)
    // println(s"commitment=${HexUtil.encodeHexString(IArray.genericWrapArray(ret).toArray)}")
    ret

/** Generator for withdraw transaction recipe */
def genWithdrawTxRecipe: Gen[(Withdrawal.Builder.Recipe, TestPeer)] = {
    for {
        peers <- genTestPeers()
        peer = peers.head
        nUtxos <- Gen.frequency(
          (1, Gen.const(0)),
          (1, Gen.const(1)),
          (1, Gen.const(2)),
          (20, Gen.posNum[Int])
        )
        fallbackTxId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        seedUtxo <- for {
            txHash <- genByteStringOfN(32).map(TransactionHash.fromByteString)
            index <- Gen.choose(0, 10)
        } yield TransactionInput(txHash, index)
        tokenNames = TokenNames(seedUtxo)

        activeSet <- genUtxosL2(nUtxos)
        withdrawalSubset <- Gen.atLeastOne(activeSet)
        feeUtxos <- Gen.nonEmptyListOf(genAdaOnlyPubKeyUtxo(peer))

        headTokenSuffix <- genByteStringOfN(28)
        beaconTokenName = cip67BeaconTokenPrefix.concat(headTokenSuffix)

        treasuryUtxo <- genResolvedTreasuryUtxo(
          fallbackTxId = fallbackTxId,
          headMp = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes)).policyId,
          beaconTokenName = beaconTokenName,
          utxosCommitment = KzgCommitment.calculateCommitment(
            KzgCommitment.hashToScalar(activeSet.asScalus)
          ),
          setupSize = withdrawalSubset.size + 1
        )

        sufficientTreasuryValue =
            treasuryUtxo.value
                + activeSet.asScalus.values.toList.foldLeft(Value.zero)((v, to) => v + to.value)
                + Value.ada(20)
        adjustedTreasuryUtxo = treasuryUtxo.copy(value = sufficientTreasuryValue)

    } yield (
      Withdrawal.Builder.Recipe(
        treasuryUtxo = adjustedTreasuryUtxo,
        withdrawalsSubset = UtxoSet[L2](withdrawalSubset.toMap),
        activeSet = activeSet,
        feeUtxos = UtxoSet[L1](
          feeUtxos
              .map(utxo => (UtxoId[L1](utxo.input), Output[L1](utxo.output)))
              .toMap
        )
      ),
      peers.head
    )
}

@nowarn("msg=unused value")
class WithdrawTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)

    ignore(
      "Withdraw tx builds successfully with valid recipe - FIXME: pair deconstruction regression"
    ) {
        forAll(genShelleyAddress, genWithdrawTxRecipe) { case (changeAddress, (recipe, peer)) =>
            val config = Withdrawal.Config(testTxBuilderCardanoInfo, changeAddress)
            Withdrawal(config).build(recipe) match {
                case Left(error) =>
                    fail(s"Build failed with valid recipe: $error")
                case Right(withdrawTx) =>
                    // Verify WithdrawTx structure
                    assert(
                      withdrawTx.treasuryUtxoSpent == recipe.treasuryUtxo,
                      "Spent treasury UTXO should match recipe input"
                    )
                    assert(
                      withdrawTx.treasuryUtxoProduced != null,
                      "Treasury UTXO produced should not be null"
                    )
            }
        }
    }

    test("Partial membership proofs check") {
        forAll(genMembershipCheck) { (subset, commitmentG1, proof) =>

            // Pre-calculated powers of tau
            val crsG2 =
                TrustedSetup.takeSrsG2(subset.length.toInt + 1).map(BLS12_381_G2_Element.apply)

            assert(
              RuleBasedTreasuryValidator.checkMembership(crsG2, commitmentG1, subset, proof) == true
            )
        }
    }

    def genMembershipCheck
        : Gen[(scalus.List[ScalusScalar], BLS12_381_G1_Element, BLS12_381_G1_Element)] =
        for {
            // Acc elements
            length <- Gen.choose(64, 1024)
            identity <- Gen.listOfN(length, Arbitrary.arbitrary[ScalusScalar])
            identityBlst = scalus.List.from(
              identity.map(ss => Scalar().from_bendian(ss._1.toByteArray))
            )

            // Accumulator
            commitmentPoint = KzgCommitment.calculateCommitment(identityBlst)
            commitmentBS = ByteString.fromArray(IArray.genericWrapArray(commitmentPoint).toArray)
            commitmentG1 = BLS12_381_G1_Element(commitmentBS)

            // Subset
            subset <- Gen.pick(Integer.min(1, 64), identity)

            // Proof
            theRest = identity.diff(subset) // I don't like the name, but diff is disjoint
            theRestBlst = scalus.List.from(
              theRest.map(ss => Scalar().from_bendian(ss._1.toByteArray))
            )

            proofPoint = KzgCommitment.calculateCommitment(theRestBlst)
            proofBS = ByteString.fromArray(IArray.genericWrapArray(proofPoint).toArray)
            proofG1 = BLS12_381_G1_Element(proofBS)

        } yield (scalus.List.from(subset), commitmentG1, proofG1)

    // TODO: upstream
    // Arbitrary instance for ScalusScalar that generates big enough (> 2^230) values
    given Arbitrary[ScalusScalar] = Arbitrary {
        for {
            bigInt <- Gen.choose(
              BigInt("1000000000000000000000000000000000000000000000000000000000000000000000"),
              ScalusScalar.fieldPrime - 1
            )
        } yield ScalusScalar.applyUnsafe(bigInt)
    }

}
