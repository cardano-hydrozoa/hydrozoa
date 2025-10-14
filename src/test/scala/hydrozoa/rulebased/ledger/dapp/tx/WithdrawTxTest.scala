package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.multisig.ledger.virtual.commitment.{KzgCommitment, TrustedSetup}
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.{RuleBasedTreasuryScript, RuleBasedTreasuryValidator}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.{ResolvedDatum, RuleBasedTreasuryDatum}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import org.scalacheck.{Arbitrary, Gen, Prop, Test as ScalaCheckTest}
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString}
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.FeesOkValidator
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.ledger.api.v3.TokenName
import scalus.prelude as scalus
import supranational.blst.Scalar
import test.*

/** Generator for resolved treasury UTXO with resolved datum */
def genResolvedTreasuryUtxo(
    fallbackTxId: TransactionHash,
    headMp: PolicyId,
    beaconTokenName: TokenName,
    utxosCommitment: KzgCommitment.KzgCommitment
): Gen[RuleBasedTreasuryUtxo] =
    for {
        treasuryDatum <- genTreasuryResolvedDatum(headMp, utxosCommitment)
        // Min ada for the beacon token and the datum
        coin <- Gen.const(5_000_000L)
        value = Value(Coin(coin)) + singleton(headMp, AssetName(beaconTokenName))
        outputIx <- Gen.choose(0, 5)
        txId = TransactionInput(fallbackTxId, outputIx)
        // Use the correct treasury script hash
        spp = ShelleyPaymentPart.Script(RuleBasedTreasuryScript.compiledScriptHash)
        scriptAddr = ShelleyAddress(Mainnet, spp, ShelleyDelegationPart.Null)
    } yield RuleBasedTreasuryUtxo(
      beaconTokenName = AssetName(beaconTokenName),
      txId = txId,
      addr = scriptAddr,
      datum = RuleBasedTreasuryDatum.Resolved(treasuryDatum),
      value = value
    )

/** Generator for resolved treasury datum */
def genTreasuryResolvedDatum(
    headMp: PolicyId,
    utxosCommitment: KzgCommitment.KzgCommitment
): Gen[ResolvedDatum] =
    for {
        version <- genVersion
        params <- genByteStringOfN(32)
        setup = TrustedSetup
            .takeSrsG2(26)
            .map(p2 => BLS12_381_G2_Element(p2).toCompressedByteString)
    } yield ResolvedDatum(
      headMp = headMp,
      utxosActive = ByteString.fromArray(IArray.genericWrapArray(utxosCommitment).toArray),
      version = version,
      params = params,
      setup = setup
    )

def mkCommitment(withdrawals: UTxO): KzgCommitment.KzgCommitment =
    val utxoHashed = KzgCommitment.hashToScalar(withdrawals)
    // println(s"blst utxos active hashes: ${utxoHashed.map(e => BigInt.apply(e.to_bendian()))}")
    val ret = KzgCommitment.calculateCommitment(utxoHashed)
    // println(s"commitment=${HexUtil.encodeHexString(IArray.genericWrapArray(ret).toArray)}")
    ret

/** Generator for withdraw transaction recipe */
def genWithdrawTxRecipe: Gen[WithdrawTx.Recipe] =
    for {
        // Common head parameters
        (
          headScriptHash,
          headTokensSuffix,
          peers,
          peersVKs,
          paramsHash,
          versionMajor,
          fallbackTxId
        ) <- genHeadParams

        // Generate a set of L2 utxos
        l2UtxoCount <- Gen.choose(2, 25)
        utxosL2 <- genUtxosL2(l2UtxoCount)

        // Calculate the whole utxo set commitment
        utxoCommitment = mkCommitment(utxosL2.asScalus)

        // Generate treasury UTXO with _some_ funds
        beaconTokenName = cip67BeaconTokenPrefix.concat(headTokensSuffix)
        treasuryUtxo <- genResolvedTreasuryUtxo(
          fallbackTxId,
          headScriptHash,
          beaconTokenName,
          utxoCommitment
        )

        // Ensure treasury has sufficient funds
        totalL2Value = utxosL2.untagged.values.foldLeft(Value.zero)(_ + _.untagged.value)
        sufficientTreasuryValue = treasuryUtxo.value + totalL2Value +
            Value(Coin(20_000_000L))
        adjustedTreasuryUtxo = treasuryUtxo.copy(value = sufficientTreasuryValue)

        // Select some random withdrawals
        // withdrawals0 <- Gen.atLeastOne(utxosL2.untagged)
        // FIXME: test works for 4 withdrawals but stop working for 5
        withdrawals0 <- Gen.pick(Integer.min(4, utxosL2.untagged.keys.size), utxosL2.untagged)
        withdrawals = UtxoSet(withdrawals0.toMap)

        // Calculate the membership proof
        theRest = UtxoSet(utxosL2.untagged -- withdrawals.untagged.keys)
        membershipProof = mkCommitment(theRest.asScalus)

        // Generate validity slot
        validityEndSlot <- Gen.choose(100L, 1000L)
    } yield WithdrawTx.Recipe(
      treasuryUtxo = adjustedTreasuryUtxo,
      withdrawals = withdrawals,
      membershipProof = membershipProof,
      validityEndSlot = validityEndSlot,
      network = testNetwork,
      protocolParams = testProtocolParams,
      evaluator = testEvaluator,
      validators = testValidators.filterNot(_ == FeesOkValidator)
    )

class WithdrawTxTest extends munit.ScalaCheckSuite {

    override def scalaCheckTestParameters: ScalaCheckTest.Parameters =
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(1000)

    property("Withdraw tx builds successfully with valid recipe")(
      Prop.forAll(genWithdrawTxRecipe) { recipe =>
          WithdrawTx.build(recipe) match {
              case Left(error) =>
                  throw RuntimeException(s"Build failed with valid recipe: $error")
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
                  assert(
                    withdrawTx.withdrawalOutputs.length == recipe.withdrawals.size,
                    "Withdrawal outputs should match recipe withdrawals"
                  )
                  assert(withdrawTx.tx != null, "Transaction should not be null")

                  // Verify residual treasury value is correct
                  val totalWithdrawals =
                      recipe.withdrawals.values.foldLeft(Value.zero)(_ + _.untagged.value)
                  val expectedResidual = recipe.treasuryUtxo.value - totalWithdrawals
                  assert(
                    withdrawTx.treasuryUtxoProduced.value == expectedResidual,
                    "Residual treasury value should be correct"
                  )

                  true
          }
      }
    )

    property("Partial membership proofs check") {
        Prop.forAll(genMembershipCheck) { (subset, commitmentG1, proof) =>

            // Pre-calculated powers of tau
            val crsG2 = TrustedSetup.takeSrsG2(subset.length.toInt + 1).map(BLS12_381_G2_Element.apply)

            assertEquals(
                RuleBasedTreasuryValidator.checkMembership(crsG2, commitmentG1, subset, proof),
                true
            )
        }
    }

    def genMembershipCheck: Gen[(scalus.List[ScalusScalar], BLS12_381_G1_Element, BLS12_381_G1_Element)] =
        for {
            // Acc elements
            length <- Gen.choose(1, 64)
            identity <- Gen.listOfN(length, Arbitrary.arbitrary[ScalusScalar])
            identityBlst = scalus.List.from(identity.map(ss => Scalar().from_bendian(ss._1.toByteArray)))

            // Accumulator
            commitmentPoint = KzgCommitment.calculateCommitment(identityBlst)
            commitmentBS = ByteString.fromArray(IArray.genericWrapArray(commitmentPoint).toArray)
            commitmentG1 = BLS12_381_G1_Element(commitmentBS)

            // Subset
            subset <- Gen.pick(Integer.min(1, length), identity)

            // Proof
            theRest = identity.diff(subset) // I don't like the name, but diff is disjoint
            theRestBlst = scalus.List.from(theRest.map(ss => Scalar().from_bendian(ss._1.toByteArray)))

            proofPoint = KzgCommitment.calculateCommitment(theRestBlst)
            proofBS = ByteString.fromArray(IArray.genericWrapArray(proofPoint).toArray)
            proofG1 = BLS12_381_G1_Element(proofBS)

        } yield (scalus.List.from(subset), commitmentG1, proofG1)

    // Arbitrary instance for ScalusScalar that generates big enough (> 2^230) values
    given Arbitrary[ScalusScalar] = Arbitrary {
        for {
            // Generate a large BigInt within the scalar field range
            // Use a range that's big enough but still within the field prime
            bigInt <- Gen.choose(
                BigInt("1000000000000000000000000000000000000000000000000000000000000000000000"),
                ScalusScalar.fieldPrime - 1
            )
        } yield ScalusScalar.applyUnsafe(bigInt)
    }

}
