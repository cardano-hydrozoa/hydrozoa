package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.multisig.ledger.commitment.KzgCommitment.asG1Element
import hydrozoa.multisig.ledger.commitment.{KzgCommitment, Membership, TrustedSetup}
import hydrozoa.multisig.ledger.eutxol2.toEvacuationMap
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.ledger.l1.script.plutus.{RuleBasedTreasuryScript, RuleBasedTreasuryValidator}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.{ResolvedDatum, RuleBasedTreasuryDatum}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedTreasuryUtxo
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.cardano.onchain.plutus.v3.TokenName
import scalus.uplc.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element}
import supranational.blst.Scalar
import test.*
import test.Generators.Hydrozoa.genPubKeyUtxo

/** Generator for resolved treasury UTXO with resolved datum */
def genResolvedTreasuryUtxo(
    fallbackTxId: TransactionHash,
    headMp: PolicyId,
    beaconTokenName: TokenName,
    utxosCommitment: KzgCommitment.KzgCommitment,
    setupSize: Int,
    network: Network
): Gen[RuleBasedTreasuryUtxo] =
    for {
        treasuryDatum <- genTreasuryResolvedDatum(headMp, utxosCommitment, setupSize)
        // Min ada for the beacon token and the datum
        coin <- Gen.const(5_000_000L)
        value = Value(Coin(coin)) + Value.asset(headMp, AssetName(beaconTokenName), 1)
        outputIx <- Gen.choose(0, 5)
        txId = TransactionInput(fallbackTxId, outputIx)
        // Use the correct treasury script hash
        spp = ShelleyPaymentPart.Script(RuleBasedTreasuryScript.compiledScriptHash)
        scriptAddr = ShelleyAddress(network, spp, ShelleyDelegationPart.Null)
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
      evacuationActive = utxosCommitment,
      version = version,
      setup = setup
    )

/** Generator for EvacuationTx transaction recipe */
// Feel free to trim down the config argument
def genEvacuationTxBuild(using config: MultiNodeConfig): Gen[EvacuationTx.Build] =
    for {
        // Generate a set of 64-1000 L2 utxos
        l2UtxoCount <- Gen.choose(64, 1000)
        // FIXME: this is partial, but I'm just trying to restore the old test for now
        Right(evacMap) <- genUtxosL2(l2UtxoCount).map(
          _.toEvacuationMap(config.headConfig)
        )
        _ = println(s"evac map: ${evacMap.size}")

        // Calculate the whole L2 utxo set commitment
        utxoCommitment = evacMap.kzgCommitment

        // Select some random number of withdrawals
        // TODO: find the limit with refscripts
        wn <- Gen.choose(1, Integer.min(5, evacMap.size))
        evacuatees0 <- Gen.pick(wn, evacMap)
        _ = println(s"evacuatees length: ${evacuatees0.length}")
        evacuatees = EvacuationMap.from(evacuatees0)

        // Check
        // _ = println(s"withdrawals: {$withdrawals0}")
        // _ = println(
        //  s"withdrawal hashes: ${withdrawalScalars.map(e => BigInt.apply(e.to_bendian()))}"
        // )

        // Calculate and validate the membership proof
        theRest = evacMap.removedAll(Set.from(evacuatees.keys))
        _ = println(s"theRest: ${theRest.size}")

        membershipProof = Membership
            .mkMembershipProofValidated(
              set = evacMap,
              subset = evacuatees
            )
            .fold(err => throw RuntimeException(err.explain), x => x)

        _ = println(
          s"validated membership proof: ${membershipProof.asG1Element.toCompressedByteString.toHex}"
        )

        fallbackTxId <- Arbitrary.arbitrary[TransactionHash]

        // Generate treasury UTXO with _some_ funds
        beaconTokenName = config.headConfig.headTokenNames.treasuryTokenName
        treasuryUtxo <- genResolvedTreasuryUtxo(
          fallbackTxId,
          config.headConfig.headMultisigScript.policyId,
          beaconTokenName.bytes,
          utxoCommitment,
          wn + 1,
          config.headConfig.network
        )

        // Ensure treasury has sufficient funds
        totalL2Value = evacMap.totalValue
        sufficientTreasuryValue = treasuryUtxo.value + totalL2Value +
            Value(Coin(20_000_000L))
        adjustedTreasuryUtxo = treasuryUtxo.copy(value = sufficientTreasuryValue)

        // Generate validity slot
        validityEndSlot <- Gen.choose(100L, 1000L)

        utxos <- Gen.listOfN(
          2,
          genPubKeyUtxo(
            address = config.nodeConfigs.head._2.ownHeadWallet.address(config.headConfig.network),
            genValue = Gen.const(Value.ada(100))
          )
        )
    } yield EvacuationTx.Build(config.nodeConfigs.head._2)(
      _treasuryUtxo = adjustedTreasuryUtxo,
      _evacuatees = evacuatees,
      _notEvacuatedYet = evacMap,
      _feeUtxos = Map.from(utxos.tail.map(_.toTuple)),
      _collateralUtxo = utxos.head
    )

def genMembershipCheck
    : Gen[(prelude.List[ScalusScalar], BLS12_381_G1_Element, BLS12_381_G1_Element)] =
    for {
        // Acc elements
        length <- Gen.choose(64, 1024)
        identity <- Gen.listOfN(length, Arbitrary.arbitrary[ScalusScalar])
        identityBlst = prelude.List.from(
          identity.map(ss => Scalar().from_bendian(ss._1.toByteArray))
        )

        // Accumulator
        commitmentPoint = KzgCommitment.calculateKzgCommitment(identityBlst)
        commitmentG1 = commitmentPoint.asG1Element

        // Subset
        subset <- Gen.pick(Integer.min(1, 64), identity)

        // Proof
        theRest = identity.diff(subset) // I don't like the name, but diff is disjoint
        theRestBlst = prelude.List.from(
          theRest.map(ss => Scalar().from_bendian(ss._1.toByteArray))
        )

        proofPoint = KzgCommitment.calculateKzgCommitment(theRestBlst)
        proofG1 = proofPoint.asG1Element

    } yield (prelude.List.from(subset), commitmentG1, proofG1)

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

object EvacuationTxTest extends Properties("EvacuationTx Test") {
    import MultiNodeConfig.*

    val _ = property(
      "EvacuationTx builds successfully with valid recipe"
    ) = runDefault(
      for {
          env <- ask
          builder <- pick(genEvacuationTxBuild(using env))
          evacuationTx <- failLeft(builder.result)
          _ <- assertWith(
            evacuationTx.treasuryUtxoSpent == builder.treasuryUtxo,
            "Spent treasury UTXO should match recipe input"
          )
          _ <- assertWith(
            evacuationTx.treasuryUtxoProduced != null,
            "Treasury UTXO produced should not be null"
          )
          _ <- assertWith(
            evacuationTx.evacuatedOutputs.length == builder.evacuatees.size,
            "Evacuated outputs should match builder's evacuatees"
            // Note this holds because we're not
            // testing large numbers of evacuations
          )
          _ <- assertWith(evacuationTx.tx != null, "Transaction should not be null")

          // Verify residual treasury value is correct
          totalEvacuations =
              builder.evacuatees.totalValue
          expectedResidual = builder.treasuryUtxo.value - totalEvacuations
          _ <- assertWith(
            evacuationTx.treasuryUtxoProduced.value == expectedResidual,
            "Residual treasury value should be correct"
          )
      } yield true
    )

    val _ = property("Partial membership proofs check") =
        Prop.forAll(genMembershipCheck)((subset, commitmentG1, proof) =>

            // Pre-calculated powers of tau
            val crsG2 =
                TrustedSetup.takeSrsG2(subset.length.toInt + 1).map(BLS12_381_G2_Element.apply)

            RuleBasedTreasuryValidator.checkMembership(crsG2, commitmentG1, subset, proof) == true
        )
}
