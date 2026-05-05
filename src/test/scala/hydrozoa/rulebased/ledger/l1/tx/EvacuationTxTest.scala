package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.multisig.ledger.commitment.KzgCommitment.asG1Element
import hydrozoa.multisig.ledger.commitment.{KzgCommitment, Membership, TrustedSetup}
import hydrozoa.multisig.ledger.eutxol2.toEvacuationMap
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.{gens as _, *}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import monocle.syntax.all.*
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.ArbitraryInstances.genByteStringOfN
import scalus.cardano.onchain.plutus.v3.TokenName
import scalus.uplc.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element}
import supranational.blst.Scalar
import test.*
import test.Generators.Hydrozoa.genPubKeyUtxo
import CommonGeneratorsTypes.*
import registry.scalacheck.*

private lazy val evacuationGens =
    gen(genEvacuationTxBuild) +:
        gen(genL2TransactionOutput) +:
        CommonGenerators.gens

def genL2TransactionOutput(config: CardanoNetwork.Section): Gen[TransactionOutput] =
    for {
        keyHash <- Arbitrary.arbitrary[AddrKeyHash]
        coin <- Gen.choose(1_000_000L, 10_000_000L)
    } yield TransactionOutput.Babbage(
      address = ShelleyAddress(
        network = config.network,
        payment = Key(keyHash),
        delegation = ShelleyDelegationPart.Null
      ),
      value = Value(Coin(coin)),
      datumOption = None,
      scriptRef = None
    )

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
        scriptAddr = HydrozoaBlueprint.mkTreasuryAddress(network)
    } yield RuleBasedTreasuryUtxo(
      utxoId = txId,
      treasuryOutput = RuleBasedTreasuryOutput(
        treasuryDatum,
        value
      )
    )

/** Generator for resolved treasury datum */
def genTreasuryResolvedDatum(
    headMp: PolicyId,
    utxosCommitment: KzgCommitment.KzgCommitment,
    setupSize: Int
): Gen[Resolved] =
    for {
        version <- evacuationGens.make[Gen[Version]]
        params <- genByteStringOfN(32)
        setup = TrustedSetup
            .takeSrsG2(setupSize)
            .map(p2 => BLS12_381_G2_Element(p2).toCompressedByteString)
    } yield Resolved(
      evacuationActive = utxosCommitment,
      version = version,
      setup = setup
    )

/** Generator for EvacuationTx transaction recipe */
// Feel free to trim down the config argument
def genEvacuationTxBuild(config: MultiNodeConfig): Gen[EvacuationTx.Build] =
    given MultiNodeConfig = config
    for {
        Right(evacMap) <- evacuationGens
            .make[Gen[Utxos]]
            .map(_.toEvacuationMap(config.headConfig))
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
        sufficientTreasuryValue = treasuryUtxo.treasuryOutput.value + totalL2Value +
            Value(Coin(20_000_000L))
        adjustedTreasuryUtxo = treasuryUtxo
            .focus(_.treasuryOutput.value)
            .set(sufficientTreasuryValue)

        // Generate validity slot
        validityEndSlot <- Gen.choose(100L, 1000L)

        addr = config.nodeConfigs.head._2.ownHeadWallet.exportVerificationKey
            .shelleyAddress()(using config.headConfig)

        feeUtxo <-
            genPubKeyUtxo(
              address = addr,
              genValue = Gen.const(Value.ada(100))
            )

        collateralUtxo <- genCollateralUtxo(addr.payment.asInstanceOf[Key].hash)

    } yield EvacuationTx.Build(
      treasuryUtxo = adjustedTreasuryUtxo,
      evacuatees = evacuatees,
      notEvacuatedYet = evacMap,
      feeUtxos = Map(feeUtxo.toTuple),
      collateralUtxo = collateralUtxo
    )

def genMembershipCheck(scalars: List[ScalusScalar])
    : Gen[(prelude.List[ScalusScalar], BLS12_381_G1_Element, BLS12_381_G1_Element)] =
    val identityBlst = prelude.List.from(
        scalars.map(ss => Scalar().from_bendian(ss._1.toByteArray))
    )
    val commitmentPoint = KzgCommitment.calculateKzgCommitment(identityBlst)
    val commitmentG1 = commitmentPoint.asG1Element

    for {
        // Subset
        subset <- Gen.pick(Integer.min(1, 64), scalars)

        // Proof
        theRest = scalars.diff(subset) // I don't like the name, but diff is disjoint
        theRestBlst = prelude.List.from(
          theRest.map(ss => Scalar().from_bendian(ss._1.toByteArray))
        )

        proofPoint = KzgCommitment.calculateKzgCommitment(theRestBlst)
        proofG1 = proofPoint.asG1Element

    } yield (prelude.List.from(subset), commitmentG1, proofG1)

object EvacuationTxTest extends Properties("EvacuationTx Test") {
    import MultiNodeConfig.*

    val _ = property(
      "EvacuationTx builds successfully with valid recipe"
    ) = runDefault(
      for {
          nc <- forAll[NodeConfig](evacuationGens)
          builder <- forAll[EvacuationTx.Build](evacuationGens)
          evacuationTx <- failLeft(builder.result(using nc))
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
          expectedResidual = builder.treasuryUtxo.treasuryOutput.value - totalEvacuations
          _ <- assertWith(
            evacuationTx.treasuryUtxoProduced.treasuryOutput.value == expectedResidual,
            "Residual treasury value should be correct"
          )
      } yield true
    )

    val _ = property("Partial membership proofs check") =
        val r = gen(genMembershipCheck) +: evacuationGens
        val tupleGen = r.makeGen[(prelude.List[ScalusScalar], BLS12_381_G1_Element, BLS12_381_G1_Element)]
        Prop.forAll(tupleGen)((subset, commitmentG1, proof) =>
            // Pre-calculated powers of tau
            val crsG2 =
                TrustedSetup.takeSrsG2(subset.length.toInt + 1).map(BLS12_381_G2_Element.apply)

            RuleBasedTreasuryValidator.checkMembership(crsG2, commitmentG1, subset, proof) == true
        )
}
