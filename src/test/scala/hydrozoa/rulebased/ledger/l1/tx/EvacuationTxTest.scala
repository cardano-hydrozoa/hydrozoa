package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.multisig.ledger.commitment.KzgCommitment.asG1Element
import hydrozoa.multisig.ledger.commitment.{KzgCommitment, Membership, TrustedSetup}
import hydrozoa.multisig.ledger.eutxol2.toEvacuationMap
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.{genCollateralUtxo, someShelleyAddress, gens as _}
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import monocle.syntax.all.*
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart}
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
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import registry.scalacheck.*

private lazy val evacuationGens =
    gen(genEvacuationTxBuild) +:
        // Bump the sampled treasury value to cover Σ evacuation outputs + a 20-ADA buffer, so the
        // on-chain `treasuryInput = treasuryOutput + Σ evacuated` invariant can hold.
        refineGen[EvacuationTx.Build](adjustTreasuryUtxo) +:
        // L1 fee utxo: a single 100-ADA pub-key utxo at the payment address.
        gen(genFeeUtxos) +:
        // Splits the sampled L2 EvacuationMap into notEvacuatedYet and evacuatees.
        gen(evacuate) +:
        // Set the per-utxo Coin to 1–10 ADA.
        refineGen[(TransactionOutput, Coin)](Gen.choose(1_000_000L, 10_000_000L)) +:
        // Set every L2 utxo's address to a ShelleyAddress
        refineGen[TransactionOutput](someShelleyAddress) +:
        // Single ShelleyAddress for transaction outputs
        gen(someShelleyAddress) +:
        // Create Babbage transaction outputs with a single coin value.
        gen(genTransactionOutput) +:
        // Treasury output value: 5-ADA + 1 beacon token. The beacon token is required for the
        // on-chain treasury invariant.
        refineGen[RuleBasedTreasuryOutput](beaconValue) +:
        // Set the treasury datum to `Resolved`
        refineGen[RuleBasedTreasuryOutput](genTreasuryResolvedDatum) +:
        // KZG commitment computed from the actual sampled L2 set
        gen(genCommitment) +:
        // Share the same generated `Utxos` between `genCommitment` and `genEvacuationMap`
        // so that the on-chain KZG check (commitment vs. evacuation map) succeeds.
        share[Utxos] +:
        CommonGenerators.gens

def paymentAddress(config: NodeConfig): ShelleyAddress =
    config.ownHeadWallet.exportVerificationKey.shelleyAddress()(using config.headConfig)

def genTransactionOutput(address: Address, coin: Coin): Gen[TransactionOutput] =
    TransactionOutput.Babbage(address, Value(coin))

def beaconValue(headConfig: HeadConfig): Value =
    val beaconTokenName = headConfig.headTokenNames.treasuryTokenName.bytes
    val headMp = headConfig.headMultisigScript.policyId
    // Treasury coin — fixed at 5M, the min-ada needed for beacon + datum.
    Value(Coin(5_000_000L)) + Value.asset(headMp, AssetName(beaconTokenName), 1)

def genCommitment(utxos: Utxos, config: MultiNodeConfig): KzgCommitment =
    val Right(evacMap) = utxos.toEvacuationMap(config.headConfig)
    // Calculate the whole L2 utxo set commitment, wrapped in the registry-side opaque alias.
    kzgCommitment(evacMap.kzgCommitment)

def adjustTreasuryUtxo(headConfig: HeadConfig, utxos: Utxos, treasuryUtxo: RuleBasedTreasuryUtxo): RuleBasedTreasuryUtxo =
    val Right(evacMap) = utxos.toEvacuationMap(headConfig)
    val totalL2Value = evacMap.totalValue
    val sufficientTreasuryValue = treasuryUtxo.treasuryOutput.value + totalL2Value + Value(Coin(20_000_000L))
    treasuryUtxo.focus(_.treasuryOutput.value).set(sufficientTreasuryValue)

/** Split the evacuation map to create the evacuatees */
def evacuate(evacMap: EvacuationMap): Gen[(EvacuationMap, EvacuationMap)] =
    for {
        wn <- Gen.choose(1, Integer.min(5, evacMap.size))
        evacuatees <- Gen.pick(wn, evacMap)
    } yield (evacMap, EvacuationMap.from(evacuatees))

/** Generator for EvacuationTx transaction recipe */
def genEvacuationTxBuild(treasuryUtxo: RuleBasedTreasuryUtxo, feeUtxos: FeeUtxos, evacMaps: (EvacuationMap, EvacuationMap), collateralUtxo: CollateralUtxo): EvacuationTx.Build =
    EvacuationTx.Build(
      treasuryUtxo,
      evacuatees = evacMaps._1,
      notEvacuatedYet = evacMaps._2,
      collateralUtxo,
      feeUtxos.toUtxos,
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

    private lazy val membershipRegistry =
        // 64..256 is plenty to exercise the validator while cutting BLS work by ~3x compared to the default generator
        listOfMinMax[ScalusScalar](64, 256) +:
            gen(genMembershipCheck) +:
            evacuationGens

    private lazy val membershipGen =
        membershipRegistry
            .makeGen[(prelude.List[ScalusScalar], BLS12_381_G1_Element, BLS12_381_G1_Element)]

    val _ = property("Partial membership proofs check") =
        Prop.forAll(membershipGen)((subset, commitmentG1, proof) =>
            // Pre-calculated powers of tau
            val crsG2 =
                TrustedSetup.takeSrsG2(subset.length.toInt + 1).map(BLS12_381_G2_Element.apply)
            RuleBasedTreasuryValidator.checkMembership(crsG2, commitmentG1, subset, proof) == true
        )
}
