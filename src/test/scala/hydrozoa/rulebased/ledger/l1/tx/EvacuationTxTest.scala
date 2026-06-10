package hydrozoa.rulebased.ledger.l1.tx

import cats.effect.unsafe.implicits.global
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.Scalar as ScalusScalar
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.multisig.ledger.commitment.KzgCommitment.asG1Element
import hydrozoa.multisig.ledger.commitment.{KzgCommitment, TrustedSetup}
import hydrozoa.multisig.ledger.eutxol2.toEvacuationMap
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import monocle.*
import monocle.syntax.all.*
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v3.TokenName
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}
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
        treasuryDatum <- genTreasuryResolvedDatum(headMp, utxosCommitment)
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
): Gen[Resolved] =
    for {
        version <- genVersion
        setup = TrustedSetup
            .takeSrsG2(EvacuationTx.Assumptions.maxEvacuationsPerTx + 1)
            .map(p2 => G2Element(p2).toCompressedByteString)
    } yield Resolved(
      evacuationActive = utxosCommitment,
      version = version,
      setup = setup
    )

/** Generator for EvacuationTx transaction recipe */
// Feel free to trim down the config argument
def genEvacuationTxBuild(using config: MultiNodeConfig): Gen[EvacuationTx.Build] =
    for {
        // Generate a set of 64-1000 L2 utxos
        l2UtxoCount <- Gen.choose(1, 100)
        // FIXME: this is partial, but I'm just trying to restore the old test for now
        Right(evacMap) <- genUtxosL2(l2UtxoCount).map(
          _.toEvacuationMap(config.headConfig)
        )
        _ = println(s"evac map: ${evacMap.size}")

        // Calculate the whole L2 utxo set commitment
        utxoCommitment = evacMap.kzgCommitment

        // Select some random number of withdrawals
        // TODO: find the limit with refscripts
        wn <- Gen.choose(1, evacMap.size)
        evacuatees0 <- Gen.pick(wn, evacMap)
        _ = println(s"evacuatees length: ${evacuatees0.length}")
        evacuatees = EvacuationMap.from(evacuatees0)

        // Calculate and validate the membership proof
        theRest = evacMap.removedAll(Set.from(evacuatees.keys))
        _ = println(s"theRest: ${theRest.size}")

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
            Value(Coin(200_000_000L))
        adjustedTreasuryUtxo = treasuryUtxo
            .focus(_.treasuryOutput.value)
            .replace(sufficientTreasuryValue)

        // Generate validity slot
        validityEndSlot <- Gen.choose(100L, 1000L)

        addr = config.nodeConfigs.head._2.ownWallet.exportVerificationKey
            .shelleyAddress()(using config.headConfig)

        feeUtxo <-
            genPubKeyUtxo(
              address = addr,
              genValue = Gen.const(Value.ada(100))
            )

        collateralUtxo <- genCollateralUtxo(addr.payment.asInstanceOf[Key].hash)(using config)

    } yield EvacuationTx.Build(
      inputTreasuryUtxo = adjustedTreasuryUtxo,
      evacuateesToTryNext = evacuatees,
      allRemainingEvacuatees = evacMap,
      feeUtxos = Map(feeUtxo.toTuple),
      collateralUtxo = collateralUtxo
    )

def genMembershipCheck: Gen[(prelude.List[ScalusScalar], G1Element, G1Element)] =
    for {
        // Acc elements
        length <- Gen.choose(EvacuationTx.Assumptions.maxEvacuationsPerTx, 1024)
        identity <- Gen.listOfN(length, Arbitrary.arbitrary[ScalusScalar])
        identityBlst = prelude.List.from(
          identity.map(ss => Scalar().from_bendian(ss._1.toByteArray))
        )

        // Accumulator
        commitmentPoint = KzgCommitment.calculateKzgCommitment(identityBlst)
        commitmentG1 = commitmentPoint.asG1Element

        // Subset
        subset <- Gen.pick(Integer.min(1, EvacuationTx.Assumptions.maxEvacuationsPerTx), identity)

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
    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withInitialSeed(
          org.scalacheck.rng.Seed.fromBase64("9PR4WvgQ9fPaxaw-w9NDv8Ug0CT9TXibdweMlDrWlQF=").get
        )

    import MultiNodeConfig.*

    val _ = property(
      "EvacuationTx builds successfully with valid recipe"
    ) = runDefault(
      for {
          env <- ask
          builder <- pick(genEvacuationTxBuild(using env))
          evacuationTx <- failLeft(builder.result(using env.nodeConfigs.head._2))
          _ <- assertWith(
            evacuationTx.treasuryUtxoSpent == builder.inputTreasuryUtxo,
            "Spent treasury UTXO should match recipe input"
          )
          _ <- assertWith(
            evacuationTx.treasuryUtxoProduced != null,
            "Treasury UTXO produced should not be null"
          )
          _ <- assertWith(
            {
                val tried = builder.evacuateesToTryNext.toSet.map((_, payout) => payout.utxo.value)
                val actual = evacuationTx.evacuatedOutputs.toSet
                actual.subsetOf(tried)
            },
            "Evacuated outputs should match builder's evacuatees"
            // Note this holds because we're not
            // testing large numbers of evacuations

          )
          _ <- assertWith(evacuationTx.tx != null, "Transaction should not be null")

          // Verify residual treasury value is correct
          totalEvacuations =
              builder.evacuateesToTryNext.totalValue
          expectedResidual = builder.inputTreasuryUtxo.treasuryOutput.value - Value.combine(
            evacuationTx.evacuatedOutputs.map(_.value)
          )
          actualResidual = evacuationTx.treasuryUtxoProduced.treasuryOutput.value
          _ <- assertWith(
            actualResidual == expectedResidual,
            s"Residual treasury value should be correct.\n\tExpected: $expectedResidual\n\tActual:$actualResidual"
          )
      } yield true
    )

    val _ = property("Partial membership proofs check") =
        Prop.forAll(genMembershipCheck)((subset, commitmentG1, proof) =>

            // Pre-calculated powers of tau
            val crsG2 =
                TrustedSetup
                    .takeSrsG2(EvacuationTx.Assumptions.maxEvacuationsPerTx + 1)
                    .map(G2Element.apply)

            RuleBasedTreasuryValidator.checkMembership(crsG2, commitmentG1, subset, proof) == true
        )
}
