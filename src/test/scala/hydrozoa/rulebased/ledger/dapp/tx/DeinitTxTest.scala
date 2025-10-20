package hydrozoa.rulebased.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.*
import hydrozoa.config.EquityShares
import hydrozoa.lib.tx.SomeBuildError
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.cip67BeaconTokenPrefix
import hydrozoa.rulebased.ledger.dapp.script.plutus.{RuleBasedTreasuryScript, RuleBasedTreasuryValidator}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.ResolvedDatum
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.*
import hydrozoa.rulebased.ledger.dapp.tx.DeinitTx.{DeinitTxError, Recipe}
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import scalus.builtin.ByteString
import scalus.builtin.ByteString.hex
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.ValueNotConservedUTxOValidator
import spire.math.Rational
import spire.syntax.literals.r
import test.*

/** Generator for resolved treasury UTXO with empty L2 state for deinit */
def genEmptyResolvedTreasuryUtxo(
    fallbackTxId: TransactionHash,
    headMp: PolicyId,
    beaconTokenName: AssetName,
    equity: Coin
): Gen[RuleBasedTreasuryUtxo] = {
    // Create empty resolved datum (G1.generator for empty UTXO set)
    val g1Generator = hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
    val dummyParams = hex"0123456789012345678901234567890123456789012345678901234567890123"
    val dummySetup = scalus.prelude.List(hex"deadbeef")

    val emptyResolvedDatum = ResolvedDatum(
        headMp = headMp,
        utxosActive = g1Generator,
        version = (BigInt(1), BigInt(0)),
        params = dummyParams,
        setup = dummySetup
    )

    for {
        outputIx <- Gen.choose(0, 5)
        headTokens <- genHeadTokensMultiAsset(headMp)
    } yield {
        val txId = TransactionInput(fallbackTxId, outputIx)
        val spp = ShelleyPaymentPart.Script(RuleBasedTreasuryScript.compiledScriptHash)
        val scriptAddr = ShelleyAddress(Mainnet, spp, ShelleyDelegationPart.Null)
        val value = Value(equity) + singleton(headMp, beaconTokenName) + Value(Coin(0), headTokens)
        
        RuleBasedTreasuryUtxo(
            beaconTokenName = beaconTokenName,
            txId = txId,
            addr = scriptAddr,
            datum = Resolved(emptyResolvedDatum),
            value = value
        )
    }
}

/** Generate head tokens MultiAsset for testing */
def genHeadTokensMultiAsset(headMp: PolicyId): Gen[MultiAsset] =
    for {
        numTokens <- Gen.choose(1, 3)
        tokens <- Gen.listOfN(numTokens, genHeadToken)
    } yield MultiAsset.fromPolicy(headMp, tokens.toMap)

def genHeadToken: Gen[(AssetName, Long)] =
    for {
        tokenName <- arbitrary[AssetName]
        amount <- Gen.choose(1L, 1000L)
    } yield (tokenName, amount)

/** Generate EquityShares for testing */
def genEquityShares(peers: NonEmptyList[TestPeer]): Gen[EquityShares] =
    for {
        addresses <- Gen.listOfN(peers.length, genPubkeyAddr(testNetwork))
    } yield {
        val shares = generateProportionalShares(peers.length)
        val equityList = addresses.zip(shares).map { case (addr, share) =>
            (Address[L1](addr), share)
        }
        EquityShares.apply(equityList).toOption.get
    }

def generateProportionalShares(n: Int): List[Rational] =
    if n == 1 then List(r"1")
    else if n == 2 then List(r"1/2", r"1/2")
    else if n == 3 then List(r"1/3", r"1/3", r"1/3")
    else if n == 4 then List(r"1/4", r"1/4", r"1/4", r"1/4")
    else {
        val share = Rational(1, n)
        List.fill(n)(share)
    }

/** Generate a simplified DeinitTx Recipe for testing */
def genSimpleDeinitTxRecipe: Gen[Recipe] =
    for {
        (hns, headTokenName, peers, peersVks, versionMajor, setupSize, fallbackTxId) <- genHeadParams
        equity <- Gen.choose(1_000_000L, 100_000_000L).map(Coin(_))
        treasuryUtxo <- genEmptyResolvedTreasuryUtxo(
            fallbackTxId,
            hns.policyId,
            AssetName(cip67BeaconTokenPrefix ++ headTokenName),
            equity
        )
        shares <- genEquityShares(peers)
        collateralUtxo <- genCollateralUtxo
    } yield Recipe(
        headNativeScript = hns,
        treasuryUtxo = treasuryUtxo,
        shares = shares,
        collateralUtxo = collateralUtxo,
        env = testTxBuilderEnvironment,
        // TODO: revert once scalus #144 is merged
        validators = testValidators.filterNot(_ == ValueNotConservedUTxOValidator)
    )

def genCollateralUtxo: Gen[Utxo[L1]] =
    for {
        txId <- arbitrary[TransactionHash]
        ix <- Gen.choose(0, 10)
        addr <- genPubkeyAddr(testNetwork)
        value <- Gen.choose(5_000_000L, 50_000_000L).map(v => Value(Coin(v)))
    } yield Utxo[L1](
        UtxoId[L1](TransactionInput(txId, ix)),
        Output[L1](Babbage(
            address = Address[L1](addr),
            value = value,
            datumOption = None,
            scriptRef = None
        ))
    )

class DeinitTxTest extends munit.ScalaCheckSuite {

    test("Recipe generator works") {
        val exampleRecipe = genSimpleDeinitTxRecipe.sample.get
        println(exampleRecipe)
    }

    property("DeinitTx builds successfully")(
        forAll(genSimpleDeinitTxRecipe) { recipe =>
            DeinitTx.build(recipe) match {
                case Left(e) =>
                    throw RuntimeException(s"DeinitTx build failed: $e")
                case Right(deinitResult) =>
                    //println(HexUtil.encodeHexString(deinitResult.tx.toCbor))
                    // Verify DeinitTx structure
                    assert(deinitResult.tx != null, "Transaction should not be null")
                    assert(deinitResult.treasuryUtxoSpent == recipe.treasuryUtxo, "Spent treasury UTXO should match recipe input")
                    ()
            }
        }
    )
}
