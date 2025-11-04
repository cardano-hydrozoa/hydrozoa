package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import test.*
import test.Generators.Hydrozoa.{genScriptAddress, genTreasuryUtxo, genTxBuilderConfigAndPeers}

//// The minimum ada required for the initial treasury utxo
//val minInitTreasuryAda: Coin = {
//    val mockTreasury = Babbage(
//        // This is a pubkey address, but the script address should be the same size
//        address = genPubkeyAddress().sample.get,
//        value = Value(
//            Coin(0L),
//            assets = MultiAsset(
//                SortedMap(
//                    (
//                        genPolicyId.sample.get,
//                        SortedMap(
//                            (CIP67.TokenNames(genAdaOnlyPubKeyUtxo(Alice).sample.get._1).headTokenName, 1L)
//                        )
//                    )
//                )
//            )
//        ),
//        datumOption = Some(Inline(TreasuryUtxo.mkInitMultisigTreasuryDatum.toData)),
//        scriptRef = None
//    )
//    ensureMinAda(mockTreasury, blockfrost544Params).value.coin
//}

val genFallbackTxRecipe: Gen[(FallbackTx.Recipe, NonEmptyList[TestPeer])] =
    for {
        (config, peers) <- genTxBuilderConfigAndPeers()
        treasuryUtxo <- genTreasuryUtxo(config)
        disputeTreausryAddress <- genScriptAddress(config.env.network)
        disputeResolutionAddress <- genScriptAddress(config.env.network)

//        // We make sure that the seed utxo has at least enough for the treasury and multisig witness UTxO, plus
//        // a max non-plutus fee
//        seedUtxo <- genAdaOnlyPubKeyUtxo(peers.head, genCoinWithMinimum = Some(minInitTreasuryAda
//            + Coin(maxNonPlutusTxFee(testProtocolParams).value * 2)))
//        otherSpentUtxos <- Gen
//            .listOf(genAdaOnlyPubKeyUtxo(peers.head, genCoinWithMinimum = Some(Coin(0))))
//
//        spentUtxos = NonEmptyList(seedUtxo, otherSpentUtxos)
//
//        // Initial deposit must be at least enough for the minAda of the treasury, and no more than the
//        // sum of the seed utxos, while leaving enough left for the estimated fee and the minAda of the change
//        // output
//        initialDeposit <- Gen.choose(
//            minInitTreasuryAda.value,
//            sumUtxoValues(spentUtxos.toList).coin.value
//                - maxNonPlutusTxFee(testTxBuilderEnvironment.protocolParams).value
//                - minPubkeyAda().value
//        ).map(Coin(_))

    }
    yield (FallbackTx.Recipe(
      config = config,
        treasuryUtxo = treasuryUtxo,
        disputeTreasuryAddress = disputeTreausryAddress,
        disputeResolutionAddress = disputeResolutionAddress,
        votingDuration = 1L
    ), peers)


object FallbackTxTest extends Properties("FallbackTx") {
    property("Fallback Happy Path") = Prop.forAll(genFallbackTxRecipe) {
        (recipe, peers) => FallbackTx.build(recipe).isRight :| "successful build" && {
            val tx = FallbackTx.build(recipe).get
            val hns = recipe.config.headNativeScript
            
            "treasury utxo spent" |: tx.tx.body.value.inputs.toSeq.contains(recipe.treasuryUtxo.asUtxo.input)
        }
    }
}

