package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.ensureMinAda
import hydrozoa.rulebased.ledger.dapp.state.{VoteDatum, VoteState}
import io.bullet.borer.Cbor
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionUnspentOutput
import scalus.ledger.api.v1.PubKeyHash
import test.*
import test.Generators.Hydrozoa.{genScriptAddress, genTreasuryUtxo, genTxBuilderConfigAndPeers}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.Fallback

import scala.collection.immutable.SortedMap

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
        tallyFeeAllowance = Coin(1_000_000L),
        disputeTreasuryAddress = disputeTreausryAddress,
        disputeResolutionAddress = disputeResolutionAddress,
        votingDuration = 1L
    ), peers)


object FallbackTxTest extends Properties("FallbackTx") {
    property("Fallback Happy Path") = Prop.forAll(genFallbackTxRecipe) {
        (recipe, peers) => FallbackTx.build(recipe).isRight :| "successful build" && {
            val tx = FallbackTx.build(recipe).get
            val txBody = tx.tx.body.value
            val hns = recipe.config.headNativeScript
            val mulitsigRegimeTokenName = recipe.config.tokenNames.multisigRegimeTokenName
            val voteTokenName = recipe.config.tokenNames.voteTokenName


            ("treasury utxo spent" |:
                txBody.inputs.toSeq.contains(recipe.treasuryUtxo.asUtxo.input))
                && ("hmrw utxo spent" |:
                txBody.inputs.toSeq.contains(recipe.config.headNativeScriptReferenceInput.input))
                && ("multisig regime utxo spent" |:
                txBody.inputs.toSeq.contains(recipe.config.headNativeScriptReferenceInput)
                )
                && ("multisig regime token burned and vote tokens minted" |: {
                val expectedMint = Some(Mint(MultiAsset(SortedMap(
                    hns.policyId -> SortedMap(mulitsigRegimeTokenName -> 1L,
                        voteTokenName -> (peers.length.toLong + 1L))
                ))))
                expectedMint == txBody.mint
            })
                && ("rules-based treasury utxo created" |: false
                // TODO: verify this utxo has a rules-based datum and contains the correct funds
                )
                && ("default vote utxo with min ada and vote token created" |: {
                val defaultVoteUtxo = TransactionUnspentOutput(
                    TransactionInput(transactionId = tx.tx.id, index = 1),
                    Babbage(
                        address = recipe.disputeResolutionAddress,
                        value = Value(
                            Coin.zero,
                            MultiAsset(
                                SortedMap(
                                    recipe.config.headNativeScript.policyId -> SortedMap(voteTokenName -> 1)))),
                        datumOption = Some(Inline(VoteDatum.default(recipe.treasuryUtxo.datum.commit).toData)),
                        scriptRef = None
                    ).ensureMinAda(recipe.config.env.protocolParams))
                txBody.outputs(1).value == defaultVoteUtxo
            })
                && ("vote utxos created per peer" |: {
                    val pkhs = NonEmptyList.fromListUnsafe(
                        recipe.config.headNativeScript.requiredSigners.map(es => PubKeyHash(es.hash)).toList)
                    val datums = VoteDatum(pkhs)
                    val expectedNonDefaultVoteOutputs = datums.map(d =>
                        Babbage(
                            address = recipe.disputeResolutionAddress,
                            value = Value(recipe.tallyFeeAllowance, MultiAsset(SortedMap(
                              hns.policyId -> SortedMap(voteTokenName -> 1L)
                            ))),
                            datumOption = Some(Inline(d.toData)),
                            scriptRef = None
                        ).ensureMinAda(recipe.config.env.protocolParams)

                    )
                    txBody.outputs.drop(2).take(recipe.config.headNativeScript.numSigners) == expectedNonDefaultVoteOutputs

                        })
                    && ("multsig regime utxo contains exactly enough ada to cover tx fee and all non-treasury outputs" |: {
                val expectedHMRWValue : Value = Value(txBody.fee + Coin(txBody.outputs.drop(1).map(_.value.value.coin.value).sum))
                recipe.config.headNativeScriptReferenceInput.output.value == expectedHMRWValue
            })
         && ("collateral utxos created per peer" |: {
                val addrs = NonEmptyList.fromListUnsafe(
                  recipe.config.headNativeScript.requiredSigners.map(es =>
                      ShelleyAddress(network = recipe.config.env.network,
                          payment = Key(es.hash),
                          delegation = Null)).toList
                )
                val expectedCollateralUtxos = addrs.map(addr => Babbage(address = addr,
                    value = Value(recipe.tallyFeeAllowance),
                    datumOption = None,
                    scriptRef = None))
                txBody.outputs.drop(2 + recipe.config.headNativeScript.numSigners) == expectedCollateralUtxos
            })
          && ("Cbor round-tripping failed" |: {
                val bytes = tx.tx.toCbor

                given OriginalCborByteArray = OriginalCborByteArray(bytes)

                (tx.tx == Cbor
                    .decode(bytes)
                    .to[Transaction]
                    .value)
            }
                )
            && 
            {
                val actual = tx.tx.auxiliaryData.map(_.value)
                val expected =
                    MD.apply(Fallback, tx.treasuryProduced.address)
                s"Unexpected metadata value. Actual: $actual, expected: $expected" |: (actual.contains(expected) )
            }
        }  


        }
    }


