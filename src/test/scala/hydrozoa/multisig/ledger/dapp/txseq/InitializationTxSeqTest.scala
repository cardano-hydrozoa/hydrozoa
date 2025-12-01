package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
import hydrozoa.multisig.ledger.dapp.tx.Metadata.L1TxTypes.{Fallback, Initialization}
import hydrozoa.multisig.ledger.dapp.tx.{Metadata as MD, minInitTreasuryAda}
import hydrozoa.rulebased.ledger.dapp.state.VoteDatum
import hydrozoa.{ensureMinAda, maxNonPlutusTxFee}
import io.bullet.borer.Cbor
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties, Test}
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State}
import scalus.cardano.txbuilder.TransactionUnspentOutput
import scalus.ledger.api.v1.PubKeyHash
import test.*
import test.Generators.Hydrozoa.*
import test.TransactionChain.observeTxChain

// TODO: make the spentUtxos contain arbitrary assets, not just ada.
val genArgs: Gen[(InitializationTxSeq.Builder.Args, NonEmptyList[TestPeer])] =
    for {
        peers <- genTestPeers

        // We make sure that the seed utxo has at least enough for the treasury and multisig witness UTxO, plus
        // a max non-plutus fee
        seedUtxo <- genAdaOnlyPubKeyUtxo(
          peers.head,
          genCoinWithMinimum = Some(
            minInitTreasuryAda
                + Coin(maxNonPlutusTxFee(testProtocolParams).value * 2)
          )
        ).map(x => TransactionUnspentOutput(x._1, x._2))
        otherSpentUtxos <- Gen
            .listOf(genAdaOnlyPubKeyUtxo(peers.head, genCoinWithMinimum = Some(Coin(0))))
            .map(_.map(x => TransactionUnspentOutput(x._1, x._2)))

        spentUtxos = NonEmptyList(seedUtxo, otherSpentUtxos)

        // Initial deposit must be at least enough for the minAda of the treasury, and no more than the
        // sum of the seed utxos, while leaving enough left for the estimated fee and the minAda of the change
        // output
        initialDeposit <- Gen
            .choose(
              minInitTreasuryAda.value,
              sumUtxoValues(spentUtxos.toList.map(_.toTuple)).coin.value
                  - maxNonPlutusTxFee(testTxBuilderEnvironment.protocolParams).value
                  - minPubkeyAda().value
            )
            .map(Coin(_))

        disputeTreasuryPP <- genScriptAddress(testTxBuilderEnvironment.network).map(_.payment)
        disputeResolutionPP <- genScriptAddress(testTxBuilderEnvironment.network).map(_.payment)

    } yield (
      InitializationTxSeq.Builder.Args(
        spentUtxos = SpentUtxos(seedUtxo, otherSpentUtxos),
        initialDeposit = initialDeposit,
        peers = peers.map(_.wallet.exportVerificationKeyBytes),
        disputeTreasuryPaymentPart = disputeTreasuryPP,
        disputeResolutionPaymentPart = disputeResolutionPP,
        env = testTxBuilderEnvironment,
        evaluator = testEvaluator,
        validators = testValidators,
        initializationTxChangePP = Key(AddrKeyHash.fromByteString(ByteString.fill(28, 1.toByte))),
        tallyFeeAllowance = Coin.ada(2),
        votingDuration = 100
      ),
      peers
    )

object InitializationTxSeqTest extends Properties("InitializationTxSeq") {
    import Prop.forAll
    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p.withMinSuccessfulTests(10_000)
    }

    val _ = property("Initialization Tx Seq Happy Path") = forAll(genArgs) { (args, testPeers) =>
        {
            // Collect all the props in a mutable buffer, and then combine them at the end
            val props = mutable.Buffer.empty[Prop]
            val res = InitializationTxSeq.Builder.build(args)
            props.append(s"Expected succesful build, but got $res" |: res.isRight)

            // ===================================
            // General data extraction
            // ===================================
            import args.*

            val initializationTxSeq = res.get
            val iTx = initializationTxSeq.initializationTx
            val fbTx = initializationTxSeq.fallbackTx
            val fbTxBody = fbTx.tx.body.value

            val multisigTreasuryUtxo = iTx.treasuryProduced
            val multisigRegimeUtxo = iTx.resultingConfig.headNativeScriptReferenceInput
            val headTokenName = CIP67.TokenNames(spentUtxos.seedUtxo.input).headTokenName
            val mulitsigRegimeTokenName =
                CIP67.TokenNames(spentUtxos.seedUtxo.input).multisigRegimeTokenName
            val expectedHeadNativeScript =
                HeadMultisigScript(testPeers.map(_.wallet.exportVerificationKeyBytes))
            val iTxOutputs: Seq[TransactionOutput] = iTx.tx.body.value.outputs.map(_.value)
            val config = iTx.resultingConfig
            val hns = config.headNativeScript

            import config.tokenNames.*

            // ===================================
            // Initialization tx props
            // ===================================
            props.append(
              "Configured inputs are spent" |:
                  spentUtxos.all
                      .map(utxo => iTx.tx.body.value.inputs.toSeq.contains(utxo.input))
                      .reduce(_ && _)
            )

            props.append(
              "Seed input is spent" |:
                  iTx.tx.body.value.inputs.toSeq.contains(spentUtxos.seedUtxo.input)
            )

            props.append(
              "Only Treasury token and mulReg tokens minted" |:
                  iTx.tx.body.value.mint.contains(
                    Mint(
                      MultiAsset(
                        SortedMap(
                          expectedHeadNativeScript.policyId ->
                              SortedMap(headTokenName -> 1L, mulitsigRegimeTokenName -> 1L)
                        )
                      )
                    )
                  )
            )

            val expectedTreasuryIndex = iTx.treasuryProduced.asUtxo.input.index

            props.append(
              "initialization tx contains treasury output at correct index" |:
                  (iTxOutputs(expectedTreasuryIndex) == iTx.treasuryProduced.asUtxo.output)
            )

            props.append(
              "initialization tx id coherent with produced treasury output" |:
                  (iTx.tx.id == iTx.treasuryProduced.asUtxo.input.transactionId)
            )

            props.append(
              "treasury utxo only contains head token in multiassets" |:
                  (iTx.treasuryProduced.asUtxo.output.value.assets ==
                      MultiAsset(
                        SortedMap(
                          expectedHeadNativeScript.policyId -> SortedMap(headTokenName -> 1L)
                        )
                      ))
            )

            props.append(
              "treasury utxo contains at least initial deposit" |:
                  (iTx.treasuryProduced.asUtxo.output.value.coin >= initialDeposit)
            )

            props.append(
              "initialization tx contains MR output at correct index" |:
                  (iTxOutputs(multisigRegimeUtxo.input.index) ==
                      multisigRegimeUtxo.output)
            )

            props.append(
              "initialization tx id coherent with produced MR output" |:
                  (iTx.tx.id == multisigRegimeUtxo.input.transactionId)
            )

            props.append(
              "MR utxo only contains MR token in multiassets" |:
                  multisigRegimeUtxo.output.value.assets ==
                  MultiAsset(
                    SortedMap(
                      expectedHeadNativeScript.policyId -> SortedMap(
                        mulitsigRegimeTokenName -> 1L
                      )
                    )
                  )
            )

            props.append(
              "MR utxo contains at least enough coin for fallback deposit" |:
                  (multisigRegimeUtxo.output.value.coin >= maxNonPlutusTxFee(env.protocolParams))
            )

            props.append {
                val actual = iTx.tx.auxiliaryData.map(_.value)
                val expected =
                    MD.apply(Initialization, iTx.treasuryProduced.address)
                s"Unexpected metadata value. Actual: $actual, expected: $expected" |: actual
                    .contains(expected)
            }

            props.append {
                val bytes = iTx.tx.toCbor

                given OriginalCborByteArray = OriginalCborByteArray(bytes)
                given ProtocolVersion = ProtocolVersion.conwayPV

                "Cbor round-tripping failed" |: (iTx.tx == Cbor
                    .decode(bytes)
                    .to[Transaction]
                    .value)
            }

            // ===================================
            // FallbackTx Props
            // ===================================

            props.append(
              "treasury utxo spent" |: fbTxBody.inputs.toSeq.contains(
                multisigTreasuryUtxo.asUtxo.input
              )
            )

            props.append(
              "hmrw utxo spent" |: fbTxBody.inputs.toSeq.contains(multisigRegimeUtxo.input)
            )

            props.append(
              "multisig regime utxo spent" |:
                  fbTxBody.inputs.toSeq.contains(config.headNativeScriptReferenceInput.input)
            )

            props.append("multisig regime token burned and vote tokens minted" |: {
                val expectedMint = Some(
                  Mint(
                    MultiAsset(
                      SortedMap(
                        hns.policyId -> SortedMap(
                          mulitsigRegimeTokenName -> -1L,
                          voteTokenName -> (peers.length.toLong + 1L)
                        )
                      )
                    )
                  )
                )
                expectedMint == fbTxBody.mint
            })

            props.append(
              "rules-based treasury utxo has at least as much coin as multisig treasury" |: {
                  // can have more because of the max fallback fee in the multisig regime utxo
                  fbTx.treasurySpent.value.coin.value <= fbTx.treasuryProduced.output.value.coin.value
              }
            )

            props.append(
              "rules-based treasury has no more than multisig treasury " +
                  "+ extra from fallback tx fee" |:
                  fbTx.treasuryProduced.output.value.coin.value <=
                  fbTx.treasurySpent.value.coin.value + maxNonPlutusTxFee(
                    env.protocolParams
                  ).value
            )

            props.append("default vote utxo with min ada and vote token created" |: {
                val defaultVoteUtxo = TransactionUnspentOutput(
                  TransactionInput(transactionId = fbTx.tx.id, index = 1),
                  Babbage(
                    address = disputeResolutionAddress,
                    value = Value(
                      tallyFeeAllowance,
                      MultiAsset(
                        SortedMap(
                          config.headNativeScript.policyId -> SortedMap(voteTokenName -> 1)
                        )
                      )
                    ),
                    datumOption =
                        Some(Inline(VoteDatum.default(multisigTreasuryUtxo.datum.commit).toData)),
                    scriptRef = None
                  ).ensureMinAda(config.env.protocolParams)
                )
                fbTxBody.outputs(1).value == defaultVoteUtxo.output
            })

            props.append("vote utxos created per peer" |: {
                val pkhs = NonEmptyList.fromListUnsafe(
                  config.headNativeScript.requiredSigners.map(es => PubKeyHash(es.hash)).toList
                )
                val datums = VoteDatum(pkhs)
                val expectedPeerVoteOutputs = datums.map(d =>
                    Babbage(
                      address = disputeResolutionAddress,
                      value = Value(
                        tallyFeeAllowance,
                        MultiAsset(
                          SortedMap(
                            hns.policyId -> SortedMap(voteTokenName -> 1L)
                          )
                        )
                      ),
                      datumOption = Some(Inline(d.toData)),
                      scriptRef = None
                    ).ensureMinAda(config.env.protocolParams)
                )
                val actualPeerVoteOutputs = NonEmptyList.fromListUnsafe(
                  fbTxBody.outputs
                      .slice(2, config.headNativeScript.numSigners + 2)
                      .toList
                      .map(_.value)
                )
                val reportedPeerVoteOutputs = fbTx.producedPeerVoteUtxos.map(_.output)
                actualPeerVoteOutputs == reportedPeerVoteOutputs
                && expectedPeerVoteOutputs == reportedPeerVoteOutputs

            })

            props.append(
              "multsig regime utxo contains at exactly enough ada to cover tx fee and all non-treasury outputs" |: {
                  val expectedHMRWCoin: Coin =
                      maxNonPlutusTxFee(env.protocolParams)
                          + Coin(fbTxBody.outputs.drop(1).map(_.value.value.coin.value).sum)
                  config.headNativeScriptReferenceInput.output.value.coin == expectedHMRWCoin
              }
            )

            props.append("collateral utxos created per peer" |: {
                val sortedKeys = config.headNativeScript.requiredSigners
                    .map(_.hash)
                    .toList
                    .sorted(using Hash.Ordering)
                val addrs = NonEmptyList.fromListUnsafe(
                  sortedKeys
                      .map(key =>
                          ShelleyAddress(
                            network = config.env.network,
                            payment = Key(key),
                            delegation = Null
                          )
                      )
                )

                val expectedCollateralUtxos = addrs.map(addr =>
                    Babbage(
                      address = addr,
                      value = Value(tallyFeeAllowance),
                      datumOption = None,
                      scriptRef = None
                    )
                )
                val actualCollateralUtxos = NonEmptyList.fromListUnsafe(
                  fbTxBody.outputs.drop(2 + config.headNativeScript.numSigners).map(_.value).toList
                )
                val reportedCollateralUtxos = fbTx.producedCollateralUtxos.map(_.output)
                // FIXME: https://github.com/cardano-hydrozoa/hydrozoa/issues/237
                (actualCollateralUtxos == reportedCollateralUtxos)
                && (actualCollateralUtxos.toList.toSet == expectedCollateralUtxos.toList.toSet)
            })

            props.append("Cbor round-tripping failed" |: {
                val bytes = fbTx.tx.toCbor

                given OriginalCborByteArray = OriginalCborByteArray(bytes)
                given ProtocolVersion = ProtocolVersion.conwayPV

                fbTx.tx == Cbor
                    .decode(bytes)
                    .to[Transaction]
                    .value
            })

            props.append {
                val actual = fbTx.tx.auxiliaryData.map(_.value)
                val expected =
                    MD.apply(
                      Fallback,
                      fbTx.treasuryProduced.output.address.asInstanceOf[ShelleyAddress]
                    )
                s"Unexpected metadata value for fallback tx. Actual: $actual, expected: $expected" |: actual
                    .contains(expected)
            }

            // ===================================
            // Tx Seq Execution
            // ===================================

            // TODO: This whole "sign and observe" is duplicated from the settlement tx seq test. We can factor
            // it out into utils

            val initialState: State = State(utxos = iTx.resolvedUtxos.utxos)

            val signedTxs: Vector[Transaction] =
                testPeers.foldLeft(Vector(iTx.tx, fbTx.tx))((txsToSign, peer) =>
                    txsToSign.map(tx => signTx(peer, tx))
                )

            val observationRes = observeTxChain(signedTxs)(initialState, CardanoMutator, Context())

            props.append("Sequence Observation successful" |: observationRes.isRight)

            props.fold(Prop(true))(_ && _)
        }

    }
}
