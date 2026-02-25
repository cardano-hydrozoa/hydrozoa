package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.config.head.HeadPeersSpec.Exact
import hydrozoa.config.head.generateHeadConfigPreInit
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.config.head.peers.generateTestPeers
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.Metadata.{Fallback, Initialization}
import hydrozoa.multisig.ledger.dapp.tx.{InitializationTx, Metadata as MD}
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.dapp.state.VoteDatum
import io.bullet.borer.Cbor
import org.scalacheck.Prop.propBoolean
import org.scalacheck.rng.Seed
import org.scalacheck.{Prop, Properties, Test}
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.Data.toData
import test.*
import test.TransactionChain.observeTxChain

object InitializationTxSeqTest extends Properties("InitializationTxSeq") {
    import Prop.forAll

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p.withMinSuccessfulTests(100)
            .withInitialSeed(Seed.fromBase64("2ilMfqvqb3K1t4SElxC9HcP1MG6stB521QKTt0zoEND=").get)
    }

    // NOTE (Peter, 2025-11-28): These properties primarily test the built transaction with coherence against
    // the "semantic" InitializationTx value produced.
    //
    // This is an important property: the actual cardano transaction that gets submitted must correspond to
    // the semantic tx we deal with internally. However, it is not the only property: we should also be checking
    // that the semantic transaction reflects what we expect, i.e., we should perform "black-box testing" to
    // ensure that the multisig regime utxo is actually carrying the correct script, with the correct value, and
    // sitting at the correct address.
    //
    // I do some of this during the parsing. But in general, it's a tedious process to ensure that
    //     intention <=> cardano transaction <=> semantic transaction
    // are all coherent in every possible way.
    //
    // All this to say: this test should not be considered exhaustive at this time. It's just here to give us
    // a reasonable level of confidence that this won't fall over the first time we run it.
    val _ = property("Initialization Tx Seq Happy Path") = forAll(generateTestPeers())(testPeers =>
        forAll(generateHeadConfigPreInit(Exact(testPeers._testPeers.length))()) { config =>
            {
                // TODO (Peter, 2026-02-16): This was originally written with the "props.append" pattern before we had
                //   better alternatives solidified. This as some downsides for readability, especially when failure
                //   happen. We could rewrite in EitherT, PropertyM, or PropertyBuilder

                // Collect all the props in a mutable buffer, and then combine them at the end
                val props = mutable.Buffer.empty[Prop]
                val res = InitializationTxSeq.Build(config).result
                props.append(s"Expected successful build, but got $res" |: res.isRight)

                // ===================================
                // General data extraction
                // ===================================
                import config.*

                // TODO: partial match; see above comment
                val Right(initializationTxSeq) = res
                val iTx = initializationTxSeq.initializationTx
                val fbTx = initializationTxSeq.fallbackTx
                val fbTxBody = fbTx.tx.body.value

                val multisigTreasuryUtxo = iTx.treasuryProduced
                val multisigRegimeUtxo = iTx.multisigRegimeProduced
                val expectedHeadTokenName =
                    CIP67.HeadTokenNames(config.initialSeedUtxo.input).treasuryTokenName
                val expectedMulitsigRegimeTokenName =
                    CIP67.HeadTokenNames(config.initialSeedUtxo.input).multisigRegimeTokenName
                val expectedHeadNativeScript = config.headMultisigScript
                val iTxOutputs: Seq[TransactionOutput] = iTx.tx.body.value.outputs.map(_.value)
                val hns = expectedHeadNativeScript
                val disputeResolutionAddress = ShelleyAddress(
                  network = config.network,
                  payment = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash),
                  delegation = Null
                )

                // ===================================
                // Initialization tx props
                // ===================================
                props.append(
                  "Configured inputs are spent" |:
                      (config.initialFundingUtxos + config.initialSeedUtxo.toTuple)
                          .map(utxo => iTx.tx.body.value.inputs.toSeq.contains(utxo._1))
                          .reduce(_ && _)
                )

                props.append(
                  "Seed input is spent" |:
                      iTx.tx.body.value.inputs.toSeq.contains(config.initialSeedUtxo.input)
                )

                props.append(
                  "Only Treasury token and mulReg tokens minted" |:
                      iTx.tx.body.value.mint.contains(
                        Mint(
                          MultiAsset(
                            SortedMap(
                              expectedHeadNativeScript.policyId ->
                                  SortedMap(
                                    expectedHeadTokenName -> 1L,
                                    expectedMulitsigRegimeTokenName -> 1L
                                  )
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
                  "initialization tx treasury output is out index 0" |:
                      expectedTreasuryIndex == 0
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
                              expectedHeadNativeScript.policyId -> SortedMap(
                                expectedHeadTokenName -> 1L
                              )
                            )
                          ))
                )

                // FIXME (2025-02-16): Prior to refacotring the configuration, we used to
                // pay the initialization tx fee exogenouly. Now it is paid out of the equity (just how
                // all other fees are paid). So this property isn't quite the same -- now the actual
                // treasury produced might have less, or always has less? I'm not sure, we should take another look.

//            // TODO use >= over the whole Value
//            props.append(
//              "treasury utxo contains at least initial treasury" |:
//                  (iTx.treasuryProduced.asUtxo.output.value.coin >= initialTreasury.coin)
//            )

                props.append(
                  "initialization tx contains MR output at correct index" |:
                      (iTxOutputs(multisigRegimeUtxo.input.index) ==
                          multisigRegimeUtxo.output) && multisigRegimeUtxo.input.index == 1
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
                            expectedMulitsigRegimeTokenName -> 1L
                          )
                        )
                      )
                )

                props.append(
                  "MR utxo contains at least enough coin for fallback deposit" |:
                      (multisigRegimeUtxo.output.value.coin >=
                          config.maxNonPlutusTxFee)
                )

                props.append {
                    val actual = iTx.tx.auxiliaryData.map(_.value)
                    val expected =
                        MD.apply(
                          Initialization(
                            headAddress = iTx.treasuryProduced.address,
                            treasuryOutputIndex = 0,
                            multisigRegimeOutputIndex = 1,
                            seedInput = config.initialSeedUtxo.input
                          )
                        )
                    s"Unexpected metadata value. Actual: $actual, expected: $expected" |: actual
                        .contains(expected)
                }

                // ============
                // Parsing
                // ============

                given ProtocolVersion = config.cardanoProtocolVersion

                // Cbor
                props.append {
                    val bytes = iTx.tx.toCbor

                    given OriginalCborByteArray = OriginalCborByteArray(bytes)

                    "Cbor round-tripping failed" |: (iTx.tx == Cbor
                        .decode(bytes)
                        .to[Transaction]
                        .value)
                }

                // Metadata
                props.append {
                    val expectedMetadata =
                        Right(
                          MD.Initialization(
                            headAddress = expectedHeadNativeScript.mkAddress(config.network),
                            seedInput = config.initialSeedUtxo.input,
                            treasuryOutputIndex = 0,
                            multisigRegimeOutputIndex = 1
                          )
                        )

                    "Metadata parsing failed" |: (MD.parse(
                      iTx.tx.auxiliaryData
                    ) == expectedMetadata)
                }

                // FIXME (2026-02-16): after the configuration rework, the initialization tx fee is
                // paid from the treasury. This means that its a bit more difficult to determine the
                // expected ADA in the treasury utxo, and the test below needs to be rewritten --
                // probably after issue #338 is complete.

                // Semantic parsing
//            props.append {
//                val expectedTx: InitializationTx = InitializationTx(
//                  validityEnd = iTx.validityEnd,
//                  treasuryProduced = MultisigTreasuryUtxo(
//                    treasuryTokenName = expectedHeadTokenName,
//                    utxoId = TransactionInput(iTx.tx.id, 0),
//                    address = expectedHeadNativeScript.mkAddress(testTxBuilderCardanoInfo.network),
//                    datum = MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum,
//                    value = initialTreasury +
//                        Value(
//                          Coin.zero,
//                          MultiAsset(SortedMap(hns.policyId -> SortedMap(treasuryTokenName -> 1L)))
//                        )
//                  ),
//                  multisigRegimeUtxo = MultisigRegimeUtxo(
//                    multisigRegimeTokenName = expectedMulitsigRegimeTokenName,
//                    utxoId = TransactionInput(iTx.tx.id, 1),
//                    address = expectedHeadNativeScript.mkAddress(testNetwork),
//                    value = multisigRegimeUtxo.output.value,
//                    script = expectedHeadNativeScript
//                  ),
//                  tokenNames = CIP67.HeadTokenNames(spentUtxos.seedUtxo.input),
//
//                  // NOTE: resolved utxos are also self-referential
//                  resolvedUtxos = iTx.resolvedUtxos,
//                  // NOTE: Tx is also self-referential
//                  tx = iTx.tx
//                )
//
//                def mockResolver(inputs: Seq[TransactionInput]) = iTx.resolvedUtxos
//
//                val parseResult = InitializationTx.parse(
//                  peerKeys = testPeers.map(_.wallet.exportVerificationKeyBytes),
//                  cardanoInfo = testTxBuilderCardanoInfo,
//                  tx = iTx.tx,
//                  txTiming = initTxConfig.txTiming,
//                  startTime = initTxConfig.startTime,
//                  resolvedUtxos = iTx.resolvedUtxos
//                )
//
//                "Semantic transaction parsed from generic transaction in unexpected way." +
//                    s"\n\n Expected parse result = ${Right(expectedTx)} \n\n, actual parse result = $parseResult" |:
//                    parseResult == Right(expectedTx)
//            }

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
                      fbTxBody.inputs.toSeq.contains(iTx.multisigRegimeProduced.input)
                )

                props.append("multisig regime token burned and vote tokens minted" |: {
                    val expectedMint = Some(
                      Mint(
                        MultiAsset(
                          SortedMap(
                            hns.policyId -> SortedMap(
                              expectedMulitsigRegimeTokenName -> -1L,
                              config.headTokenNames.voteTokenName -> (config.headPeerIds.length.toLong + 1L)
                            )
                          )
                        )
                      )
                    )
                    expectedMint == fbTxBody.mint
                })

                props.append(
                  "rules-based treasury utxo has at least as much coin as multisig treasury (minus equity)" |: {
                      // can have more because of the max fallback fee in the multisig regime utxo
                      fbTx.treasurySpent.value.coin.value - fbTx.treasurySpent.equity.coin.value
                          <= fbTx.treasuryProduced.output.value.coin.value
                  }
                )

                props.append(
                  "rules-based treasury has no more than multisig treasury " +
                      "+ extra from fallback tx fee" |:
                      fbTx.treasuryProduced.output.value.coin.value <=
                      fbTx.treasurySpent.value.coin.value + config.maxNonPlutusTxFee.value
                )

                props.append(
                  "default vote utxo with default vote deposit and vote token created" |: {

                      val defaultVoteUtxo = Utxo(
                        TransactionInput(transactionId = fbTx.tx.id, index = 1),
                        Babbage(
                          address = disputeResolutionAddress,
                          value = Value(
                            config.headParams.fallbackContingency.collectiveContingency.defaultVoteDeposit,
                            MultiAsset(
                              SortedMap(
                                expectedHeadNativeScript.policyId -> SortedMap(
                                  config.headTokenNames.voteTokenName -> 1
                                )
                              )
                            )
                          ),
                          datumOption = Some(
                            Inline(VoteDatum.default(multisigTreasuryUtxo.datum.commit).toData)
                          ),
                          scriptRef = None
                        ).ensureMinAda(config)
                      )
                      fbTxBody.outputs.last.value == defaultVoteUtxo.output
                  }
                )

                props.append("vote utxos created per peer" |: {
                    val pkhs =
                        config.headPeers.headPeerVKeys.map(vkey => PubKeyHash(blake2b_224(vkey)))

                    val datums = VoteDatum(pkhs)
                    val expectedPeerVoteOutputs = datums.map(d =>
                        Babbage(
                          address = disputeResolutionAddress,
                          value = Value(
                            config.individualContingency.forVoteUtxo,
                            MultiAsset(
                              SortedMap(
                                hns.policyId -> SortedMap(config.headTokenNames.voteTokenName -> 1L)
                              )
                            )
                          ),
                          datumOption = Some(Inline(d.toData)),
                          scriptRef = None
                        ).ensureMinAda(config.cardanoNetwork)
                    )
                    // Order should be:
                    // - Treasury (1)
                    // - Collaterals (n)
                    // - Peer Votes (n)
                    // - Default Vote (1)
                    val actualPeerVoteOutputs = NonEmptyList.fromListUnsafe(
                      fbTxBody.outputs
                          .slice(
                            1 + expectedHeadNativeScript.numSigners,
                            1 + (expectedHeadNativeScript.numSigners * 2)
                          )
                          .toList
                          .map(_.value)
                    )
                    val reportedPeerVoteOutputs = fbTx.peerVoteUtxosProduced.map(_.output)
                    actualPeerVoteOutputs == reportedPeerVoteOutputs
                    && expectedPeerVoteOutputs == reportedPeerVoteOutputs

                })

//            props.append(
//                "multsig regime utxo contains at exactly enough ada to cover tx fee and all non-treasury outputs" |: {
//                    val expectedHMRWCoin: Coin =
//                        config.maxNonPlutusTxFee
//                            + Coin(fbTxBody.outputs.drop(1).map(_.value.value.coin.value).sum)
//                    iTx.multisigRegimeProduced.output.value.coin == expectedHMRWCoin
//                }
//            )

                props.append(
                  "multsig regime utxo contains at exactly enough ada to cover tx fee and all non-treasury outputs" |: {
                      val expectedHMRWCoin: Coin = config.totalFallbackContingency
                      iTx.multisigRegimeProduced.output.value.coin == expectedHMRWCoin
                  }
                )

                // FIXME: depends on issue #338
//            props.append("collateral utxos created per peer" |: {
//                val sortedKeys = expectedHeadNativeScript.requiredSigners
//                    .map(_.hash)
//                    .toList
//                    .sorted(using Hash.Ordering)
//                val addrs = NonEmptyList.fromListUnsafe(
//                  sortedKeys
//                      .map(key =>
//                          ShelleyAddress(
//                            network = config.network,
//                            payment = Key(key),
//                            delegation = Null
//                          )
//                      )
//                )
//
//                val expectedCollateralUtxos = addrs.map(addr =>
//                    Babbage(
//                      address = addr,
//                      value = Value(config.fallbackContingency.individualContingency.collateralUtxo) +
//                          config.distributeEquity(fbTx.treasurySpent.equity),
//                      datumOption = None,
//                      scriptRef = None
//                    )
//                )
//                val actualCollateralUtxos = NonEmptyList.fromListUnsafe(
//                  fbTxBody.outputs
//                      .drop(2 + expectedHeadNativeScript.numSigners)
//                      .map(_.value)
//                      .toList
//                )
//                val reportedCollateralUtxos = fbTx.producedCollateralUtxos.map(_.output)
//                // FIXME: https://github.com/cardano-hydrozoa/hydrozoa/issues/237
//                (actualCollateralUtxos == reportedCollateralUtxos)
//                && (actualCollateralUtxos.toList.toSet == expectedCollateralUtxos.toList.toSet)
//            })

                props.append("Cbor round-tripping failed" |: {
                    val bytes = fbTx.tx.toCbor

                    given OriginalCborByteArray = OriginalCborByteArray(bytes)

                    fbTx.tx == Cbor
                        .decode(bytes)
                        .to[Transaction]
                        .value
                })

                props.append {
                    val actual = fbTx.tx.auxiliaryData.map(_.value)
                    val expected =
                        MD.apply(
                          Fallback(
                            fbTx.treasuryProduced.output.address.asInstanceOf[ShelleyAddress]
                          )
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
                    testPeers._testPeers.foldLeft(Vector(iTx.tx, fbTx.tx))((txsToSign, peer) =>
                        txsToSign.map(tx => peer._2.signTx(tx))
                    )

                val observationRes =
                    observeTxChain(signedTxs)(
                      initialState,
                      TransactionChain.ObserverMutator,
                      Context(
                        fee = Coin.zero,
                        env = UtxoEnv(
                          slot = 0,
                          params = config.cardanoProtocolParams,
                          certState = CertState.empty,
                          network = config.network
                        ),
                        slotConfig = config.slotConfig,
                        evaluatorMode = EvaluateAndComputeCost
                      )
                    )

                props.append {
                    observationRes match {
                        case Left(e)  => s"Sequence Observation unsuccessful $e" |: false
                        case Right(_) => Prop(true)
                    }
                }

                // Semantic parsing of entire sequence
                props.append {
                    val txSeq = (iTx.tx, fbTx.tx)

                    val parseRes = InitializationTxSeq
                        .Parse(config)(
                          txSeq,
                          resolvedUtxos = iTx.resolvedUtxos,
                        )
                        .result

                    s"InitializationTxSeq should parse successfully $parseRes" |: parseRes.isRight
                }

                props.fold(Prop(true))(_ && _)
            }

        }
    )
}
