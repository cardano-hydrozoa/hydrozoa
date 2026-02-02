package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.default
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.InitializationTx.SpentUtxos
import hydrozoa.multisig.ledger.dapp.tx.Metadata.{Fallback, Initialization}
import hydrozoa.multisig.ledger.dapp.tx.{InitializationTx, Metadata as MD, minInitTreasuryAda}
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.dapp.state.VoteDatum
import hydrozoa.{L1, Output, UtxoId, UtxoSetL1, ensureMinAda, maxNonPlutusTxFee, given}
import io.bullet.borer.Cbor
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties, Test}
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.concurrent.duration.{FiniteDuration, HOURS}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, State}
import scalus.ledger.api.v1.PubKeyHash
import test.*
import test.Generators.Hydrozoa.*
import test.TransactionChain.observeTxChain

object InitializationTxSeqTest extends Properties("InitializationTxSeq") {
    import Prop.forAll

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p.withMinSuccessfulTests(1_000)
    }

    /** TODO: make the spentUtxos contain arbitrary assets, not just ada.
      *
      * @param txTiming
      * @param mbUtxosAvailable
      *   When set, this limits what the generator can pick as seed/funding utxos. When None a
      *   random utxos will be used.
      */
    def genArgs(
        txTiming: TxTiming = default(testTxBuilderCardanoInfo.slotConfig),
        mbUtxosAvailable: Option[Map[TestPeer, UtxoSetL1]] = None
    ): Gen[(InitializationTxSeq.Config, InitializationTxSeq.Builder.Args, NonEmptyList[TestPeer])] =
        for {
            peers <- genTestPeers()
            prime = peers.head

            (seedUtxo, fundingUtxos) <- mbUtxosAvailable match {
                case Some(utxos) =>
                    for {
                        // Random prime peer's utxo
                        seedUtxo <- Gen
                            .oneOf(utxos(prime))
                            .map(u => Utxo(u._1.untagged, u._2.untagged))
                        // Utxos of the other peers
                        rest = utxos.view
                            .filterKeys(peers.tail.contains)
                            .values
                            .map(_.untagged)
                            .foldLeft(
                              Map.empty[UtxoId[L1], Output[L1]]
                            )((l, r) => l ++ r)
                            .toList
                        // Some random utxos
                        fundingUtxos <- Gen
                            .someOf(rest)
                            .flatMap(l => l.map(u => Utxo(u._1.untagged, u._2.untagged)).toList)
                    } yield (seedUtxo, fundingUtxos)

                case None =>
                    for {
                        // We make sure that the seed utxo has at least enough for the treasury and multisig witness UTxO, plus
                        // a max non-plutus fee
                        seedUtxo <- genAdaOnlyPubKeyUtxo(
                          prime,
                          minimumCoin = minInitTreasuryAda
                              + Coin(maxNonPlutusTxFee(testProtocolParams).value * 2)
                        ).map(x => Utxo(x._1, x._2))

                        fundingUtxos <- Gen
                            .listOf(genAdaOnlyPubKeyUtxo(prime))
                            .map(_.map(x => Utxo(x._1, x._2)))
                    } yield (seedUtxo, fundingUtxos)
            }

            spentUtxos = NonEmptyList(seedUtxo, fundingUtxos)

            // Initial treasury must be at least enough for the minAda of the treasury, and no more than the
            // sum of the seed utxos, while leaving enough left for the estimated fee and the minAda of the change
            // output
            initialTreasuryCoin <- Gen
                .choose(
                  minInitTreasuryAda.value,
                  sumUtxoValues(spentUtxos.toList).coin.value
                      - maxNonPlutusTxFee(testTxBuilderCardanoInfo.protocolParams).value
                      - minPubkeyAda().value
                )
                .map(Coin(_))

            initialTreasury = Value(initialTreasuryCoin)

            // Use [Preview.zeroTime..now] as the block zero creation time
            startTime <- Gen
                .choose(
                  java.time.Instant.ofEpochMilli(testTxBuilderCardanoInfo.slotConfig.zeroTime),
                  java.time.Instant.now()
                )
                .map(_.quantize(testTxBuilderCardanoInfo.slotConfig))

            initTxConfig = InitializationTxSeq.Config(
              tallyFeeAllowance = Coin.ada(2),
              votingDuration =
                  FiniteDuration(24, HOURS).quantize(testTxBuilderCardanoInfo.slotConfig),
              cardanoInfo = testTxBuilderCardanoInfo,
              peerKeys = peers.map(_.wallet.exportVerificationKeyBytes),
              startTime = startTime,
              txTiming = txTiming
            )

            initTxArgs = InitializationTxSeq.Builder.Args(
              spentUtxos = SpentUtxos(seedUtxo, fundingUtxos),
              initialTreasury = initialTreasury,
              initializationTxChangePP =
                  Key(AddrKeyHash.fromByteString(ByteString.fill(28, 1.toByte))),
            )

        } yield (
          initTxConfig,
          initTxArgs,
          peers
        )

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

    val _ = property("Initialization Tx Seq Happy Path") = forAll(
      genArgs(default(testTxBuilderCardanoInfo.slotConfig))
    ) { (initTxConfig, args, testPeers) =>
        {
            // Collect all the props in a mutable buffer, and then combine them at the end
            val props = mutable.Buffer.empty[Prop]
            val res = InitializationTxSeq.Builder.build(args, initTxConfig)
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
            val multisigRegimeUtxo = iTx.multisigRegimeUtxo
            val expectedHeadTokenName =
                CIP67.HeadTokenNames(spentUtxos.seedUtxo.input).headTokenName
            val expectedMulitsigRegimeTokenName =
                CIP67.HeadTokenNames(spentUtxos.seedUtxo.input).multisigRegimeTokenName
            val expectedHeadNativeScript =
                HeadMultisigScript(testPeers.map(_.wallet.exportVerificationKeyBytes))
            val iTxOutputs: Seq[TransactionOutput] = iTx.tx.body.value.outputs.map(_.value)
//                val config = Tx.Builder.Config(
//                  headMultisigScript = expectedHeadNativeScript,
//                  multisigRegimeUtxo = multisigRegimeUtxo,
//                  tokenNames = iTx.tokenNames,
//                  cardanoInfo = testTxBuilderCardanoInfo,
//                  evaluator = testEvaluator,
//                  validators = nonSigningValidators
//                )
            val hns = expectedHeadNativeScript
            val disputeResolutionAddress = ShelleyAddress(
              network = testTxBuilderCardanoInfo.network,
              payment = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash),
              delegation = Null
            )

            import iTx.tokenNames.*

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

            // TODO use >= over the whole Value
            props.append(
              "treasury utxo contains at least initial treasury" |:
                  (iTx.treasuryProduced.asUtxo.output.value.coin >= initialTreasury.coin)
            )

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
                  (multisigRegimeUtxo.output.value.coin >= maxNonPlutusTxFee(
                    testTxBuilderCardanoInfo.protocolParams
                  ))
            )

            props.append {
                val actual = iTx.tx.auxiliaryData.map(_.value)
                val expected =
                    MD.apply(
                      Initialization(
                        headAddress = iTx.treasuryProduced.address,
                        treasuryOutputIndex = 0,
                        multisigRegimeOutputIndex = 1,
                        seedInput = args.spentUtxos.seedUtxo.input
                      )
                    )
                s"Unexpected metadata value. Actual: $actual, expected: $expected" |: actual
                    .contains(expected)
            }

            // ============
            // Parsing
            // ============

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
                        headAddress =
                            expectedHeadNativeScript.mkAddress(testTxBuilderCardanoInfo.network),
                        seedInput = args.spentUtxos.seedUtxo.input,
                        treasuryOutputIndex = 0,
                        multisigRegimeOutputIndex = 1
                      )
                    )

                "Metadata parsing failed" |: (MD.parse(
                  iTx.tx.auxiliaryData
                ) == expectedMetadata)
            }

            // Semantic parsing
            props.append {
                val expectedTx: InitializationTx = InitializationTx(
                  validityEnd = iTx.validityEnd,
                  treasuryProduced = MultisigTreasuryUtxo(
                    treasuryTokenName = expectedHeadTokenName,
                    utxoId = TransactionInput(iTx.tx.id, 0),
                    address = expectedHeadNativeScript.mkAddress(testTxBuilderCardanoInfo.network),
                    datum = MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum,
                    value = initialTreasury +
                        Value(
                          Coin.zero,
                          MultiAsset(SortedMap(hns.policyId -> SortedMap(headTokenName -> 1L)))
                        )
                  ),
                  multisigRegimeUtxo = MultisigRegimeUtxo(
                    multisigRegimeTokenName = expectedMulitsigRegimeTokenName,
                    utxoId = TransactionInput(iTx.tx.id, 1),
                    address = expectedHeadNativeScript.mkAddress(testNetwork),
                    value = multisigRegimeUtxo.output.value,
                    script = expectedHeadNativeScript
                  ),
                  tokenNames = CIP67.HeadTokenNames(spentUtxos.seedUtxo.input),

                  // NOTE: resolved utxos are also self-referential
                  resolvedUtxos = iTx.resolvedUtxos,
                  // NOTE: Tx is also self-referential
                  tx = iTx.tx
                )

                def mockResolver(inputs: Seq[TransactionInput]) = iTx.resolvedUtxos

                val parseResult = InitializationTx.parse(
                  peerKeys = testPeers.map(_.wallet.exportVerificationKeyBytes),
                  cardanoInfo = testTxBuilderCardanoInfo,
                  tx = iTx.tx,
                  txTiming = initTxConfig.txTiming,
                  startTime = initTxConfig.startTime,
                  resolvedUtxos = iTx.resolvedUtxos
                )

                "Semantic transaction parsed from generic transaction in unexpected way." +
                    s"\n\n Expected parse result = ${Right(expectedTx)} \n\n, actual parse result = $parseResult" |:
                    parseResult == Right(expectedTx)
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
                  fbTxBody.inputs.toSeq.contains(iTx.multisigRegimeUtxo.input)
            )

            props.append("multisig regime token burned and vote tokens minted" |: {
                val expectedMint = Some(
                  Mint(
                    MultiAsset(
                      SortedMap(
                        hns.policyId -> SortedMap(
                          expectedMulitsigRegimeTokenName -> -1L,
                          voteTokenName -> (testPeers.length.toLong + 1L)
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
                    testTxBuilderCardanoInfo.protocolParams
                  ).value
            )

            props.append("default vote utxo with min ada and vote token created" |: {

                val defaultVoteUtxo = Utxo(
                  TransactionInput(transactionId = fbTx.tx.id, index = 1),
                  Babbage(
                    address = disputeResolutionAddress,
                    value = Value(
                      initTxConfig.tallyFeeAllowance,
                      MultiAsset(
                        SortedMap(
                          expectedHeadNativeScript.policyId -> SortedMap(voteTokenName -> 1)
                        )
                      )
                    ),
                    datumOption = Some(
                      Inline(VoteDatum.default(multisigTreasuryUtxo.datum.commit).toData)
                    ),
                    scriptRef = None
                  ).ensureMinAda(testTxBuilderCardanoInfo.protocolParams)
                )
                fbTxBody.outputs(1).value == defaultVoteUtxo.output
            })

            props.append("vote utxos created per peer" |: {
                val pkhs = NonEmptyList.fromListUnsafe(
                  expectedHeadNativeScript.requiredSigners
                      .map(es => PubKeyHash(es.hash))
                      .toList
                )
                val datums = VoteDatum(pkhs)
                val expectedPeerVoteOutputs = datums.map(d =>
                    Babbage(
                      address = disputeResolutionAddress,
                      value = Value(
                        initTxConfig.tallyFeeAllowance,
                        MultiAsset(
                          SortedMap(
                            hns.policyId -> SortedMap(voteTokenName -> 1L)
                          )
                        )
                      ),
                      datumOption = Some(Inline(d.toData)),
                      scriptRef = None
                    ).ensureMinAda(testTxBuilderCardanoInfo.protocolParams)
                )
                val actualPeerVoteOutputs = NonEmptyList.fromListUnsafe(
                  fbTxBody.outputs
                      .slice(2, expectedHeadNativeScript.numSigners + 2)
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
                      maxNonPlutusTxFee(testTxBuilderCardanoInfo.protocolParams)
                          + Coin(fbTxBody.outputs.drop(1).map(_.value.value.coin.value).sum)
                  iTx.multisigRegimeUtxo.output.value.coin == expectedHMRWCoin
              }
            )

            props.append("collateral utxos created per peer" |: {
                val sortedKeys = expectedHeadNativeScript.requiredSigners
                    .map(_.hash)
                    .toList
                    .sorted(using Hash.Ordering)
                val addrs = NonEmptyList.fromListUnsafe(
                  sortedKeys
                      .map(key =>
                          ShelleyAddress(
                            network = testTxBuilderCardanoInfo.network,
                            payment = Key(key),
                            delegation = Null
                          )
                      )
                )

                val expectedCollateralUtxos = addrs.map(addr =>
                    Babbage(
                      address = addr,
                      value = Value(initTxConfig.tallyFeeAllowance),
                      datumOption = None,
                      scriptRef = None
                    )
                )
                val actualCollateralUtxos = NonEmptyList.fromListUnsafe(
                  fbTxBody.outputs
                      .drop(2 + expectedHeadNativeScript.numSigners)
                      .map(_.value)
                      .toList
                )
                val reportedCollateralUtxos = fbTx.producedCollateralUtxos.map(_.output)
                // FIXME: https://github.com/cardano-hydrozoa/hydrozoa/issues/237
                (actualCollateralUtxos == reportedCollateralUtxos)
                && (actualCollateralUtxos.toList.toSet == expectedCollateralUtxos.toList.toSet)
            })

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
                testPeers.foldLeft(Vector(iTx.tx, fbTx.tx))((txsToSign, peer) =>
                    txsToSign.map(tx => peer.signTx(tx))
                )

            val observationRes =
                observeTxChain(signedTxs)(
                  initialState,
                  TransactionChain.ObserverMutator,
                  Context.testMainnet()
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

                val parseRes = InitializationTxSeq.parse(
                  txSeq,
                  peerKeys = testPeers.map(_.wallet.exportVerificationKeyBytes),
                  tallyFeeAllowance = initTxConfig.tallyFeeAllowance,
                  votingDuration = initTxConfig.votingDuration,
                  cardanoInfo = testTxBuilderCardanoInfo,
                  resolvedUtxos = iTx.resolvedUtxos,
                  startTime = initTxConfig.startTime,
                  txTiming = TxTiming.default(testTxBuilderCardanoInfo.slotConfig)
                )

                s"InitializationTxSeq should parse successfully $parseRes" |: parseRes.isRight
            }

            props.fold(Prop(true))(_ && _)
        }

    }
}
