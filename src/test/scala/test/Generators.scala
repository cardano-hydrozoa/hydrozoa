package test

import cats.data.{NonEmptyList, NonEmptyVector}
import hydrozoa.multisig.ledger.VirtualLedger
import hydrozoa.multisig.ledger.VirtualLedger.{Config, State}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.{GenesisObligation, L2EventTransaction}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.genShelleyAddress
import monocle.*
import monocle.syntax.all.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scala.collection.immutable.SortedMap
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{*, given}
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.cardano.txbuilder.{Environment, TransactionUnspentOutput}
import scalus.prelude.Option as SOption
import scalus.|>

/** This module contains shared generators and arbitrary instances that may be shared among multiple
  * tests. We separate them into "Hydrozoa" and "Other" objects for ease of upstreaming.
  */

object Generators {

    /** NOTE: generators here are opinionated. They are not directly suitable for upstreaming and
      * contain reasonable, hydrozoa-specific defaults.
      */
    object Hydrozoa {
        // ===================================
        // Generators
        // ===================================
        /** Generate random bytestring data. Good for testing user-provided, untrusted data against
          * size attacks
          */
        def genByteStringData: Gen[Data] =
            Gen.sized(size => genByteStringOfN(size).flatMap(_.toData))

        /** Generate an inline datum with random bytestring data. Optionally, set the relative
          * frequencies for an empty datum
          */
        def genByteStringInlineDatumOption(
            noneFrequency: Int = 0,
            someFrequency: Int = 1
        ): Gen[SOption[DatumOption]] =
            Gen.frequency(
              (someFrequency, genByteStringData.map(data => SOption.Some(Inline(data)))),
              (noneFrequency, SOption.None)
            )

        /** Generates the general configuration for a hydrozoa (non-init) transaction, and returns
          * the set of peers to use for signing.
          */
        def genTxBuilderConfigAndPeers(
            env: Environment = testTxBuilderEnvironment,
            evaluator: PlutusScriptEvaluator = testEvaluator,
            validators: Seq[Validator] = nonSigningValidators
        ): Gen[(Tx.Builder.Config, NonEmptyList[TestPeer])] =
            for {
                peers <- genTestPeers
                hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
                seedUtxo <- arbitrary[TransactionInput]
                tokenNames = TokenNames(seedUtxo)
                multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(
                  hns,
                  env.network,
                  Some(tokenNames.multisigRegimeTokenName)
                )
            } yield (
              Tx.Builder.Config(
                headNativeScript = hns,
                headNativeScriptReferenceInput = multisigWitnessUtxo,
                tokenNames = tokenNames,
                env = env,
                evaluator = evaluator,
                validators = validators
              ),
              peers
            )

        def genTxConfig(
            env: Environment = testTxBuilderEnvironment,
            evaluator: PlutusScriptEvaluator = testEvaluator,
            validators: Seq[Validator] = nonSigningValidators
        ): Gen[Tx.Builder.Config] = genTxBuilderConfigAndPeers(env, evaluator, validators).map(_._1)

        val genHeadTokenName: Gen[AssetName] =
            for {
                ti <- arbitrary[TransactionInput]
            } yield CIP67.TokenNames(ti).headTokenName

        val genTreasuryDatum: Gen[TreasuryUtxo.Datum] = {
            for {
                mv <- Gen.posNum[BigInt]
                // Verify that this is the correct length!
                kzg <- genByteStringOfN(32)
                paramsHash <- genByteStringOfN(32)

            } yield TreasuryUtxo.Datum(commit = kzg, versionMajor = mv, paramsHash = paramsHash)
        }

        /** Generate a positive Ada value */
        val genAdaOnlyValue: Gen[Value] =
            for {
                coin <- arbitrary[Coin]
            } yield Value(coin)

        val genAddrKeyHash: Gen[AddrKeyHash] =
            genByteStringOfN(28).map(AddrKeyHash.fromByteString)

        val genScriptHash: Gen[ScriptHash] = genByteStringOfN(28).map(ScriptHash.fromByteString)

        val genPolicyId: Gen[PolicyId] = genScriptHash

        def genPubkeyAddress(
            network: Network = testNetwork,
            delegation: ShelleyDelegationPart = ShelleyDelegationPart.Null
        ): Gen[ShelleyAddress] =
            genAddrKeyHash.flatMap(akh =>
                ShelleyAddress(network = network, payment = Key(akh), delegation = delegation)
            )

        def genScriptAddress(
            network: Network = testNetwork,
            delegation: ShelleyDelegationPart = ShelleyDelegationPart.Null
        ): Gen[ShelleyAddress] =
            for {
                sh <- genScriptHash
            } yield ShelleyAddress(
              network = network,
              payment = ShelleyPaymentPart.Script(sh),
              delegation = delegation
            )

        def genFakeMultisigWitnessUtxo(
            script: HeadMultisigScript,
            network: Network,
            // Pass this if you need a specific token name for coherence with the rest of your test.
            // In general, it should be obtained via `CIP67.TokenNames(seedUtxo).multisigRegimeTokenName
            hmrwTokenName: Option[AssetName] = None
        ): Gen[Utxo] = for {
            utxoId <- Arbitrary.arbitrary[TransactionInput]

            hmrwTn <- hmrwTokenName match {
                case None =>
                    arbitrary[TransactionInput].flatMap(ti =>
                        CIP67.TokenNames(ti).multisigRegimeTokenName
                    )
                case Some(n) => Gen.const(n)
            }
            hmrwToken = Value(
              Coin.zero,
              MultiAsset(
                SortedMap(
                  script.policyId -> SortedMap(
                    hmrwTn -> 1L
                  )
                )
              )
            )

            output = Babbage(
              script.mkAddress(network),
              Value.ada(2) + hmrwToken,
              None,
              Some(ScriptRef.apply(script.script))
            )
        } yield TransactionUnspentOutput((utxoId, output))

        def genPayoutObligation(network: Network): Gen[Payout.Obligation] =
            for {
                l2Input <- arbitrary[TransactionInput]

                address0 <- arbitrary[ShelleyAddress]
                address = address0.copy(network = network)
                coin <- arbitrary[Coin]
                datum <- arbitrary[ByteString]
                output = Babbage(
                  address = address,
                  value = Value(coin),
                  datumOption = Some(Inline(datum.toData)),
                  scriptRef = None
                )
            } yield Payout.Obligation(l2UtxoId = l2Input, utxo = output)

        /** Ada-only pub key utxo from the given peer, at least minAda, random tx id, random index,
          * no datum, no script ref
          */
        // TODO: make this take all fields as Option and default to generation if None.
        def genAdaOnlyPubKeyUtxo(
            peer: TestPeer,
            params: ProtocolParams = blockfrost544Params,
            network: Network = testNetwork,
            minimumCoin: Coin = Coin.zero,
            datumGenerator: Option[Gen[Option[DatumOption]]] = None
        ): Gen[Utxo] =
            for {
                txId <- arbitrary[TransactionInput]
                txOutput <- genAdaOnlyBabbageOutput(
                  peer,
                  params,
                  network,
                  minimumCoin,
                  datumGenerator
                )
            } yield Utxo(txId, txOutput)

        /** @param peer
          *   The test peer who's PKH this output will be at
          * @param network
          *   The network of the address, defaults to Testnet
          * @param params
          *   The protocol params, defaults to testProtocolParams
          * @param minimumCoin
          *   an optional minimum coin. Should be positive, defaults to 0
          * @param datumGenerator
          *   an Optional datum generator. Will generate an empty datum if not passed
          * @return
          */
        def genAdaOnlyBabbageOutput(
            peer: TestPeer,
            params: ProtocolParams = testProtocolParams,
            network: Network = testNetwork,
            minimumCoin: Coin = Coin.zero,
            datumGenerator: Option[Gen[Option[DatumOption]]] = None
        ): Gen[Babbage] =
            for {
                value <- genAdaOnlyValue
                coin <- arbitrary[Coin].map(_ + minimumCoin)

                datum <- datumGenerator match {
                    case None      => Gen.const(None)
                    case Some(gen) => gen
                }
                txOutput: TransactionOutput.Babbage = ensureMinAda(
                  Babbage(
                    address = peer.address(network),
                    value = Value(coin),
                    datumOption = datum,
                    scriptRef = None
                  ),
                  params
                ).asInstanceOf[Babbage].focus(_.value).modify(_ + value)
            } yield txOutput

        // Has duplication with genAdaOnlyBabbageOutput
        def genGenesisObligation(
            peer: TestPeer,
            params: ProtocolParams = testProtocolParams,
            network: Network = testNetwork,
            minimumCoin: Coin = Coin.zero,
            datumGenerator: Option[Gen[Option[Inline]]] = None
        ): Gen[GenesisObligation] =
            for {
                value <- genAdaOnlyValue
                coin <- arbitrary[Coin].map(_ + minimumCoin)

                datum <- datumGenerator match {
                    case None      => Gen.const(None)
                    case Some(gen) => gen
                }
                txOutput: TransactionOutput.Babbage = ensureMinAda(
                  Babbage(
                    address = peer.address(network),
                    value = Value(coin),
                    datumOption = datum,
                    scriptRef = None
                  ),
                  params
                ).asInstanceOf[Babbage].focus(_.value).modify(_ + value)

                genesisObligation = GenesisObligation(
                  l2OutputPaymentAddress = peer.address(network).payment,
                  l2OutputNetwork = network,
                  l2OutputDatum = datum match {
                      case None    => SOption.None
                      case Some(d) => SOption.Some(d.data)
                  },
                  l2OutputValue = txOutput.value.coin,
                  l2OutputRefScript = None
                )

            } yield genesisObligation

        /** Generate a treasury utxo with at least minAda */
        def genTreasuryUtxo(
            network: Network = testNetwork,
            params: ProtocolParams = blockfrost544Params,
            headAddress: Option[ShelleyAddress],
            coin: Option[Coin] = None
        ): Gen[TreasuryUtxo] =
            for {
                txId <- arbitrary[TransactionInput]
                headTn <- genHeadTokenName

                scriptAddress = headAddress.getOrElse {
                    ShelleyAddress(
                      network,
                      ShelleyPaymentPart.Script(genScriptHash.sample.get),
                      Null
                    )
                }
                datum <- genTreasuryDatum

                treasuryToken: Value = Value(
                  Coin.zero,
                  MultiAsset(
                    SortedMap(
                      scriptAddress.payment.asInstanceOf[ShelleyPaymentPart.Script].hash ->
                          SortedMap(headTn -> 1L)
                    )
                  )
                )

                treasuryMinAda = ensureMinAda(
                  TreasuryUtxo(
                    treasuryTokenName = headTn,
                    utxoId = txId,
                    address = scriptAddress,
                    datum = datum,
                    value = Value(Coin(0L)) + treasuryToken
                  ).asUtxo._2,
                  params
                ).value.coin

                treasuryAda <- arbitrary[Coin].map(l => l - Coin(1L) + treasuryMinAda)

            } yield TreasuryUtxo(
              treasuryTokenName = headTn,
              utxoId = txId,
              datum = datum,
              address = scriptAddress,
              value = Value(coin.getOrElse(treasuryAda)) + treasuryToken
            )

        /** Generate a treasury utxo according to a builder config */
        def genTreasuryUtxo(config: Tx.Builder.Config): Gen[TreasuryUtxo] =
            genTreasuryUtxo(
              network = config.env.network,
              params = config.env.protocolParams,
              headAddress = Some(config.headAddress),
              coin = None
            )

        /** Given a set of inputs event, construct a withdrawal event attempting to withdraw all
          * inputs with the given key to a single output
          */
        def genL2WithdrawalFromUtxosAndPeer(
            inputUtxos: Utxos,
            peer: TestPeer
        ): Gen[L2EventTransaction] =
            for {
                addr <- genShelleyAddress

                inputValue: Value = inputUtxos.values.foldLeft(Value.zero)((acc, output) => {
                    acc + output.value
                })

                output = Babbage(addr, inputValue, None, None)

                txBody: TransactionBody = TransactionBody(
                  inputs = TaggedSortedSet.from(inputUtxos.keySet),
                  outputs = IndexedSeq(Sized(output)),
                  fee = Coin(0L)
                )

                txUnsigned: Transaction =
                    Transaction(
                      body = KeepRaw(txBody),
                      witnessSet = TransactionWitnessSet.empty,
                      isValid = true,
                      auxiliaryData = Some(
                        KeepRaw(
                          Metadata(
                            Map(
                              Word64(CIP67.Tags.head)
                                  -> Metadatum.List(IndexedSeq(Metadatum.Int(1)))
                            )
                          )
                        )
                      )
                    )

            } yield L2EventTransaction(signTx(peer, txUnsigned))

        /** Generate an "attack" that, given a context, state, and L2EventTransaction, returns a
          * tuple containing:
          *   - a mutated L2EventTransaction in such a way that a given ledger rule will be
          *     violated.
          *   - the expected error to be raised from the L2 ledger STS when the mutated transaction
          *     is applied.
          *
          * Note that, at this time, only one such attack can be applied at time; applying multiple
          * attacks and observing the exception would require using `Validation` rather than
          * `Either`, and probably some threading through of the various mutations to determine the
          * actual context of the errors raised.
          */
        def genL2EventTransactionAttack: Gen[
          (VirtualLedger.Config, State, L2EventTransaction) => (
              L2EventTransaction,
              (String | TransactionException)
          )
        ] = {

            // Violates "AllInputsMustBeInUtxoValidator" ledger rule
            def inputsNotInUtxoAttack: (VirtualLedger.Config, State, L2EventTransaction) => (
                L2EventTransaction,
                (String | TransactionException)
            ) =
                (context, state, transaction) => {
                    // Generate a random TxId that is _not_ present in the state
                    val bogusInputId: TransactionHash = Hash(
                      genByteStringOfN(32)
                          .suchThat(txId =>
                              !state.activeUtxos.toSeq
                                  .map(_._1.transactionId.bytes)
                                  .contains(txId.bytes)
                          )
                          .sample
                          .get
                    )

                    val bogusTxIn = TransactionInput(transactionId = bogusInputId, index = 0)

                    val newTx: L2EventTransaction = {
                        val underlyingOriginal = transaction.transaction
                        val underlyingModified = underlyingOriginal
                            |>
                                // First focus on the inputs of the transaction
                                Focus[Transaction](_.body)
                                    .andThen(KeepRaw.lens[TransactionBody]())
                                    .refocus(_.inputs)
                                    // then modify those inputs: the goal is to replace the txId of one input with
                                    // our bogusInputId
                                    .modify(x =>
                                        TaggedSortedSet.from(
                                          // Inputs come as set, and I don't think monocle can `_.index(n)` a set,
                                          // so we convert to and from List
                                          x.toSet.toList
                                              // Focus on the first element of the list, and...
                                              .focus(_.index(0))
                                              // replace its transactionId with our bogus txId
                                              .replace(bogusTxIn)
                                        )
                                    )

                        L2EventTransaction(underlyingModified)
                    }

                    val expectedException = new TransactionException.BadAllInputsUTxOException(
                      transactionId = newTx.transaction.id,
                      missingInputs = Set(bogusTxIn),
                      missingCollateralInputs = Set.empty,
                      missingReferenceInputs = Set.empty
                    )
                    (newTx, expectedException)
                }

            Gen.oneOf(Seq(inputsNotInUtxoAttack))
        }

        /** NOTE: These will generate _fully_ arbitrary data. It is probably not what you want, but
          * may be a good starting point. For example, an arbitrary payout obligation may be for a
          * different network than the one you intend.
          *
          * Import as (...).ArbitraryInstances.{*, given}
          */
        object ArbitraryInstances {

            /** NOTE: You can't change the network very easily because this is an opaque type. You
              * should only use this for fuzz testing.
              */
            given Arbitrary[Payout.Obligation] = Arbitrary {
                for {
                    l2Input <- arbitrary[TransactionInput]

                    address <- arbitrary[ShelleyAddress]
                    coin <- arbitrary[Coin]
                    datum <- arbitrary[ByteString]
                    output = Babbage(
                      address = address,
                      value = Value(coin),
                      datumOption = Some(Inline(datum.toData)),
                      scriptRef = None
                    )
                } yield ??? // Payout.Obligation(l2Input = l2Input, output = output)
            }
        }
    }

    object Other {
        def vectorOf[A](g: Gen[A]): Gen[Vector[A]] =
            Gen.containerOf[Vector, A](g)

        def vectorOfN[A](n: Int, g: Gen[A]): Gen[Vector[A]] = {
            Gen.containerOfN[Vector, A](n, g)
        }

        def nonEmptyVectorOf[A](g: Gen[A]): Gen[NonEmptyVector[A]] =
            Gen.nonEmptyContainerOf[Vector, A](g).map(NonEmptyVector.fromVectorUnsafe)

        def nonEmptyVectorOfN[A](n: Int, g: Gen[A]): Gen[NonEmptyVector[A]] = {
            require(n >= 1, s"invalid size given: $n")
            Gen.containerOfN[Vector, A](n, g).map(NonEmptyVector.fromVectorUnsafe)
        }
    }

}
