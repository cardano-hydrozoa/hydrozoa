package test

import cats.data.{NonEmptyList, NonEmptyVector}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.value.coin.Distribution
import hydrozoa.lib.cardano.value.coin.Distribution.NormalizedWeights
import hydrozoa.multisig.ledger.VirtualLedgerM
import hydrozoa.multisig.ledger.VirtualLedgerM.{Config, State}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.multisig.ledger.event.LedgerEvent.L2TxEvent
import hydrozoa.multisig.ledger.event.{LedgerEvent, LedgerEventId}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.virtual.tx.{GenesisObligation, L2Tx}
import hydrozoa.rulebased.ledger.dapp.tx.CommonGenerators.genShelleyAddress
import monocle.*
import monocle.syntax.all.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scala.collection.immutable.SortedMap
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{*, given}
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.{Babbage, valueLens}
import scalus.cardano.onchain.plutus.prelude.Option as SOption
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.|>
import spire.math.{Rational, SafeLong}
import test.Generators.Hydrozoa.genAdaOnlyPubKeyUtxo
import test.TestPeer.Alice

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

        val genHeadTokenName: Gen[AssetName] =
            for {
                ti <- arbitrary[TransactionInput]
            } yield CIP67.HeadTokenNames(ti).treasuryTokenName

        val genTreasuryDatum: Gen[MultisigTreasuryUtxo.Datum] = {
            for {
                mv <- Gen.posNum[BigInt]
                // Verify that this is the correct length!
                kzg <- genByteStringOfN(32)
                paramsHash <- genByteStringOfN(32)

            } yield MultisigTreasuryUtxo.Datum(
              commit = kzg,
              versionMajor = mv
            )
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
            config: CardanoNetwork.Section,
            delegation: ShelleyDelegationPart = ShelleyDelegationPart.Null
        ): Gen[ShelleyAddress] =
            genAddrKeyHash.flatMap(akh =>
                ShelleyAddress(
                  network = config.network,
                  payment = Key(akh),
                  delegation = delegation
                )
            )

        def genScriptAddress(
            config: CardanoNetwork.Section,
            delegation: ShelleyDelegationPart = ShelleyDelegationPart.Null
        ): Gen[ShelleyAddress] =
            for {
                sh <- genScriptHash
            } yield ShelleyAddress(
              network = config.network,
              payment = ShelleyPaymentPart.Script(sh),
              delegation = delegation
            )

        def genFakeMultisigWitnessUtxo(
            script: HeadMultisigScript,
            network: Network,
            // Pass this if you need a specific token name for coherence with the rest of your test.
            // In general, it should be obtained via `CIP67.TokenNames(seedUtxo).multisigRegimeTokenName
            hmrwTokenName: Option[AssetName] = None
        ): Gen[MultisigRegimeUtxo] = for {
            utxoId <- Arbitrary.arbitrary[TransactionInput]

            hmrwTn <- hmrwTokenName match {
                case None =>
                    arbitrary[TransactionInput].flatMap(ti =>
                        CIP67.HeadTokenNames(ti).multisigRegimeTokenName
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

        } yield MultisigRegimeUtxo(
          multisigRegimeTokenName = hmrwTn,
          utxoId = utxoId,
          address = script.mkAddress(network),
          value = Value.ada(2) + hmrwToken,
          script = script
        )

        def genPayoutObligation(network: CardanoNetwork.Section): Gen[Payout.Obligation] =
            for {
                coin <- arbitrary[Coin]
                res <- genKnownCoinPayoutObligation(network, coin)
            } yield res

        def genKnownCoinPayoutObligation(
            network: CardanoNetwork.Section,
            coin: Coin
        ): Gen[Payout.Obligation] =
            for {
                l2Input <- arbitrary[TransactionInput]

                address0 <- arbitrary[ShelleyAddress]
                address = address0.copy(network = network.network)
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
            config: CardanoNetwork.Section,
            peer: TestPeer,
            minimumCoin: Coin = Coin.zero,
            datumGenerator: Option[Gen[Option[DatumOption]]] = None
        ): Gen[Utxo] =
            for {
                txId <- arbitrary[TransactionInput]
                txOutput <- genAdaOnlyBabbageOutput(
                  peer,
                  config,
                  minimumCoin,
                  datumGenerator
                )
            } yield Utxo(txId, txOutput)

        /** @param peer
          *   The test peer who's PKH this output will be at
          * @param minimumCoin
          *   an optional minimum coin. Should be positive, defaults to 0
          * @param datumGenerator
          *   an Optional datum generator. Will generate an empty datum if not passed
          * @return
          */
        def genAdaOnlyBabbageOutput(
            peer: TestPeer,
            config: CardanoNetwork.Section,
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
                    address = peer.address(config.network),
                    value = Value(coin),
                    datumOption = datum,
                    scriptRef = None
                  ),
                  config.cardanoProtocolParams
                ).asInstanceOf[Babbage].focus(_.value).modify(_ + value)
            } yield txOutput

        // Has duplication with genAdaOnlyBabbageOutput
        def genGenesisObligation(
            config: CardanoNetwork.Section,
            peer: TestPeer,
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
                    address = peer.address(config.network),
                    value = Value(coin),
                    datumOption = datum,
                    scriptRef = None
                  ),
                  config.cardanoProtocolParams
                ).asInstanceOf[Babbage].focus(_.value).modify(_ + value)

                genesisObligation = GenesisObligation(
                  l2OutputPaymentAddress = peer.address(config.network).payment,
                  l2OutputNetwork = config.network,
                  l2OutputDatum = datum match {
                      case None    => SOption.None
                      case Some(d) => SOption.Some(d.data)
                  },
                  l2OutputValue = txOutput.value.coin,
                  l2OutputRefScript = None
                )

            } yield genesisObligation

        /** Given a set of inputs event, construct a withdrawal event attempting to withdraw all
          * inputs with the given key to a single output
          */
        def genL2WithdrawalFromUtxosAndPeer(
            config: CardanoNetwork.Section,
            inputUtxos: Utxos,
            peer: TestPeer
        ): Gen[L2Tx] =
            for {
                addr <- genShelleyAddress(config)

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
                      witnessSetRaw = KeepRaw(TransactionWitnessSet.empty),
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

            } yield ??? // L2Tx(peer.signTx(txUnsigned))

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
          (VirtualLedgerM.Config, State, L2Tx) => (
              L2Tx,
              String | TransactionException
          )
        ] = {

            // Violates "AllInputsMustBeInUtxoValidator" ledger rule
            def inputsNotInUtxoAttack: (VirtualLedgerM.Config, State, L2Tx) => (
                L2Tx,
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

                    val newTx: L2Tx = {
                        val underlyingOriginal = transaction.tx
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

                        ??? // L2Tx(underlyingModified)
                    }

                    val expectedException = new TransactionException.BadAllInputsUTxOException(
                      transactionId = newTx.tx.id,
                      missingInputs = Set(bogusTxIn),
                      missingCollateralInputs = Set.empty,
                      missingReferenceInputs = Set.empty
                    )
                    (newTx, expectedException)
                }

            Gen.oneOf(Seq(inputsNotInUtxoAttack))
        }

        // TODO: improve
        def genEventId: Gen[LedgerEventId] = for {
            headPeerNumber <- Gen.choose(0, 10)
            eventNumber <- Gen.choose(0, 1024)
        } yield LedgerEventId(headPeerNumber, eventNumber)

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
                } yield Payout.Obligation(l2UtxoId = l2Input, utxo = output)
            }

            given Arbitrary[LedgerEvent] = Arbitrary {
                for {
                    eventId <- genEventId
                    // genesisObligation <-genGenesisObligation()
                    event <- Gen.frequency(
                      // TODO: improve
                      2 -> Gen.const(L2TxEvent(eventId, Array.empty))
                      // 8 -> Gen.const(RegisterDeposit(eventId, Array.empty))
                    )
                } yield event
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

        def normalizedWeights[Record](n: Int): Gen[NormalizedWeights] = {
            require(
              n > 0,
              "`normalizedWeights(n : Int) : Gen[NormalizedWeights]` requires a positive `n`, but it " +
                  s"received $n"
            )
            // One entry gets everything, other entries get none
            val singletonDistributions: Gen[NormalizedWeights] =
                Gen.oneOf(
                  List
                      .range(0, n)
                      .foldLeft(List.empty[NormalizedWeights])((acc, index) =>
                          acc.prepended(
                            Distribution.unsafeNormalizeWeights(
                              NonEmptyList.fromListUnsafe(
                                Vector
                                    .fill(n)(Rational.zero)
                                    .updated(index, Rational.one)
                                    .toList
                              )
                            )
                          )
                      )
                )
            // Every entry gets the same amount
            val evenDistribution: Gen[NormalizedWeights] =
                Gen.const(
                  Distribution.unsafeNormalizeWeights(
                    NonEmptyList.fromListUnsafe(List.fill(n)(Rational.one))
                  )
                )

            // Every entry gets a random amount
            val randomDistributions: Gen[NormalizedWeights] =
                Gen
                    .listOfN(n, Gen.posNum[BigInt].map(Rational(_)))
                    .map(l => Distribution.unsafeNormalizeWeights(NonEmptyList.fromListUnsafe(l)))
            Gen.frequency(
              (1, evenDistribution),
              (3, singletonDistributions),
              (10, randomDistributions)
            )
        }

        /** Distribute an integral amount over `n > 0` bags. If there is a surplus amount left after
          * this initial distribution, it is evenly spread across the shares (in order) until it is
          * exhausted.
          */
        def distribution(amount: SafeLong, n: Int): Gen[NonEmptyList[SafeLong]] = {
            require(
              n > 0,
              "`distribution(amount: SafeLong, n : Int) : Gen[NonEmptyList[SafeLong]]` requires a positive `n`, but it " +
                  s"received $n"
            )
            for {
                weights <- normalizedWeights(n)
            } yield weights.distribute(amount)
        }

        /** Generate a coin distribution among `n` bags. Note: Some bags may be empty
          */
        def genCoinDistribution(coin: Coin, n: Int): Gen[NonEmptyList[Coin]] = {
            require(
              n > 0,
              "`genCoinDistribution(coin: Coin, n : Int) : Gen[NonEmptyList[Coin]]` requires a positive `n`, but it " +
                  s"received ${n}"
            )
            distribution(SafeLong(coin.value), n).map(nel => nel.map(sl => Coin(sl.toLong)))
        }

        /** Distribute an amount of coin over transaction outputs, ensuring that min ada
          * requirements are first met. This will ONLY increase the lovelace in each transaction
          * output and will throw an exception if there is not enough ada to cover min ada.
          * @param additionalCoin
          *   additional coin to add to the existing value in [[transactionOutputs]]
          */
        def genAdditionalCoinDistributionWithMinAda(
            additionalCoin: Coin,
            transactionOutputs: NonEmptyList[TransactionOutput],
            params: ProtocolParams
        ): Gen[NonEmptyList[TransactionOutput]] =

            val sumBefore = transactionOutputs.toList.map(_.value.coin.value).sum
            val withMinAda = transactionOutputs.toList.map(ensureMinAda(_, params))
            val withMinAdaSum = withMinAda.map(_.value.coin.value).sum
            val remainderToDistribute = additionalCoin.value - (withMinAdaSum - sumBefore)
            require(
              remainderToDistribute >= 0,
              "genCoinDistribution: there is not enough lovelace" +
                  " to distribute while ensuring minAda is met."
            )
            for {
                coinDist <- genCoinDistribution(
                  Coin(remainderToDistribute),
                  transactionOutputs.length
                )
                zipped = withMinAda.zip(coinDist.toList)
                summed: List[TransactionOutput] = zipped.foldRight(List.empty)((x, acc) => {
                    val to: TransactionOutput = x._1
                    val coin: Coin = x._2
                    (to |> valueLens
                        .andThen(Focus[Value](_.coin.value))
                        .modify(_ + coin.value)) :: acc
                })
            } yield NonEmptyList.fromListUnsafe(summed)

        /** Like [[genAdditionalCoinDistributionWithMinAda]], but replaces the transaction output
          * for a given list of utxos.
          */
        def genCoinDistributionWithMinAdaUtxo(
            coin: Coin,
            utxoList: NonEmptyList[Utxo],
            params: ProtocolParams
        ): Gen[NonEmptyList[Utxo]] =
            val transactionOutputs = utxoList.map(_.output)
            for {
                outputDist <- genAdditionalCoinDistributionWithMinAda(
                  coin,
                  transactionOutputs,
                  params
                )
            } yield utxoList.map(_.input).zip(outputDist).map(Utxo(_))

    }

}

object GeneratorTests extends Properties("Generator Tests") {
    val _ = property("distribution sums to original amount") =
        Prop.forAll(Gen.posNum[Long], Gen.posNum[Int])((amount, n) =>
            Prop.forAll(Generators.Other.distribution(SafeLong(amount), n))(distribution =>
                distribution.toList.map(_.toLong).sum == amount
            )
        )

    val _ = property("coin distribution sums to original amount") =
        Prop.forAll(Gen.posNum[Long], Gen.posNum[Int])((amount, n) =>
            Prop.forAll(Generators.Other.genCoinDistribution(Coin(amount), n))(distribution =>
                distribution.toList.map(_.value).sum == amount
            )
        )

    val _ = property("genCoinDistributionWithMinAda sums to original amount") = Prop.forAll(
      Gen.posNum[Long],
      Gen.posNum[Int],
      Gen.nonEmptyListOf(genAdaOnlyPubKeyUtxo(CardanoNetwork.Mainnet, Alice))
    )((amount, n, utxos) =>
        Prop.forAll(
          Generators.Other.genCoinDistributionWithMinAdaUtxo(
            coin = Coin(amount),
            utxoList = NonEmptyList.fromListUnsafe(utxos),
            params = CardanoNetwork.Mainnet.cardanoProtocolParams
          )
        )(distribution =>
            val expectedAmount = distribution.toList.map(_.output.value.coin.value).sum
            expectedAmount == amount + utxos.toList.map(_.output.value.coin.value).sum
        )
    )

}
