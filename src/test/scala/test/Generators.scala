package test

import cats.data.{NonEmptyList, NonEmptyVector}
import cats.syntax.all.toTraverseOps
import cats.{Hash as _, *}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.value.coin.Distribution
import hydrozoa.lib.cardano.value.coin.Distribution.NormalizedWeights
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.ledger
import hydrozoa.multisig.ledger.eutxol2.EutxoL2Ledger
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, L2Tx}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.rulebased.ledger.l1.tx.CommonGenerators.genShelleyAddress
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
import scalus.cardano.txbuilder.TransactionBuilder
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.|>
import spire.math.{Rational, SafeLong}
import test.Generators.Hydrozoa.{genPositiveValue, genPubKeyUtxo}

// Annoyingly, `Gen` doesn't have `Monad[Gen]` already. But I want to use `traverse`, so I'm vendoring it here
given genMonad: Monad[Gen] = new Monad[Gen] {
    def pure[A](a: A): Gen[A] = Gen.const(a)
    def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Gen[Either[A, B]]): Gen[B] = Gen.tailRecM(a)(f)
}

/** This module contains shared generators and arbitrary instances that may be shared among multiple
  * tests. We separate them into "Hydrozoa" and "Other" objects for ease of upstreaming.
  */

object Generators {

    // This guy is used everywhere through tests to log some traces when generating various things.
    // Use:
    //   - trace level for diversity traces (z-print-results property)
    val loggerGenerators = Logging.logger("Generators")

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

        /** Generate a value */
        def genValue(genCoin: Gen[Coin], genMultiAsset: Gen[MultiAsset]): Gen[Value] =
            for {
                coin <- genCoin
                ma <- genMultiAsset
            } yield Value(coin, ma)

        /** Helper generator with sensible defaults. The value will have at least 5 ada and may
          * contain multiassets.
          */
        val genPositiveValue: Gen[Value] = genValue(
          genCoin = Arbitrary.arbitrary[Coin].map(_ + Coin.ada(5)),
          genMultiAsset = Arbitrary.arbitrary[MultiAsset].map(_.onlyPositive)
        )

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

        def genPayoutObligation(
            network: CardanoNetwork.Section,
            genValue: Gen[Value] = Hydrozoa.genPositiveValue
        ): Gen[Payout.Obligation] =
            for {
                value <- genValue
                res <- genKnownValuePayoutObligationWithMinAdaEnsured(network, value)
            } yield res

        def genKnownValuePayoutObligationWithMinAdaEnsured(
            network: CardanoNetwork.Section,
            value: Value
        ): Gen[Payout.Obligation] = {
            for {
                l2Input <- arbitrary[TransactionInput]

                address0 <- arbitrary[ShelleyAddress]
                address = address0.copy(network = network.network)
                datum <- arbitrary[ByteString]
                output = Babbage(
                  address = address,
                  value = value,
                  datumOption = Some(Inline(datum.toData)),
                  scriptRef = None
                )
            } yield Payout
                .Obligation(KeepRaw(ensureMinAda(output, network.cardanoProtocolParams)), network)
                .toOption
                .get
        }

        /** Ada-only pub key utxo from the given peer, at least minAda, random tx id, random index,
          * no datum, no script ref
          */
        // TODO: make this take all fields as Option and default to generation if None.
        def genPubKeyUtxo(
            config: CardanoNetwork.Section,
            address: Address,
            genValue: Gen[Value],
            datumGenerator: Option[Gen[Option[DatumOption]]] = None,
            ensureMinAda: Boolean = false
        ): Gen[Utxo] =
            for {
                txId <- arbitrary[TransactionInput]
                txOutput <- genBabbageOutput(
                  address,
                  config,
                  genValue,
                  datumGenerator,
                  ensureMinAda
                )
            } yield Utxo(txId, txOutput)

        def genBabbageOutput(
            address: Address,
            config: CardanoNetwork.Section,
            genValue: Gen[Value],
            datumGenerator: Option[Gen[Option[DatumOption]]] = None,
            ensureMinAda: Boolean = false
        ): Gen[Babbage] =
            for {
                value <- genValue
                datum <- datumGenerator match {
                    case None      => Gen.const(None)
                    case Some(gen) => gen
                }
                babbage = Babbage(
                  address = address, // peer.address(config.network),
                  value = value,
                  datumOption = datum,
                  scriptRef = None
                )
            } yield
                if ensureMinAda
                then
                    TransactionBuilder
                        .ensureMinAda(babbage, config.cardanoProtocolParams)
                        .asInstanceOf[Babbage]
                else babbage

        // Has duplication with genAdaOnlyBabbageOutput
        def genGenesisObligation(
            config: CardanoNetwork.Section,
            address: ShelleyAddress,
            datumGenerator: Option[Gen[Option[Inline]]] = None,
            genValue: Gen[Value]
        ): Gen[GenesisObligation] =
            for {
                value <- genValue

                datum <- datumGenerator match {
                    case None      => Gen.const(None)
                    case Some(gen) => gen
                }
                txOutput: TransactionOutput.Babbage = ensureMinAda(
                  Babbage(
                    address = address, // peer.address(config.network),
                    value = value,
                    datumOption = datum,
                    scriptRef = None
                  ),
                  config.cardanoProtocolParams
                ).asInstanceOf[Babbage]

                genesisObligation = GenesisObligation(
                  l2OutputPaymentAddress = address.payment, // peer.address(config.network).payment,
                  l2OutputNetwork = config.network,
                  l2OutputDatum = datum match {
                      case None    => SOption.None
                      case Some(d) => SOption.Some(d.data)
                  },
                  l2OutputValue = txOutput.value,
                  l2OutputRefScript = None
                )

            } yield genesisObligation

        /** Given a set of inputs event, construct a withdrawal event attempting to withdraw all
          * inputs with the given key to a single output
          */
        def genL2WithdrawalFromUtxosAndPeer(
            config: CardanoNetwork.Section,
            inputUtxos: Utxos,
            peer: TestPeerName
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
          (EutxoL2Ledger.Config, EutxoL2Ledger.State, L2Tx) => (
              L2Tx,
              String | TransactionException
          )
        ] = {

            // Violates "AllInputsMustBeInUtxoValidator" ledger rule
            def inputsNotInUtxoAttack: (EutxoL2Ledger.Config, EutxoL2Ledger.State, L2Tx) => (
                L2Tx,
                String | TransactionException
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
        def genRequestId: Gen[RequestId] = for {
            headPeerNumber <- Gen.choose(0, 10)
            requestNumber <- Gen.choose(0, 1024)
        } yield RequestId(headPeerNumber, requestNumber)

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
                } yield Payout
                    .Obligation(
                      KeepRaw(output),
                      CardanoNetwork.fromOrdinal(address.network.networkId.toInt)
                    )
                    .toOption
                    .get
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
        def genDistribution(amount: SafeLong, n: Int): Gen[NonEmptyList[SafeLong]] = {
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
                  s"received $n"
            )
            genDistribution(SafeLong(coin.value), n).map(nel => nel.map(sl => Coin(sl.toLong)))
        }

        def genValueDistribution(value: Value, n: Int): Gen[NonEmptyList[Value]] = {
            require(
              n > 0,
              "`genValueDistribution(value: Value, n : Int) : Gen[NonEmptyList[Value]]` requires a positive `n`, but it " +
                  s"received $n"
            )
            val genCoinDist: Gen[NonEmptyList[Value]] =
                genCoinDistribution(value.coin, n).map(_.map(Value(_)))

            val flat: Iterable[(PolicyId, AssetName, Long)] =
                value.assets.assets.flatMap((policyId, innerMap) =>
                    innerMap.map((assetName, amount) => (policyId, assetName, amount))
                )

            // Strategy:
            //   - Start with a constant generator of the coin dist accumulator
            //   - For each policyId, assetName, amount in the (flattened) total to be distributed:
            //     - Do a single (scalar) distribution of the amount
            //     - convert it to a multiasset distribution
            //     - map over the accumulator generator, zip the generator distribution with the
            //       multiasset distribution and sum element-wise (can these be fused?)
            flat.foldLeft(genCoinDist) { case (genDist, (policyId, assetName, amount)) =>
                for {
                    singleAssetDist <- genDistribution(SafeLong(amount), n)

                    res <- genDist.map(dist =>
                        dist.zip(singleAssetDist)
                            .map((baseDist, additionalAmount) =>
                                val asValue = Value(
                                  Coin.zero,
                                  MultiAsset.asset(policyId, assetName, additionalAmount.toLong)
                                )
                                baseDist + asValue
                            )
                    )
                } yield res
            }
        }

        /** Distribute an amount of value over transaction outputs, ensuring that min ada
          * requirements are first met. This will ONLY increase the lovelace in each transaction
          * output and will throw an exception if there is not enough ada to cover min ada.
          * @param additionalValue
          *   additional value to add to the existing value in [[transactionOutputs]]
          */
        def genAdditionalValueDistributionWithMinAda(
            additionalValue: Value,
            transactionOutputs: NonEmptyList[TransactionOutput],
            params: ProtocolParams
        ): Gen[NonEmptyList[TransactionOutput]] =

            val coinSumBefore = transactionOutputs.toList.map(_.value.coin.value).sum
            val withMinAda = transactionOutputs.toList.map(ensureMinAda(_, params))
            val withMinAdaSum = withMinAda.map(_.value.coin.value).sum
            val remainderCoinToDistribute =
                additionalValue.coin.value - (withMinAdaSum - coinSumBefore)
            require(
              remainderCoinToDistribute >= 0,
              "genCoinDistribution: there is not enough lovelace" +
                  " to distribute while ensuring minAda is met."
            )
            for {
                valueDist <- genValueDistribution(
                  Value(coin = Coin(remainderCoinToDistribute), additionalValue.assets),
                  transactionOutputs.length
                )

                zipped = withMinAda.zip(valueDist.toList)
                summed: List[TransactionOutput] = zipped.foldRight(List.empty)((x, acc) => {
                    val to: TransactionOutput = x._1
                    val extraValue = x._2
                    (to |> valueLens.modify(_ + extraValue)) :: acc
                })
            } yield NonEmptyList.fromListUnsafe(summed)

        /** Like [[genAdditionalValueDistributionWithMinAda]], but replaces the transaction output
          * for a given list of utxos.
          */
        def genValueDistributionWithMinAdaUtxo(
            value: Value,
            utxoList: NonEmptyList[Utxo],
            params: ProtocolParams
        ): Gen[NonEmptyList[Utxo]] =
            val transactionOutputs = utxoList.map(_.output)
            for {
                outputDist <- genAdditionalValueDistributionWithMinAda(
                  value,
                  transactionOutputs,
                  params
                )
            } yield utxoList.map(_.input).zip(outputDist).map(Utxo(_))

        /** We need to be able to generate deposit utxos and payouts with a consistent set of
          * allowed assets, otherwise the treasury utxo will exceed size limits (i.e., if we do 10
          * deposits and payouts which each have 10 unique policy id <> assetname pairs, we end up
          * with 200 pairs total).
          *
          * This helps with that by distributing a single value.
          */
        def genSequencedValueDistribution[A](
            maxNumValues: Int,
            mapping: Value => Gen[A],
            minCoin: Coin = Coin.zero
        ): Gen[NonEmptyList[A]] =
            for {
                totalValue <- genPositiveValue
                nValues <- Gen.choose(1, maxNumValues)
                valueDist <- genValueDistribution(totalValue, nValues)
                valueDistWithMinAda = valueDist.map(_.focus(_.coin).modify(_ + minCoin))
                res <- valueDistWithMinAda.map(mapping).sequence
            } yield res

    }

}

object GeneratorTests extends Properties("Generator Tests") {
    val _ = property("distribution sums to original amount") =
        Prop.forAll(Gen.posNum[Long], Gen.posNum[Int])((amount, n) =>
            Prop.forAll(Generators.Other.genDistribution(SafeLong(amount), n))(distribution =>
                distribution.toList.map(_.toLong).sum == amount
            )
        )

    val _ = property("value distribution sums to original amount") =
        Prop.forAll(Arbitrary.arbitrary[Value], Gen.posNum[Int])((amount, n) =>
            Prop.forAll(Generators.Other.genValueDistribution(amount, n))(distribution =>
                distribution.foldLeft(Value.zero)(_ + _) == amount
            )
        )

    val _ = property("genValueDistributionWithMinAda sums to original amount") = Prop.forAll(
      arbitrary[ShelleyAddress]
    )(address =>
        Prop.forAll(
          Arbitrary.arbitrary[Value],
          Gen.posNum[Int],
          Gen.nonEmptyListOf(
            genPubKeyUtxo(CardanoNetwork.Mainnet, address, genValue = Arbitrary.arbitrary[Value])
          )
        )((amount, n, utxos) =>
            Prop.forAll(
              Generators.Other.genValueDistributionWithMinAdaUtxo(
                value = amount,
                utxoList = NonEmptyList.fromListUnsafe(utxos),
                params = CardanoNetwork.Mainnet.cardanoProtocolParams
              )
            )(distribution =>
                val expectedAmount =
                    distribution.toList.map(_.output.value).foldLeft(Value.zero)(_ + _)
                expectedAmount == amount + utxos
                    .map(_.output.value)
                    .foldLeft(Value.zero)(_ + _)
            )
        )
    )

}
