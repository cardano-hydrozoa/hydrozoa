package test

import cats.data.{NonEmptyList, NonEmptyVector}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import monocle.*
import monocle.syntax.all.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}

import scala.collection.immutable.SortedMap
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.address.Network.Testnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{*, given}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.cardano.txbuilder.{Environment, TransactionUnspentOutput}
import scalus.prelude.Option as SOption

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
            validators: Seq[Validator] = testValidators
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
                validators = validators,
                disputeResolutionPaymentPart =
                    ShelleyPaymentPart.Script(Hash(ByteString.fromArray(DisputeResolutionScript.getScriptHash))),
                disputeTreasuryPaymentPart =
                    ShelleyPaymentPart.Script(Hash(ByteString.fromArray(RuleBasedTreasuryScript.getScriptHash)))
              ),
              peers
            )

        def genTxConfig(
            env: Environment = testTxBuilderEnvironment,
            validators: Seq[Validator] = testValidators
        ): Gen[Tx.Builder.Config] = genTxBuilderConfigAndPeers(env, validators).map(_._1)

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

        def genPayoutObligationL1(network: Network): Gen[Payout.Obligation.L1] =
            genPayoutObligationL2(network).map(Payout.Obligation.L1(_))

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
        ): Gen[TransactionUnspentOutput] = for {
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

        def genPayoutObligationL2(network: Network): Gen[Payout.Obligation.L2] =
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
            } yield Payout.Obligation.L2(l2Input = l2Input, output = output)

        /** Ada-only pub key utxo from the given peer, at least minAda, random tx id, random index,
          * no datum, no script ref
          */
        // TODO: make this take all fields as Option and default to generation if None.
        def genAdaOnlyPubKeyUtxo(
            peer: TestPeer,
            params: ProtocolParams = blockfrost544Params,
            network: Network = Testnet,

            /** `None` for minAda; Some(Coin)` to generate lovelace values with some minimum amount
              */
            genCoinWithMinimum: Option[Coin] = None,

            /** `None` for no datum; `Some(gen)` to generate an optional datum */
            datumGenerator: Option[Gen[Option[DatumOption]]] = None
        ): Gen[(TransactionInput, Babbage)] =
            for {
                txId <- arbitrary[TransactionInput]
                value <- genAdaOnlyValue
                coin <- genCoinWithMinimum match {
                    case None      => Gen.const(Coin(0))
                    case Some(min) => arbitrary[Coin].map(_ + min)
                }
                datum <- datumGenerator match {
                    case None      => Gen.const(None)
                    case Some(gen) => gen
                }
            } yield (
              txId,
              ensureMinAda(
                Babbage(
                  address = peer.address(network),
                  value = Value(coin),
                  datumOption = datum,
                  scriptRef = None
                ),
                params
              ).asInstanceOf[Babbage]
            ).focus(_._2.value).modify(_ + value)

        /** Generate a treasury utxo with at least minAda */
        def genTreasuryUtxo(
            network: Network = testNetwork,
            params: ProtocolParams = blockfrost544Params,
            headAddress: Option[ShelleyAddress],
            coin: Option[Coin] = None // None to generate
        ): Gen[TreasuryUtxo] =
            for {
                txId <- arbitrary[TransactionInput]
                headTn <- genHeadTokenName

                scriptAddress = headAddress.getOrElse({
                    ShelleyAddress(
                      network,
                      ShelleyPaymentPart.Script(genScriptHash.sample.get),
                      Null
                    )
                })
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
        def genTreasuryUtxo(config: Config): Gen[TreasuryUtxo] =
            genTreasuryUtxo(
              network = config.env.network,
              params = config.env.protocolParams,
              headAddress = Some(config.headAddress),
              coin = None
            )

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
            given Arbitrary[Payout.Obligation.L2] = Arbitrary {
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
                } yield Payout.Obligation.L2(l2Input = l2Input, output = output)
            }

            given Arbitrary[Payout.Obligation.L1] = Arbitrary {
                for {
                    l2 <- arbitrary[Payout.Obligation.L2]
                } yield Payout.Obligation.L1(l2)
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
