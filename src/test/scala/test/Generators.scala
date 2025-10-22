package test

import cats.data.NonEmptyVector
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
import hydrozoa.multisig.ledger.joint.utxo.Payout
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.{Environment, TransactionUnspentOutput}

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
        def genTxConfig(
            env: Environment = testTxBuilderEnvironment,
            validators: Seq[Validator] = testValidators
        ): Gen[Tx.Builder.Config] =
            for {
                peers <- genTestPeers
                hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
                multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(hns, env.network)
            } yield Config(hns, multisigWitnessUtxo, env, validators)

        def genPayoutObligationL1(network: Network): Gen[Payout.Obligation.L1] =
            genPayoutObligationL2(network).map(Payout.Obligation.L1(_))

        def genFakeMultisigWitnessUtxo(
            script: HeadMultisigScript,
            network: Network
        ): Gen[TransactionUnspentOutput] = for {
            utxoId <- Arbitrary.arbitrary[TransactionInput]
            output = Babbage(
              script.mkAddress(network),
              Value.ada(2),
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
        def nonEmptyVectorOf[A](g: Gen[A]): Gen[NonEmptyVector[A]] =
            Gen.nonEmptyContainerOf[Vector, A](g).map(NonEmptyVector.fromVectorUnsafe)

        def nonEmptyVectorOfN[A](n: Int, g: Gen[A]): Gen[NonEmptyVector[A]] = {
            require(n >= 1, s"invalid size given: $n")
            Gen.containerOfN[Vector, A](n, g).map(NonEmptyVector.fromVectorUnsafe)
        }
    }

}
