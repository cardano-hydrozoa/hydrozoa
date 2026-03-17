package hydrozoa.multisig.ledger.l1.tx

import cats.*
import cats.data.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.l1.utxo.RolloutUtxo
import org.scalacheck.*
import org.scalacheck.util.Pretty
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.addDummySignatures
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other as GenOther

// TODO: All of these tests can be written in PropertyM[Either, _], or a shrinking variant
object RolloutTxTest extends Properties("RolloutTxTest") {
    given ppLastBuilder: (RolloutTx.Build.Last => Pretty) = builder =>
        Pretty(_ => builder.config.headMultisigScript.policyId.toString)

    private def genPayouts(network: CardanoNetwork.Section) =
        GenOther
            .genSequencedValueDistribution(
              500,
              v => genKnownValuePayoutObligationWithMinAdaEnsured(network, v)
            )
            .map(nel => NonEmptyVector.fromVectorUnsafe(Vector.from(nel.toList)))

    val genLastBuilder: Gen[RolloutTx.Build.Last] =
        for {
            multiNodeConfig <- MultiNodeConfig.generate(TestPeersSpec.default)()
            payouts <- genPayouts(multiNodeConfig.headConfig)
        } yield RolloutTx.Build.Last(multiNodeConfig.nodeConfigs(HeadPeerNumber.zero))(payouts)

    given ppNotLastBuilder: (RolloutTx.Build.NotLast => Pretty) = builder =>
        Pretty(_ => "NotLast (too long to print)")
    val genNotLastBuilder: Gen[RolloutTx.Build.NotLast] =
        for {
            multiNodeConfig <- MultiNodeConfig.generate(TestPeersSpec.default)()
            payouts <- genPayouts(multiNodeConfig.headConfig)
            rolloutSpentVal <- Gen.choose(1, 100_000_000).map((x: Int) => Value(Coin(x)))
        } yield RolloutTx.Build
            .NotLast(multiNodeConfig.nodeConfigs(HeadPeerNumber.zero))(payouts, rolloutSpentVal)

    // ===================================
    // Last
    // ===================================
    val _ = property("Build Last Rollout Tx Partial Result") = {
        {
            Prop.forAll(genLastBuilder) { builder =>
                val res = for {
                    pr <- builder.partialResult.left.map(e =>
                        s"Partial result should build successfully: ${e}" |: Prop(false)
                    )

                    unsignedSize = pr.ctx.transaction.toCbor.length
                    withDummySigners = addDummySignatures(
                      pr.builder.config.headMultisigScript.numSigners,
                      pr.ctx.transaction
                    )
                    signedSize = withDummySigners.toCbor.length

                    maxSize = builder.config.cardanoInfo.protocolParams.maxTxSize
                    _ <-
                        if signedSize <= maxSize
                        then Right(())
                        else
                            Left(
                              "Partial result size with dummy signatures should not be too big:" +
                                  s" unsigned size: $unsignedSize; signed size: $signedSize; max size: $maxSize"
                                  |: Prop(false)
                            )
                } yield ()
                res match {
                    case Left(fail) => fail
                    case Right(())  => Prop(true)
                }
            }
        }
    }

    val _ = property("Complete Last Partial Result") = {
        Prop.forAll(genLastBuilder, Arbitrary.arbitrary[TransactionHash])((builder, txId) => {
            val res = for {
                pr <- builder.partialResult.left.map(e =>
                    s"Build failed with error ${e}" |: Prop(false)
                )
                input = TransactionInput(txId, 0)
                output = Babbage(
                  address = builder.config.headMultisigAddress,
                  value = pr.inputValueNeeded
                )
                rolloutUtxo = RolloutUtxo(Utxo(input, output))
                res <- pr
                    .complete(rolloutUtxo)
                    .left
                    .map(e => s"Compeletion failed with error ${e}" |: Prop(false))
            } yield res
            res match {
                case Left(fail) => fail
                case Right(_)   => Prop(true)
            }
        })

    }

    // ===================================
    // Not Last
    // ===================================
    val _ = property("Build NotLast Partial Result") = Prop.forAll(genNotLastBuilder) { builder =>
        builder.partialResult.left.map(e => s"Build failed with error $e" |: Prop(false)) match {
            case Left(fail) => fail
            case Right(_)   => Prop(true)
        }
    }

    // TODO: shall we add that?
    // ignore("Post-process last rollout tx partial result")(???)

    val _ = property("Build Last Rollout Tx") = {
        Prop.forAll(genLastBuilder) { builder =>
            builder.partialResult.left.map(e =>
                s"Build failed with error $e" |: Prop(false)
            ) match {
                case Left(fail) => fail
                case Right(_)   => Prop(true)
            }
        }
    }
}
