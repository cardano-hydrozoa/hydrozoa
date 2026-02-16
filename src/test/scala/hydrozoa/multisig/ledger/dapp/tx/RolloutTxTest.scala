package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.node.TestNodeConfig.generateTestNodeConfig
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import org.scalacheck.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.addDummySignatures
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other as GenOther

// TODO: All of these tests can be written in PropertyM[Either, _], or a shrinking variant
object RolloutTxTest extends Properties("RolloutTxTest") {
    val genLastBuilder: Gen[RolloutTx.Build.Last] =
        for {
            testNodeConfig <- generateTestNodeConfig
            genPayouts = genPayoutObligation(testNodeConfig.nodeConfig.cardanoNetwork)
            // We want to test small, medium, and large, so we do it with frequency
            payouts <-
                Gen.frequency(
                  (1, GenOther.nonEmptyVectorOf(genPayouts)),
                  (7, Gen.sized(size => GenOther.nonEmptyVectorOfN(size * 3 + 1, genPayouts))),
                  (2, Gen.sized(size => GenOther.nonEmptyVectorOfN(size * 6 + 1, genPayouts)))
                )
        } yield RolloutTx.Build.Last(testNodeConfig.nodeConfig)(payouts)

    val genNotLastBuilder: Gen[RolloutTx.Build.NotLast] =
        for {
            testNodeConfig <- generateTestNodeConfig
            payouts <- GenOther.nonEmptyVectorOf(
              genPayoutObligation(testNodeConfig.nodeConfig.cardanoNetwork)
            )
            rolloutSpentVal <- Arbitrary.arbitrary[Coin].map(Value(_))
        } yield RolloutTx.Build.NotLast(testNodeConfig.nodeConfig)(payouts, rolloutSpentVal)

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
        Prop.forAll(genLastBuilder)(builder => {
            val res = for {
                pr <- builder.partialResult.left.map(e =>
                    s"Build failed with error ${e}" |: Prop(false)
                )
                txId = Arbitrary.arbitrary[TransactionHash].sample.get
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
