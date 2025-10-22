package hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionUnspentOutput
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other as GenOther

class RolloutTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {
    val genLastBuilder: Gen[(RolloutTx.Builder.Last, RolloutTx.Builder.Args.Last)] =
        for {
            config <- genTxConfig()
            genPayouts = genPayoutObligationL1(config.env.network)
            // We want to test small, medium, and large, so we do it with frequency
            payouts <- GenOther.nonEmptyVectorOfN(200, genPayouts)
            //                Gen.frequency(
            //                (1, GenOther.nonEmptyVectorOf(genPayouts)),
            //                (7, Gen.sized(size => GenOther.nonEmptyVectorOfN(size*3+1, genPayouts))),
            //                (2, Gen.sized(size => GenOther.nonEmptyVectorOfN(size*6+1, genPayouts))))
        } yield (
          RolloutTx.Builder.Last(config),
          RolloutTx.Builder.Args.Last(payouts)
        )
    val genNotLastBuilder: Gen[(RolloutTx.Builder.NotLast, RolloutTx.Builder.Args.NotLast)] =
        for {
            config <- genTxConfig()
            payouts <- GenOther.nonEmptyVectorOf(genPayoutObligationL1(config.env.network))
            rolloutSpentVal <- Arbitrary.arbitrary[Coin].map(Value(_))
        } yield (
          RolloutTx.Builder.NotLast(config),
          RolloutTx.Builder.Args.NotLast(payouts, rolloutSpentVal)
        )

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 1000)

    // FIXME: How to do this in ScalaTest?
    //override def scalaCheckInitialSeed = "espemfZrEC9vyjQ_8nzYe9Pikzd1423i2sM8_TQft9E="

    // ===================================
    // Last
    // ===================================
    test("Build Last Rollout Tx Partial Result") {
         {
            forAll(genLastBuilder) { (builder, args) =>
              val pr = builder.partialResult(args)
              println(pr.get.ctx.transaction.body.value.outputs.size)
              assert(pr.isRight, "Partial result didn't build successfully")

              val unsignedSize = pr.get.ctx.transaction.toCbor.length
              val witnessesSize = (32 + 64) * builder.config.headNativeScript.numSigners
              val maxSize = builder.config.env.protocolParams.maxTxSize
              assert(unsignedSize + witnessesSize <= maxSize,
                        "Partial result unsigned tx size plus signature size " +
                            s"($unsignedSize + $witnessesSize = ${unsignedSize + witnessesSize})" +
                            s" exceeds max tx size $maxSize"
              )
            }
            }
        }


  test("Complete Last Partial Result")({
    forAll(genLastBuilder)((builder, args) => {
      val res = for {
        pr <- builder.partialResult(args)
        txId = Arbitrary.arbitrary[TransactionHash].sample.get
        input = TransactionInput(txId, 0)
        output = Babbage(address = builder.config.headAddress, value = pr.inputValueNeeded)
        rolloutUtxo = RolloutUtxo(TransactionUnspentOutput(input, output))
        res <- pr.complete(rolloutUtxo)
      } yield res
      assert(res.isRight)
    }
        )
    })

    // ===================================
    // Not Last
    // ===================================
    test("Build NotLast Partial Result")(
      forAll(genNotLastBuilder) { (builder, args) =>
          assert(builder.partialResult(args).isRight)
      }
    )

    test("Post-process last rollout tx partial result")(???)
    
    test("Build Last Rollout Tx") {
        forAll(genLastBuilder) { (builder, args) =>
            builder.partialResult(args) match
                case Left(e)  => fail(e.toString)
                case Right(_) => ()
        }
    }
}
