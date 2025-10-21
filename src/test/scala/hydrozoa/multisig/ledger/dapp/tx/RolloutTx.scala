package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.lib.tx.TransactionUnspentOutput
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import org.scalacheck.Prop.*
import org.scalacheck.{Prop, Test as ScalaCheckTest, *}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{*, given}
import scalus.cardano.ledger.TransactionOutput.Babbage
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other as GenOther

class RolloutTxTest extends munit.ScalaCheckSuite {
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

    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(1)
    }

    // ===================================
    // Last
    // ===================================
    property("Build Last Rollout Tx Partial Result")(
      Prop.forAll(genLastBuilder) { (builder, args) =>
          {
              val pr = builder.partialResult(args)
              println(pr.get.ctx.transaction.body.value.outputs.size)
              pr.toProp :| "Partial result didn't build successfully"
              && {
                  val actualSize = pr.get.ctx.transaction.toCbor.length
                  val maxSize = builder.config.env.protocolParams.maxTxSize
                  (actualSize <= maxSize) :| s"Partial result tx size  $actualSize exceeds max tx size $maxSize"
              }
          }
      }
    )

    property("Complete Last Partial Result")({
        Prop.forAll(genLastBuilder)((builder, args) =>
            (for {
                pr <- builder.partialResult(args)
                txId = Arbitrary.arbitrary[TransactionHash].sample.get
                input = TransactionInput(txId, 0)
                output = Babbage(address = builder.config.headAddress, value = pr.inputValueNeeded)
                rolloutUtxo = RolloutUtxo(TransactionUnspentOutput(input, output))
                res <- pr.complete(rolloutUtxo)
            } yield res).toProp
        )
    })

    // ===================================
    // Not Last
    // ===================================
    property("Build NotLast Partial Result")(
      Prop.forAll(genNotLastBuilder) { (builder, args) =>
          builder.partialResult(args).toProp
      }
    )

    property("Post-process last rollout tx partial result")(???)
}
