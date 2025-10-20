package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyVector
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
import hydrozoa.multisig.ledger.dapp.tx.{
    RolloutTx,
    genPayoutObligationL1
}
import hydrozoa.multisig.ledger.joint.utxo.Payout
import org.scalacheck.*
import test.*
import test.Generators.Hydrozoa.*

class RolloutTxTest extends munit.ScalaCheckSuite {
    val genLastBuilder: Gen[(RolloutTx.Builder.Last, RolloutTx.Builder.Args.Last)] =
        for {
            config <- genTxConfig()
            payouts <- Gen.nonEmptyListOf(genPayoutObligationL1(config.env.network))
        } yield (
          RolloutTx.Builder.Last(config),
          RolloutTx.Builder.Args.Last(NonEmptyVector.fromVectorUnsafe(payouts.toVector))
        )
    
    property("Build Last Rollout Tx Partial Result")(
      Prop.forAll(genLastBuilder) { (builder, args) =>
          builder.partialResult(args) match
              case Left(e)  => throw new RuntimeException(e.toString)
              case Right(_) => ()
      }
    )

    property("Post-process last rollout tx partial result")(???)
}
