package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyVector
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
import hydrozoa.multisig.ledger.dapp.tx.{genFakeMultisigWitnessUtxo, genPayoutObligationL1}
import org.scalacheck.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import test.*

class RolloutTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {
    val genLastBuilder: Gen[(RolloutTx.Builder.Last, RolloutTx.Builder.Args.Last)] =
        for {
            peers <- genTestPeers
            hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
            env = testTxBuilderEnvironment
            multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(hns, env.network)
            payouts <- Gen.nonEmptyListOf(genPayoutObligationL1(env.network))
        } yield (
          RolloutTx.Builder.Last(
            Config(
              headNativeScript = hns,
              headNativeScriptReferenceInput = multisigWitnessUtxo,
              env = env,
              validators = testValidators
            )
          ),
          RolloutTx.Builder.Args.Last(NonEmptyVector.fromVectorUnsafe(payouts.toVector))
        )

    test("Build Last Rollout Tx") {
        forAll(genLastBuilder) { (builder, args) =>
            builder.partialResult(args) match
                case Left(e)  => fail(e.toString)
                case Right(_) => ()
        }
    }
}
