package hydrozoa.multisig.ledger.dapp.txseq.tx

import cats.data.NonEmptyVector
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.{genFakeMultisigWitnessUtxo, genPayoutObligationL1}
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder.Config
import org.scalacheck.*
import test.*

class RolloutTxTest extends munit.ScalaCheckSuite {
    val genLastBuilder : Gen[RolloutTx.Builder.Last] =
        for {
            peers <- genTestPeers
            hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
            env = testTxBuilderEnvironment
            multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(hns, env.network)
            payouts <- Gen.nonEmptyListOf(genPayoutObligationL1(env.network))
        } yield RolloutTx.Builder.Last(config = Config(
          headNativeScript = hns,
          headNativeScriptReferenceInput = multisigWitnessUtxo,
          env = env,
          validators = testValidators),
            payouts = NonEmptyVector.fromVectorUnsafe(payouts.toVector))

    property("Build Last Rollout Tx")(
      Prop.forAll(genLastBuilder){
         builder => builder.buildPartial() match
           case Left(e) => throw new RuntimeException(e.toString)
           case Right(_) => ()
      }
    )
}
