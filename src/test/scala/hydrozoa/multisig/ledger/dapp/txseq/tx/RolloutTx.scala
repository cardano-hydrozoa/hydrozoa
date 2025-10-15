package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.joint.utxo.Payout
import org.scalacheck.*
import hydrozoa.multisig.ledger.dapp.tx.ArbitraryInstances.given
import hydrozoa.multisig.ledger.dapp.tx.genFakeMultisigWitnessUtxo
import hydrozoa.multisig.ledger.dapp.txseq.tx.RolloutTx.Builder.Config
import test.*

class RolloutTxTest extends munit.ScalaCheckSuite {
    val genLastBuilder : Gen[RolloutTx.Builder.Last] =
        for {
            peers <- genTestPeers
            hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
            env = testTxBuilderEnvironment
            multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(hns, env.network)
            payouts <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Payout.Obligation.L1])
        } yield RolloutTx.Builder.Last(config = Config(
          headNativeScript = hns,
          headNativeScriptReferenceInput = multisigWitnessUtxo,
          env = env,
          validators = testValidators),
          payouts = Vector.from(payouts))

    property("Build Last Rollout Tx")(
      Prop.forAll(genLastBuilder){
         builder => builder.buildPartial() match
           case Left(e) => throw new RuntimeException(e.toString)
           case Right(_) => ()
      }
    )
}
