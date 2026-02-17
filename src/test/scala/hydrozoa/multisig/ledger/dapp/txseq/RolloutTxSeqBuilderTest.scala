package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.config.node.TestNodeConfig
import hydrozoa.config.node.TestNodeConfig.generateTestNodeConfig
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.obligation.Payout
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import test.*
import test.Generators.Hydrozoa.*

object RolloutTxSeqBuilderTest extends Properties("RolloutTxSeq Builder") {

    val genBuilder: Gen[RolloutTxSeq.Build] =
        for {
            config <- generateTestNodeConfig
            payouts <- Gen
                .containerOfN[Vector, Payout.Obligation](
                  160,
                  genPayoutObligation(config.nodeConfig.cardanoNetwork)
                )
                .map(NonEmptyVector.fromVectorUnsafe)

        } yield RolloutTxSeq.Build(config.nodeConfig)(payouts)

    val _ = property("Build partial rollout seq") =
        Prop.forAll(genBuilder)(builder => Prop(builder.partialResult.isRight))

    val _ = property("Finish partial result rollout seq") = Prop.forAll(genBuilder)(builder =>
        val res = for {
            pr <- builder.partialResult
            txId = Arbitrary.arbitrary[TransactionHash].sample.get
            input = TransactionInput(txId, 0)
            output = Babbage(
              address = builder.config.headMultisigAddress,
              value = pr.firstOrOnly.inputValueNeeded
            )
            rolloutUtxo = RolloutUtxo(Utxo(input, output))
            res <- pr.finishPostProcess(rolloutUtxo)
        } yield res
        res match {
            case Left(e) => s"Finishing partial result failed: $e" |: Prop(false)
            case _       => Prop(true)
        }
    )
}
