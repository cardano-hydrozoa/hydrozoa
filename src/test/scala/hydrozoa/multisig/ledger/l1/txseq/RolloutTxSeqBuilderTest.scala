package hydrozoa.multisig.ledger.l1.txseq

import cats.data.NonEmptyVector
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.ledger.l1.utxo.RolloutUtxo
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other.genSequencedValueDistribution

object RolloutTxSeqBuilderTest extends Properties("RolloutTxSeq Builder") {

    val genBuilder: Gen[RolloutTxSeq.Build] =
        for {
            config <- MultiNodeConfig.generate(TestPeersSpec.default)()
            payouts <- genSequencedValueDistribution(
              160,
              v =>
                  genKnownValuePayoutObligationWithMinAdaEnsured(
                    config.headConfig.cardanoNetwork,
                    v
                  )
            )
                .map(nel => NonEmptyVector.fromVectorUnsafe(Vector.from(nel.toList)))

        } yield RolloutTxSeq.Build(config.headConfig)(payouts)

//    val _ = property("Build partial rollout seq") =
//        Prop.forAll(genBuilder)(builder => Prop(builder.partialResult.isRight))

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
