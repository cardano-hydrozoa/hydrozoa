package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.obligation.old.Payout
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{TransactionHash, TransactionInput}
import scalus.cardano.txbuilder.TransactionUnspentOutput
import test.*
import test.Generators.Hydrozoa.*

class RolloutTxSeqBuilderTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    val genBuilder: Gen[(RolloutTxSeq.Builder, NonEmptyVector[Payout.Obligation.L1])] =
        for {
            (config, _) <- genTxBuilderConfigAndPeers()
            payouts <- Gen
                .containerOfN[Vector, Payout.Obligation.L1](
                  160,
                  genPayoutObligationL1(config.env.network)
                )
                .map(NonEmptyVector.fromVectorUnsafe)
        } yield (RolloutTxSeq.Builder(config), payouts)

    test("Build partial rollout seq") {
        forAll(genBuilder)((builder, payouts) => assert(builder.buildPartial(payouts).isRight))
    }

    test("Finish partial result rollout seq") {
        forAll(genBuilder)((builder, payouts) =>
            val res = for {
                pr <- builder.buildPartial(payouts)
                txId = Arbitrary.arbitrary[TransactionHash].sample.get
                input = TransactionInput(txId, 0)
                output = Babbage(
                  address = builder.config.headAddress,
                  value = pr.firstOrOnly.inputValueNeeded
                )
                rolloutUtxo = RolloutUtxo(TransactionUnspentOutput(input, output))
                res <- pr.finishPostProcess(rolloutUtxo)
            } yield res
            res match {
                case Left(e) => fail(e.toString())
                case _       => ()
            }
        )
    }
}
