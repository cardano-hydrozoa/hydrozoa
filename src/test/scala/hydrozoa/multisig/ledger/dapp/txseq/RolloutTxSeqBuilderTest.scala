package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.utxo.Payout
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionUnspentOutput
import scalus.cardano.ledger.{TransactionHash, TransactionInput}
import test.*
import test.Generators.Hydrozoa.*

class RolloutTxSeqBuilderTest extends AnyFunSuite {

    val genBuilder: Gen[(RolloutTxSeq.Builder, NonEmptyVector[Payout.Obligation.L1])] =
        for {
            config <- genTxConfig()
            payouts <- Gen
                .containerOfN[Vector, Payout.Obligation.L1](
                  160,
                  genPayoutObligationL1(config.env.network)
                )
                .map(NonEmptyVector.fromVectorUnsafe)
        } yield (RolloutTxSeq.Builder(config), payouts)

    test("Build partial rollout seq")({
        Prop.forAll(genBuilder)((builder, payouts) => builder.buildPartial(payouts).toProp)
    })

    test("Finish partial result rollout seq")({
        Prop.forAll(genBuilder)((builder, payouts) =>
            (for {
                pr <- builder.buildPartial(payouts)
                txId = Arbitrary.arbitrary[TransactionHash].sample.get
                input = TransactionInput(txId, 0)
                output = Babbage(
                  address = builder.config.headAddress,
                  value = pr.firstOrOnly.inputValueNeeded
                )
                rolloutUtxo = RolloutUtxo(TransactionUnspentOutput(input, output))
                res <- pr.finishPostProcess(rolloutUtxo)
            } yield res).toProp
        )
    })
}
