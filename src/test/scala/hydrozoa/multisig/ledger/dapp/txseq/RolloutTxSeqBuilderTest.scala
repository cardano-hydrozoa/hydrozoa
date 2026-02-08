package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.obligation.Payout
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{TransactionHash, TransactionInput, Utxo}
import test.*
import test.Generators.Hydrozoa.*
import test.TestPeer.mkWallet

class RolloutTxSeqBuilderTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    val genBuilder: Gen[(RolloutTxSeq.Build, NonEmptyVector[Payout.Obligation])] =
        for {
            payouts <- Gen
                .containerOfN[Vector, Payout.Obligation](
                  160,
                  genPayoutObligation(testTxBuilderCardanoInfo.network)
                )
                .map(NonEmptyVector.fromVectorUnsafe)
            peers <- genTestPeers()
            hms = HeadMultisigScript(peers.map(mkWallet(_).exportVerificationKeyBytes))
            mw <- genFakeMultisigWitnessUtxo(
              hms,
              testTxBuilderCardanoInfo.network
            )
        } yield (
          RolloutTxSeq.Build(RolloutTxSeq.Config(testTxBuilderCardanoInfo, mw, hms)),
          payouts
        )

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
                  address = builder.rolloutTxConfig.headAddress,
                  value = pr.firstOrOnly.inputValueNeeded
                )
                rolloutUtxo = RolloutUtxo(Utxo(input, output))
                res <- pr.finishPostProcess(rolloutUtxo)
            } yield res
            res match {
                case Left(e) => fail(e.toString())
                case _       => ()
            }
        )
    }
}
