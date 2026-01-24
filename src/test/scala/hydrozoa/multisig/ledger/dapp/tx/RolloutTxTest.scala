package hydrozoa.multisig.ledger.dapp.tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import hydrozoa.multisig.ledger.joint.obligation.Payout
import org.scalacheck.*
import org.scalacheck.Gen.Parameters
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.addDummySignatures
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other as GenOther
import test.TestPeer.mkWallet

class RolloutTxTest extends AnyFunSuite with ScalaCheckPropertyChecks {
    private val genPayouts: Gen[Payout.Obligation] = genPayoutObligation(
      testTxBuilderCardanoInfo.network
    )

    val genLastBuilder: Gen[(RolloutTx.Builder.Last, RolloutTx.Builder.Args.Last)] =
        for {
            // We want to test small, medium, and large, so we do it with frequency
            payouts <-
                Gen.frequency(
                  (1, GenOther.nonEmptyVectorOf(genPayouts)),
                  (7, Gen.sized(size => GenOther.nonEmptyVectorOfN(size * 3 + 1, genPayouts))),
                  (2, Gen.sized(size => GenOther.nonEmptyVectorOfN(size * 6 + 1, genPayouts)))
                )
            peers <- genTestPeers()
            hms = HeadMultisigScript(peers.map(mkWallet(_).exportVerificationKeyBytes))
            mw <- genFakeMultisigWitnessUtxo(
              hms,
              testTxBuilderCardanoInfo.network
            )

        } yield (
          RolloutTx.Builder.Last(RolloutTx.Config(testTxBuilderCardanoInfo, mw, hms)),
          RolloutTx.Builder.Args.Last(payouts)
        )
    val genNotLastBuilder: Gen[(RolloutTx.Builder.NotLast, RolloutTx.Builder.Args.NotLast)] =
        for {
            payouts <- GenOther.nonEmptyVectorOf(genPayouts)
            rolloutSpentVal <- Arbitrary.arbitrary[Coin].map(Value(_))
            peers <- genTestPeers()
            hms = HeadMultisigScript(peers.map(mkWallet(_).exportVerificationKeyBytes))
            mw <- genFakeMultisigWitnessUtxo(
              hms,
              testTxBuilderCardanoInfo.network
            )
        } yield (
          RolloutTx.Builder.NotLast(RolloutTx.Config(testTxBuilderCardanoInfo, mw, hms)),
          RolloutTx.Builder.Args.NotLast(payouts, rolloutSpentVal)
        )

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 1)

    // FIXME: How to do this in ScalaTest?
    // override def scalaCheckInitialSeed = "espemfZrEC9vyjQ_8nzYe9Pikzd1423i2sM8_TQft9E="

    // ===================================
    // Last
    // ===================================
    test("Build Last Rollout Tx Partial Result") {
        {
            val gen = genLastBuilder.apply(Parameters.default, Seed.apply(3476397946951439811L)).get
            forAll(gen) { (builder, args) =>
                val pr = builder.partialResult(args)

                assert(pr.isRight, "Partial result didn't build successfully")

                val unsignedSize = pr.get.ctx.transaction.toCbor.length
                val withDummySigners = addDummySignatures(
                  pr.get.builder.config.headMultisigScript.numSigners,
                  pr.get.ctx.transaction
                )
                val signedSize = withDummySigners.toCbor.length

                val maxSize = builder.config.cardanoInfo.protocolParams.maxTxSize
                assert(
                  signedSize <= maxSize,
                  "\n\t\tPartial result size with dummy signatures is too big: " +
                      s" unsigned size: $unsignedSize; signed size: $signedSize; max size: $maxSize"
                )
            }
        }
    }

    test("Complete Last Partial Result") {
        forAll(genLastBuilder)((builder, args) => {
            val res = for {
                pr <- builder.partialResult(args)
                txId = Arbitrary.arbitrary[TransactionHash].sample.get
                input = TransactionInput(txId, 0)
                output = Babbage(address = builder.config.headAddress, value = pr.inputValueNeeded)
                rolloutUtxo = RolloutUtxo(Utxo(input, output))
                res <- pr.complete(rolloutUtxo)
            } yield res
            assert(res.isRight)
        })
    }

    // ===================================
    // Not Last
    // ===================================
    test("Build NotLast Partial Result")(
      forAll(genNotLastBuilder) { (builder, args) =>
          assert(builder.partialResult(args).isRight)
      }
    )

    // TODO: shall we add that?
    // ignore("Post-process last rollout tx partial result")(???)

    test("Build Last Rollout Tx") {
        forAll(genLastBuilder) { (builder, args) =>
            builder.partialResult(args) match
                case Left(e)  => fail(e.toString)
                case Right(_) => ()
        }
    }
}
