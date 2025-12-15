package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import cats.implicits.catsSyntaxFlatMapOps
import com.suprnation.actor.{ActorSystem, test as _}
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.multisig.ledger.DappLedger.Requests.{GetState, RegisterDeposit}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.ledger.dapp.txseq.{DepositRefundTxSeq, InitializationTxSeq, InitializationTxSeqTest}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.protocol.types.LedgerEvent
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import scalus.cardano.ledger.*
import scalus.prelude.Option as SOption
import scalus.testing.kit.TestUtil
import test.Generators.Hydrozoa.*
import test.{TestPeer, nonSigningValidators, signTx, testEvaluator}
import org.scalacheck.effect.PropF
import org.scalacheck.Test
import cats.effect.{ExitCode, IO, IOApp}
import org.scalacheck.rng.Seed

object Example extends IOApp {
    def run(args: List[String]): IO[ExitCode] = {
        val p: PropF[IO] =
            for {
                res <- PropF.forAllF { (x: Int) =>
                    IO(x).map(res => assert(res == x))
                }
            } yield res

        val result: IO[Test.Result] = p.check()

        result.flatMap(r => IO(println(r))).as(ExitCode.Success)
    }
}

/*
genProp :: Gen[Prop]


forall(gen1)(args1 =>
  (some IO)
  args2 = gen2(args1).sample.get
  (some more IO)
  prop
)


// This works
forAllM(gen1)(args1 =>
  (some IO)
  args2 <- pick(gen2(args1))
  (some more IO)
  prop
  (gen3)(...)
      forall(gen4)(
  )
)

do
  result1 <- run(prop1)
  args <- pick(gen2(result1)


 */

object DappLedgerTest extends Properties("DappLedger") {

    import EitherT.right
    import Prop.forAll

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withMinSuccessfulTests(100)
            .withInitialSeed(Seed.fromBase64("W28rrQBwU4e2me7TydWPZDGl22_0duuU4iuVz5Y6QxN=").get)
    }

    val _ = property("DappLedger Register Deposit Happy Path") = {
        forAll(InitializationTxSeqTest.genArgs) { (args, testPeers) =>
            val peer: TestPeer = testPeers.head
            val hns = HeadMultisigScript(testPeers.map(_.wallet.exportVerificationKeyBytes))

            // Making the error type "Any" because we end up needing to combine a bunch of different errors together.
            // Is there a better way?
            val eitherT: EitherT[IO, Any, Prop] = for {

                system <-
                    right(
                      ActorSystem[IO](
                        "DappLedger"
                      ).allocated.map(_._1)
                    )

                initTx: InitializationTxSeq <- EitherT.fromEither[IO](
                  InitializationTxSeq.Builder.build(args)
                )

                config = Tx.Builder.Config(
                  headNativeScript = hns,
                  headNativeScriptReferenceInput = initTx.initializationTx.multisigRegimeWitness,
                  tokenNames = initTx.initializationTx.tokenNames,
                  env = TestUtil.testEnvironment,
                  evaluator = testEvaluator,
                  validators = nonSigningValidators
                )

                virtualLedgerConfig = VirtualLedger.Config(
                  slotConfig = config.env.slotConfig,
                  slot = 0L,
                  protocolParams = config.env.protocolParams,
                  network = config.env.network
                )
                virtualLedger <- right(
                  VirtualLedger(virtualLedgerConfig) >>= (vl => system.actorOf(vl))
                )
                dappLedger <- right(
                  system.actorOfWithDebug(
                    new DappLedger(
                      initTx.initializationTx.treasuryProduced,
                      config,
                      virtualLedger
                    ) {}
                  )
                )

                virtualOutputs = NonEmptyList.fromListUnsafe(
                  Gen.nonEmptyListOf(genGenesisObligation(peer, minimumCoin = Coin.ada(5)))
                      .sample
                      .get
                )
                virtualOutputsValue = Value.combine(
                  virtualOutputs.map(vo => Value(vo.l2OutputValue)).toList
                )

                utxosFundingTail = Gen
                    .listOf(
                      genAdaOnlyPubKeyUtxo(peer, minimumCoin = Coin.ada(5))
                    )
                    .sample
                    .get
                utxosFundingHead = genAdaOnlyPubKeyUtxo(
                  peer,
                  minimumCoin = virtualOutputsValue.coin
                ).sample.get

                utxosFunding = NonEmptyList(utxosFundingHead, utxosFundingTail)

                utxosFundingValue = Value.combine(utxosFunding.toList.map(_._2.value))

                depositRefundSeqBuilder = DepositRefundTxSeq.Builder(
                  config = config,
                  refundInstructions = DepositUtxo.Refund.Instructions(
                    LedgerToPlutusTranslation.getAddress(peer.address()),
                    SOption.None,
                    100
                  ),
                  donationToTreasury = Coin.zero,
                  refundValue = virtualOutputsValue,
                  virtualOutputs = virtualOutputs,
                  changeAddress = peer.address(),
                  utxosFunding = utxosFunding
                )

                depositRefundTxSeq <- EitherT.fromEither[IO](depositRefundSeqBuilder.build)

                signedTx = signTx(peer, depositRefundTxSeq.depositTx.tx)

                serializedDeposit = signedTx.toCbor
                req <- EitherT.right(
                  RegisterDeposit(
                    serializedDeposit = serializedDeposit,
                    virtualOutputs = virtualOutputs,
                    eventId = LedgerEvent.Id(0, 1)
                  )
                )

                _ <- EitherT.right(
                  dappLedger ! req
                )

                stateReq <- right(GetState())
                s <- EitherT(dappLedger ?: stateReq)

                prop = {
                    (s"We should only have 1 deposit in the state, but we have ${s.deposits.length}"
                        |: s.deposits.length == 1)
                    && ("Incorrect deposit(s) in state" |: s.deposits.head._2 == depositRefundTxSeq.depositTx.depositProduced)
                    && ("Incorrect treasury in state" |: s.treasury == initTx.initializationTx.treasuryProduced)

                }
            } yield prop
            eitherT.value.unsafeRunSync() match {
                case Left(e) => {
                    val error =
                        e // only putting this here because I can't figure out how to get the debugger to
                    // break on the error otherwise
                    s"register deposit happy path failed: $error" |: false
                }
                case Right(p) => p
            }
        }
    }
}
