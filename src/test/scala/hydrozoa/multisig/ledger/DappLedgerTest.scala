//package hydrozoa.multisig.ledger
//
//import cats.*
//import cats.data.*
//import cats.effect.*
//import cats.effect.unsafe.implicits.*
//import cats.syntax.all.*
//import com.suprnation.actor.ActorSystem
//import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
//import hydrozoa.multisig.ledger.dapp.tx.Tx
//import hydrozoa.multisig.ledger.dapp.txseq.{InitializationTxSeq, InitializationTxSeqTest}
//import org.scalacheck.*
//import org.scalacheck.Prop.propBoolean
//import scalus.testing.kit.TestUtil
//import test.*
//import com.suprnation.actor.*
//import com.suprnation.typelevel.actors.syntax.*
//import hydrozoa.multisig.ledger.virtual.genDepositFromPeer
//
//object DappLedgerTest extends Properties("DappLedger") {
//
//    import Prop.forAll
//    import EitherT.{right, left}
//
//    val _ = property("DappLedger Register Deposit Happy Path") = {
//        forAll(InitializationTxSeqTest.genArgs) { (args, testPeers) =>
//
//                val eitherT = for {
//                    debugMessage <- right(Ref.of[IO, Option[String]](None))
//
//                    system <-
//                        right(ActorSystem[IO]("DappLedger", (event : Any) => debugMessage.set(Some(event.toString)))
//                            .allocated.map(_._1))
//
//                    initTx: InitializationTxSeq <- EitherT.fromEither[IO](
//                      InitializationTxSeq.Builder.build(args))
//
//                    config = Tx.Builder.Config(
//                        headNativeScript = HeadMultisigScript(testPeers.map(_.wallet.exportVerificationKeyBytes)),
//                        headNativeScriptReferenceInput = initTx.initializationTx.multisigRegimeWitness,
//                        tokenNames = initTx.initializationTx.tokenNames,
//                        env = TestUtil.testEnvironment,
//                        evaluator = testEvaluator,
//                        validators = testValidators)
//
//                    dappLedger <- right(
//                        system.actorOfWithDebug(new DappLedger(initTx.initializationTx.treasuryProduced, config) {}))
//
//                    deposit = genDepositFromPeer
//
//                    _ <- dappLedger
//
//                } yield IO.pure(())
//                eitherT.value
//            )
//
//
//            // We could also use https://github.com/typelevel/scalacheck-effect
//            "Test" |: res.unsafeRunSync().isRight
//        }
//    }
//}
//
///*
//forAll(gen1)(arg1 => forAll(gen2)(arg2 => ???))
//
//for {
//  arg1 <- gen1
//  arg2 <- gen2
//} yield
//
//*/
