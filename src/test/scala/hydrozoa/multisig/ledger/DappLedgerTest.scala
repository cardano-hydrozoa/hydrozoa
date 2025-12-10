package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import com.suprnation.actor.{ActorSystem, test as _, *}
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.multisig.ledger.DappLedger.Requests.{GetState, RegisterDeposit}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.{DepositTx, Tx}
import hydrozoa.multisig.ledger.dapp.txseq.{InitializationTxSeq, InitializationTxSeqTest}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.protocol.types.LedgerEvent
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import scalus.cardano.address.Network.Testnet
import scalus.cardano.ledger.*
import scalus.prelude.Option as SOption
import scalus.testing.kit.TestUtil
import scalus.testing.kit.TestUtil.testEnvironment
import test.Generators.Hydrozoa.*
import test.{TestPeer, nonSigningValidators, signTx, testEvaluator}

object DappLedgerTest extends Properties("DappLedger") {

    import EitherT.right
    import Prop.forAll

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p.withMinSuccessfulTests(100)
    }

    val _ = property("DappLedger Register Deposit Happy Path") = {
        forAll(InitializationTxSeqTest.genArgs) { (args, testPeers) =>
            val peer: TestPeer = testPeers.head
            val hns = HeadMultisigScript(testPeers.map(_.wallet.exportVerificationKeyBytes))

            val eitherT: EitherT[IO, String, Prop] = for {
                debugMessage <- right(Ref.of[IO, Option[String]](None))

                system <-
                    right(
                      ActorSystem[IO](
                        "DappLedger",
                        (event: Any) => debugMessage.set(Some(event.toString))
                      ).allocated.map(_._1)
                    ).leftMap(_.toString)

                initTx: InitializationTxSeq <- EitherT.fromEither[IO](
                  InitializationTxSeq.Builder.build(args).leftMap(_.toString)
                )

                config = Tx.Builder.Config(
                  headNativeScript = hns,
                  headNativeScriptReferenceInput = initTx.initializationTx.multisigRegimeWitness,
                  tokenNames = initTx.initializationTx.tokenNames,
                  env = TestUtil.testEnvironment,
                  evaluator = testEvaluator,
                  validators = nonSigningValidators
                )

                dappLedger <- right(
                  system.actorOfWithDebug(
                    new DappLedger(initTx.initializationTx.treasuryProduced, config) {}
                  )
                )

                utxosFunding = Gen
                    .nonEmptyListOf(
                      genAdaOnlyPubKeyUtxo(peer, genCoinWithMinimum = Some(Coin.ada(5)))
                    )
                    .sample
                    .get

                peerCredential = LedgerToPlutusTranslation.getCredential(
                  Credential.KeyHash(AddrKeyHash(peer.address().payment.asHash))
                )
                depositRecipe = DepositTx.Recipe(
                  depositAmount = Coin(utxosFunding.map(_._2.value.coin.value).sum / 2),
                  datum = DepositUtxo.Datum(
                    address = peerCredential,
                    datum = SOption.None,
                    deadline = 0,
                    refundAddress = LedgerToPlutusTranslation.getAddress(peer.address()),
                    refundDatum = SOption.None
                  ),
                  headAddress = hns.mkAddress(Testnet),
                  utxosFunding = NonEmptyList.fromListUnsafe(utxosFunding),
                  changeAddress = peer.address(),
                  network = Testnet,
                  protocolParams = testEnvironment.protocolParams,
                  evaluator = testEvaluator,
                  validators = nonSigningValidators
                )

                depositTx <- EitherT.fromEither(DepositTx.build(depositRecipe).leftMap(_.toString))
                signedTx = signTx(peer, depositTx.tx)
                serializedDeposit = signedTx.toCbor
                req <- EitherT.right(RegisterDeposit(serializedDeposit, LedgerEvent.Id(0, 1)))

                _ <- EitherT.right(
                  dappLedger ! req
                )

                stateReq <- right(GetState()).leftMap(_.toString)
                s <- EitherT(dappLedger ?: stateReq).leftMap(_.toString)

                prop = {
                    (s"We should only have 1 deposit in the state, but we have ${s.deposits.length}"
                        |: s.deposits.length == 1)
                    && ("Inorrect deposit(s) in state" |: s.deposits.head._2 == depositTx.depositProduced)
                    && ("Incorrect treasury in state" |: s.treasury == initTx.initializationTx.treasuryProduced)

                }
            } yield prop
            eitherT.value.unsafeRunSync() match {
                case Left(e)  => s"register deposit happy path failed: $e" |: false
                case Right(p) => p
            }
        }
    }
}
