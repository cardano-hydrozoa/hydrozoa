package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
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

                dappLedger <- right(
                  system.actorOfWithDebug(
                    new DappLedger(initTx.initializationTx.treasuryProduced, config) {}
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
                  refundValue = virtualOutputsValue + Value.ada(5),
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
                    && ("Inorrect deposit(s) in state" |: s.deposits.head._2 == depositRefundTxSeq.depositTx.depositProduced)
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
