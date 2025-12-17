package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import com.suprnation.actor.{ActorSystem, test as _}
import hydrozoa.multisig.ledger.DappLedger.Requests.{GetState, RegisterDeposit}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.ledger.dapp.txseq.{DepositRefundTxSeq, InitializationTxSeq, InitializationTxSeqTest}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import hydrozoa.multisig.protocol.types.LedgerEvent
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import org.scalacheck.rng.Seed
import scalus.cardano.ledger.*
import scalus.prelude.Option as SOption
import scalus.testing.kit.TestUtil
import test.Generators.Hydrozoa.*
import test.{TestPeer, nonSigningValidators, signTx, testEvaluator, testNetwork}

object DappLedgerTest extends Properties("DappLedger") {

    import EitherT.*
    import test.lib.PropertyM.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withMinSuccessfulTests(100)
            .withInitialSeed(Seed.fromBase64("W28rrQBwU4e2me7TydWPZDGl22_0duuU4iuVz5Y6QxN=").get)
    }

    // Accept "any" as the error
    def runner(mProp: EitherT[IO, Any, Prop]): Prop =
        Prop.secure(mProp.value.unsafeRunSync() match {
            case Left(e)  => s"Failed: $e" |: false
            case Right(p) => p
        })

    // FIXME(?): I used "Any" for the error type here, simply because I was too lazy to accumulate all of the
    // possible errors. Maybe it should be "Throwable"?
    type ET[A] = EitherT[IO, Any, A]

    val _ = property("DappLedger Register Deposit Happy Path") = monadic(
      runner = runner,
      m = for {
          argsAndPeers <- pick[ET, (InitializationTxSeq.Builder.Args, NonEmptyList[TestPeer])](
            InitializationTxSeqTest.genArgs
          )
          args = argsAndPeers._1
          peer = argsAndPeers._2.head
          hns = HeadMultisigScript(argsAndPeers._2.map(_.wallet.exportVerificationKeyBytes))

          system <- run(right(ActorSystem[IO]("DappLedger").allocated.map(_._1)))
          initTx <- run(fromEither[IO](InitializationTxSeq.Builder.build(args)))

          config = Tx.Builder.Config(
            headNativeScript = hns,
            multisigRegimeUtxo = initTx.initializationTx.multisigRegimeWitness,
            tokenNames = initTx.initializationTx.tokenNames,
            env = TestUtil.testEnvironment,
            evaluator = testEvaluator,
            validators = nonSigningValidators
          )

          virtualLedgerConfig = VirtualLedger.Config(
            slotConfig = config.env.slotConfig,
            slot = 0,
            protocolParams = config.env.protocolParams,
            network = testNetwork
          )
          virtualLedger <- run(right(system.actorOf(VirtualLedger(virtualLedgerConfig))))

          dappLedger <- run(
            right(
              system.actorOf(
                new DappLedger(initTx.initializationTx.treasuryProduced, config, virtualLedger) {}
              )
            )
          )

          virtualOutputs <-
              pick[ET, NonEmptyList[GenesisObligation]](
                Gen.nonEmptyListOf(genGenesisObligation(peer, minimumCoin = Coin.ada(5)))
                    .map(NonEmptyList.fromListUnsafe)
              )

          virtualOutputsValue = Value.combine(
            virtualOutputs.map(vo => Value(vo.l2OutputValue)).toList
          )

          utxosFundingTail <- pick[ET, List[Utxo]](
            Gen
                .listOf(
                  genAdaOnlyPubKeyUtxo(peer, minimumCoin = Coin.ada(5))
                )
          )
          utxosFundingHead <- pick[ET, Utxo](
            genAdaOnlyPubKeyUtxo(
              peer,
              minimumCoin = virtualOutputsValue.coin
            )
          )

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

          depositRefundTxSeq <- run(EitherT.fromEither[IO](depositRefundSeqBuilder.build))

          signedTx = signTx(peer, depositRefundTxSeq.depositTx.tx)

          serializedDeposit = signedTx.toCbor
          req <- run(
            EitherT.right(
              RegisterDeposit(
                serializedDeposit = serializedDeposit,
                virtualOutputs = virtualOutputs,
                eventId = LedgerEvent.Id(0, 1)
              )
            )
          )

          _ <- run(EitherT.right(dappLedger ! req))
          stateReq <- run(right(GetState()))
          s <- run(EitherT(dappLedger ?: stateReq))

          _ <- assertWith[ET](
            msg = s"We should only have 1 deposit in the state, but we have ${s.deposits.length}",
            condition = s.deposits.length == 1
          )
          _ <- assertWith[ET](
            msg = "Incorrect deposit(s) in state",
            condition = s.deposits.head._2 == depositRefundTxSeq.depositTx.depositProduced
          )
          _ <- assertWith[ET](
            msg = "Incorrect treasury in state",
            condition = s.treasury == initTx.initializationTx.treasuryProduced
          )
      } yield true
    )
}
