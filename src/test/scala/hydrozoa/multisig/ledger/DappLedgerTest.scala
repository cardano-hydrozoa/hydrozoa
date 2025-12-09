package hydrozoa.multisig.ledger

import cats.*
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import com.suprnation.actor.test as _
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, InitializationTx, Tx, genFinalizationTxSeqBuilder, genNextSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, InitializationTxSeq, InitializationTxSeqTest, SettlementTxSeq}
import org.scalacheck.*
import org.scalacheck.Gen.{choose, tailRecM}
import scalus.cardano.ledger.*
import test.Generators.Hydrozoa.*
import test.{TestPeer, nonSigningValidators, testEvaluator, testTxBuilderEnvironment}

object DappLedgerTest extends Properties("DappLedger") {

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p.withMinSuccessfulTests(100)
    }

    final case class Fish(
        initializationTx: InitializationTx,
        initializationFallbackTx: FallbackTx,
        settlementTxSeq: SettlementTxSeq
    )

    val _ = property("Fish skeleton gets generated") = {
        val skeletonGen = for {
            // init args
            initArgs <- InitializationTxSeqTest.genArgs
            (args, peers) = initArgs
            peer: TestPeer = peers.head
            hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))

            // init tx
            initializationTxSeq = InitializationTxSeq.Builder
                .build(args)
                .leftMap(_.toString)
                .getOrElse(???)

            // multisig regime utxo
            tokenNames = TokenNames(args.spentUtxos.seedUtxo.input)
            multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(
              hns,
              testTxBuilderEnvironment.network,
              Some(tokenNames.multisigRegimeTokenName)
            )

            // Settlements
            initialTreasuryToSpend = initializationTxSeq.initializationTx.treasuryProduced
            config = Config(
              headNativeScript = hns,
              headNativeScriptReferenceInput = multisigWitnessUtxo,
              tokenNames = tokenNames,
              env = testTxBuilderEnvironment,
              evaluator = testEvaluator,
              validators = nonSigningValidators
            )

            numOfSettlements <- choose(10, 25)

            settlementTxSeqs <- tailRecM(
              (initialTreasuryToSpend, List.empty : List[SettlementTxSeq.Builder.Result], 1)
            ) { case (treasuryToSpend, acc, settlementNum) =>
                if settlementNum >= numOfSettlements
                then Gen.const(Right(acc.reverse))
                else
                    for {
                        settlementBuilderAndArgs <- genNextSettlementTxSeqBuilder(
                          treasuryToSpend,
                          settlementNum,
                          hns,
                          config
                        )
                        (builder, args) = settlementBuilderAndArgs
                        seq = builder.build(args).getOrElse(???)
                        nextTreasuryToSpend = seq.settlementTxSeq.settlementTx.treasuryProduced
                    } yield Left((nextTreasuryToSpend, seq :: acc, settlementNum + 1))
            }

            // Finalization seq
            lastSettlementTreasury = settlementTxSeqs.last.settlementTxSeq.settlementTx.treasuryProduced

            finalizationTxSeqBuilderAndArgs
                <- genFinalizationTxSeqBuilder(lastSettlementTreasury, numOfSettlements + 1, config, peers)
            (builder, fArgs) = finalizationTxSeqBuilderAndArgs
            finalizationTxSeq = builder.build(fArgs)

        } yield (
          initializationTxSeq,
          settlementTxSeqs,
          finalizationTxSeq.getOrElse(???)
        )

        val sample = skeletonGen.sample.get

        // dump some info
        println(s"initialization  tx: ${sample._1.initializationTx.tx.id}")
        println(s"fallback tx: ${sample._1.fallbackTx.tx.id}")
        sample._2.foreach(settlement =>
            settlement.settlementTxSeq match {
                case SettlementTxSeq.NoRollouts(settlementTx) =>
                    println(s"settlementTx: ${settlementTx.tx.id}")
                    println(s"fallbackTx: ${settlement.fallbackTx.tx.id}")
                case SettlementTxSeq.WithRollouts(settlementTx, rolloutTxSeq) =>
                    println(
                      s"settlementTx: ${settlementTx.tx.id}, rollouts: ${rolloutTxSeq.notLast.length + 1}"
                    )
                    println(s"fallbackTx: ${settlement.fallbackTx.tx.id}")

            }
        )
        sample._3 match {
            case FinalizationTxSeq.Monolithic(finalizationTx) =>
                println(s"monolithic finalization tx: ${finalizationTx.tx.id}")
            case FinalizationTxSeq.WithDeinit(finalizationTx, deinitTx) =>
                println(s"finalization tx: ${finalizationTx.tx.id}")
                println(s"deinit tx: ${deinitTx.tx.id}")
            case FinalizationTxSeq.WithRollouts(finalizationTx, rolloutTxSeq) => ???
                println(s"finalizationTx: ${finalizationTx.tx.id}, rollouts: ${rolloutTxSeq.notLast.length + 1}")
            case FinalizationTxSeq.WithDeinitAndRollouts(finalizationTx, deinitTx, rolloutTxSeq) => ???
                println(s"finalization tx: ${finalizationTx.tx.id}")
                println(s"finalizationTx: ${finalizationTx.tx.id}, rollouts: ${rolloutTxSeq.notLast.length + 1}")
                println(s"deinit tx: ${deinitTx.tx.id}")
        }
        // FIXME
        Prop.proved
    }

    // val _ = property("DappLedger Register Deposit Happy Path") = {
    //    forAll(InitializationTxSeqTest.genArgs) { (args, testPeers) =>
    //        val peer: TestPeer = testPeers.head
    //        val hns = HeadMultisigScript(testPeers.map(_.wallet.exportVerificationKeyBytes))
    //
    //        val eitherT: EitherT[IO, String, Prop] = for {
    //            debugMessage <- right(Ref.of[IO, Option[String]](None))
    //
    //            system <-
    //                right(
    //                  ActorSystem[IO](
    //                    "DappLedger",
    //                    (event: Any) => debugMessage.set(Some(event.toString))
    //                  ).allocated.map(_._1)
    //                ).leftMap(_.toString)
    //
    //            initTx: InitializationTxSeq <- EitherT.fromEither[IO](
    //              InitializationTxSeq.Builder.build(args).leftMap(_.toString)
    //            )
    //
    //            config = Tx.Builder.Config(
    //              headNativeScript = hns,
    //              headNativeScriptReferenceInput = initTx.initializationTx.multisigRegimeWitness,
    //              tokenNames = initTx.initializationTx.tokenNames,
    //              env = TestUtil.testEnvironment,
    //              evaluator = testEvaluator,
    //              validators = nonSigningValidators
    //            )
    //
    //            dappLedger <- right(
    //              system.actorOfWithDebug(
    //                new DappLedger(initTx.initializationTx.treasuryProduced, config) {}
    //              )
    //            )
    //
    //            utxosFunding = Gen
    //                .nonEmptyListOf(
    //                  genAdaOnlyPubKeyUtxo(peer, genCoinWithMinimum = Some(Coin.ada(5)))
    //                )
    //                .sample
    //                .get
    //
    //            peerCredential = LedgerToPlutusTranslation.getCredential(
    //              Credential.KeyHash(AddrKeyHash(peer.address().payment.asHash))
    //            )
    //            depositRecipe = DepositTx.Recipe(
    //              depositAmount = Coin(utxosFunding.map(_._2.value.coin.value).sum / 2),
    //              datum = DepositUtxo.Datum(
    //                address = peerCredential,
    //                datum = SOption.None,
    //                deadline = 0,
    //                refundAddress = LedgerToPlutusTranslation.getAddress(peer.address()),
    //                refundDatum = SOption.None
    //              ),
    //              headAddress = hns.mkAddress(Testnet),
    //              utxosFunding = NonEmptyList.fromListUnsafe(utxosFunding),
    //              changeAddress = peer.address(),
    //              network = Testnet,
    //              protocolParams = testEnvironment.protocolParams,
    //              evaluator = testEvaluator,
    //              validators = nonSigningValidators
    //            )
    //
    //            depositTx <- EitherT.fromEither(DepositTx.build(depositRecipe).leftMap(_.toString))
    //
    //            signedTx = signTx(peer, depositTx.tx)
    //
    //            serializedDeposit = signedTx.toCbor
    //
    //            _ <- EitherT.right(
    //              dappLedger ! RegisterDeposit(serializedDeposit, LedgerEvent.Id(0, 1))
    //            )
    //
    //            stateReq <- right(GetState()).leftMap(_.toString)
    //            s <- EitherT(dappLedger ?: stateReq).leftMap(_.toString)
    //
    //            prop = {
    //                (s"We should only have 1 deposit in the state, but we have ${s.deposits.length}"
    //                    |: s.deposits.length == 1)
    //                && ("Inorrect deposit(s) in state" |: s.deposits.head._2 == depositTx.depositProduced)
    //                && ("Incorrect treasury in state" |: s.treasury == initTx.initializationTx.treasuryProduced)
    //
    //            }
    //        } yield prop
    //        eitherT.value.unsafeRunSync() match {
    //            case Left(e)  => s"register deposit happy path failed: $e" |: false
    //            case Right(p) => p
    //        }
    //    }
    // }
}
