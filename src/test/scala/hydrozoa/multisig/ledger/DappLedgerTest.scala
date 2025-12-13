package hydrozoa.multisig.ledger

import cats.*
import cats.data.EitherT.right
import cats.data.{EitherT, NonEmptyList}
import cats.effect.unsafe.implicits.*
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.suprnation.actor.Actor.Receive
import com.suprnation.actor.test.TestKit
import com.suprnation.actor.{ActorSystem, test as _}
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.UtxoIdL1
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.CardanoLiaison
import hydrozoa.multisig.ledger.DappLedger.Requests.{GetState, RegisterDeposit}
import hydrozoa.multisig.ledger.DappLedgerTest.Skeleton.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
import hydrozoa.multisig.ledger.dapp.tx.{DepositTx, Tx, genFinalizationTxSeqBuilder, genNextSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq.Builder
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, InitializationTxSeq, InitializationTxSeqTest, RolloutTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.protocol.CardanoBackendProtocol.CardanoBackend.{GetCardanoHeadState, GetCardanoHeadStateResp, GetTxInfo, GetTxInfoResp, Request, SubmitL1Effects}
import hydrozoa.multisig.protocol.ConsensusProtocol.{ConfirmFinalBlock, ConfirmMajorBlock}
import hydrozoa.multisig.protocol.types.{Block, LedgerEvent}
import java.util.concurrent.TimeoutException
import org.scalacheck.*
import org.scalacheck.Gen.{choose, tailRecM}
import org.scalacheck.Prop.{forAll, propBoolean}
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.concurrent.duration.DurationInt
import scalus.cardano.address.Network.Testnet
import scalus.cardano.ledger.*
import scalus.prelude.Option as SOption
import scalus.testing.kit.TestUtil
import scalus.testing.kit.TestUtil.testEnvironment
import test.Generators.Hydrozoa.*
import test.{TestPeer, nonSigningValidators, signTx, testEvaluator, testTxBuilderEnvironment}

object DappLedgerTest extends Properties("DappLedger"), TestKit {

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p.withMinSuccessfulTests(5)
    }

    type Skeleton = (InitializationTxSeq, List[Builder.Result], FinalizationTxSeq)

    object Skeleton:

        def skeletonGen(
            minSettlements: Int = 5,
            maxSettlements: Int = 25
        ): Gen[(InitializationTxSeq, List[Builder.Result], FinalizationTxSeq)] = for {
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

            numOfSettlements <- choose(minSettlements, maxSettlements)

            settlementTxSeqs <- tailRecM(
              (initialTreasuryToSpend, List.empty: List[SettlementTxSeq.Builder.Result], 1)
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
                        seq = builder.build(args) match {
                            case Left(err)  => throw RuntimeException(err.toString)
                            case Right(seq) => seq
                        }
                        nextTreasuryToSpend = seq.settlementTxSeq.settlementTx.treasuryProduced
                    } yield Left((nextTreasuryToSpend, seq :: acc, settlementNum + 1))
            }

            // Finalization seq
            lastSettlementTreasury =
                settlementTxSeqs.last.settlementTxSeq.settlementTx.treasuryProduced

            finalizationTxSeqBuilderAndArgs <- genFinalizationTxSeqBuilder(
              lastSettlementTreasury,
              numOfSettlements + 1,
              config,
              peers
            )
            (builder, fArgs) = finalizationTxSeqBuilderAndArgs
            finalizationTxSeq = builder.build(fArgs)

        } yield (
          initializationTxSeq,
          settlementTxSeqs,
          finalizationTxSeq.getOrElse(???)
        )

        def mbRollouts(rolloutTxSeq: RolloutTxSeq): List[Transaction] =
            rolloutTxSeq.notLast.map(_.tx).toList :+ rolloutTxSeq.last.tx

        extension (skeleton: Skeleton)
            def happyPathTxs: Set[Transaction] = {

                def settlementTxs(r: Builder.Result): List[Transaction] = r.settlementTxSeq match {
                    case SettlementTxSeq.NoRollouts(settlementTx) => List(settlementTx.tx)
                    case SettlementTxSeq.WithRollouts(settlementTx, rolloutTxSeq) =>
                        settlementTx.tx +: mbRollouts(rolloutTxSeq)
                }

                def finalizationTxs(seq: FinalizationTxSeq): List[Transaction] = seq match {
                    case FinalizationTxSeq.Monolithic(finalizationTx) =>
                        List(finalizationTx.tx)
                    case FinalizationTxSeq.WithDeinit(finalizationTx, deinitTx) =>
                        List(finalizationTx.tx, deinitTx.tx)
                    case FinalizationTxSeq.WithRollouts(finalizationTx, rolloutTxSeq) =>
                        finalizationTx.tx +: mbRollouts(rolloutTxSeq)
                    case FinalizationTxSeq.WithDeinitAndRollouts(
                          finalizationTx,
                          deinitTx,
                          rolloutTxSeq
                        ) =>
                        finalizationTx.tx +: mbRollouts(rolloutTxSeq) :+ deinitTx.tx
                }

                (
                  (skeleton._1.initializationTx.tx
                      +: skeleton._2.flatMap(settlementTxs))
                      ++ finalizationTxs(skeleton._3)
                ).toSet
            }

            /** The list of backbone transaction hashes in the skeleton. */
            def backboneTxs: List[Transaction] =
                (skeleton._1.initializationTx.tx +: skeleton._2.map(
                  _.settlementTxSeq.settlementTx.tx
                )) ++
                    {
                        skeleton._3 match {
                            case FinalizationTxSeq.Monolithic(finalizationTx) =>
                                List(finalizationTx.tx)
                            case FinalizationTxSeq.WithDeinit(finalizationTx, deinitTx) =>
                                List(finalizationTx.tx, deinitTx.tx)
                            case FinalizationTxSeq.WithRollouts(finalizationTx, _) =>
                                List(finalizationTx.tx)
                            case FinalizationTxSeq.WithDeinitAndRollouts(
                                  finalizationTx,
                                  deinitTx,
                                  _
                                ) =>
                                List(finalizationTx.tx, deinitTx.tx)
                        }
                    }

            def rolloutSeqsBefore(
                mbBackbonePoint: Option[TransactionHash] = None
            ): List[List[Transaction]] = {
                // common structure for settlements and finalization
                val nodesWithRollouts =
                    skeleton._2
                        .map(node =>
                            node.settlementTxSeq.settlementTx.tx.id -> node.settlementTxSeq.mbRolloutSeq
                                .map(mbRollouts)
                                .getOrElse(List.empty)
                        )
                        :+ skeleton._3.finalizationTx.tx.id -> skeleton._3.mbRolloutSeq
                            .map(mbRollouts)
                            .getOrElse(List.empty)

                mbBackbonePoint match {
                    case Some(point) => nodesWithRollouts.takeWhile(_._1 != point).map(_._2)
                    case None        => nodesWithRollouts.map(_._2)
                }

            }

            def mkRollback(
                backbonePoint: TransactionHash,
                rolloutPoints: List[TransactionHash]
            ): Rollback = {
                // Backbone
                val backboneTxs1 = backboneTxs
                val backboneIndex = backboneTxs1.indexWhere(_.id == backbonePoint)
                // val backboneTx = backboneTxs1(backboneIndex)
                val (backboneSurvived, backboneLost) = backboneTxs1
                    .splitAt(backboneIndex)
                // Rollouts
                val allRolloutSeqs = rolloutSeqsBefore(Some(backbonePoint))
                val rolloutIndices =
                    allRolloutSeqs.zip(rolloutPoints).map(e => e._1.indexWhere(_.id == e._2))
                val rolloutTxsPartitions = allRolloutSeqs
                    .zip(rolloutIndices)
                    .map(e => e._1.splitAt(e._2))
                //
                val edgeTxs = backboneLost.head +: rolloutTxsPartitions.map(_._2.head)
                Rollback(
                  utxoIds =
                      edgeTxs.flatMap(_.body.value.inputs.toSet.toList).map(UtxoIdL1.apply).toSet,
                  txsSurvived =
                      (backboneSurvived ++ rolloutTxsPartitions.flatMap(_._1)).map(_.id).toSet,
                  txsLost = (backboneLost ++ rolloutTxsPartitions.flatMap(_._2)).map(_.id).toSet
                )
            }

        /** Generate the (sub)state of utxos that represent a L1 rollback:
          *   - picks random node in the backbone, adds the inputs of the settlemnt
          *
          * @param skeleton
          * @return
          */

        /** Utxo ids + transaction partitions (survived/lost). */
        final case class Rollback(
            /** IDs of utxos that exist immedaitely after the rollback. */
            utxoIds: Set[UtxoIdL1],
            txsSurvived: Set[TransactionHash],
            txsLost: Set[TransactionHash]
        )

        def genRollback(skeleton: Skeleton): Gen[Rollback] = for {
            backboneTxs <- Gen.const(skeleton.backboneTxs.map(_.id))
            // The node in the backbone with the leading rolled back backbone tx
            backbonePoint <- Gen.oneOf(backboneTxs)
            // Survived rollout sequences and their rollback points
            rolloutSeqsSurvived = skeleton.rolloutSeqsBefore(Some(backbonePoint)).map(_.map(_.id))
            // Why does it default to java ArrayList?
            rolloutSeqPoints <- Gen
                .sequence(rolloutSeqsSurvived.filter(_.nonEmpty).map(xs => Gen.oneOf(xs)))
                .map(_.asScala.toList)
        } yield skeleton.mkRollback(backbonePoint, rolloutSeqPoints)

    end Skeleton

    // val _ = property("Fish skeleton gets generated") = {
    //   val sample = skeletonGen().sample.get
    //   dumpSkeletonInfo(sample)
    //   Prop.proved
    // }

    def dumpSkeletonInfo(sample: Skeleton): Unit =
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
            case FinalizationTxSeq.WithRollouts(finalizationTx, rolloutTxSeq) =>
                println(
                  s"finalizationTx: ${finalizationTx.tx.id}, rollouts: ${rolloutTxSeq.notLast.length + 1}"
                )
            case FinalizationTxSeq.WithDeinitAndRollouts(finalizationTx, deinitTx, rolloutTxSeq) =>
                println(s"finalization tx: ${finalizationTx.tx.id}")
                println(
                  s"finalizationTx: ${finalizationTx.tx.id}, rollouts: ${rolloutTxSeq.notLast.length + 1}"
                )
                println(s"deinit tx: ${deinitTx.tx.id}")
        }

    // val _ = property("forced multiple inner tests") = forAll { (outer: Int) =>
    //    // Create inner property
    //    val innerProp = forAll { (inner: Int) => outer + inner == inner + outer}
    //
    //    // Run it multiple times manually
    //    val result = Test.check(
    //        Test.Parameters.default.withMinSuccessfulTests(20),
    //        innerProp
    //    )
    //
    //    // Return success/failure
    //    result.passed
    // }

    // val outerValues = List(0, 1, 10, 100, 1000) ++
    //    (1 to 10).flatMap(_ => Gen.choose(0, 10000).sample)
    //
    // outerValues.distinct.zipWithIndex.foreach { case (outer, idx) =>
    //
    //    val innerProp = forAll { (inner: Int) =>
    //        val sum = outer + inner
    //        val product = outer * inner
    //
    //        Prop.all(
    //          s"commutative addition" |: (outer + inner == inner + outer),
    //          s"product commutative" |: (outer * inner == inner * outer)
    //        )
    //    }
    //
    //    include(new Properties(s"outer_${idx}_val_$outer") {
    //        val _ = property("inner_test") = innerProp
    //    })
    // }

    // Test skeletons against which the (multiple) properties are checked.
    // TODO: figure out why this fails with (1,1)
    val testSkeletons: Seq[(InitializationTxSeq, List[Builder.Result], FinalizationTxSeq)] =
        // List((2, 2), (2, 5), (10, 10))
        List((2, 2))
            .flatMap((min, max) => Skeleton.skeletonGen(min, max).sample)

    testSkeletons.distinct.zipWithIndex.foreach { case (skeleton, idx) =>
        include(new Properties(s"Skeleton ${idx}") {
            // val _ = property("head is initialized") = mkInitHappensProperty(skeleton)
            val _ = property("rollbacks are handled") = mkRollbackAreHandled(skeleton)
        })
    }

    def mkInitHappensProperty(skeleton: Skeleton): Prop = {

        dumpSkeletonInfo(skeleton)

        val result: EitherT[IO, String, Prop] = for {
            system <- right(ActorSystem[IO]("Cardano Liaison SUT").allocated.map(_._1))
                .leftMap(_.toString)

            cardanoBackendMock <- right(
              system.actorOf(TestCardanoBackend.trackWithCache("tracked cardano backend"))
            )

            config = CardanoLiaison.Config(
              cardanoBackendMock,
              skeleton._1.initializationTx,
              skeleton._1.fallbackTx
            )

            cardanoLiaison <- right(
              // TODO: uncomment if you want to see all passed msgs
              // system.actorOfWithDebug(
              system.actorOf(
                new CardanoLiaison(config) {}
              )
            )

            _ <- right(
              skeleton._2.traverse_(s =>
                  cardanoLiaison ! ConfirmMajorBlock(
                    Block.Number(s.settlementTxSeq.settlementTx.majorVersionProduced),
                    s.settlementTxSeq,
                    s.fallbackTx
                  )
              )
            )

            _ <- right(
              cardanoLiaison ! ConfirmFinalBlock(
                Block.Number(skeleton._3.finalizationTx.majorVersionProduced),
                skeleton._3
              )
            )

            expectMsg = expectMsgPF(
              actor = cardanoBackendMock,
              timeout = 25.seconds
            )

            _ <- right(expectMsg { case m: GetCardanoHeadState => () })

            _ <- right(IO.println(s"happy path txs: ${skeleton.happyPathTxs.map(_.id)}"))

            check <- right(
              (expectMsg {
                  case m: SubmitL1Effects if m.txs.toSet == happyPathTxs(skeleton) => ()
              } >> IO.pure(Prop.proved))
                  .handleErrorWith { case _: TimeoutException =>
                      IO.pure("custom msg" |: false)
                  }
            )

        } yield check

        result.value.unsafeRunSync() match {
            case Left(e)     => s"init happens property failed: $e" |: false
            case Right(prop) => prop
        }
    }

    object TestCardanoBackend extends CardanoBackend:
        override def receive: Receive[IO, Request] = {
            case r: SubmitL1Effects     => submitL1Effects(r)
            case r: GetCardanoHeadState => getCardanoHeadState(r)
            case r: GetTxInfo           => getTxInfo(r)
        }

        private def submitL1Effects(r: SubmitL1Effects): IO[Unit] =
            IO.println(s"submitL1Effects: ${r.txs.map(_.id)}")

        private def getCardanoHeadState(r: GetCardanoHeadState): IO[Unit] = for {
            _ <- IO.println("getCardanoHeadState")
            _ <- r.dResponse.complete(Right(GetCardanoHeadStateResp(Set.empty, Slot(0))))
        } yield ()

        private def getTxInfo(r: GetTxInfo): IO[Unit] = for {
            _ <- IO.println("getTxInfo")
            _ <- r.handleRequest(_ => IO.pure(GetTxInfoResp(false)))
        } yield ()

    def mkRollbackAreHandled(skeleton: Skeleton): Prop = {

        dumpSkeletonInfo(skeleton)

        forAll(genRollback(skeleton)) { rollback =>

            println(
              s"Rollback is (utxos: ${rollback.utxoIds.size}, " +
                  s"survived: ${rollback.txsSurvived}, " +
                  s"lost: ${rollback.txsLost}"
            )

            val result: EitherT[IO, String, Prop] = for {
                system <- right(ActorSystem[IO]("Cardano Liaison SUT").allocated.map(_._1))
                    .leftMap(_.toString)

                cardanoBackendMock = TestCardanoBackendWithRollbacks(rollback.utxoIds)

                cardanoBackendMockActor <- right(
                  system.actorOf(
                    cardanoBackendMock.trackWithCache("cardano-backend-mock")
                  )
                )

                config = CardanoLiaison.Config(
                  cardanoBackendMockActor,
                  skeleton._1.initializationTx,
                  skeleton._1.fallbackTx
                )

                cardanoLiaison = new CardanoLiaison(config) {}

                _ <- right(
                  skeleton._2.traverse_(s =>
                      cardanoLiaison.handleMajorBlockL1Effects(
                        ConfirmMajorBlock(
                          Block.Number(s.settlementTxSeq.settlementTx.majorVersionProduced),
                          s.settlementTxSeq,
                          s.fallbackTx
                        )
                      )
                  )
                )

                _ <- right(
                  cardanoLiaison.handleFinalBlockL1Effects(
                    ConfirmFinalBlock(
                      Block.Number(skeleton._3.finalizationTx.majorVersionProduced),
                      skeleton._3
                    )
                  )
                )

                cardanoLiaisonActor <- right(
                  // TODO: uncomment if you want to see all passed msgs
                  // system.actorOfWithDebug(
                  system.actorOf(cardanoLiaison)
                )

                expectMsg = expectMsgPF(
                  actor = cardanoBackendMockActor,
                  timeout = 10.seconds
                )

                _ <- right(expectMsg { case m: GetCardanoHeadState => () })

                check <- right(
                  (expectMsg {
                      case m: SubmitL1Effects if m.txs.toSet.map(_.id) == rollback.txsLost => ()
                  } >> IO.pure(Prop.proved))
                      .handleErrorWith { case _: TimeoutException =>
                          IO.pure("not all txs that were rolled back are submitted" |: false)
                      }
                )

            } yield check

            result.value.unsafeRunSync() match {
                case Left(e)     => s"init happens property failed: $e" |: false
                case Right(prop) => prop
            }
        }
    }

    class TestCardanoBackendWithRollbacks(utxoIds: Set[UtxoIdL1]) extends CardanoBackend:

        override def receive: Receive[IO, Request] = {
            case r: SubmitL1Effects     => submitL1Effects(r)
            case r: GetCardanoHeadState => getCardanoHeadState(r)
            case r: GetTxInfo           => getTxInfo(r)
        }

        private def submitL1Effects(r: SubmitL1Effects): IO[Unit] =
            IO.println(s"submitL1Effects: ${r.txs.map(_.id)}")

        private def getCardanoHeadState(r: GetCardanoHeadState): IO[Unit] = for {
            _ <- IO.println("getCardanoHeadState")
            _ <- r.dResponse.complete(Right(GetCardanoHeadStateResp(utxoIds, Slot(0))))
        } yield ()

        private def getTxInfo(r: GetTxInfo): IO[Unit] = for {
            _ <- IO.println("getTxInfo")
            // TODO: should be found by ScalaCheck, we need to know whether the initialization was rolled back or not.
            _ <- r.handleRequest(_ => IO.pure(GetTxInfoResp(false)))
        } yield ()

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

                _ <- EitherT.right(
                  dappLedger ! RegisterDeposit(serializedDeposit, LedgerEvent.Id(0, 1))
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
