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
import hydrozoa.multisig.ledger.dapp.tx.{DepositTx, FallbackTx, Tx, TxTiming, genFinalizationTxSeqBuilder, genNextSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, InitializationTxSeq, InitializationTxSeqTest, RolloutTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.protocol.CardanoBackendProtocol.CardanoBackend.{GetCardanoHeadState, GetCardanoHeadStateResp, GetTxInfo, GetTxInfoResp, Request, SubmitL1Effects}
import hydrozoa.multisig.protocol.ConsensusProtocol.{ConfirmFinalBlock, ConfirmMajorBlock}
import hydrozoa.multisig.protocol.types.{Block, LedgerEvent}
import java.util.concurrent.TimeoutException
import org.scalacheck.*
import org.scalacheck.Gen.{choose, tailRecM}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.rng.Seed
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
        p
            .withWorkers(1) // useful for debugging
            .withMinSuccessfulTests(100)
            .withInitialSeed(Seed.fromBase64("8czA3wXox-sglWTZMEUIycey4uTcf53dVpyUFswYW4A=").get)
    }

    // TODO: use BlockEffects as BlockEffectChain
    // TODO: use (SettlementTxSeq, FallbackTx) not Result
    type Skeleton = (InitializationTxSeq, List[SettlementTxSeq.Builder.Result], FinalizationTxSeq)

    object Skeleton:

        /** Generates the "skeleton", i.e. random chain of happy-path transactions and fallbacks
          * with requested parameters.
          */
        def genL1BlockEffectsChain(
            minSettlements: Int = 5,
            maxSettlements: Int = 25,
            txTiming: TxTiming = TxTiming.default
        ): Gen[Skeleton] = for {
            // init args
            initArgs <- InitializationTxSeqTest.genArgs(txTiming)
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
            initializationTreasuryProduced = initializationTxSeq.initializationTx.treasuryProduced
            initializationFallbackValidityStart = testTxBuilderEnvironment.slotConfig.slotToTime(
              initializationTxSeq.fallbackTx.validityStart.slot
            )

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
              (
                initializationTreasuryProduced,
                initializationFallbackValidityStart,
                List.empty: List[SettlementTxSeq.Builder.Result],
                1
              )
            ) { case (treasuryToSpend, fallbackValidityStart, acc, settlementNum) =>
                if settlementNum >= numOfSettlements
                then Gen.const(Right(acc.reverse))
                else
                    for {
                        settlementBuilderAndArgs <- genNextSettlementTxSeqBuilder(
                          treasuryToSpend,
                          fallbackValidityStart,
                          settlementNum,
                          hns,
                          config,
                          txTiming
                        )
                        (builder, args) = settlementBuilderAndArgs
                        seq = builder.build(args) match {
                            case Left(err)  => throw RuntimeException(err.toString)
                            case Right(seq) => seq
                        }
                        nextTreasuryToSpend = seq.settlementTxSeq.settlementTx.treasuryProduced
                        fallbackValidityStart = testTxBuilderEnvironment.slotConfig.slotToTime(
                          seq.fallbackTx.validityStart.slot
                        )

                    } yield Left(
                      (nextTreasuryToSpend, fallbackValidityStart, seq :: acc, settlementNum + 1)
                    )
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

                def settlementTxs(r: SettlementTxSeq.Builder.Result): List[Transaction] =
                    r.settlementTxSeq match {
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

            def fallbackTxFor(backbonePoint: TransactionHash): Option[FallbackTx] = {
                backboneTxs
                    .map(_.id)
                    .zip(
                      // There is no (None) fallback txs for the initialization tx and the deinit tx
                      List(None, Some(skeleton._1.fallbackTx)) ++ skeleton._2.map(node =>
                          Some(node.fallbackTx)
                      ) ++ List(None)
                    )
                    .find(_._1 == backbonePoint)
                    .flatMap(_._2)
            }

            // The common structure for the initialization tx, settlement txs, and the finalization tx
            // with the linked rollouts (where applicable,otherwise None)
            def backboneNodesWithRollouts = {
                (skeleton._1.initializationTx.tx.id -> List.empty)
                    +:
                        skeleton._2
                            .map(node =>
                                node.settlementTxSeq.settlementTx.tx.id -> node.settlementTxSeq.mbRolloutSeq
                                    .map(mbRollouts)
                                    .getOrElse(List.empty)
                            )
                        :+ skeleton._3.finalizationTx.tx.id -> skeleton._3.mbRolloutSeq
                            .map(mbRollouts)
                            .getOrElse(List.empty)
            }

            def rolloutSeqsBefore(
                backbonePoint: TransactionHash
            ): List[List[Transaction]] =
                backboneNodesWithRollouts
                    .takeWhile(_._1 != backbonePoint)
                    .filter(_._2.nonEmpty)
                    .map(_._2)

            def rolloutSeqsStarting(
                backbonePoint: TransactionHash
            ): List[List[Transaction]] =
                backboneNodesWithRollouts
                    .dropWhile(_._1 != backbonePoint)
                    .filter(_._2.nonEmpty)
                    .map(_._2)

            def dumpSkeletonInfo: Unit =
                // dump some info
                println(s"initialization  tx: ${skeleton._1.initializationTx.tx.id}")
                println(s"fallback tx: ${skeleton._1.fallbackTx.tx.id}")
                skeleton._2.foreach(settlement =>
                    settlement.settlementTxSeq match {
                        case SettlementTxSeq.NoRollouts(settlementTx) =>
                            println(s"settlementTx: ${settlementTx.tx.id}")
                            println(s"fallbackTx: ${settlement.fallbackTx.tx.id}")
                        case SettlementTxSeq.WithRollouts(settlementTx, rolloutTxSeq) =>
                            println(
                              s"settlementTx: ${settlementTx.tx.id}, rollouts: ${rolloutTxSeq.notLast.length + 1}" +
                                  s" (${rolloutTxSeq.notLast.map(_.tx.id)} and ${rolloutTxSeq.last.tx.id})"
                            )
                            println(s"fallbackTx: ${settlement.fallbackTx.tx.id}")
                    }
                )
                skeleton._3 match {
                    case FinalizationTxSeq.Monolithic(finalizationTx) =>
                        println(s"monolithic finalization tx: ${finalizationTx.tx.id}")
                    case FinalizationTxSeq.WithDeinit(finalizationTx, deinitTx) =>
                        println(s"finalization tx: ${finalizationTx.tx.id}")
                        println(s"deinit tx: ${deinitTx.tx.id}")
                    case FinalizationTxSeq.WithRollouts(finalizationTx, rolloutTxSeq) =>
                        println(
                          s"finalizationTx: ${finalizationTx.tx.id}, rollouts: ${rolloutTxSeq.notLast.length + 1}"
                        )
                    case FinalizationTxSeq.WithDeinitAndRollouts(
                          finalizationTx,
                          deinitTx,
                          rolloutTxSeq
                        ) =>
                        println(s"finalization tx: ${finalizationTx.tx.id}")
                        println(
                          s"finalizationTx: ${finalizationTx.tx.id}, rollouts: ${rolloutTxSeq.notLast.length + 1}"
                        )
                        println(s"deinit tx: ${deinitTx.tx.id}")
                }

    end Skeleton

    object Rollback:

        /** Utxo ids + transaction partitions (survived/lost). */
        final case class Rollback(
            /** IDs of utxos that exist immediately after the rollback. */
            utxoIds: Set[UtxoIdL1],
            // NB: this field can be used for debugging purposes, it's not used directly in the tests
            txsSurvived: Set[TransactionHash],
            txsLost: Set[TransactionHash],
            // The slot number (as of rollback is completed)
            currentSlot: Slot,
            // The tx id of the first possible fallback transaction. It's not defined for the deinit tx.
            mbFallbackTx: Option[TransactionHash],
            fallbackIsValid: Boolean
        )

        def genRollback(skeleton: Skeleton): Gen[Rollback] = for {
            backboneTxs <- Gen.const(skeleton.backboneTxs.map(_.id))
            // The node in the backbone with the leading rolled back backbone tx
            backbonePoint <- Gen.oneOf(backboneTxs)
            // Survived rollout sequences
            rolloutSeqsSurvived = skeleton.rolloutSeqsBefore(backbonePoint).map(_.map(_.id))
            // Generate rollback points for evert survived sequence
            // Why does it default to java ArrayList?
            rolloutSeqPoints <- Gen
                .sequence(rolloutSeqsSurvived.map(xs => Gen.oneOf(xs)))
                .map(_.asScala.toList)
            // The depth of the rollback in slots, for now:
            //  - 0 indicates it's an instant rollback, the happy path is expected to be submitted
            //  - 100 indicates this is a long-living rollback, the fallback tx is expected to be submitted
            currentSlot <- Gen.oneOf(Slot(0), Slot(100))
        } yield mkRollback(skeleton, backbonePoint, rolloutSeqPoints, currentSlot)

        def mkRollback(
            skeleton: Skeleton,
            backbonePoint: TransactionHash,
            rolloutPoints: List[TransactionHash],
            currentSlot: Slot
        ): Rollback = {
            // Backbone
            val backboneTxs1 = skeleton.backboneTxs
            val backboneIndex = backboneTxs1.indexWhere(_.id == backbonePoint)
            val (backboneSurvived, backboneLost) = backboneTxs1.splitAt(backboneIndex)

            // Rollouts lost due to te backbone point
            val rolloutLost = skeleton.rolloutSeqsStarting(backbonePoint).flatten

            // Rolled back rollouts
            val allRolloutSeqs = skeleton.rolloutSeqsBefore(backbonePoint)
            val rolloutIndices =
                allRolloutSeqs.zip(rolloutPoints).map(e => e._1.indexWhere(_.id == e._2))
            val rolloutTxsPartitions = allRolloutSeqs
                .zip(rolloutIndices)
                .map(e => e._1.splitAt(e._2))

            // Txs at the verge: these are needed to build the utxo state
            val edgeTxs = backboneLost.head +: rolloutTxsPartitions.map(_._2.head)
            // println(s"edge txs: ${edgeTxs.map(_.id)}")
            // Fallback tx
            val mbFallbackTx = skeleton.fallbackTxFor(backbonePoint)
            // Result
            Rollback(
              utxoIds = edgeTxs.flatMap(_.body.value.inputs.toSet.toList).map(UtxoIdL1.apply).toSet,
              txsSurvived =
                  (backboneSurvived ++ rolloutTxsPartitions.flatMap(_._1)).map(_.id).toSet,
              txsLost = (
                backboneLost
                    ++ rolloutLost
                    ++ rolloutTxsPartitions.flatMap(_._2)
              ).map(_.id).toSet,
              currentSlot = currentSlot,
              mbFallbackTx = mbFallbackTx.map(_.tx.id),
              fallbackIsValid = mbFallbackTx.fold(false)(currentSlot.slot >= _.validityStart.slot)
            )
        }

    // val _ = property("Fish skeleton gets generated") = {
    //   val sample = skeletonGen().sample.get
    //   dumpSkeletonInfo(sample)
    //   Prop.proved
    // }

    // Test skeletons against which the (multiple) properties are checked.
    // TODO: figure out why this fails with (1,1)
    // TODO: it's really slow, benchmark to see why
    val testSkeletons: Seq[Skeleton] =
        List(
          (3, 5), // short skeleton for fast feedback loop
          (20, 20) // more realistic skeleton
        ).flatMap((min, max) => Skeleton.genL1BlockEffectsChain(min, max).sample)

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
                      IO.pure("not all happy path txs are submitted" |: false)
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
            // TODO: constant
            _ <- r.handleRequest(_ => IO.pure(GetTxInfoResp(false)))
        } yield ()

    def mkRollbackAreHandled(skeleton: Skeleton): Prop = {

        dumpSkeletonInfo(skeleton)

        forAll(Rollback.genRollback(skeleton)) { rollback =>

            println(
              s"Rollback is (utxos: ${rollback.utxoIds.size}, " +
                  s"\nsurvived: ${rollback.txsSurvived}, " +
                  s"\nlost: ${rollback.txsLost}" +
                  s"\ncurrent slot: ${rollback.currentSlot}" +
                  s"\nfallbackTx: ${rollback.mbFallbackTx}" +
                  s"\nfallbackIsValid: ${rollback.fallbackIsValid}"
            )

            val result: EitherT[IO, String, Prop] = for {
                system <- right(ActorSystem[IO]("Cardano Liaison SUT").allocated.map(_._1))
                    .leftMap(_.toString)

                cardanoBackendMock = TestCardanoBackendWithRollbacks(
                  rollback.utxoIds,
                  rollback.currentSlot
                )

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
                  // TODO: uncomment if you want to see all msgs passing
                  // system.actorOfWithDebug(
                  system.actorOf(cardanoLiaison)
                )

                expectMsg = expectMsgPF(
                  actor = cardanoBackendMockActor,
                  timeout = 10.seconds
                )

                check <- right(
                  (expectMsg {
                      case m: SubmitL1Effects
                          if m.txs.toSet.map(_.id) == (if rollback.fallbackIsValid then
                                                           Set(rollback.mbFallbackTx.get)
                                                       else rollback.txsLost) =>
                          ()
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

    class TestCardanoBackendWithRollbacks(utxoIds: Set[UtxoIdL1], currentSlot: Slot)
        extends CardanoBackend:

        override def receive: Receive[IO, Request] = {
            case r: SubmitL1Effects     => submitL1Effects(r)
            case r: GetCardanoHeadState => getCardanoHeadState(r)
            case r: GetTxInfo           => getTxInfo(r)
        }

        private def submitL1Effects(r: SubmitL1Effects): IO[Unit] =
            IO.println(s"submitL1Effects: ${r.txs.map(_.id)}")

        private def getCardanoHeadState(r: GetCardanoHeadState): IO[Unit] = for {
            _ <- IO.println("getCardanoHeadState")
            _ <- r.dResponse.complete(Right(GetCardanoHeadStateResp(utxoIds, currentSlot)))
        } yield ()

        private def getTxInfo(r: GetTxInfo): IO[Unit] = for {
            _ <- IO.println("getTxInfo")
            // _ <- IO.raiseError(RuntimeException("should not be called in this property"))
        } yield ()

    val _ = property("DappLedger Register Deposit Happy Path") = {
        forAll(InitializationTxSeqTest.genArgs()) { (args, testPeers) =>
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
