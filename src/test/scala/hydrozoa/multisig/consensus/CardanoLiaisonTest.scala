package hydrozoa.multisig.consensus

import cats.*
import cats.data.{EitherT, NonEmptyList}
import cats.effect.unsafe.implicits.*
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.suprnation.actor.Actor.Receive
import com.suprnation.actor.test.TestKit
import com.suprnation.actor.{ActorSystem, test as _}
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.UtxoIdL1
import hydrozoa.lib.actor.SyncRequest
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.CardanoLiaisonTest.Rollback.SettlementTiming
import hydrozoa.multisig.consensus.CardanoLiaisonTest.Skeleton.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
import hydrozoa.multisig.ledger.dapp.tx.TxTiming.*
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, Tx, TxTiming, genFinalizationTxSeqBuilder, genNextSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, InitializationTxSeq, InitializationTxSeqTest, RolloutTxSeq, SettlementTxSeq}
import hydrozoa.multisig.protocol.CardanoBackendProtocol.CardanoBackend.{CardanoBackendError, GetCardanoHeadState, GetTxInfo, Request, SubmitL1Effects}
import hydrozoa.multisig.protocol.ConsensusProtocol.{ConfirmFinalBlock, ConfirmMajorBlock}
import hydrozoa.multisig.protocol.types.Block
import org.scalacheck.*
import org.scalacheck.Gen.{choose, tailRecM}
import org.scalacheck.Prop.forAll
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.concurrent.duration.DurationInt
import scalus.cardano.ledger.*
import test.Generators.Hydrozoa.*
import test.{TestPeer, testTxBuilderEnvironment}

object CardanoLiaisonTest extends Properties("Cardano Liaison"), TestKit {

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withWorkers(1) // useful for debugging
            .withMinSuccessfulTests(100)
        // .withInitialSeed(Seed.fromBase64("SEAube5f5bBsTlLeYTuh3am1vO5iJvmzNRFEXSOlXlD=").get)
    }

    // TODO: use BlockEffects as BlockEffectChain
    // TODO: use (SettlementTxSeq, FallbackTx) not Result
    type Skeleton = (InitializationTxSeq, List[SettlementTxSeq.Builder.Result], FinalizationTxSeq)

    object Skeleton:

        // TODO: figure out why this fails with (minSettlements=1, maxSettlements=1)
        // TODO: this gives no result sometimes
        // TODO: this is always very slow
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
                .fold(e => throw RuntimeException(e.toString), x => x)

            // multisig regime utxo
            tokenNames = TokenNames(args.spentUtxos.seedUtxo.input)
            multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(
              hns,
              testTxBuilderEnvironment.network,
              Some(tokenNames.multisigRegimeTokenName)
            )

            // Settlements
            initializationTx = initializationTxSeq.initializationTx
            initializationTreasuryProduced = initializationTx.treasuryProduced
            initializationFallbackValidityStart = initializationTxSeq.fallbackTx.validityStart

            // This config is used in the settlement and finalization builders
            config = Config(
              headNativeScript = hns,
              multisigRegimeUtxo = initializationTx.multisigRegimeWitness,
              tokenNames = tokenNames,
              env = args.env,
              evaluator = args.evaluator,
              validators = args.validators
            )

            numOfSettlements <- choose(minSettlements, maxSettlements)

            settlements <- tailRecM(
              (
                initializationTreasuryProduced,
                args.initializedOn,
                initializationFallbackValidityStart,
                List.empty: List[SettlementTxSeq.Builder.Result],
                1
              )
            ) {
                case (
                      treasuryToSpend,
                      previousBlockTimestamp,
                      fallbackValidityStart,
                      acc,
                      settlementNum
                    ) =>
                    if settlementNum > numOfSettlements
                    then
                        Gen.const(
                          Right((acc.reverse, previousBlockTimestamp, fallbackValidityStart))
                        )
                    else
                        for {
                            // TODO: Implement Gen.Choose[Instant]
                            blockCreatedOn <- Gen.choose(
                              previousBlockTimestamp + 10.seconds,
                              fallbackValidityStart - (txTiming.silenceDuration + 10.seconds)
                            )
                            settlementBuilderAndArgs <- genNextSettlementTxSeqBuilder(
                              treasuryToSpend,
                              fallbackValidityStart,
                              blockCreatedOn,
                              settlementNum,
                              hns,
                              config,
                              txTiming
                            )
                            (builder, args) = settlementBuilderAndArgs
                            seq = builder
                                .build(args)
                                .fold(e => throw RuntimeException(e.toString), x => x)
                            nextTreasuryToSpend = seq.settlementTxSeq.settlementTx.treasuryProduced
                            fallbackValidityStart = seq.fallbackTx.validityStart

                        } yield Left(
                          (
                            nextTreasuryToSpend,
                            blockCreatedOn,
                            fallbackValidityStart,
                            seq :: acc,
                            settlementNum + 1
                          )
                        )
            }

            (settlementTxSeqs, lastSettlementBlockTimestamp, fallbackValidityStart) = settlements

            // Finalization seq
            lastSettlementTreasury =
                settlementTxSeqs.last.settlementTxSeq.settlementTx.treasuryProduced

            finalizationBlockCreatedOn <- Gen.choose(
              lastSettlementBlockTimestamp + 10.seconds,
              fallbackValidityStart - (txTiming.silenceDuration + 10.seconds)
            )

            finalizationTxSeqBuilderAndArgs <- genFinalizationTxSeqBuilder(
              lastSettlementTreasury,
              numOfSettlements + 1,
              fallbackValidityStart,
              finalizationBlockCreatedOn,
              txTiming,
              config,
              peers
            )
            (builder, fArgs) = finalizationTxSeqBuilderAndArgs
            finalizationTxSeq = builder.build(fArgs)

        } yield (
          initializationTxSeq,
          settlementTxSeqs,
          finalizationTxSeq.fold(e => throw RuntimeException(e.toString), x => x)
        )

        def mbRollouts(rolloutTxSeq: RolloutTxSeq): List[Transaction] =
            rolloutTxSeq.notLast.map(_.tx).toList :+ rolloutTxSeq.last.tx

        extension (skeleton: Skeleton)

            /** Returns all happypath txs of the skeleton, i.e. all but post dated fallback txs. */
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

            /** Returns the competing fallback transaction if it exists for a backbone node
              * identified by tx hash.
              *
              * @param backbonePoint
              *   a backbone tx hash
              * @return
              *   some for settlement/finalization txs, none fot initialization/deinit txs
              */
            def competingFallbackFor(backbonePoint: TransactionHash): Option[FallbackTx] = {
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
            private def backboneNodesWithRollouts = {
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

            def earliestTtl: Slot =
                // TODO: for now we use initialization tx ttl, see the comment on `ttl` field
                skeleton._1.initializationTx.validityEnd.toSlot(testTxBuilderEnvironment.slotConfig)

            def dumpSkeletonInfo: Unit =

                println("--------- Hydrozoa L1 effects chain sample --------- ")

                val initSeq = skeleton._1
                val initTx = initSeq.initializationTx

                println(s"Initialization  tx: ${initTx.tx.id}, TTL: ${initTx.validityEnd}")

                val fallbackTx = initSeq.fallbackTx
                println(
                  s"\t=> Fallback 0 tx: ${fallbackTx.tx.id}, validity start: ${fallbackTx.validityStart}"
                )

                // Settlements
                skeleton._2.foreach(settlement => {
                    val fallbackTx = settlement.fallbackTx
                    settlement.settlementTxSeq match {
                        case SettlementTxSeq.NoRollouts(settlementTx) => {
                            println(
                              s"Settlement ${settlementTx.majorVersionProduced}: " +
                                  s"${settlementTx.tx.id}, TTL: ${settlementTx.validityEnd}"
                            )
                            println(
                              s"\t=> Fallback ${settlementTx.majorVersionProduced}: " +
                                  s"${fallbackTx.tx.id}, validity start: ${fallbackTx.validityStart}"
                            )
                        }
                        case SettlementTxSeq.WithRollouts(settlementTx, rolloutTxSeq) =>
                            println(
                              s"Settlement ${settlementTx.majorVersionProduced}: " +
                                  s"${settlementTx.tx.id}, TTL: ${settlementTx.validityEnd}, " +
                                  s"deposits: ${settlementTx.depositsSpent.length}, " +
                                  s"rollouts: ${rolloutTxSeq.notLast.length + 1}"
                            )
                            val rollouts = rolloutTxSeq.notLast :+ rolloutTxSeq.last
                            rollouts.foreach(r => println(s"\t\t - rollout: ${r.tx.id}"))
                            println(
                              s"\t=> Fallback ${settlementTx.majorVersionProduced}: " +
                                  s"${fallbackTx.tx.id}, validity start: ${fallbackTx.validityStart}"
                            )
                    }
                })

                // Finalization
                skeleton._3 match {
                    case FinalizationTxSeq.Monolithic(finalizationTx) =>
                        println(
                          s"Monolithic finalization: ${finalizationTx.tx.id}, TTL: ${finalizationTx.validityEnd}"
                        )
                    case FinalizationTxSeq.WithDeinit(finalizationTx, deinitTx) =>
                        println(
                          s"Finalization: ${finalizationTx.tx.id}, TTL: ${finalizationTx.validityEnd}"
                        )
                        println(s"Deinit: ${deinitTx.tx.id}")
                    case FinalizationTxSeq.WithRollouts(finalizationTx, rolloutTxSeq) =>
                        println(
                          s"Finalization: ${finalizationTx.tx.id}, TTL: ${finalizationTx.validityEnd}" +
                              s", rollouts: ${rolloutTxSeq.notLast.length + 1}"
                        )
                        val rollouts = rolloutTxSeq.notLast :+ rolloutTxSeq.last
                        rollouts.foreach(r => println(s"\t\t - rollout: ${r.tx.id}"))
                    case FinalizationTxSeq.WithDeinitAndRollouts(
                          finalizationTx,
                          deinitTx,
                          rolloutTxSeq
                        ) =>
                        println(
                          s"Finalization: ${finalizationTx.tx.id}, TTL: ${finalizationTx.validityEnd}" +
                              s", rollouts: ${rolloutTxSeq.notLast.length + 1}"
                        )
                        val rollouts = rolloutTxSeq.notLast :+ rolloutTxSeq.last
                        rollouts.foreach(r => println(s"\t\t - rollout: ${r.tx.id}"))
                        println(s"Deinit: ${deinitTx.tx.id}")
                }
                println("--------- END of Hydrozoa L1 effects chain sample --------- ")

    end Skeleton

    object Rollback:

        /** The relation between [[Rollback.currentSlot]] end the settlement/finalization tx, may
          * absent if only deinit is rolled back or all transactions are rolled back.
          */
        enum SettlementTiming:
            case BeforeBackboneTtlExpires
            case WithinSilencePeriod
            case AfterFallbackBecomesValid

        /** Utxo ids + transaction partitions (survived/lost). */
        final case class Rollback(
            /** IDs of utxos that exist immediately after the rollback. */
            utxoIds: Set[UtxoIdL1],
            // NB: this field can be used for debugging purposes, it's not used directly in the tests
            txsSurvived: Set[TransactionHash],
            /** Lost backbone txs and rollout txs that sprout off them. */
            txsLost: Set[TransactionHash],
            /** Lost rollout txs that beside before the first rolled back backbone tx. */
            txsLostPreviousRollout: Set[TransactionHash],
            // The slot number (as of the rollback is completed)
            currentSlot: Slot,
            // The tx id of the competing fallback transaction and timing relation.
            // It's not defined if only deinit tx was rolled back or the whole skeleton
            // is rolled back.
            mbFallback: Option[(TransactionHash, SettlementTiming)]
        )

        def genRollback(skeleton: Skeleton): Gen[Rollback] =
            import SettlementTiming.*
            for {
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

                // The depth of the rollback in slots. There are several options possible:
                // - (1) We have a settlement/finalization on the verge of the rollback
                //   - (a) the current slot falls before backbone tx TTL expires -
                //         this is sort of a "quick" rollback
                //   - (b) the current slot falls exactly on the silence period
                //   - (c) the current slot falls after a competing fallback becomes valid -
                //         this is a long-running rollback
                //
                // - (2) The deinit tx is chosen as the backbone rollback point.
                //       The TTL and competing fallbacks are absent.
                //       It means we can pick up an arbitrary slot.
                //
                // - (3) The whole skeleton was rolled back.
                //       We can choose a slot around initialization tx.
                backboneTx = skeleton.backboneTxs.find(_.id == backbonePoint).get

                slotAndMbFallback <-
                    skeleton.competingFallbackFor(backbonePoint) match {
                        // We have a settlement/finalization and need to decide on the timing
                        case Some(fallbackTx) =>
                            Gen.oneOf(
                              BeforeBackboneTtlExpires,
                              WithinSilencePeriod,
                              AfterFallbackBecomesValid
                            ).flatMap {
                                case BeforeBackboneTtlExpires =>
                                    // should exist, may absent only for deinit which is handled in `case None`
                                    val ttl = backboneTx.body.value.ttl.get
                                    Gen.choose(ttl - 1000, ttl)
                                        .flatMap(slot =>
                                            (slot, Some(fallbackTx.tx.id, BeforeBackboneTtlExpires))
                                        )
                                case WithinSilencePeriod =>
                                    val ttl = backboneTx.body.value.ttl.get
                                    val fallbackValidityStart = fallbackTx.validityStart
                                        .toSlot(testTxBuilderEnvironment.slotConfig)
                                        .slot
                                    Gen.choose(ttl, fallbackValidityStart)
                                        .flatMap(slot =>
                                            (slot, Some(fallbackTx.tx.id, WithinSilencePeriod))
                                        )
                                case AfterFallbackBecomesValid =>
                                    val fallbackValidityStart = fallbackTx.validityStart
                                        .toSlot(testTxBuilderEnvironment.slotConfig)
                                        .slot
                                    Gen.choose(fallbackValidityStart, fallbackValidityStart + 1000)
                                        .flatMap(slot =>
                                            (
                                              slot,
                                              Some(fallbackTx.tx.id, AfterFallbackBecomesValid)
                                            )
                                        )
                            }
                        // We have initialization ot deinit
                        case None => {
                            // We choose the slot in a way so initialization/first settlement
                            // is valid or not valid. This doesn't affect deinit since it doesn't
                            // have validity range.
                            val earliestTtl = skeleton.earliestTtl
                            Gen.choose(earliestTtl.slot - 1000, earliestTtl.slot + 1000)
                                .flatMap(slot => (slot, None))
                        }
                    }
            } yield mkRollback(
              skeleton,
              backbonePoint,
              rolloutSeqPoints,
              Slot(slotAndMbFallback._1),
              slotAndMbFallback._2
            )

        def mkRollback(
            skeleton: Skeleton,
            backbonePoint: TransactionHash,
            rolloutPoints: List[TransactionHash],
            currentSlot: Slot,
            depth: Option[(TransactionHash, SettlementTiming)]
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

            // Result
            Rollback(
              utxoIds = edgeTxs.flatMap(_.body.value.inputs.toSet.toList).map(UtxoIdL1.apply).toSet,
              txsSurvived =
                  (backboneSurvived ++ rolloutTxsPartitions.flatMap(_._1)).map(_.id).toSet,
              txsLost = (backboneLost ++ rolloutLost).map(_.id).toSet,
              txsLostPreviousRollout = rolloutTxsPartitions.flatMap(_._2).map(_.id).toSet,
              currentSlot = currentSlot,
              mbFallback = depth
            )
        }

    // TODO: this maybe useful for debugging
    // val _ = property("Fish skeleton gets generated") = {
    //   val sample = skeletonGen().sample.get
    //   dumpSkeletonInfo(sample)
    //   Prop.proved
    // }

    // Test skeletons against which the (multiple) properties are checked.
    val testSkeletons: Seq[Skeleton] =
        List(
          (3, 3), // short skeleton for fast feedback loop
          // (20, 20) // more realistic skeleton
        ).flatMap((min, max) => {
            // val seed = Seed.fromBase64("SEAube5f5bBsTlLeYTuh3am1vO5iJvmzNRFEXSOlXlD=").get
            // val params = Gen.Parameters.default.withInitialSeed(seed)
            val sample = Skeleton
                .genL1BlockEffectsChain(min, max)
                .sample // apply(params, params.initialSeed.get)
            dumpSkeletonInfo(sample.get)
            sample
        })

    testSkeletons.distinct.zipWithIndex.foreach { case (skeleton, idx) =>
        include(new Properties(s"Skeleton ${idx}") {
            val _ = property("rollbacks are handled") = mkRollbackAreHandled(skeleton)
        })
    }

    def mkRollbackAreHandled(skeleton: Skeleton): Prop = {
        import SettlementTiming.*

        forAll(Rollback.genRollback(skeleton)) { rollback =>

            println("<~~ ROLLBACK ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~")
            println(
              s"Rollback is (utxos: ${rollback.utxoIds.size}, " +
                  s"\nsurvived: ${rollback.txsSurvived}, " +
                  s"\nlost: ${rollback.txsLost}" +
                  s"\nlost previous rollouts: ${rollback.txsLostPreviousRollout}" +
                  s"\ncurrent slot: ${rollback.currentSlot}" +
                  s"\ndepth: ${rollback.mbFallback}"
            )

            val result: IO[Prop] = for {
                system <- ActorSystem[IO]("Cardano Liaison SUT").allocated.map(_._1)

                cardanoBackendMock = CardanoBackendMockWithState(
                  rollback.utxoIds,
                  rollback.currentSlot
                )

                cardanoBackendMockActor <-
                    system.actorOf(
                      cardanoBackendMock.trackWithCache("cardano-backend-mock")
                    )

                config = CardanoLiaison.Config(
                  cardanoBackendMockActor,
                  skeleton._1.initializationTx,
                  skeleton._1.fallbackTx,
                  100.millis,
                  slotConfig = testTxBuilderEnvironment.slotConfig
                )

                cardanoLiaison = new CardanoLiaison(config) {}

                // Use protected handlers directly to present all effects
                _ <- skeleton._2.traverse_(s =>
                    cardanoLiaison.handleMajorBlockL1Effects(
                      ConfirmMajorBlock(
                        Block.Number(s.settlementTxSeq.settlementTx.majorVersionProduced),
                        s.settlementTxSeq,
                        s.fallbackTx
                      )
                    )
                )

                _ <- cardanoLiaison.handleFinalBlockL1Effects(
                  ConfirmFinalBlock(
                    Block.Number(skeleton._3.finalizationTx.majorVersionProduced),
                    skeleton._3
                  )
                )

                cardanoLiaisonActor <-
                    // TODO: uncomment if you want to see all msgs passing
                    // system.actorOfWithDebug(
                    system.actorOf(cardanoLiaison)

                expectedTxs = rollback.mbFallback match {
                    // Settlement/finalization tx is involved, we need to analyze
                    // timing
                    case Some(fallback, timing) =>
                        timing match {
                            case BeforeBackboneTtlExpires =>
                                rollback.txsLost ++ rollback.txsLostPreviousRollout
                            case WithinSilencePeriod => rollback.txsLostPreviousRollout
                            case AfterFallbackBecomesValid =>
                                Set(fallback) ++ rollback.txsLostPreviousRollout
                        }
                    // This can happen in two cases
                    //   - only deinit was rolled back
                    //   - or all the effects were rolled  back
                    case None =>
                        rollback.txsLost.size match {
                            // Only deinit
                            case 1 => rollback.txsLost ++ rollback.txsLostPreviousRollout
                            // All effects
                            case _ =>
                                if rollback.currentSlot > skeleton.earliestTtl
                                then rollback.txsLostPreviousRollout
                                else rollback.txsLost ++ rollback.txsLostPreviousRollout
                        }
                }

                _ <- IO.println(s"===> Expected txs: ${expectedTxs}")

                _ <- awaitCond(
                  p = cardanoBackendMock.getGetStateCounter.map(_ >= 2),
                  max = 10.seconds,
                  interval = 100.millis,
                  message = "Cardano liaison didn't come its full circle"
                )

                check <- awaitCond(
                  p = cardanoBackendMock.getKnownTxs.map(_ == expectedTxs),
                  max = 10.second,
                  interval = 100.millis,
                  message = "the list of known transaction should match the list of expected txs"
                ) >> IO.pure(Prop.proved)

                _ <- system.terminate()

            } yield check

            result.unsafeRunSync()
        }
    }

    /** The simplest version of L1 mock that remembers ids of txs submitted and can be used to check
      * the target state by the list of transactions that should be known to L1. Also, it tracks the
      * number of [[GetCardanoHeadState]] requests so tests can check that the cardano liaison came
      * its full cycle.
      *
      * @param utxoIds
      *   utxos ids for the response for [[GetCardanoState]], goes unchanged even a tx is submitted
      *   for simplicity's sake
      * @param currentSlot
      *   the slot for the response for [[GetCardanoState]], goes unchanged as well
      */
    class CardanoBackendMockWithState(utxoIds: Set[UtxoIdL1], currentSlot: Slot)
        extends CardanoBackend:

        val knownTxs: Ref[IO, Set[TransactionHash]] = Ref.unsafe(Set.empty)
        val getStateCounter: Ref[IO, Int] = Ref.unsafe(0)

        def getKnownTxs: IO[Set[TransactionHash]] = knownTxs.get
        def getGetStateCounter: IO[Int] = getStateCounter.get

        override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)

        private def receiveTotal(req: Request): IO[Unit] = req match {
            case req: SyncRequest.Any =>
                req.request match {
                    case x: GetCardanoHeadState.type => x.handleSync(req, getCardanoHeadState)
                    case x: GetTxInfo                => x.handleSync(req, getTxInfo)
                }
            case x: SubmitL1Effects => submitL1Effects(x)
        }

        private def submitL1Effects(r: SubmitL1Effects): IO[Unit] = for {
            txIds <- IO.pure(r.txs.map(_.id))
            _ <- IO.println(s"submitL1Effects: ${txIds}")
            _ <- knownTxs.update(s => s ++ txIds)
        } yield ()

        private def getCardanoHeadState(
            r: GetCardanoHeadState.type
        ): EitherT[IO, CardanoBackendError, GetCardanoHeadState.Response] = for {
            _ <- EitherT.right(
              IO.println("getCardanoHeadState") >>
                  getStateCounter.update(_ + 1)
            )
        } yield GetCardanoHeadState.Response(utxoIds, currentSlot)

        private def getTxInfo(r: GetTxInfo): EitherT[IO, CardanoBackendError, GetTxInfo.Response] =
            for {
                txs <- EitherT.right(
                  IO.println(s"getTxInfo txId: ${r.txHash}") >>
                      knownTxs.get
                )
            } yield GetTxInfo.Response(txs.contains(r.txHash))

}
