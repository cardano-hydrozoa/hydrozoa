package hydrozoa.multisig.consensus

import cats.*
import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.*
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.suprnation.actor.test.TestKit
import com.suprnation.actor.{ActorSystem, test as _}
import hydrozoa.lib.cardano.scalus.QuantizedTime.toQuantizedInstant
import hydrozoa.multisig.backend.cardano.CardanoBackend.GetTxInfo
import hydrozoa.multisig.backend.cardano.{CardanoBackend, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.CardanoLiaison.{FinalBlockConfirmed, MajorBlockConfirmed}
import hydrozoa.multisig.consensus.CardanoLiaisonTest.Rollback.SettlementTiming
import hydrozoa.multisig.consensus.CardanoLiaisonTest.Skeleton.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.Config
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, RolloutTx, Tx, TxTiming, genFinalizationTxSeqBuilder, genNextSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, InitializationTxSeq, InitializationTxSeqTest, RolloutTxSeq, SettlementTxSeq}
import hydrozoa.multisig.protocol.types.Block
import hydrozoa.{L1, Output, UtxoId, UtxoIdL1, UtxoSet, UtxoSetL1}
import org.scalacheck.*
import org.scalacheck.Gen.{choose, tailRecM}
import org.scalacheck.Prop.forAll
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import test.Generators.Hydrozoa.*
import test.{TestPeer, testNetwork, testTxBuilderEnvironment}

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

object CardanoLiaisonTest extends Properties("Cardano Liaison"), TestKit {

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withWorkers(1) // useful for debugging
            .withMinSuccessfulTests(100)
        // .withInitialSeed(Seed.fromBase64("SEAube5f5bBsTlLeYTuh3am1vO5iJvmzNRFEXSOlXlD=").get)
    }

    // TODO: use BlockEffects as BlockEffectChain
    // TODO: use ConsensusActor.BlockConfirmed? These are supposed to contain SIGNED effects.
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
            mkTxTiming: SlotConfig => TxTiming = TxTiming.default,
            slotConfig: SlotConfig = testTxBuilderEnvironment.slotConfig
        ): Gen[Skeleton] = for {

            // Init tx is generated using the available set of utxos.
            txTiming <- Gen.const(mkTxTiming(slotConfig))
            (args, peers) <- InitializationTxSeqTest.genArgs(
              txTiming = txTiming,
              mbUtxosAvailable = Some(yaciTestSauceGenesis(testNetwork))
            )

            // init tx
            initializationTxSeq = InitializationTxSeq.Builder
                .build(args)
                .fold(e => throw RuntimeException(e.toString), x => x)
            initializationTx = initializationTxSeq.initializationTx
            initializationTreasuryProduced = initializationTx.treasuryProduced
            initializationFallbackValidityStart = initializationTxSeq.fallbackTx.validityStart

            // multisig regime utxo
            hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
            tokenNames = TokenNames(args.spentUtxos.seedUtxo.input)
            multisigWitnessUtxo <- genFakeMultisigWitnessUtxo(
              hns,
              testTxBuilderEnvironment.network,
              Some(tokenNames.multisigRegimeTokenName)
            )

            // Settlement txs uses RANDOM deposit utxos so far, we don't use
            // deposit txs per se in this test suite. After generation, the
            // deposit utxos are added to the "genesis" utxos to allow the mock
            // handling settlement transactions.

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

            (settlementTxSeqs, lastSettlementBlockTimestamp, fallbackValidityStart) <- tailRecM(
              (
                initializationTreasuryProduced,
                args.blockZeroCreationTime,
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
                            // TODO: Implement Gen.Choose[QuantizedInstant]
                            blockCreatedOnL <- Gen.choose(
                              (previousBlockTimestamp + 10.seconds).toSlot.slot,
                              (fallbackValidityStart - txTiming.silenceDuration - 10.seconds).toSlot.slot
                            )
                            blockCreatedOn = Slot(blockCreatedOnL).toQuantizedInstant(slotConfig)
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

            // Finalization seq
            lastSettlementTreasury =
                settlementTxSeqs.last.settlementTxSeq.settlementTx.treasuryProduced

            finalizationBlockCreatedOnL <- Gen.choose(
              (lastSettlementBlockTimestamp + 10.seconds).toSlot.slot,
              (fallbackValidityStart - txTiming.silenceDuration - 10.seconds).toSlot.slot
            )
            finalizationBlockCreatedOn = Slot(finalizationBlockCreatedOnL).toQuantizedInstant(
              slotConfig
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

        extension (skeleton: Skeleton)

            /** Returns all happy path txs of the skeleton, i.e. all but post dated fallback txs. */
            def happyPathTxs: Set[Transaction] = {

                def settlementTxs(r: SettlementTxSeq.Builder.Result): List[Transaction] =
                    r.settlementTxSeq match {
                        case SettlementTxSeq.NoRollouts(settlementTx) => List(settlementTx.tx)
                        case SettlementTxSeq.WithRollouts(settlementTx, rolloutTxSeq) =>
                            settlementTx.tx +: rolloutTxSeq.mbRollouts.map(_.tx)
                    }

                def finalizationTxs(seq: FinalizationTxSeq): List[Transaction] = seq match {
                    case FinalizationTxSeq.Monolithic(finalizationTx) =>
                        List(finalizationTx.tx)
                    case FinalizationTxSeq.WithDeinit(finalizationTx, deinitTx) =>
                        List(finalizationTx.tx, deinitTx.tx)
                    case FinalizationTxSeq.WithRollouts(finalizationTx, rolloutTxSeq) =>
                        finalizationTx.tx +: rolloutTxSeq.mbRollouts.map(_.tx)
                    case FinalizationTxSeq.WithDeinitAndRollouts(
                          finalizationTx,
                          deinitTx,
                          rolloutTxSeq
                        ) =>
                        finalizationTx.tx +: rolloutTxSeq.mbRollouts.map(_.tx) :+ deinitTx.tx
                }

                (
                  (skeleton._1.initializationTx.tx
                      +: skeleton._2.flatMap(settlementTxs))
                      ++ finalizationTxs(skeleton._3)
                ).toSet
            }

            /** All backbone transactions from the skeleton. */
            def backboneTxs: List[Tx[?]] =
                (skeleton._1.initializationTx +: skeleton._2.map(
                  _.settlementTxSeq.settlementTx
                )) ++
                    {
                        skeleton._3 match {
                            case FinalizationTxSeq.Monolithic(finalizationTx) =>
                                List(finalizationTx)
                            case FinalizationTxSeq.WithDeinit(finalizationTx, deinitTx) =>
                                List(finalizationTx, deinitTx)
                            case FinalizationTxSeq.WithRollouts(finalizationTx, _) =>
                                List(finalizationTx)
                            case FinalizationTxSeq.WithDeinitAndRollouts(
                                  finalizationTx,
                                  deinitTx,
                                  _
                                ) =>
                                List(finalizationTx, deinitTx)
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
                    .map(_.tx.id)
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
            private def backboneNodesWithRollouts: List[(TransactionHash, List[RolloutTx])] = {
                (skeleton._1.initializationTx.tx.id -> List.empty)
                    +:
                        skeleton._2
                            .map(node =>
                                node.settlementTxSeq.settlementTx.tx.id -> node.settlementTxSeq.mbRollouts
                            )
                        :+ skeleton._3.finalizationTx.tx.id -> skeleton._3.mbRollouts
            }

            /** @param backbonePoint
              * @return
              *   all rollout sequences that come before [[backbonePoint]]
              */
            def rolloutSeqsBefore(
                backbonePoint: TransactionHash
            ): List[List[Tx[RolloutTx]]] =
                backboneNodesWithRollouts
                    .takeWhile(_._1 != backbonePoint)
                    .filter(_._2.nonEmpty)
                    .map(_._2)

            /** @param backbonePoint
              * @return
              *   all rollout sequences starting from [[backbonePoint]] as ordinary txs
              */
            def rolloutSeqsStartingWith(
                backbonePoint: TransactionHash
            ): List[List[Transaction]] =
                backboneNodesWithRollouts
                    .dropWhile(_._1 != backbonePoint)
                    .filter(_._2.nonEmpty)
                    .map(_._2.map(_.tx))

            def earliestTtl: Slot =
                // TODO: for now we use initialization tx ttl, see the comment on `ttl` field
                skeleton._1.initializationTx.validityEnd.toSlot

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
                            val rollouts: Vector[RolloutTx] =
                                rolloutTxSeq.notLast :+ rolloutTxSeq.last
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
                        val rollouts: Vector[RolloutTx] = rolloutTxSeq.notLast :+ rolloutTxSeq.last
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
                        val rollouts: Vector[RolloutTx] = rolloutTxSeq.notLast :+ rolloutTxSeq.last
                        rollouts.foreach(r => println(s"\t\t - rollout: ${r.tx.id}"))
                        println(s"Deinit: ${deinitTx.tx.id}")
                }
                println("--------- END of Hydrozoa L1 effects chain sample --------- ")

    end Skeleton

    object Rollback:

        /** The relation between [[Rollback.rollbackTime]] end the settlement/finalization tx, may
          * absent if only deinit is rolled back or all transactions are rolled back.
          */
        enum SettlementTiming:
            case BeforeBackboneTtlExpires
            case WithinSilencePeriod
            case AfterFallbackBecomesValid

        /** Utxo ids + transaction partitions (survived/lost). */
        final case class Rollback(
            /** Utxos that exist immediately after the rollback is done. */
            utxos: UtxoSetL1,
            // NB: this field can be useful for debugging purposes, it's not used directly in the tests
            txsSurvived: Set[TransactionHash],
            /** Lost backbone txs and rollout txs that branch off them. */
            txsLost: Set[TransactionHash],
            /** Lost rollout txs that come before the first rolled back backbone tx. */
            txsLostPreviousRollout: Set[TransactionHash],
            /** The rollback time, i.e. the time when rollback was done. */
            rollbackTime: Slot,
            // The tx id of the competing fallback transaction and timing relation.
            // It's not defined if only deinit tx or the whole skeleton was rolled back.
            // TODO: do we use the second element of the tuple in the liaison?
            //   I think now we do, but we would like to get rig of it.
            mbFallback: Option[(TransactionHash, SettlementTiming)]
        )

        def genRollback(skeleton: Skeleton): Gen[Rollback] =
            import SettlementTiming.*

            for {
                // All possible point on the backbone
                backboneTxIds <- Gen.const(skeleton.backboneTxs.map(_.tx.id))
                // The node in the backbone that contains the first rolled back tx, i.e., the depth.
                rollbackBackbonePoint <- Gen.oneOf(backboneTxIds)
                // Survived rollout sequences, i.e., those that come before the [[rollbackBackbonePoint]]
                rolloutSeqsSurvived = skeleton
                    .rolloutSeqsBefore(rollbackBackbonePoint)
                    .map(_.map(_.tx.id))
                // Choose rollback points for every survived rollout sequence
                // TODO: Why does it default to java ArrayList?
                rollbackRolloutSeqPoints <- Gen
                    .sequence(rolloutSeqsSurvived.map(xs => Gen.oneOf(xs)))
                    .map(_.asScala.toList)

                // TODO: use time not slots
                // The time when the rollback occurred - the "rollback time".
                // There are several possible options depending on the rollback depth, or
                // more precisely on which type of tx the rollback point fell down.
                //
                // - (1) The rollback backbone point is settlement/finalization tx:
                //   - (a) The rollback time is before the backbone tx TTL expires -
                //         sort of a "quick" rollback [[BeforeBackboneTtlExpires]].
                //   - (b) The rollback time falls exactly on the silence period between
                //         that tx and the competing fallback tx [[WithinSilencePeriod]].
                //   - (c) The rollback time is equals or greater than the competing fallback
                //         tx validity start - sort of late rollback [[AfterFallbackBecomesValid]].
                //
                // - (2) The deinit tx was chosen as the rollback backbone point:
                //       The TTL and competing fallbacks are absent.
                //       It means we can pick up an arbitrary rollback time that makes sense.
                //
                // - (3) The whole skeleton was rolled back.
                //       We can choose an instant of time around the block zero creation time.
                //
                backboneTx = skeleton.backboneTxs.find(_.tx.id == rollbackBackbonePoint).get

                slotAndMbFallback <-
                    skeleton.competingFallbackFor(rollbackBackbonePoint) match {
                        // We have a settlement/finalization and need to decide on the timing
                        case Some(fallbackTx) =>
                            Gen.oneOf(
                              BeforeBackboneTtlExpires,
                              WithinSilencePeriod,
                              AfterFallbackBecomesValid
                            ).flatMap {
                                case BeforeBackboneTtlExpires =>
                                    // should exist, may absent only for deinit which is handled in `case None`
                                    val ttl = backboneTx.tx.body.value.ttl.get
                                    Gen.choose(ttl - 1000, ttl)
                                        .flatMap(slot =>
                                            (slot, Some(fallbackTx.tx.id, BeforeBackboneTtlExpires))
                                        )
                                case WithinSilencePeriod =>
                                    val ttl = backboneTx.tx.body.value.ttl.get
                                    val fallbackValidityStart = fallbackTx.validityStart.toSlot.slot
                                    Gen.choose(ttl, fallbackValidityStart)
                                        .flatMap(slot =>
                                            (slot, Some(fallbackTx.tx.id, WithinSilencePeriod))
                                        )
                                case AfterFallbackBecomesValid =>
                                    val fallbackValidityStart = fallbackTx.validityStart.toSlot.slot
                                    Gen.choose(fallbackValidityStart, fallbackValidityStart + 1000)
                                        .flatMap(slot =>
                                            (
                                              slot,
                                              Some(fallbackTx.tx.id, AfterFallbackBecomesValid)
                                            )
                                        )
                            }
                        // The rollback point points to the initialization ot deinit
                        case None => {
                            // We choose the slot in a way so initialization/first settlement
                            // is valid or not valid.
                            // This doesn't affect deinit since it doesn't have validity range.
                            val earliestTtl = skeleton.earliestTtl
                            Gen.choose(earliestTtl.slot - 1000, earliestTtl.slot + 1000)
                                .flatMap(slot => (slot, None))
                        }
                    }
            } yield mkRollback(
              skeleton,
              rollbackBackbonePoint,
              rollbackRolloutSeqPoints,
              Slot(slotAndMbFallback._1),
              slotAndMbFallback._2
            )

        def mkRollback(
            skeleton: Skeleton,
            rollbackBackbonePoint: TransactionHash,
            rollbackRolloutPoints: List[TransactionHash],
            rollbackTime: Slot,
            mbFallback: Option[(TransactionHash, SettlementTiming)]
        ): Rollback = {
            // Backbone
            val backboneTxs1 = skeleton.backboneTxs
            val backboneIndex = backboneTxs1.indexWhere(_.tx.id == rollbackBackbonePoint)
            val (backboneSurvived, backboneLost) = backboneTxs1.splitAt(backboneIndex)

            // Rollouts lost due to te backbone point
            val rolloutLost = skeleton.rolloutSeqsStartingWith(rollbackBackbonePoint).flatten

            // Rolled back rollouts
            val allRolloutSeqs = skeleton.rolloutSeqsBefore(rollbackBackbonePoint)

            val rollbackRolloutIndices =
                allRolloutSeqs.zip(rollbackRolloutPoints).map(e => e._1.indexWhere(_.tx.id == e._2))
            val rolloutTxsPartitions = allRolloutSeqs
                .zip(rollbackRolloutIndices)
                .map(e => e._1.splitAt(e._2))

            // Txs at the verge: these are needed to build the utxo state
            val edgeTxs = backboneLost.head +: rolloutTxsPartitions.map(_._2.head)
            // println(s"edge txs: ${edgeTxs.map(_.id)}")

            // Result
            Rollback(
              utxos = UtxoSet[L1](
                edgeTxs
                    .flatMap(_.resolvedUtxos.utxos.toList)
                    .map((i, o) => UtxoId[L1](i) -> Output[L1](o.asInstanceOf[Babbage]))
                    .toMap
              ),
              txsSurvived =
                  (backboneSurvived ++ rolloutTxsPartitions.flatMap(_._1)).map(_.tx.id).toSet,
              txsLost = (backboneLost.map(_.tx) ++ rolloutLost).map(_.id).toSet,
              txsLostPreviousRollout = rolloutTxsPartitions.flatMap(_._2).map(_.tx.id).toSet,
              rollbackTime = rollbackTime,
              mbFallback = mbFallback
            )
        }

    // TODO: this maybe useful for debugging
    // val _ = property("Fish skeleton gets generated") = {
    //   val sample = genL1BlockEffectsChain().sample.get
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
              s"Rollback is (utxos: ${rollback.utxos.size}, " +
                  s"\nsurvived: ${rollback.txsSurvived}, " +
                  s"\nlost: ${rollback.txsLost}" +
                  s"\nlost previous rollouts: ${rollback.txsLostPreviousRollout}" +
                  s"\ncurrent slot: ${rollback.rollbackTime}" +
                  s"\ndepth: ${rollback.mbFallback}"
            )

            val result: IO[Prop] = for {
                system <- ActorSystem[IO]("Cardano Liaison SUT").allocated.map(_._1)

                cardanoBackendMock = CardanoBackendMockWithState(
                  rollback.utxos
                )

                config = CardanoLiaison.Config(
                  ???,
                  skeleton._1.initializationTx,
                  skeleton._1.fallbackTx,
                  100.millis,
                  slotConfig = testTxBuilderEnvironment.slotConfig,
                  blockWeaver = ???
                )

                cardanoLiaison = new CardanoLiaison(
                  config,
                  Ref.unsafe[IO, CardanoLiaison.State](CardanoLiaison.State.initialState(config))
                )

                // Use protected handlers directly to present all effects
                _ <- skeleton._2.traverse_(s =>
                    cardanoLiaison.handleMajorBlockL1Effects(
                      MajorBlockConfirmed(
                        blockNum =
                            Block.Number(s.settlementTxSeq.settlementTx.majorVersionProduced),
                        settlementSigned = s.settlementTxSeq.settlementTx,
                        rolloutsSigned = s.settlementTxSeq.mbRollouts,
                        fallbackSigned = s.fallbackTx
                      )
                    )
                )

                _ <- cardanoLiaison.handleFinalBlockL1Effects(
                  FinalBlockConfirmed(
                    blockNum = Block.Number(skeleton._3.finalizationTx.majorVersionProduced),
                    finalizationSigned = skeleton._3.finalizationTx,
                    rolloutsSigned = skeleton._3.mbRollouts,
                    mbDeinitSigned = skeleton._3.mbDeinit
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
                                if rollback.rollbackTime > skeleton.earliestTtl
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

            // result.unsafeRunSync()
            Prop.falsified
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
    class CardanoBackendMockWithState(utxoIds: Set[UtxoIdL1]) extends CardanoBackend[IO]:

        val knownTxs: Ref[IO, Set[TransactionHash]] = Ref.unsafe(Set.empty)
        val getStateCounter: Ref[IO, Int] = Ref.unsafe(0)

        def getKnownTxs: IO[Set[TransactionHash]] = knownTxs.get
        def getGetStateCounter: IO[Int] = getStateCounter.get

        override def utxosAt(address: ShelleyAddress): IO[Either[CardanoBackend.Error, UtxoSetL1]] =
            ???

        override def utxosAt(
            address: ShelleyAddress,
            asset: (PolicyId, AssetName)
        ): IO[Either[CardanoBackend.Error, UtxoSetL1]] = ???

        override def getTxInfo(
            txHash: TransactionHash
        ): IO[Either[CardanoBackend.Error, GetTxInfo.Response]] = ???

        override def submitTx(tx: Transaction): IO[Either[CardanoBackend.Error, Unit]] = ???

        // override def receive: Receive[IO, Request] = PartialFunction.fromFunction(receiveTotal)
        //
        // private def receiveTotal(req: Request): IO[Unit] = req match {
        //    case req: SyncRequest.Any =>
        //        req.request match {
        //            case x: GetCardanoHeadState.type => x.handleSync(req, getCardanoHeadState)
        //            case x: GetTxInfo                => x.handleSync(req, getTxInfo)
        //        }
        //    case x: SubmitL1Effects => submitL1Effects(x)
        // }
        //
        // private def submitL1Effects(r: SubmitL1Effects): IO[Unit] = for {
        //    txIds <- IO.pure(r.txs.map(_.id))
        //    _ <- IO.println(s"submitL1Effects: ${txIds}")
        //    _ <- knownTxs.update(s => s ++ txIds)
        // } yield ()
        //
        // private def getCardanoHeadState(
        //    r: GetCardanoHeadState.type
        // ): EitherT[IO, CardanoBackendError, GetCardanoHeadState.Response] = for {
        //    _ <- EitherT.right(
        //      IO.println("getCardanoHeadState") >>
        //          getStateCounter.update(_ + 1)
        //    )
        // } yield GetCardanoHeadState.Response(utxoIds, currentSlot)
        //
        // private def getTxInfo(r: GetTxInfo): EitherT[IO, CardanoBackendError, GetTxInfo.Response] =
        //    for {
        //        txs <- EitherT.right(
        //          IO.println(s"getTxInfo txId: ${r.txHash}") >>
        //              knownTxs.get
        //        )
        //    } yield GetTxInfo.Response(txs.contains(r.txHash))

}
