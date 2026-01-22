package hydrozoa.multisig.consensus

import cats.*
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.test.TestKit
import com.suprnation.actor.{ActorSystem, test as _}
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toQuantizedInstant}
import hydrozoa.lib.cardano.scalus.given
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState, yaciTestSauceGenesis}
import hydrozoa.multisig.consensus.BlockWeaver.Request
import hydrozoa.multisig.consensus.CardanoLiaison.{FinalBlockConfirmed, MajorBlockConfirmed}
import hydrozoa.multisig.consensus.CardanoLiaisonTest.BlockEffectsSignedChain.*
import hydrozoa.multisig.consensus.CardanoLiaisonTest.Rollback.SettlementTiming
import hydrozoa.multisig.consensus.CardanoLiaisonTest.Rollback.SettlementTiming.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, FinalizationTx, RolloutTx, SettlementTx, Tx, TxTiming, genFinalizationTxSeqBuilder, genNextSettlementTxSeqBuilder}
import hydrozoa.multisig.ledger.dapp.txseq.{FinalizationTxSeq, InitializationTxSeq, InitializationTxSeqTest, SettlementTxSeq}
import hydrozoa.multisig.protocol.types.{Block, BlockEffectsSigned}
import hydrozoa.{L1, Output, UtxoId, UtxoSet, UtxoSetL1, attachVKeyWitnesses}
import java.util.concurrent.TimeUnit
import monocle.Focus.focus
import org.scalacheck.*
import org.scalacheck.Gen.{choose, tailRecM}
import org.scalacheck.Prop.forAll
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.jdk.CollectionConverters.*
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.*
import test.Generators.Hydrozoa.*
import test.{TestPeer, testNetwork, testTxBuilderEnvironment}

object CardanoLiaisonTest extends Properties("Cardano Liaison"), TestKit {

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withWorkers(1) // useful for debugging
            .withMinSuccessfulTests(100)
        // .withInitialSeed(Seed.fromBase64("SEAube5f5bBsTlLeYTuh3am1vO5iJvmzNRFEXSOlXlD=").get)
    }

    /** This is the "skeleton" of the Hydrozoa transactions (see diagrams to get an idea what it
      * looks like). After introducing [[BlockEffectsSigned]] we use it rather than tx sequences
      * which are unwieldy and were designed to be used with the tx builders.
      *
      * TODO: case class?
      */
    type BlockEffectsSignedChain = (
        BlockEffectsSigned.Initial,
        List[BlockEffectsSigned.Major],
        BlockEffectsSigned.Final
    )

    object BlockEffectsSignedChain:

        // TODO: figure out why this fails with (minSettlements=1, maxSettlements=1)
        // TODO: this gives no result sometimes
        // TODO: this is always very slow
        /** Generates the "skeleton", i.e. random chain of happy-path transactions and fallbacks
          * with requested parameters.
          */
        def gen(
            minSettlements: Int = 5,
            maxSettlements: Int = 25,
            mkTxTiming: SlotConfig => TxTiming = TxTiming.default,
            slotConfig: SlotConfig = testTxBuilderEnvironment.slotConfig
        ): Gen[BlockEffectsSignedChain] = for {

            // Init tx is generated using the available set of utxos.
            txTiming <- Gen.const(mkTxTiming(slotConfig))
            (initialArgs, peers) <- InitializationTxSeqTest.genArgs(
              txTiming = txTiming,
              mbUtxosAvailable = Some(yaciTestSauceGenesis(testNetwork))
            )

            _ = println(s"peers: ${peers.map(_.walletId)}")

            // Initial block effects
            initializationTxSeq = InitializationTxSeq.Builder
                .build(initialArgs)
                .fold(e => throw RuntimeException(e.toString), x => x)
            initializationTx = initializationTxSeq.initializationTx
            fallbackTx = initializationTxSeq.fallbackTx

            initTxWitnesses: List[VKeyWitness] =
                peers.map(p => p.wallet.createTxKeyWitness(initializationTx.tx)).toList
            fallbackTxWitnesses: List[VKeyWitness] =
                peers.map(p => p.wallet.createTxKeyWitness(fallbackTx.tx)).toList

            initialBlockEffectsSigned: BlockEffectsSigned.Initial = BlockEffectsSigned.Initial(
              blockNum = Block.Number.zero,
              initialSigned = initializationTx.txLens.modify(tx =>
                  attachVKeyWitnesses(tx, initTxWitnesses)
              )(initializationTx),
              fallbackSigned = fallbackTx.txLens.modify(tx =>
                  attachVKeyWitnesses(tx, fallbackTxWitnesses)
              )(fallbackTx)
            )

            initializationTreasuryProduced = initializationTx.treasuryProduced
            initializationFallbackValidityStart = fallbackTx.validityStart

            // multisig regime utxo
            hns = HeadMultisigScript(peers.map(_.wallet.exportVerificationKeyBytes))
            tokenNames = TokenNames(initialArgs.spentUtxos.seedUtxo.input)
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
              headMultisigScript = hns,
              multisigRegimeUtxo = initializationTx.multisigRegimeUtxo,
              tokenNames = tokenNames,
              cardanoInfo = initialArgs.env,
              evaluator = initialArgs.evaluator,
              validators = initialArgs.validators
            )

            numOfSettlements <- choose(minSettlements, maxSettlements)

            (majorBlocksEffectsSigned, lastSettlementBlockTimestamp, fallbackValidityStart) <-
                tailRecM(
                  (
                    initializationTreasuryProduced,
                    initialArgs.blockZeroCreationTime,
                    initializationFallbackValidityStart,
                    List.empty: List[BlockEffectsSigned.Major],
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
                                blockCreatedOn <- Gen.choose(
                                  previousBlockTimestamp + 10.seconds,
                                  fallbackValidityStart - txTiming.silenceDuration - 10.seconds
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
                                result = builder
                                    .build(args)
                                    .fold(e => throw RuntimeException(e.toString), x => x)

                                settlementTx = result.settlementTxSeq.settlementTx
                                settlementTxWitnesses: List[VKeyWitness] =
                                    peers
                                        .map(p => p.wallet.createTxKeyWitness(settlementTx.tx))
                                        .toList

                                fallbackTx = result.fallbackTx
                                fallbackTxWitnesses: List[VKeyWitness] =
                                    peers
                                        .map(p => p.wallet.createTxKeyWitness(fallbackTx.tx))
                                        .toList

                                rolloutTxs = result.settlementTxSeq.mbRollouts
                                rolloutTxWitnesses: List[List[VKeyWitness]] =
                                    rolloutTxs.map(r =>
                                        peers.map(p => p.wallet.createTxKeyWitness(r.tx)).toList
                                    )

                                blockEffects: BlockEffectsSigned.Major =
                                    BlockEffectsSigned.Major(
                                      blockNum = Block.Number(settlementNum),
                                      settlementSigned = settlementTx.txLens.modify(tx =>
                                          attachVKeyWitnesses(tx, settlementTxWitnesses)
                                      )(settlementTx),
                                      fallbackSigned = fallbackTx.txLens.modify(tx =>
                                          attachVKeyWitnesses(tx, fallbackTxWitnesses)
                                      )(fallbackTx),
                                      rolloutsSigned = rolloutTxs
                                          .zip(rolloutTxWitnesses)
                                          .map((r, ws) =>
                                              r.txLens.modify(tx => attachVKeyWitnesses(tx, ws))(r)
                                          ),
                                      postDatedRefundsSigned = List.empty
                                    )

                                nextTreasuryToSpend =
                                    settlementTx.treasuryProduced
                                fallbackValidityStart = result.fallbackTx.validityStart

                            } yield Left(
                              (
                                nextTreasuryToSpend,
                                blockCreatedOn,
                                fallbackValidityStart,
                                blockEffects :: acc,
                                settlementNum + 1
                              )
                            )
                }

            // Finalization seq
            lastSettlementTreasury =
                majorBlocksEffectsSigned.last.settlementSigned.treasuryProduced

            finalizationBlockCreatedOnL <- Gen.choose(
              (lastSettlementBlockTimestamp + 10.seconds).toSlot.slot,
              (fallbackValidityStart - txTiming.silenceDuration - 10.seconds).toSlot.slot
            )
            finalizationBlockCreatedOn = Slot(finalizationBlockCreatedOnL).toQuantizedInstant(
              slotConfig
            )

            finalBlockNum = numOfSettlements + 1

            (builder, finalArgs) <- genFinalizationTxSeqBuilder(
              lastSettlementTreasury,
              finalBlockNum,
              fallbackValidityStart,
              finalizationBlockCreatedOn,
              txTiming,
              config,
              peers
            )

            // Run the builder
            finalizationTxSeq = builder
                .build(finalArgs)
                .fold(e => throw RuntimeException(e.toString), x => x)

            finalizationTx = finalizationTxSeq.finalizationTx
            finalizationTxWitnesses: List[VKeyWitness] =
                peers
                    .map(p => p.wallet.createTxKeyWitness(finalizationTx.tx))
                    .toList

            rolloutTxs = finalizationTxSeq.mbRollouts
            rolloutTxWitnesses: List[List[VKeyWitness]] =
                rolloutTxs.map(r => peers.map(p => p.wallet.createTxKeyWitness(r.tx)).toList)

            mbDeinitTx = finalizationTxSeq.mbDeinit
            deinitTxWitnesses: List[VKeyWitness] =
                mbDeinitTx
                    .map(deinitTx =>
                        peers
                            .map(p => p.wallet.createTxKeyWitness(deinitTx.tx))
                            .toList
                    )
                    .getOrElse(List.empty)

            finalBlockEffectsSigned: BlockEffectsSigned.Final = BlockEffectsSigned.Final(
              blockNum = Block.Number(finalBlockNum),
              rolloutsSigned = rolloutTxs
                  .zip(rolloutTxWitnesses)
                  .map((r, ws) => r.txLens.modify(tx => attachVKeyWitnesses(tx, ws))(r)),
              mbDeinitSigned = mbDeinitTx.map(d =>
                  d.txLens.modify(tx => attachVKeyWitnesses(tx, deinitTxWitnesses))(d)
              ),
              finalizationSigned = finalizationTx.txLens.modify(tx =>
                  attachVKeyWitnesses(tx, finalizationTxWitnesses)
              )(finalizationTx),
            )

        } yield (
          initialBlockEffectsSigned,
          majorBlocksEffectsSigned,
          finalBlockEffectsSigned
        )

        extension (skeleton: BlockEffectsSignedChain)

            /** Returns all happy path txs of the skeleton, i.e. all but post dated fallback txs. */
            def happyPathTxs: Set[Transaction] = {

                def finalizationTxs(effects: BlockEffectsSigned.Final): List[Transaction] =
                    List(effects.finalizationSigned.tx)
                        ++ effects.rolloutsSigned.map(_.tx)
                        ++ effects.mbDeinitSigned.map(_.tx).toList

                ((skeleton._1.initialSigned.tx
                    +: skeleton._2.map(_.settlementSigned.tx))
                    ++ finalizationTxs(skeleton._3)).toSet
            }

            /** All backbone transactions from the skeleton. */
            def backboneTxs: List[Tx[?]] =
                (skeleton._1.initialSigned +: skeleton._2.map(_.settlementSigned)) ++
                    List(skeleton._3.finalizationSigned)
                    ++ skeleton._3.mbDeinitSigned.toList

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
                      List(None, Some(skeleton._1.fallbackSigned)) ++ skeleton._2.map(node =>
                          Some(node.fallbackSigned)
                      ) ++ List(None)
                    )
                    .find(_._1 == backbonePoint)
                    .flatMap(_._2)
            }

            // The common structure for the initialization tx, settlement txs, and the finalization tx
            // with the linked rollouts (where applicable, otherwise None)
            private def backboneNodesWithRollouts: List[(TransactionHash, List[RolloutTx])] = {
                (skeleton._1.initialSigned.tx.id -> List.empty)
                    +:
                        skeleton._2
                            .map(node => node.settlementSigned.tx.id -> node.rolloutsSigned)
                        :+ skeleton._3.finalizationSigned.tx.id -> skeleton._3.rolloutsSigned
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

            def earliestTtl: QuantizedInstant =
                skeleton._1.initialSigned.validityEnd

            def dumpSkeletonInfo: Unit =

                println("--------- Hydrozoa L1 effects chain sample --------- ")

                val initTx = skeleton._1.initialSigned

                println(s"Initialization tx: ${initTx.tx.id}, TTL: ${initTx.validityEnd}")

                val fallbackTx = skeleton._1.fallbackSigned
                println(
                  s"\t=> Fallback 0 tx: ${fallbackTx.tx.id}, validity start: ${fallbackTx.validityStart}"
                )

                // Settlements
                skeleton._2.foreach(effects => {
                    val fallbackTx = effects.fallbackSigned

                    println(
                      s"Settlement, block ${effects.blockNum}: " +
                          s"${effects.settlementSigned.tx.id}, TTL: ${effects.settlementSigned.validityEnd}, " +
                          s"deposits: ${effects.settlementSigned.depositsSpent.length}, " +
                          s"rollouts: ${effects.rolloutsSigned.size}"
                    )

                    effects.rolloutsSigned.foreach(r => println(s"\t\t - rollout: ${r.tx.id}"))
                    println(
                      s"\t=> Fallback block ${effects.blockNum}: " +
                          s"${fallbackTx.tx.id}, validity start: ${fallbackTx.validityStart}"
                    )
                })

                // Finalization
                println(
                  s"Finalization: ${skeleton._3.finalizationSigned.tx.id}, TTL: ${skeleton._3.finalizationSigned.validityEnd}" +
                      s", rollouts: ${skeleton._3.rolloutsSigned.size}"
                )
                skeleton._3.rolloutsSigned.foreach(r => println(s"\t\t - rollout: ${r.tx.id}"))
                skeleton._3.mbDeinitSigned.foreach(d => println(s"Deinit: ${d.tx.id}"))

                println("--------- END of Hydrozoa L1 effects chain sample --------- ")

    end BlockEffectsSignedChain

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
            rollbackTime: QuantizedInstant,
            // The tx id of the competing fallback transaction and timing relation.
            // It's not defined if only deinit tx or the whole skeleton was rolled back.
            // TODO: do we use the second element of the tuple in the liaison?
            //   I think now we do, but we would like to get rig of it.
            mbFallback: Option[(TransactionHash, SettlementTiming)]
        )

        def genRollback(skeleton: BlockEffectsSignedChain): Gen[Rollback] =
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

                (rollbackTime, mbFallback) <-
                    skeleton.competingFallbackFor(rollbackBackbonePoint) match {
                        // This maybe not the easiest way to put it, but by the presence of
                        // competing fallback tx for the rollback backbone point we effectively
                        // differentiate the type of that point:

                        case Some(fallbackTx) =>
                            // -> we have a settlement/finalization and need to decide on the timing

                            Gen.oneOf(
                              BeforeBackboneTtlExpires,
                              WithinSilencePeriod,
                              AfterFallbackBecomesValid
                            ).flatMap {
                                case BeforeBackboneTtlExpires =>
                                    // should exist, may absent only for deinit which is handled in `case None`
                                    val validityEnd: QuantizedInstant = backboneTx match {
                                        case s: SettlementTx   => s.validityEnd
                                        case f: FinalizationTx => f.validityEnd
                                        case _                 => ??? // should never happen
                                    }
                                    Gen.choose(validityEnd - 1000.seconds, validityEnd - 20.seconds)
                                        .flatMap(instant =>
                                            (
                                              instant,
                                              Some(fallbackTx.tx.id, BeforeBackboneTtlExpires)
                                            )
                                        )
                                case WithinSilencePeriod =>
                                    val validityEnd: QuantizedInstant = backboneTx match {
                                        case s: SettlementTx   => s.validityEnd
                                        case f: FinalizationTx => f.validityEnd
                                        case _ => ??? // never happens, rewrite the outer match
                                    }
                                    val fallbackValidityStart = fallbackTx.validityStart
                                    Gen.choose(validityEnd, fallbackValidityStart)
                                        .flatMap(instant =>
                                            (instant, Some(fallbackTx.tx.id, WithinSilencePeriod))
                                        )
                                case AfterFallbackBecomesValid =>
                                    val fallbackValidityStart = fallbackTx.validityStart
                                    Gen.choose(
                                      fallbackValidityStart,
                                      fallbackValidityStart + 1000.seconds
                                    ).flatMap(instant =>
                                        (
                                          instant,
                                          Some(fallbackTx.tx.id, AfterFallbackBecomesValid)
                                        )
                                    )
                            }
                        case None => {
                            // The rollback point points to the initialization ot deinit
                            // Rollback time doesn't affect the deinit tx since it doesn't have validity range.

                            // We choose the slot in a way so initialization/first settlement
                            // is valid or not valid.
                            // TODO: maybe that should be represented by constructors as well
                            val earliestTtl = skeleton.earliestTtl
                            Gen.choose(earliestTtl - 1000.seconds, earliestTtl + 1000.seconds)
                                .flatMap(instant => (instant, None))
                        }
                    }
            } yield mkRollback(
              skeleton,
              rollbackBackbonePoint,
              rollbackRolloutSeqPoints,
              rollbackTime,
              mbFallback
            )

        def mkRollback(
            skeleton: BlockEffectsSignedChain,
            rollbackBackbonePoint: TransactionHash,
            rollbackRolloutPoints: List[TransactionHash],
            rollbackTime: QuantizedInstant,
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

            // Utxo set contains two kinds of things:
            //  -- all resolved outputs from edge txs
            //  -- deposit utxos to be consumed by all the rolled back settlements
            // (The latest is a bit of cheating that we decided to accept for now).

            // Txs at the verge: these are needed to build the utxo state
            val edgeTxs = backboneLost.head +: rolloutTxsPartitions.map(_._2.head)
            // println(s"edge txs: ${edgeTxs.map(_.id)}")
            val edgeTxsResolvedUtxos = edgeTxs
                .flatMap(_.resolvedUtxos.utxos.toList)
                .map((i, o) => UtxoId[L1](i) -> Output[L1](o))
                .toMap

            val depositUtxos = backboneLost
                .filter(_.isInstanceOf[SettlementTx])
                .map(_.asInstanceOf[SettlementTx])
                .flatMap(s => s.depositsSpent)
                .map(_.toUtxo)
                .map(u => UtxoId[L1](u.input) -> Output[L1](u.output))

            // Result
            Rollback(
              utxos = UtxoSet[L1](edgeTxsResolvedUtxos ++ depositUtxos),
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
    val testSkeletons: Seq[BlockEffectsSignedChain] =
        List(
          (5, 5), // short skeleton for fast feedback loop
          // (20, 20) // more realistic skeleton
        ).flatMap((min, max) => {
            // val seed = Seed.fromBase64("SEAube5f5bBsTlLeYTuh3am1vO5iJvmzNRFEXSOlXlD=").get
            // val params = Gen.Parameters.default.withInitialSeed(seed)
            val sample = BlockEffectsSignedChain
                .gen(min, max)
                .sample // apply(params, params.initialSeed.get)
            dumpSkeletonInfo(sample.get)
            sample
        })

    testSkeletons.distinct.zipWithIndex.foreach { case (skeleton, idx) =>
        include(new Properties(s"Skeleton ${idx}") {
            val _ = property("rollbacks are handled") = mkRollbackAreHandled(skeleton)
        })
    }

    def mkRollbackAreHandled(skeleton: BlockEffectsSignedChain): Prop = {

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

            val program = TestControl.executeEmbed {
                // NB: always do long sleeps before starting the actor system
                IO.sleep(
                  // Skip to the rollback time
                  FiniteDuration.apply(
                    rollback.rollbackTime.instant.toEpochMilli,
                    TimeUnit.MILLISECONDS
                  )
                ) >>
                    ActorSystem[IO]("Cardano Liaison SUT").use { system =>
                        for {
                            now <- IO.realTimeInstant
                            _ <- IO.println(s"now=$now")

                            state = MockState.apply(rollback.utxos.asScalus)
                            cardanoBackend <- CardanoBackendMock.mockIO(state)

                            blockWeaver <- system.actorOf(new BlockWeaverMock)

                            config = CardanoLiaison.Config(
                              cardanoBackend,
                              skeleton._1.initialSigned,
                              skeleton._1.fallbackSigned,
                              100.millis,
                              slotConfig = testTxBuilderEnvironment.slotConfig,
                              blockWeaver = blockWeaver
                            )

                            cardanoLiaison <- CardanoLiaison.apply(config)

                            // Use protected handlers directly to present all effects
                            _ <- skeleton._2.traverse_(s =>
                                cardanoLiaison.handleMajorBlockL1Effects(
                                  MajorBlockConfirmed(
                                    blockNum = s.blockNum,
                                    settlementSigned = s.settlementSigned,
                                    rolloutsSigned = s.rolloutsSigned,
                                    fallbackSigned = s.fallbackSigned
                                  )
                                )
                            )

                            _ <- cardanoLiaison.handleFinalBlockL1Effects(
                              FinalBlockConfirmed(
                                blockNum =
                                    // This is technically correct in that particular case, but it's not nice
                                    Block.Number(
                                      skeleton._3.finalizationSigned.majorVersionProduced
                                    ),
                                finalizationSigned = skeleton._3.finalizationSigned,
                                rolloutsSigned = skeleton._3.rolloutsSigned,
                                mbDeinitSigned = skeleton._3.mbDeinitSigned
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
                                    import hydrozoa.lib.cardano.scalus.QuantizedTime.given

                                    rollback.txsLost.size match {
                                        // Only deinit
                                        case 1 =>
                                            rollback.txsLost ++ rollback.txsLostPreviousRollout
                                        // All effects
                                        case _ =>
                                            if rollback.rollbackTime > skeleton.earliestTtl
                                            then rollback.txsLostPreviousRollout
                                            else rollback.txsLost ++ rollback.txsLostPreviousRollout
                                    }
                            }

                            _ <- IO.println(s"===> Expected txs: ${expectedTxs}")

                            // Timeouts doesn't work under TestControl since the current implementation of
                            // [[ReceiveTimeout]] uses System.currentTimeMillis
                            // So for now we send timeout signals explicitly which is just fine.

                            // One timeout is enough for the Cardano Liaison to send all the effects
                            _ <- cardanoLiaisonActor ! CardanoLiaison.Timeout

                            check <- awaitCond(
                              p = IO
                                  .traverse(expectedTxs.toList)(cardanoBackend.getTxInfo)
                                  .flatMap(l => IO.println(l) >> IO.pure(l))
                                  .flatMap(l =>
                                      IO.pure(l.map(_.fold(_ => false, _.isKnown)).forall(identity))
                                  ),
                              max = 1.second,
                              interval = 1.second,
                              message =
                                  "the list of known transaction in the mock should match the list of expected txs"
                            ) >> IO.pure(Prop.proved)

                        } yield check
                    }
            }

            program.unsafeRunSync()
        }

    }

    class BlockWeaverMock extends Actor[IO, BlockWeaver.Request] {
        override def receive: Receive[IO, BlockWeaver.Request] = _ => IO.pure(())
    }
}
