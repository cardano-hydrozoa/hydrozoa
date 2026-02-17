package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.{ActorSystem, test as _}
import hydrozoa.config.head.HeadPeersSpec.Exact
import hydrozoa.config.head.peers.{TestPeers, generateTestPeers}
import hydrozoa.config.node.{NodeConfig, generateNodeConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.multisig.consensus.ConsensusActor
import hydrozoa.multisig.consensus.ConsensusActor.Request
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.JointLedger.{Done, Producing}
import hydrozoa.multisig.ledger.JointLedgerTestHelpers.*
import hydrozoa.multisig.ledger.JointLedgerTestHelpers.Requests.*
import hydrozoa.multisig.ledger.JointLedgerTestHelpers.Scenarios.*
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.event.LedgerEvent.RegisterDeposit
import hydrozoa.multisig.ledger.event.LedgerEventId
import hydrozoa.multisig.ledger.event.LedgerEventId.ValidityFlag.{Invalid, Valid}
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.tx.{GenesisObligation, L2Genesis}
import io.bullet.borer.Cbor
aimport java.util.concurrent.TimeUnit
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import org.scalacheck.PropertyM.monadForPropM
import org.scalacheck.util.Pretty
import scala.collection.immutable.Queue
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.{Block as _, BlockHeader as _, Coin, *}
import scalus.cardano.onchain.plutus.prelude.Option as SOption
import test.*
import test.Generators.Hydrozoa.*
import test.Generators.Other.genCoinDistributionWithMinAdaUtxo
import test.TestM.*

// Pretty Printers for more managable scalacheck logs
given ppNodeConfig: (NodeConfig => Pretty) = nodeConfig =>
    Pretty(_ => "NodeConfig (too long to print)")

given ppTestPeers: (TestPeers => Pretty) = testPeers =>
    Pretty(_ =>
        "TestPeers:"
            + s"\n\t Num Peers: ${testPeers._testPeers.length}"
            + (testPeers._testPeers.map(testPeer =>
                f"\n\t${testPeer._1.peerNum.toInt}%2d"
                    + s" | ${testPeer._2.wallet.exportVerificationKey.take(2)}(...)"
                    + s" | ${testPeer._2.name} "
            ))
    )

/** This object contains component-specific helpers to utilize the TestM type.
  *
  * It defines the following bits:
  *
  *   - A [[JLTest]] type alias over [[TestM]], to cut down on type signature noise
  *   - A [[TestR]] environment, specific to the joint ledger tests
  *   - A [[defaultInitializer]] that initializers the TestR environment under which the joint
  *     ledger tests run
  *   - A [[Requests]] object that "thinly" lifts [[IO]] actor requests into [[JLTest]], which
  *     simplifies writing monadic code. "Thinly" here means that the defined functions don't
  *     _morally_ add additional effects.
  *     - One exception to this may be testing pre-conditions or post-conditions; for example, every
  *       non-exception-throwing call to `completeBlockRegular` should result transition of a
  *       [[JointLedger.State]] from [[Producing]] to [[Done]], but this can only be accomplished by
  *       sending two additional messages to the JointLedger actor in order to observe the state
  *       before and after. If your test is sensitive to the EXACT messages that are sent to the
  *       actor's mailbox, then you may need to make some modifications.
  *   - A [[Scenarios]] object that includes pre-defined, composable test snippets, making full use
  *     of the embedded PropertyM and IO of the JLTest.
  */
object JointLedgerTestHelpers {

    type JLTest[A] = TestM[TestR, A]
    val defaultInitializer: PropertyM[IO, TestR] = {
        for {
            testPeers <- PropertyM.pick[IO, TestPeers](generateTestPeers())
            config <- PropertyM.pick[IO, NodeConfig](
              generateNodeConfig(Exact(testPeers.headPeers.nHeadPeers.toInt))()
            )
            system <- PropertyM.run(ActorSystem[IO]("DappLedger").allocated.map(_._1))

            consensusActorStub <- PropertyM.run(
              system.actorOf(new Actor[IO, ConsensusActor.Request] {
                  override def receive: Receive[IO, ConsensusActor.Request] = _ => IO.unit
              })
            )

            jointLedger <- PropertyM.run(
              system.actorOf(
                JointLedger(
                  config,
                  JointLedger.Connections(
                    consensusActor = consensusActorStub,
                    peerLiaisons = List()
                  )
                )
              )
            )
        } yield TestR(
          testPeers = testPeers,
          config = config,
          actorSystem = system,
          jointLedger = jointLedger
        )
    }

    /** The "environment" that is contained in the ReaderT of the JLTest
      */
    case class TestR(
        // These might not strictly be needed
        testPeers: TestPeers,
        config: JointLedger.Config,
        actorSystem: ActorSystem[IO],
        jointLedger: ActorRef[IO, JointLedger.Requests.Request]
    )

    /** Helper utilities to send actor Requests to the JointLedger
      */
    object Requests {

        val getState: JLTest[JointLedger.State] =
            for {
                env <- ask
                state <- lift(env.jointLedger ?: JointLedger.Requests.GetState)
            } yield state

        def registerDeposit(req: RegisterDeposit): JLTest[Unit] = {
            for {
                jl <- asks[TestR, ActorRef[IO, JointLedger.Requests.Request]](_.jointLedger)
                _ <- lift(jl ? req)
            } yield ()
        }

        /** Start the block at the current real time */
        def startBlockNow(blockNum: BlockNumber): JLTest[QuantizedInstant] =
            for {
                env <- ask
                startTime <- lift(realTimeQuantizedInstant(env.config.slotConfig))
                _ <- startBlock(blockNum, startTime)
            } yield startTime

        def startBlock(
            blockNum: BlockNumber,
            blockCreationTime: QuantizedInstant,
        ): JLTest[Unit] =
            startBlock(StartBlock(blockNum, blockCreationTime))

        def startBlock(req: StartBlock): JLTest[Unit] =
            ask.flatMap(env => lift(env.jointLedger ! req))

        def completeBlockRegular(
            referenceBlock: Option[BlockBrief.Intermediate],
            pollResults: Set[TransactionInput]
        ): JLTest[Unit] =
            completeBlockRegular(
              CompleteBlockRegular(referenceBlock, pollResults: Set[TransactionInput], false)
            )

        /** WAARNING: This method performs pre-and-post condition checks on the joint ledger. This
          * means two things:
          *   - This will send three messages to the JointLedger -- two to check the state before
          *     and after, and one to actually send the CompleteBlockRegular request
          *   - There is a race condition if this function is used in a context where multiple
          *     messages are being sent to the JointLedger from different sources
          *
          * I (Peter) am trying this out for now (2026-01-08), but I suspect I'll want a better way
          * to do this in the future -- feel free to remove these checks if they're annoying.
          * Perhaps hacking something into to cats-actors so that I can send multiple messages that
          * I know will be processed as a batch is a way forward?
          */
        def completeBlockRegular(req: CompleteBlockRegular): JLTest[Unit] =
            for {
                jl <- asks[TestR, ActorRef[IO, JointLedger.Requests.Request]](_.jointLedger)
                beforeState <- lift(jl ?: JointLedger.Requests.GetState)
                _ <- lift(jl ! req)
                afterState <- lift(jl ?: JointLedger.Requests.GetState)
                _ <- assertWith[TestR](
                  condition = beforeState.isInstanceOf[JointLedger.Producing],
                  msg = "A CompleteBlockRegular request was sent to the JointLedger and it succeeded, but" +
                      " the JointLedger wasn't in the Producing state before the request was sent"
                )
                _ <- assertWith[TestR](
                  condition = afterState.isInstanceOf[JointLedger.Done],
                  msg = "A CompleteBlockRegular request was sent to the JointLedger and it succeeded, but" +
                      " the JointLedger didn't transition to the Done state after the request was processed"
                )
            } yield ()

        def completeBlockFinal(
            referenceBlock: Option[BlockBrief.Final],
        ): JLTest[Unit] =
            completeBlockFinal(
              CompleteBlockFinal(referenceBlockBrief = referenceBlock)
            )

        def completeBlockFinal(req: CompleteBlockFinal): JLTest[Unit] =
            for {
                jl <- asks[TestR, ActorRef[IO, JointLedger.Requests.Request]](_.jointLedger)
                _ <- lift(jl ! req)
            } yield ()

    }

    /** Helper utilties to execute particular scenarios, such as "generating a random deposit and
      * sending it to the JointLedger"
      */
    object Scenarios {

        def unsafeGetProducing: JLTest[Producing] =
            for {
                state <- getState
                p <- state match {
                    case _: Done => throw RuntimeException("Expected a Producing State, got Done")
                    case p: Producing => TestM.pure[TestR, Producing](p)
                }
            } yield p

        def unsafeGetDone: JLTest[Done] =
            for {
                state <- getState
                d <- state match {
                    case d: Done => TestM.pure[TestR, Done](d)
                    case _: Producing =>
                        throw RuntimeException("Expected a Done State, got Producing")
                }
            } yield d

        /** Generate a random (sensible) deposit from the first peer and send it to the joint ledger
          */
        def deposit(
            validityEnd: QuantizedInstant,
            eventId: LedgerEventId,
            blockStartTime: QuantizedInstant
        ): JLTest[(DepositRefundTxSeq, RegisterDeposit)] = {
            import Requests.*
            for {
                env <- ask[TestR]
                peer = env.testPeers._testPeers.head._2

                virtualOutputs <-
                    pick[TestR, NonEmptyList[GenesisObligation]](
                      Gen.nonEmptyListOf(
                        genGenesisObligation(env.config, peer, minimumCoin = Coin.ada(5))
                      ).map(NonEmptyList.fromListUnsafe)
                          .label(s"Virtual Outputs for deposit $eventId")
                    )

                virtualOutputsBytes =
                    Cbor
                        .encode(
                          virtualOutputs.toList.map(_.toBabbage.asInstanceOf[TransactionOutput])
                        )
                        .toByteArray

                virtualOutputsValue = Value.combine(
                  virtualOutputs.map(vo => Value(vo.l2OutputValue)).toList
                )

                utxosFunding <- pick[TestR, NonEmptyList[Utxo]]((for {
                    utxosWith0Coin <- Gen
                        .nonEmptyListOf(
                          genAdaOnlyPubKeyUtxo(env.config, peer, minimumCoin = Coin.ada(3))
                        )
                    utxoDist <- genCoinDistributionWithMinAdaUtxo(
                      virtualOutputsValue.coin,
                      NonEmptyList.fromListUnsafe(utxosWith0Coin),
                      env.config.cardanoProtocolParams
                    )
                } yield utxoDist).label("Funding Utxos"))

                utxosFundingValue = Value.combine(utxosFunding.toList.map(_._2.value))

                depositRefundSeqBuilder = DepositRefundTxSeq.Build(env.config)(
                  refundInstructions = DepositUtxo.Refund.Instructions(
                    LedgerToPlutusTranslation.getAddress(peer.address(env.config.network)),
                    SOption.None,
                    validityEnd
                        + env.config.txTiming.depositMaturityDuration
                        + env.config.txTiming.depositAbsorptionDuration
                        + env.config.txTiming.silenceDuration
                  ),
                  depositFee = Coin.zero,
                  refundValue = virtualOutputsValue,
                  virtualOutputs = virtualOutputs,
                  changeAddress = peer.address(env.config.network),
                  utxosFunding = utxosFunding,
                )

                depositRefundTxSeq <- lift(depositRefundSeqBuilder.result.liftTo[IO])

                // refund(i).validity_start = deposit(i).absorption_end + silence_period
                // refund(i).validity_end = ∅
                _ <- assertWith[TestR](
                  msg = "refund start validity is incorrect",
                  condition = {
                      depositRefundTxSeq.refundTx.tx.body.value.validityStartSlot.isDefined
                      && Slot(depositRefundTxSeq.refundTx.tx.body.value.validityStartSlot.get)
                          .toQuantizedInstant(env.config.slotConfig)
                          ==
                          depositRefundTxSeq.depositTx.validityEnd
                          + env.config.txTiming.depositMaturityDuration
                          + env.config.txTiming.depositAbsorptionDuration
                          + env.config.txTiming.silenceDuration
                  }
                )

                _ <- assertWith[TestR](
                  msg = "refund end validity is incorrect",
                  condition = depositRefundTxSeq.refundTx.tx.body.value.ttl.isEmpty
                )

                req =
                    RegisterDeposit(
                      depositTxBytes = peer.signTx(depositRefundTxSeq.depositTx.tx).toCbor,
                      refundTxBytes = peer.signTx(depositRefundTxSeq.refundTx.tx).toCbor,
                      depositFee = Coin.zero,
                      virtualOutputsBytes = virtualOutputsBytes,
                      eventId = eventId
                    )

                _ <- registerDeposit(req)
            } yield (depositRefundTxSeq, req)
        }
    }
}

object JointLedgerTest extends Properties("Joint Ledger Test") {

    import TestM.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withMinSuccessfulTests(100)
    }

    val _ = property("Joint Ledger Happy Path") = run(
      initializer = defaultInitializer,
      testM = for {
          env <- ask[TestR]

          // Put the joint ledger in producing mode
          startTime <- startBlockNow(BlockNumber.zero.increment)

          // Generate a deposit and observe that it appears in the dapp ledger correctly
          firstDepositValidityEnd = startTime + 10.minutes
          seqAndReq <- deposit(
            validityEnd = firstDepositValidityEnd,
            eventId = LedgerEventId(0, 1),
            blockStartTime = startTime
          )
          (depositRefundTxSeq, depositReq) = seqAndReq

          _ <- for {
              jlState <- getState
              dlState = jlState.dappLedgerState

              _ <- assertWith[TestR](
                msg = s"We should have 1 deposit in the state. We have ${dlState.deposits.length}",
                condition = dlState.deposits.length == 1
              )
              _ <- assertWith[TestR](
                msg = "Correct deposit(s) in state",
                condition = dlState.deposits.head._2 == depositRefundTxSeq.depositTx.depositProduced
              )
              _ <- assertWith[TestR](
                msg = "Correct treasury in state",
                condition = dlState.treasury == env.config.initializationTx.treasuryProduced
              )
          } yield ()

          // Complete a block, but assume the deposit didn't show up in the poll results
          _ <- completeBlockRegular(None, Set.empty)
          _ <-
              for {
                  jointLedgerState <- getState
                  _ <- assertWith[TestR](
                    msg = "Block with no deposits/withdrawal should be Minor",
                    condition = jointLedgerState match {
                        case JointLedger.Done(block: Block.Unsigned.Minor, _, _, _) => true
                        case _                                                      => false
                    }
                  )
                  minorBlock: Block.Unsigned.Minor =
                      jointLedgerState
                          .asInstanceOf[JointLedger.Done]
                          .producedBlock
                          .asInstanceOf[Block.Unsigned.Minor]
                  _ <- assertWith[TestR](
                    msg = "Block's deposit absorbed and deposits refunded should both be empty",
                    condition = minorBlock.body.depositsRefunded.isEmpty
                        && minorBlock.body.depositsAbsorbed.isEmpty
                  )
              } yield ()

          // Complete another block, assume the deposit shows up in the poll results -- but its not mature yet
          _ <- startBlockNow(BlockNumber.zero.increment.increment)
          _ <- completeBlockRegular(
            None,
            Set(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input)
          )
          _ <- for {
              jointLedgerState <- getState
              _ <- assertWith[TestR](
                msg = "Finished block should be minor because no deposits were absorbed",
                condition = jointLedgerState match {
                    case JointLedger.Done(block: Block.Unsigned.Minor, _, _, _) => true
                    case _                                                      => false
                }
              )
          } yield ()

          // Complete another block, including the deposit in the state.
          _ <- startBlock(
            BlockNumber.zero.increment.increment.increment,
            firstDepositValidityEnd + env.config.txTiming.depositMaturityDuration
          )
          _ <- completeBlockRegular(
            None,
            Set(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input)
          )

          _ <- for {
              jlState <- getState
              majorBlock <- jlState match {
                  case JointLedger.Done(block: Block.Unsigned.Major, _, _, _) =>
                      pure[TestR, Block.Unsigned.Major](block)
                  case _ =>
                      fail[TestR, Block.Unsigned.Major]("FAIL: finished block should be major")
              }

              _ <- assertWith[TestR](
                msg = "Deposits should be correct with absorbed deposit",
                condition = majorBlock.body.depositsAbsorbed == List(depositReq.eventId) &&
                    majorBlock.body.depositsRefunded == List.empty
              )

              // Expected UTxOs: The genesis utxos from the deposit + the initial l2 sete
              expectedUtxos = L2Genesis(
                Queue.from(depositRefundTxSeq.depositTx.depositProduced.virtualOutputs.toList),
                TransactionHash.fromByteString(
                  scalus.uplc.builtin.platform.blake2b_256(
                    env.config.headTokenNames.treasuryTokenName.bytes ++
                        ByteString.fromBigIntBigEndian(
                          BigInt(BlockVersion.Full.unapply(majorBlock.header.blockVersion)._1)
                        )
                  )
                )
              ).asUtxos ++ env.config.initialL2Utxos

              _ <- assertWith[TestR](
                msg = "Virtual Ledger should contain expected active utxo",
                condition = jlState.virtualLedgerState.activeUtxos == expectedUtxos
              )

              kzgCommit = jlState.virtualLedgerState.kzgCommitment

              expectedKzg = KzgCommitment.calculateKzgCommitment(
                KzgCommitment.hashToScalar(expectedUtxos)
              )

              _ <- assertWith[TestR](
                msg =
                    s"KZG Commitment is correct.\n\tObtained: ${kzgCommit}\n\tExpected: ${expectedKzg}",
                condition = kzgCommit == expectedKzg
              )

          } yield ()

          // Step 5: Finalize
          _ <- startBlockNow(BlockNumber.zero.increment.increment.increment.increment)
          _ <- completeBlockFinal(None)
      } yield true
    )

    // TODO: This could probably just be unit tests. We're not generating much of interest. Perhaps we set the
    // number of tests to 1? Also, these should be adapted into a model test
    val _ = property("Deposit absorption end Timing Test") = run(
      initializer = defaultInitializer,
      testM = for {
          env <- ask[TestR]
          blockStartTime <- startBlockNow(BlockNumber.zero.increment)

          // Test: Deposit absorption window ends 1 second before block start time
          _ <- for {
              // We need the absorption end to be prior to the start of the block
              //
              //    depositAbsorptionEnd = depositValidity_end + deposit_maturity_duration + deposit_absorption_duration
              //                         = blockStartTime - 1 slot
              // thus
              // depositValidity_end = blockStartTime - 1 slot - deposit_maturity_duration - deposit_absorption_duration
              seqAndReq <- deposit(
                validityEnd = blockStartTime - env.config.txTiming.depositMaturityDuration
                    - env.config.txTiming.depositAbsorptionDuration
                    - FiniteDuration(
                      env.config.slotConfig.slotLength,
                      TimeUnit.MILLISECONDS
                    ),
                LedgerEventId(0, 1),
                blockStartTime
              )
              (depositRefundTxSeq, depositReq) = seqAndReq
              jlState <- unsafeGetProducing

              _ <- assertWith[TestR](
                msg = "Deposit should not be in dapp ledger state",
                condition = jlState.dappLedgerState.deposits == Queue.empty
              )

              _ <- assertWith[TestR](
                msg = "Deposit should be in transient fields as invalid",
                condition = jlState.nextBlockData.events == List((depositReq.eventId, Invalid))
              )
          } yield ()

          // Test: Deposit absorption window ends exactly at block start time
          _ <- for {
              seqAndReq <- deposit(
                blockStartTime -
                    env.config.txTiming.depositMaturityDuration -
                    env.config.txTiming.depositAbsorptionDuration,
                LedgerEventId(0, 2),
                blockStartTime
              )
              (depositRefundTxSeq, depositReq) = seqAndReq
              jlState <- unsafeGetProducing

              _ <- assertWith[TestR](
                msg = "Deposit should be in dapp ledger state",
                condition = jlState.dappLedgerState.deposits == Queue(
                  (depositReq.eventId, depositRefundTxSeq.depositTx.depositProduced)
                )
              )

              _ <- assertWith[TestR](
                msg = "Deposit should be in transient fields as valid",
                condition = jlState.nextBlockData.events.last == (depositReq.eventId, Valid)
              )
          } yield ()
      } yield true
    )

    val _ = property(
      "Deposit absorption start timing test: absorption starts at block start time, deposit gets absorbed"
    ) = run(
      initializer = defaultInitializer,
      testM = for {
          env <- ask[TestR]
          blockStartTime <- startBlockNow(BlockNumber.zero.increment)

          // Test: Deposit absorption window starts exactly at block start time
          _ <- for {
              // We need the absorption start to be prior to the start of the block
              //
              //    blockStartTime         =
              //    depositAbsorptionStart = depositValidity_end + deposit_maturity_duration
              //
              // thus
              //
              //    depositValidityEnd = blockStartTime - deposit_maturity_duration
              seqAndReq <- deposit(
                validityEnd = blockStartTime - env.config.txTiming.depositMaturityDuration,
                LedgerEventId(0, 1),
                blockStartTime = blockStartTime
              )
              (depositRefundTxSeq, depositReq) = seqAndReq
              jlState <- unsafeGetProducing

              _ <- assertWith[TestR](
                msg = "Deposit should be in dapp ledger state",
                condition = jlState.dappLedgerState.deposits == Queue(
                  (depositReq.eventId, depositRefundTxSeq.depositTx.depositProduced)
                )
              )

              _ <- assertWith[TestR](
                msg = "Deposit should be in transient fields as valid",
                condition = jlState.nextBlockData.events == List((depositReq.eventId, Valid))
              )

              // Now we complete the block, including this deposit in the poll results.
              _ <- completeBlockRegular(
                None,
                Set(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input)
              )
              jlState <- unsafeGetDone

              _ <- assertWith[TestR](
                msg = "Produced block should be Major",
                condition = jlState.producedBlock.isInstanceOf[Block.Unsigned.Major]
              )

              majorBlock = jlState.producedBlock.asInstanceOf[Block.Unsigned.Major]

              _ <- assertWith[TestR](
                msg = "Block should contain absorbed deposit",
                condition = majorBlock.body.depositsAbsorbed == List(depositReq.eventId)
              )
          } yield ()
      } yield true
    )

    val _ = property(
      "Deposit absorption start timing test: absorption starts 1 second after block start time"
    ) = run(
      initializer = defaultInitializer,
      testM = for {
          env <- ask[TestR]
          blockStartTime <- startBlockNow(BlockNumber.zero.increment)

          // Test: Deposit absorption window starts 1 second after block start time
          _ <- for {
              // We want:
              //    blockStartTime + 1 =
              //    depositAbsorptionStart = depositValidity_end + deposit_maturity_duration
              //
              // thus
              //
              //    depositValidityEnd = blockStartTime + 1 - deposit_maturity_duration
              seqAndReq <- deposit(
                blockStartTime + 1.seconds - env.config.txTiming.depositMaturityDuration,
                LedgerEventId(0, 1),
                blockStartTime
              )
              (depositRefundTxSeq, depositReq) = seqAndReq
              jlState <- unsafeGetProducing

              _ <- assertWith[TestR](
                msg = "Deposit should be in dapp ledger state",
                condition = jlState.dappLedgerState.deposits == Queue(
                  (depositReq.eventId, depositRefundTxSeq.depositTx.depositProduced)
                )
              )

              _ <- assertWith[TestR](
                msg = "Deposit should be in transient fields as valid",
                condition = jlState.nextBlockData.events == List((depositReq.eventId, Valid))
              )

              // Now we complete the block, including this deposit in the poll results.
              _ <- completeBlockRegular(
                None,
                Set(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input)
              )
              jlState <- unsafeGetDone

              _ <- assertWith[TestR](
                msg = "Produced block should be Minor",
                condition = jlState.producedBlock.isInstanceOf[Block.Unsigned.Minor]
              )

              minorBlock = jlState.producedBlock.asInstanceOf[Block.Unsigned.Minor]

              // This is trivial statement which may be removed
              _ <- assertWith[TestR](
                msg = "Block should not contain absorbed deposit",
                condition = minorBlock.body.depositsAbsorbed == List.empty
              )
          } yield ()
      } yield true
    )

}
