package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import com.suprnation.actor.test as _
import hydrozoa.UtxoIdL1
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.dapp.tx.TxTiming
import hydrozoa.multisig.ledger.dapp.tx.TxTiming.*
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.{GenesisObligation, L2EventGenesis}
import hydrozoa.multisig.protocol.types.*
import hydrozoa.multisig.protocol.types.Block.Version.Full
import hydrozoa.multisig.protocol.types.LedgerEvent.RegisterDeposit
import java.time.Instant
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import scala.collection.immutable.Queue
import scala.concurrent.duration.DurationInt
import scalus.builtin.ByteString
import scalus.cardano.ledger.{Block as _, *}
import scalus.prelude.Option as SOption
import test.*
import test.Generators.Hydrozoa.*

object JointLedgerTest extends Properties("Joint Ledger Test") {

    import TestM.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withMinSuccessfulTests(100)
    }

    /** Helper utilities to send actor Requests to the JointLedger
      */
    object Requests {

        val getState: TestM[JointLedger.State] =
            for {
                env <- ask
                state <- liftR(env.jointLedger ?: JointLedger.Requests.GetState)
            } yield state

        def registerDeposit(
            serializedDeposit: Array[Byte],
            eventId: LedgerEventId,
            virtualOutputs: NonEmptyList[GenesisObligation]
        ): TestM[Unit] =
            registerDeposit(RegisterDeposit(eventId, serializedDeposit, virtualOutputs))

        def registerDeposit(req: RegisterDeposit): TestM[Unit] = {
            for {
                jl <- asks(_.jointLedger)
                _ <- liftR(jl ? req)
            } yield ()
        }

        def startBlock(req: StartBlock): TestM[Unit] =
            ask.flatMap(env => liftR(env.jointLedger ! req))

        def startBlock(
            blockNum: Block.Number,
            blockCreationTime: Instant,
        ): TestM[Unit] =
            startBlock(StartBlock(blockNum, blockCreationTime))

        /** Start the block at the current real time */
        def startBlockNow(blockNum: Block.Number): TestM[Instant] =
            for {
                startTime <- liftR(IO.realTimeInstant)
                _ <- startBlock(blockNum, startTime)
            } yield startTime

        // TODO: State should always be "Done" after complete block regular
        def completeBlockRegular(req: CompleteBlockRegular): TestM[Unit] =
            for {
                jl <- asks(_.jointLedger)
                _ <- liftR(jl ! req)
            } yield ()

        def completeBlockRegular(
            referenceBlock: Option[Block],
            pollResults: Set[UtxoIdL1]
        ): TestM[Unit] =
            completeBlockRegular(
              CompleteBlockRegular(referenceBlock, pollResults: Set[UtxoIdL1])
            )

        def completeBlockFinal(req: CompleteBlockFinal): TestM[Unit] =
            for {
                jl <- asks(_.jointLedger)
                _ <- liftR(jl ! req)
            } yield ()

        def completeBlockFinal(
            referenceBlock: Option[Block],
            // FIXME: These can be removed
            pollResults: Set[UtxoIdL1]
        ): TestM[Unit] =
            completeBlockFinal(
              CompleteBlockFinal(referenceBlock = referenceBlock)
            )

    }

    /** Helper utilties to execute particular scenarios, such as "generating a random deposit and
      * sending it to the JointLedger"
      */
    object Scenarios {

        /** Generate a random (sensible) deposit from the first peer and send it to the joint ledger
          */
        def deposit(validityEnd: Instant): TestM[(DepositRefundTxSeq, RegisterDeposit)] = {
            import Requests.*
            for {
                env <- ask
                peer = env.peers.head
                virtualOutputs <-
                    pick(
                      Gen.nonEmptyListOf(genGenesisObligation(peer, minimumCoin = Coin.ada(5)))
                          .map(NonEmptyList.fromListUnsafe)
                          .label("Virtual Outputs")
                    )

                virtualOutputsValue = Value.combine(
                  virtualOutputs.map(vo => Value(vo.l2OutputValue)).toList
                )

                utxosFundingTail <- pick(
                  Gen
                      .listOf(
                        genAdaOnlyPubKeyUtxo(peer, minimumCoin = Coin.ada(5))
                      )
                      .label("Funding Utxos: Tail")
                )

                utxosFundingHead <- pick(
                  genAdaOnlyPubKeyUtxo(
                    peer,
                    minimumCoin = virtualOutputsValue.coin
                  ).label("Funding Utxos: Head")
                )

                utxosFunding = NonEmptyList(utxosFundingHead, utxosFundingTail)

                utxosFundingValue = Value.combine(utxosFunding.toList.map(_._2.value))

                depositRefundSeqBuilder = DepositRefundTxSeq.Builder(
                  config = env.config,
                  refundInstructions = DepositUtxo.Refund.Instructions(
                    LedgerToPlutusTranslation.getAddress(peer.address()),
                    SOption.None,
                    BigInt((validityEnd + env.txTiming.silenceDuration).toEpochMilli)
                  ),
                  donationToTreasury = Coin.zero,
                  refundValue = virtualOutputsValue,
                  virtualOutputs = virtualOutputs,
                  changeAddress = peer.address(),
                  utxosFunding = utxosFunding,
                  validityEnd = validityEnd
                )

                depositRefundTxSeq <- lift(depositRefundSeqBuilder.build)

                signedTx = signTx(peer, depositRefundTxSeq.depositTx.tx)

                serializedDeposit = signedTx.toCbor
                req =
                    RegisterDeposit(
                      serializedDeposit = serializedDeposit,
                      virtualOutputs = virtualOutputs,
                      eventId = LedgerEventId(0, 1)
                    )

                _ <- registerDeposit(req)
            } yield (depositRefundTxSeq, req)
        }
    }

    val _ = property("Joint Ledger Happy Path") =
        import Requests.*
        import Scenarios.*
        run(for {
            env <- ask

            // Put the joint ledger in producing mode
            startTime <- startBlockNow(Block.Number.first)

            // Generate a deposit and observe that it appears in the dapp ledger correctly
            firstDepositValidityEnd = startTime + 10.minutes
            seqAndReq <- deposit(firstDepositValidityEnd)
            (depositRefundTxSeq, depositReq) = seqAndReq

            _ <- for {
                jlState <- getState
                dlState = jlState.dappLedgerState

                _ <- assertWith(
                  msg =
                      s"We should have 1 deposit in the state. We have ${dlState.deposits.length}",
                  condition = dlState.deposits.length == 1
                )
                _ <- assertWith(
                  msg = "Correct deposit(s) in state",
                  condition =
                      dlState.deposits.head._2 == depositRefundTxSeq.depositTx.depositProduced
                )
                _ <- assertWith(
                  msg = "Correct treasury in state",
                  condition = dlState.treasury == env.initTx.initializationTx.treasuryProduced
                )
            } yield ()

            // Complete a block, but assume the deposit didn't show up in the poll results
            _ <- completeBlockRegular(None, Set.empty)
            _ <-
                for {
                    jointLedgerState <- getState
                    _ <- assertWith(
                      msg = "First finished block should always be Major",
                      condition = jointLedgerState match {
                          case JointLedger.Done(block: Block.Major, _, _, _) => true
                          case _                                             => false
                      }
                    )
                    majorBlock: Block.Major =
                        jointLedgerState
                            .asInstanceOf[JointLedger.Done]
                            .producedBlock
                            .asInstanceOf[Block.Major]
                    _ <- assertWith(
                      msg = "Block deposits should be correct",
                      condition = majorBlock.body.depositsRefunded.isEmpty
                          && majorBlock.body.depositsAbsorbed.isEmpty
                    )
                } yield ()

            // Complete another block, assume the deposit shows up in the poll results -- but its not mature yet
            _ <- startBlockNow(Block.Number.first.increment)
            _ <- completeBlockRegular(
              None,
              Set(UtxoIdL1(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input))
            )
            _ <- for {
                jointLedgerState <- getState
                _ <- assertWith(
                  msg = "Finished block should be minor because no deposits were absorbed",
                  condition = jointLedgerState match {
                      case JointLedger.Done(block: Block.Minor, _, _, _) => true
                      case _                                             => false
                  }
                )
            } yield ()

            // Complete another block, including the deposit in the state.
            _ <- startBlock(
              Block.Number.first.increment.increment,
              firstDepositValidityEnd + env.txTiming.depositMaturityDuration
            )
            _ <- completeBlockRegular(
              None,
              Set(UtxoIdL1(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input))
            )

            _ <- for {
                jlState <- getState
                majorBlock <- jlState match {
                    case JointLedger.Done(block: Block.Major, _, _, _) => pure(block)
                    case _ => fail("FAIL: finished block should be major")
                }

                _ <- assertWith(
                  msg = "Deposits should be correct with absorbed deposit",
                  condition = majorBlock.body.depositsAbsorbed == List(depositReq.eventId) &&
                      majorBlock.body.depositsRefunded == List.empty
                )

                expectedUtxos = L2EventGenesis(
                  Queue.from(depositRefundTxSeq.depositTx.depositProduced.virtualOutputs.toList),
                  TransactionHash.fromByteString(
                    scalus.builtin.platform.blake2b_256(
                      env.config.tokenNames.headTokenName.bytes ++
                          ByteString.fromBigIntBigEndian(
                            BigInt(Full.unapply(majorBlock.header.blockVersion)._1)
                          )
                    )
                  )
                ).asUtxos

                _ <- assertWith(
                  msg = "Virtual Ledger should contain expected active utxo",
                  condition = jlState.virtualLedgerState.activeUtxos == expectedUtxos
                )

                kzgCommit = IArray
                    .genericWrapArray(jlState.virtualLedgerState.kzgCommitment)
                    .toArray

                expectedKzg = IArray
                    .genericWrapArray(
                      KzgCommitment.calculateCommitment(KzgCommitment.hashToScalar(expectedUtxos))
                    )
                    .toArray

                _ <- assertWith(
                  msg =
                      s"KZG Commitment is correct.\n\tObtained: ${kzgCommit.mkString("Array(", ", ", ")")}\n\tExpected: ${expectedKzg.mkString("Array(", ", ", ")")}",
                  condition = kzgCommit.sameElements(expectedKzg)
                )
            } yield ()

            // Step 5: Finalize
            _ <- startBlockNow(Block.Number.first.increment.increment.increment)
            _ <- completeBlockFinal(None, Set.empty)
        } yield true)
}
