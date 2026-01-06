package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import com.suprnation.actor.test as _
import hydrozoa.UtxoIdL1
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.{GenesisObligation, L2EventGenesis}
import hydrozoa.multisig.protocol.types.*
import hydrozoa.multisig.protocol.types.Block.Version.Full
import hydrozoa.multisig.protocol.types.LedgerEvent.RegisterDeposit
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import scala.collection.immutable.Queue
import scala.concurrent.duration.FiniteDuration
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
            blockCreationTime: FiniteDuration
        ): TestM[Unit] =
            startBlock(StartBlock(blockNum, blockCreationTime))

        /** Start the block at the current real time */
        def startBlockNow(blockNum: Block.Number): TestM[Unit] =
            for {
                blockCreationTime <- liftR(IO.realTime)
                _ <- startBlock(blockNum, blockCreationTime)
            } yield ()

        def completeBlockRegular(req: CompleteBlockRegular): TestM[Unit] =
            for {
                jl <- asks(_.jointLedger)
                _ <- liftR(jl ! req)
            } yield ()

        def completeBlockRegular(
            referenceBlock: Option[Block],
            pollResults: Set[UtxoIdL1]
        ): TestM[Unit] =
            completeBlockRegular(CompleteBlockRegular(referenceBlock, pollResults))

    }

    /** Helper utilties to execute particular scenarios, such as "generating a random deposit and
      * sending it to the JointLedger"
      */
    object Scenarios {

        /** Generate a random (sensible) deposit from the first peer and send it to the joint ledger
          */
        val deposit: TestM[(DepositRefundTxSeq, RegisterDeposit)] = {
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
                    100
                  ),
                  donationToTreasury = Coin.zero,
                  refundValue = virtualOutputsValue,
                  virtualOutputs = virtualOutputs,
                  changeAddress = peer.address(),
                  utxosFunding = utxosFunding
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
            // Step 1: Put the joint ledger in producing mode
            now1 <- liftR(IO.monotonic)
            _ <- startBlock(Block.Number(1), now1)

            // Step 2: generate a deposit and observe that it appears in the dapp ledger correctly
            seqAndReq <- deposit
            (depositRefundTxSeq, depositReq) = seqAndReq

            env <- ask
            // Putting state access in a for block just so that it doesn't accidentally get reused
            _ <- for {
                jlState <- getState
                dlState = jlState.dappLedgerState

                _ <- assertWith(
                  msg =
                      s"We should only have 1 deposit in the state. We have ${dlState.deposits.length}",
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

            // Step 3: Complete a block
            _ <- completeBlockRegular(None, Set.empty)
            _ <-
                for {
                    jointLedgerState <- getState
                    _ <- assertWith(
                      msg = "Finished block should be minor",
                      condition = jointLedgerState match {
                          case JointLedger.Done(block: Block.Minor, _, _) => true
                          case _                                          => false
                      }
                    )
                } yield ()

            // Step 4: Complete another block.
            now2 <- liftR(IO.monotonic)
            _ <- startBlock(Block.Number(2), now2)
            _ <- completeBlockRegular(
              None,
              Set(UtxoIdL1(depositRefundTxSeq.depositTx.depositProduced.toUtxo.input))
            )

            _ <- for {
                jlState <- getState
                majorBlock <- jlState match {
                    case JointLedger.Done(block: Block.Major, _, _) => pure(block)
                    case _ => fail("finished block should be major")
                }
                kzgCommit = IArray
                    .genericWrapArray(jlState.virtualLedgerState.kzgCommitment)
                    .toArray
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

        } yield true)
}
