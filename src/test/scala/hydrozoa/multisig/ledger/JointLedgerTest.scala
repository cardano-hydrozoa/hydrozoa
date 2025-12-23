package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import com.suprnation.actor.test as _
import hydrozoa.multisig.ledger.DappLedger.Requests.{GetState, RegisterDeposit}
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import hydrozoa.multisig.protocol.types.*
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import scalus.cardano.ledger.{Block as _, *}
import scalus.ledger.api.v3.PosixTime
import scalus.prelude.Option as SOption
import test.*
import test.Generators.Hydrozoa.*

object JointLedgerTest extends Properties("Joint Ledger Test") {

    import TestM.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withMinSuccessfulTests(1)
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
            eventId: LedgerEvent.Id,
            virtualOutputs: NonEmptyList[GenesisObligation]
        ): TestM[Unit] =
            registerDeposit(RegisterDeposit(serializedDeposit, eventId, virtualOutputs))

        def registerDeposit(req: RegisterDeposit): TestM[Unit] = {
            for {
                jl <- asks(_.jointLedger)
                _ <- liftR(jl ? req)
            } yield ()
        }

        def startBlock(req: StartBlock): TestM[Unit] =
            ask.flatMap(env => liftR(env.jointLedger ! req))

        def startBlock(
            blockCreationTime: PosixTime,
            pollResults: Set[LedgerEvent.Id]
        ): TestM[Unit] =
            startBlock(StartBlock(blockCreationTime, pollResults))

        def completeBlockRegular(req: CompleteBlockRegular): TestM[Unit] =
            for {
                jl <- asks(_.jointLedger)
                _ <- liftR(jl ! req)
            } yield ()

        def completeBlockRegular(referenceBlock: Option[Block]): TestM[Unit] =
            completeBlockRegular(CompleteBlockRegular(referenceBlock))

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
                      eventId = LedgerEvent.Id(0, 1)
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
            _ <- startBlock(1, Set.empty)

            // Step 2: generate a deposit and observe that it appears in the dapp ledger correctly
            seqAndReq <- deposit
            (depositRefundTxSeq, depositReq) = seqAndReq

            env <- ask
            s <- liftR(env.dappLedger ?: GetState)

            _ <- assertWith(
              msg = s"We should only have 1 deposit in the state. We have ${s.deposits.length}",
              condition = s.deposits.length == 1
            )
            _ <- assertWith(
              msg = "Correct deposit(s) in state",
              condition = s.deposits.head._2 == depositRefundTxSeq.depositTx.depositProduced
            )
            _ <- assertWith(
              msg = "Correct treasury in state",
              condition = s.treasury == env.initTx.initializationTx.treasuryProduced
            )

            // Step 3: Complete a block
            _ <- completeBlockRegular(None)
            jointLedgerState <- getState
            _ <- assertWith(
              msg = "Finished block should be minor",
              condition = jointLedgerState match {
                  case JointLedger.Done(block: Block.Minor) => true
                  case _                                    => false
              }
            )

            // Step 4: Complete another block.
            _ <- startBlock(2, Set(depositReq.eventId))
            _ <- completeBlockRegular(None)

            jointLedgerState <- getState
            majorBlock <- jointLedgerState match {
                case JointLedger.Done(block: Block.Major) => pure(block)
                case _                                    => fail("finished block should be major")
            }

//      kzgCommit <- run(EitherT.right(initTestEnv.virtualLedger ?: VirtualLedger.GetCurrentKzgCommitment))
//      expectedUtxos = L2EventGenesis(depositRefundTxSeq.depositTx.depositProduced.virtualOutputs,
//          TransactionHash.fromByteString(platform.blake2b_256(initTestEnv.config.tokenNames.headTokenName.bytes ++
//            ByteString.fromBigIntBigEndian(BigInt(Full.unapply(majorBlock.header.blockVersion)._1))))).asUtxos
//
//      _ <- assertWith[ET](
//        msg = "KZG Commitment is correct",
//        condition = kzgCommit == KzgCommitment.calculateCommitment(KzgCommitment.hashToScalar(expectedUtxos))
//      )

        } yield true)
}
