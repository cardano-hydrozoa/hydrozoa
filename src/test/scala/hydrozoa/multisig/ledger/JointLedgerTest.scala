package hydrozoa.multisig.ledger

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import com.suprnation.actor.test as _
import hydrozoa.multisig.ledger.JointLedger.Requests.*
import hydrozoa.multisig.ledger.JointLedgerTest.Requests.*
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.{GenesisObligation, L2EventGenesis, L2EventTransaction}
import hydrozoa.multisig.protocol.types.*
import hydrozoa.multisig.protocol.types.Block.Version.Full
import org.scalacheck.*
import org.scalacheck.Prop.propBoolean
import scala.collection.immutable.Queue
import scalus.builtin.ByteString
import scalus.cardano.address.Network
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Block as _, Transaction as STransaction, *}
import scalus.ledger.api.v3.PosixTime
import scalus.prelude.Option as SOption
import scalus.prelude.Ord.<
import test.*
import test.Generators.Hydrozoa.*

object JointLedgerTest extends Properties("Joint Ledger Test") {

    import TestM.*

    override def overrideParameters(p: Test.Parameters): Test.Parameters = {
        p
            .withMinSuccessfulTests(100)
    }

    object Utilities {

        /** Grab the PeerId number (according to the ordered set of public keys of the head's peers)
          * pertaining to the current test environment
          */
        def peerIdNumber(peer: TestPeer): TestM[Option[Int]] =
            for {
                sortedPeersByPKH <- asks(
                  _.peers.sorted(using
                    Order.fromLessThan((x, y) =>
                        x.wallet.exportVerificationKeyBytes.bytes < y.wallet.exportVerificationKeyBytes.bytes
                    )
                  )
                )
            } yield sortedPeersByPKH.zipWithIndex.find(x => x._1 == peer).map(_._2)
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

        def applyInternalTxL2(id: LedgerEvent.Id, tx: Array[Byte]): TestM[Unit] =
            applyInternalTxL2(ApplyInternalTxL2(id, tx))

        def applyInternalTxL2(req: ApplyInternalTxL2): TestM[Unit] = {
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

        def completeBlockFinal(referenceBlock: Option[Block]): TestM[Unit] =
            completeBlockFinal(CompleteBlockFinal(referenceBlock))

        def completeBlockFinal(req: CompleteBlockFinal): TestM[Unit] =
            for {
                jl <- asks(_.jointLedger)
                _ <- liftR(jl ! req)
            } yield ()

    }

    /** Helper utilties to execute particular scenarios, such as "generating a random deposit and
      * sending it to the JointLedger"
      */
    object Scenarios {

        /** Generate a random (sensible) deposit from the first peer and send it to the joint ledger
          */
        val doRegisterDeposit: TestM[(DepositRefundTxSeq, RegisterDeposit)] = {
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
                      eventId = LedgerEvent.Id(
                        0,
                        1
                      ) // FIXME: this should be gotten from the block weaver (or a mock)
                    )

                _ <- registerDeposit(req)
            } yield (depositRefundTxSeq, req)
        }

        /** Send all inputs specified to `outPeer`
          * @param inputs
          * @param outputsAndDesignation
          * @param l2UtxoSet
          * @param inPeer
          * @param outPeer
          * @param network
          */
        // TODO: Make this more capable of expressing interesting things, and make it generate more
        // adapted from the old "l2EventTransactionFromInputsAndPeer"
        def doApplyL2Tx(
            inputs: TaggedSortedSet[TransactionInput],
            inPeer: TestPeer,
            outPeer: TestPeer
        ): TestM[ApplyInternalTxL2] = for {
            env <- ask
            utxoSet <- getState.map(_.virtualLedgerState.activeUtxos)
            tx = {
                val totalVal: Value =
                    inputs.toSeq.foldLeft(Value.zero)((v, ti) => v + utxoSet(ti).value)

                val txBody: TransactionBody = TransactionBody(
                  inputs = inputs,
                  outputs = IndexedSeq(
                    Babbage(
                      address = TestPeer.address(outPeer, env.config.env.network),
                      value = totalVal,
                      datumOption = None,
                      scriptRef = None
                    )
                  ).map(b => Sized(b.asInstanceOf[TransactionOutput])),
                  fee = Coin(0L)
                )

                val txUnsigned: STransaction =
                    STransaction(
                      body = KeepRaw(txBody),
                      witnessSet = TransactionWitnessSet.empty,
                      isValid = false,
                      auxiliaryData = Some(
                        KeepRaw(
                          Metadata(
                            Map(
                              Word64(CIP67.Tags.head) ->
                                  Metadatum.List(IndexedSeq(Metadatum.Int(2)))
                            )
                          )
                        )
                      )
                    )

                L2EventTransaction(signTx(inPeer, txUnsigned))
            }

            inPeerId <-
                for {
                    mbId <- Utilities.peerIdNumber(inPeer)
                    id <- mbId.match {
                        case Some(id) => TestM.pure(id)
                        case None =>
                            TestM.fail(
                              "doApplyL2Tx requested to send l2 transaction from peer that is not part of" +
                                  s" the head. The peer requested was:\n\t${inPeer}\nbut the peer set is:\n\t${env.peers}"
                            )
                    }
                } yield id
            req = JointLedger.Requests.ApplyInternalTxL2(
              id = LedgerEvent.Id(
                inPeerId,
                1
              ), // FIXME: This should be gotten from the block weaver, or  mock
              tx = tx.transaction.toCbor
            )

            _ <- applyInternalTxL2(req)
        } yield req
    }

    val _ = property("Joint Ledger Happy Path") =
        import Requests.*
        import Scenarios.*
        run(for {
            env <- ask
            // Segment: Put the joint ledger in producing mode
            _ <- startBlock(1, Set.empty)

            // Segement: generate a deposit and observe that it appears in the dapp ledger correctly
            seqAndReq <- doRegisterDeposit
            (depositRefundTxSeq, depositReq) = seqAndReq
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
            } yield (depositRefundTxSeq, depositReq)

            _ <-
                for {
                    _ <- completeBlockRegular(None)
                    jointLedgerState <- getState
                    _ <- assertWith(
                      msg = "Finished block should be minor",
                      condition = jointLedgerState match {
                          case JointLedger.Done(block: Block.Minor, _, _) => true
                          case _                                          => false
                      }
                    )
                } yield ()

            _ <- for {
                // Segment: Complete another block.
                _ <- startBlock(2, Set(depositReq.eventId))
                _ <- completeBlockRegular(None)
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

            // Segement: Pay all L2 UTxos from one peer to the other
            _ <- {
                for {
                    jlState <- getState
                    inputs = TaggedSortedSet.from(jlState.virtualLedgerState.activeUtxos.keySet)
                    _ <- doApplyL2Tx(
                      inputs = inputs,
                      inPeer = env.peers.head,
                      outPeer = env.peers.tail.head,
                    )
                } yield ()
            }

        } yield true)
}
