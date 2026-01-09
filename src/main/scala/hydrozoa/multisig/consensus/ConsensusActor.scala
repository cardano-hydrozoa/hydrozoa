package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.consensus.ConsensusActor.{Config, Request}
import hydrozoa.multisig.ledger.dapp.tx.{DeinitTx, FallbackTx, FinalizationTx, RefundTx, RolloutTx, SettlementTx, Tx}
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.types.AckBlock.HeaderSignature.given
import hydrozoa.multisig.protocol.types.AckBlock.{HeaderSignature, TxSignature}
import hydrozoa.multisig.protocol.types.{AckBlock, AugmentedBlock, Block, Peer}
import hydrozoa.{VerificationKeyBytes, attachWitnesses}
import scalus.builtin.{ByteString, platform}
import scalus.cardano.ledger.{TransactionHash, VKeyWitness}

import scala.Function.tupled
import scala.util.control.NonFatal

/** Consensus actor:
  */

object ConsensusActor {

    final case class Config(
        /** Own peer number */
        peerId: Peer.Number,

        /** The mapping from head's peers to their verification keys. */
        verificationKeys: Map[Peer.Number, VerificationKeyBytes],

        /** Latest confirmed block, initially - zero. */
        latestConfirmed: Block.Number = Block.Number.zero,

        /** Requests that haven't been handled, initially empty. */
        recoveredRequests: Seq[Request] = Seq.empty,

        // Actors
        peerLiaison: PeerLiaison.PeerLiaisonRef,
        blockWeaver: BlockWeaver.BlockWeaverRef,
        cardanoLiaison: CardanoLiaison.CardanoLiaisonRef,
        eventSequencer: EventSequencer.EventSequencerRef,
    )

    type Request = AugmentedBlock.Next | AckBlock

    def apply(config: Config): IO[ConsensusActor] = {
        IO(new ConsensusActor(config = config) {})
    }
}

trait ConsensusActor(config: Config) extends Actor[IO, Request] {

    private val stateRef = Ref.unsafe[IO, State](State.mkInitialState(config.latestConfirmed))

    override def preStart: IO[Unit] =
        for {
            _ <- IO.traverse_(config.recoveredRequests)(receive)
        } yield ()

    override def receive: Receive[IO, Request] = {
        case augBlock: AugmentedBlock.Next => handleAugBlock(augBlock)
        case ack: AckBlock                 => handleAck(ack)
    }

    /** Since the ReqBlock messages are sent by the joint ledger actor directly, all we need to do
      * when receiving a new block is to store it in the proper cell.
      */
    def handleAugBlock(augBlock: AugmentedBlock.Next): IO[Unit] = for {
        state <- stateRef.get
        // This is a bit annoying but Scala cannot infer types unless we PM explicitly
        _ <- augBlock match {
            case minor: AugmentedBlock.Minor =>
                state.withCellFor(minor)(_.applyAugBlock(minor))
            case major: AugmentedBlock.Major =>
                state.withCellFor(major)(_.applyAugBlock(major))
            case final_ : AugmentedBlock.Final =>
                state.withCellFor(final_)(_.applyAugBlock(final_))
        }
    } yield ()

    /** Handling acks on the other hand in addition to storing the ack in a cell, requires checking
      * whether it's an own acknowledgement and if so putting it into the ack queue. This is needed
      * only for minor acks and Major1/Final1 acks, since round two acks should always be postponed.
      *
      * @param ack
      *   an arbitrary ack, whether an own one or someone else's one
      */
    def handleAck(ack: AckBlock): IO[Unit] = for {
        state <- stateRef.get
        // This is a bit annoying but Scala cannot infer types unless we PM explicitly
        _ <- ack match {
            case minor: AckBlock.Minor =>
                state.withCellFor(minor)(_.applyAck(minor)) >>
                    IO.whenA(ack.id.peerNum == config.peerId)(state.announceOwnImmediateAck(minor))
            case major1: AckBlock.Major1 =>
                state.withCellFor(major1)(_.applyAck(major1)) >>
                    IO.whenA(ack.id.peerNum == config.peerId)(state.announceOwnImmediateAck(major1))
            case major2: AckBlock.Major2 =>
                state.withCellFor(major2) {
                    case round1: MajorBlockRoundOne => round1.applyAck(major2)
                    case round2: MajorBlockRoundTwo => round2.applyAck(major2)
                }
            case final1: AckBlock.Final1 =>
                state.withCellFor(final1)(_.applyAck(final1)) >>
                    IO.whenA(ack.id.peerNum == config.peerId)(state.announceOwnImmediateAck(final1))
            case final2: AckBlock.Final2 =>
                state.withCellFor(final2) {
                    case round1: FinalBlockRoundOne => round1.applyAck(final2)
                    case round2: FinalBlockRoundTwo => round2.applyAck(final2)
                }
        }
    } yield ()

    // ===================================
    // Actor's internal results - confirmed blocks
    // ===================================
    sealed trait BlockConfirmed

    object BlockConfirmed:

        final case class Minor(
            augBlock: AugmentedBlock.Minor,
            // Verified header signatures
            headerSignatures: Set[HeaderSignature],
            // Fully signed txs
            postDatedRefundsSigned: List[RefundTx.PostDated],
            finalizationRequested: Boolean
        ) extends BlockConfirmed

        final case class Major(
            augBlock: AugmentedBlock.Major,
            fallbackSigned: FallbackTx,
            rolloutsSigned: List[RolloutTx],
            postDatedRefundsSigned: List[RefundTx.PostDated],
            settlementSigned: SettlementTx,
            finalizationRequested: Boolean
        ) extends BlockConfirmed

        final case class Final(
            augBlock: AugmentedBlock.Final,
            rolloutsSigned: List[RolloutTx],
            deinitSigned: Option[DeinitTx],
            finalizationSigned: FinalizationTx
        ) extends BlockConfirmed

    // ===================================
    // Actor's state
    // ===================================

    /** In the currently supported unanimous consensus, there might be no more than N-1 blocks
      * involved in the worst-case scenario, so we need to use map here. Currently, we don't limit
      * the size of the map. Every cell can contain a consensus object for a particular block.
      */
    final case class State(
        /** Consensus cells */
        cells: Map[Block.Number, BlockConsensus[?]],
        /** Latest confirmed block */
        latestConfirmed: Block.Number,
        /** Postponed acks */
        acksPostponed: Seq[AckBlock]
    ) {

        /**   - Gives access to a consensus cell or raise an error if it can't be done
          *   - Afterwards checks whether the cell is saturated and tries to complete it which leads
          *     the either a new round in the cell or to the final result, so we can eliminate the
          *     cell.
          */
        def withCellFor(msg: AugmentedBlock.Next | AckBlock)(
            f: ConsensusFor[msg.type] => IO[ConsensusFor[msg.type]]
        ): IO[Unit] = {

            for {
                blockNum <- IO.pure(msgBlockNum(msg))
                tryCastCell1 = tryCastCell(msg)
                cellAndCallbacks <-
                    this.cells.get(blockNum) match {
                        case None =>
                            spawnCell(msg)
                                .flatMap(newCell =>
                                    IO.pure((newCell, updateCell(blockNum), completeCell(blockNum)))
                                )
                        case Some(cell) =>
                            tryCastCell1(cell)
                                .flatMap(typedCell =>
                                    IO.pure(
                                      (typedCell, updateCell(blockNum), completeCell(blockNum))
                                    )
                                )
                    }

                (consensus, updateCell, completeCell) = cellAndCallbacks

                consensus1 <- f(consensus)
                _ <-
                    if consensus1.isSaturated
                    then {
                        // Complete the round/cell
                        for {
                            ret <- consensus1.complete
                            _ <- ret match {
                                // Switch the cell to the next round
                                case Left(newRound, ownAck) =>
                                    // These casts are sound since the only alternative is Void
                                    announceOwnAck(ownAck.asInstanceOf[AckBlock]) >>
                                        stateRef.set(
                                          updateCell(newRound.asInstanceOf[BlockConsensus[?]])
                                        )
                                // Complete the consensus in the cell
                                case Right(result) =>
                                    for {
                                        // This cast is sound since the only alternative is Void
                                        state1 <- completeCell(result.asInstanceOf[BlockConfirmed])
                                        _ <- stateRef.set(state1)
                                    } yield ()
                            }
                        } yield ()
                    } else {
                        // Update the state with the new consensus state
                        stateRef.set(updateCell(consensus1))
                    }
            } yield ()
        }

        /** Acks, that can be announced upon their receiving in the main handler. */
        type ImmediateAck = AckBlock.Minor | AckBlock.Major1 | AckBlock.Final1

        def announceOwnImmediateAck(immediateAck: ImmediateAck): IO[Unit] = announceOwnAck(
          immediateAck
        )

        // ===================================
        // Private methods
        // ===================================

        private def msgBlockNum(msg: AugmentedBlock.Next | AckBlock): Block.Number = msg match {
            case augBlock: AugmentedBlock.Next => augBlock.block.id
            case ack: AckBlock                 => ack.blockNum
        }

        private def spawnCell(msg: AugmentedBlock.Next | AckBlock): IO[ConsensusFor[msg.type]] = {

            for {
                blockNum <- IO.pure(msgBlockNum(msg))
                result <- msg match {
                    case _: (AugmentedBlock.Minor | AckBlock.Minor) =>
                        MinorBlockConsensus.apply(blockNum)
                    case _: (AugmentedBlock.Major | AckBlock.Major1) =>
                        MajorBlockRoundOne.apply(blockNum)
                    case _: AckBlock.Major2 =>
                        MajorBlockRoundOne.apply(blockNum)
                    case _: (AugmentedBlock.Final | AckBlock.Final1) =>
                        FinalBlockRoundOne.apply(blockNum)
                    case _: AckBlock.Final2 =>
                        FinalBlockRoundOne.apply(blockNum)
                }
            } yield result.asInstanceOf[ConsensusFor[msg.type]]
        }

        private def tryCastCell(
            msg: AugmentedBlock.Next | AckBlock
        )(cell: BlockConsensus[?]): IO[ConsensusFor[msg.type]] = {
            import Error.*

            msg match {
                case _: (AugmentedBlock.Minor | AckBlock.Minor) =>
                    cell match {
                        case c: MinorBlockConsensus =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case _ => IO.raiseError(MsgCellMismatch)
                    }
                case _: (AugmentedBlock.Major | AckBlock.Major1) =>
                    cell match {
                        case c: MajorBlockRoundOne =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case _ => IO.raiseError(MsgCellMismatch)
                    }
                case _: AckBlock.Major2 =>
                    cell match {
                        case c: MajorBlockRoundOne =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case c: MajorBlockRoundTwo =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case _ => IO.raiseError(MsgCellMismatch)
                    }
                case _: (AugmentedBlock.Final | AckBlock.Final1) =>
                    cell match {
                        case c: FinalBlockRoundOne =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case _ => IO.raiseError(MsgCellMismatch)
                    }
                case _: AckBlock.Final2 =>
                    cell match {
                        case c: FinalBlockRoundOne =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case c: FinalBlockRoundTwo =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case _ => IO.raiseError(MsgCellMismatch)
                    }
            }
        }

        private def updateCell(blockNum: Block.Number)(cell: BlockConsensus[?]): State =
            this.copy(cells = cells.updated(blockNum, cell))

        private def completeCell(
            blockNum: Block.Number
        )(_blockConfirmed: BlockConfirmed): IO[State] =
            for {
                // TODO: handle the block confirmed
                _ <- IO.pure(())
            } yield this.copy(cells = cells - blockNum)

        private def announceOwnAck(ack: AckBlock): IO[Unit] = for {
            // Check again it's our own ack
            _ <- IO.raiseWhen(ack.id.peerNum != config.peerId)(Error.AlienAckAnnouncement)
            // TODO: do we really need it or can we always send regardless of the previous block confirmation?
            // By doing that in that worst-case scenario when a node doesn't send its acks to Alice
            // and which may lead to having roughly N cells at the same time, we will
            // just prevents having more than two cells and degrade the head.
            // So I think we can always send acks once they are ready - immediate acks (minor, *1)
            // as soon as we see them in the mailbox, *2 acks once we are done with round one.

            // Check the latest confirmed block and
            _ <-
                if this.latestConfirmed.increment >= ack.blockNum
                then {
                    // Send immediately if latest confirmed is big enough
                    // TODO: send Ack to the peer liaison
                    IO.pure(())
                } else {
                    // Push to the ack queue
                    stateRef.set(this.copy(acksPostponed = this.acksPostponed :+ ack))
                }
        } yield ()

        enum Error extends Throwable:
            case MsgCellMismatch
            case AlienAckAnnouncement
    }

    object State:
        def mkInitialState(latestConfirmed: Block.Number): State =
            State(
              cells = Map.empty,
              latestConfirmed = latestConfirmed,
              acksPostponed = Seq.empty
            )

    /** Represents the state of the consensus on any (except Initial) block [[blockNum]]. The
      * content of the consensus actors cells.
      */
    sealed trait BlockConsensus[+T]:

        /** Type of the augmented block the cell can handle. May be Void if no augmented block is
          * expected.
          */
        type AugBlockType <: AugmentedBlock | Void

        /** Type of the acks the cell can handle */
        type AckType <: AckBlock

        // Invariant: NextRound + OwnAck | ResultType
        /** Next round */
        type NextRound <: BlockConsensus[?] | Void

        /** Type of the ack that should be announced upon switching to the [[NextRound]] */
        type OwnAck <: AckBlock.Major2 | AckBlock.Final2 | Void

        /** Type of the final result */
        type ResultType <: BlockConfirmed | Void

        def blockNum: Block.Number

        def applyAugBlock(augBlock: AugBlockType): IO[T]
        def applyAck(ack: AckType): IO[T]

        /** Whether the cell is ready to be completed */
        def isSaturated: Boolean

        /** Try to complete the cell producing either next round and own ack OR the result */
        def complete: IO[Either[(NextRound, OwnAck), ResultType]]

    // Scala 3 match type is like Haskell closed type family
    type ConsensusFor[B] <: BlockConsensus[?] = B match {
        case AugmentedBlock.Minor => MinorBlockConsensus
        case AckBlock.Minor       => MinorBlockConsensus
        case AugmentedBlock.Major => MajorBlockRoundOne
        case AckBlock.Major1      => MajorBlockRoundOne
        case AckBlock.Major2      => MajorBlockRoundOne | MajorBlockRoundTwo
        case AugmentedBlock.Final => FinalBlockRoundOne
        case AckBlock.Final1      => FinalBlockRoundOne
        case AckBlock.Final2      => FinalBlockRoundOne | FinalBlockRoundTwo
    }

    /** TDOO: find better place, it's used in all type of cells
      *
      * @param someTx
      * @param vkeysAndSigs
      * @tparam T
      * @return
      */
    def validateAndAttachTxSignature[T <: Tx[T]](
        someTx: T,
        vkeysAndSigs: List[(VerificationKeyBytes, TxSignature)]
    ): IO[T] = {
        import someTx.*
        for {
            txId <- IO.pure(tx.id)
            vkeyWitnesses <- IO.pure(
              vkeysAndSigs.map((vk, sig) => VKeyWitness.apply(vk.bytes, sig))
            )
            _ <- IO.traverse_(vkeyWitnesses)(w =>
                IO.delay(platform.verifyEd25519Signature(w.vkey, txId, w.signature))
                    .handleErrorWith {
                        case NonFatal(_) =>
                            IO.raiseError(CompletionError.WrongTxSignature(txId, w.vkey))
                        case e => IO.raiseError(e)
                    }
            )
            signedTx = attachWitnesses(tx, vkeyWitnesses)
        } yield txLens.replace(signedTx)(someTx)
    }

    /** Minor block consensus, may start off by receiving a local augmented block (and local ack) or
      * someone else's acknowledgment.
      *
      * @param augBlock
      * @param acks
      */
    final case class MinorBlockConsensus(
        override val blockNum: Block.Number,
        augBlock: Option[AugmentedBlock.Minor],
        acks: Map[VerificationKeyBytes, AckBlock.Minor]
    ) extends BlockConsensus[MinorBlockConsensus] {
        import CollectingError.*

        override type AugBlockType = AugmentedBlock.Minor
        override type AckType = AckBlock.Minor
        override type NextRound = Void
        override type OwnAck = Void
        override type ResultType = BlockConfirmed.Minor

        override def applyAugBlock(augmentedBlock: AugmentedBlock.Minor): IO[MinorBlockConsensus] =
            for {
                blockNumber <- IO.pure(augmentedBlock.block.id)
                _ <- IO.raiseWhen(this.blockNum != blockNumber)(
                  UnexpectedBlockNumber(this.blockNum, blockNumber)
                )
                _ <- IO.raiseWhen(this.augBlock.isDefined)(
                  UnexpectedAugmentedBlock(this.blockNum)
                )
                newRound = this.copy(augBlock = Some(augmentedBlock))
            } yield newRound

        override def applyAck(ack: AckBlock.Minor): IO[MinorBlockConsensus] = for {
            // Check block number
            blockNum <- IO.pure(ack.blockNum)
            _ <- IO.raiseWhen(this.blockNum != blockNum)(
              UnexpectedBlockNumber(this.blockNum, blockNum)
            )
            // Check peer number
            peerNum = ack.id.peerNum
            verificationKey <- config.verificationKeys
                .get(peerNum)
                .liftTo[IO](UnexpectedPeer(peerNum))
            // Check whether ack already exists
            _ <- this.acks.get(verificationKey).liftTo[IO](UnexpectedAck(blockNum, peerNum))
            newRound = this.copy(acks = this.acks + (verificationKey -> ack))
        } yield newRound

        override def isSaturated: Boolean =
            this.augBlock.isDefined && this.acks.keys == config.verificationKeys.values

        override def complete: IO[Either[(Void, Void), BlockConfirmed.Minor]] = {

            def validateHeaderSignature(
                msg: Block.HeaderMsg
            )(vk: VerificationKeyBytes, sig: HeaderSignature): IO[Unit] =
                IO.delay(platform.verifyEd25519Signature(vk.bytes, msg, sig))
                    .handleErrorWith {
                        case NonFatal(_) =>
                            IO.raiseError(CompletionError.WrongHeaderSignature(vk.bytes))
                        case e => IO.raiseError(e)
                    }
                    .void

            for {
                augBlock <- IO.pure(this.augBlock.get)
                block = augBlock.block

                // 1. Verify header signatures
                validate = validateHeaderSignature(block.header.mkMessage)
                vksAndSigs <- IO.pure(this.acks.map((vk, ack) => vk -> ack.headerSignature).toList)
                _ <- IO.traverse_(vksAndSigs)((vk, sig) => validate(vk, sig))

                // 2. Zip post-dated refunds with signatures, verify them, and attach to the transactions
                refunds = augBlock.effects.postDatedRefunds
                // TODO: can we use transpose or is it too permissive?
                witnessSets = this.acks
                    .map((vk, ack) => ack.postDatedRefunds.map(vk -> _))
                    .toList
                    .transpose
                postDatedRefundsSigned <- refunds
                    .zip(witnessSets)
                    .traverse(tupled(validateAndAttachTxSignature[RefundTx.PostDated]))

                // 3. Check whether someone requested the head to finalize
                finalizationRequested = this.acks.values.foldLeft(false)(_ || _.finalizationRequested)
            } yield Right(
              BlockConfirmed.Minor(
                augBlock = augBlock,
                headerSignatures = vksAndSigs.map(_._2).toSet,
                postDatedRefundsSigned = postDatedRefundsSigned,
                finalizationRequested = finalizationRequested
              )
            )
        }
    }

    object MinorBlockConsensus:
        import CollectingError.*

        def apply(blockNum: Block.Number): IO[MinorBlockConsensus] =
            IO.pure(
              MinorBlockConsensus(
                blockNum,
                None,
                Map.empty
              )
            )

        def apply(ack: AckBlock.Minor): IO[MinorBlockConsensus] = for {
            peerNum <- IO.pure(ack.id.peerNum)
            verificationKey <- config.verificationKeys
                .get(peerNum)
                .liftTo[IO](UnexpectedPeer(peerNum))
        } yield MinorBlockConsensus(
          ack.blockNum,
          None,
          Map(verificationKey -> ack)
        )

    enum CollectingError extends Throwable:
        case UnexpectedBlockNumber(roundBlockNumber: Block.Number, blockNumber: Block.Number)
        case UnexpectedAck(blockNum: Block.Number, peerId: Peer.Number)
        case UnexpectedAugmentedBlock(blockNum: Block.Number)
        case UnexpectedPeer(peer: Peer.Number)

        def msg: String = this match {
            case UnexpectedBlockNumber(roundBlockNumber, blockNumber) =>
                s"Unexpected block number $blockNumber in round for block number $roundBlockNumber"
            case UnexpectedAck(blockNum, peerId) =>
                s"Round for block $blockNum already has an ack from peer $peerId"
            case UnexpectedAugmentedBlock(blockNum) =>
                s"Round for block $blockNum already has an augmented block"
            case UnexpectedPeer(peer) =>
                s"Unexpected peer number: $peer"
        }

    // TODO: add block numbers / peer numbers
    enum CompletionError extends Throwable:
        case WrongHeaderSignature(vkey: ByteString)
        case WrongTxSignature(txId: TransactionHash, vkey: ByteString)

    sealed trait MajorBlockConsensus[T] extends BlockConsensus[T]

    final case class MajorBlockRoundOne(
        override val blockNum: Block.Number,
        augmentedBlock: Option[AugmentedBlock.Major],
        acks1: Map[VerificationKeyBytes, AckBlock.Major1],
        acks2: Map[VerificationKeyBytes, AckBlock.Major2]
    ) extends MajorBlockConsensus[MajorBlockRoundOne] {
        import CollectingError.*

        override type AugBlockType = AugmentedBlock.Major
        override type AckType = AckBlock.Major1 | AckBlock.Major2
        override type NextRound = MajorBlockRoundTwo
        override type OwnAck = AckBlock.Major2
        override type ResultType = Void

        override def applyAugBlock(augmentedBlock: AugmentedBlock.Major): IO[MajorBlockRoundOne] =
            for {
                blockNumber <- IO.pure(augmentedBlock.block.id)
                _ <- IO.raiseWhen(this.blockNum != blockNumber)(
                  UnexpectedBlockNumber(this.blockNum, blockNumber)
                )
                _ <- IO.raiseWhen(this.augmentedBlock.isDefined)(
                  UnexpectedAugmentedBlock(this.blockNum)
                )
                newRound = this.copy(augmentedBlock = Some(augmentedBlock))
            } yield newRound

        override def applyAck(ack: AckBlock.Major1 | AckBlock.Major2): IO[MajorBlockRoundOne] =
            ack match {
                case ack1: AckBlock.Major1 => applyAck1(ack1)
                case ack2: AckBlock.Major2 => applyAck2(ack2)
            }

        private def applyAck1(ack: AckBlock.Major1): IO[MajorBlockRoundOne] = for {
            blockNum <- IO.pure(ack.blockNum)
            _ <- IO.raiseWhen(this.blockNum != blockNum)(
              UnexpectedBlockNumber(this.blockNum, blockNum)
            )
            peerNum = ack.id.peerNum
            verificationKey <- config.verificationKeys
                .get(peerNum)
                .liftTo[IO](UnexpectedPeer(peerNum))
            _ <- this.acks1.get(verificationKey).liftTo[IO](UnexpectedAck(blockNum, peerNum))
            newRound = this.copy(acks1 = this.acks1 + (verificationKey -> ack))
        } yield newRound

        private def applyAck2(ack: AckBlock.Major2): IO[MajorBlockRoundOne] = for {
            blockNum <- IO.pure(ack.blockNum)
            _ <- IO.raiseWhen(this.blockNum != blockNum)(
              UnexpectedBlockNumber(this.blockNum, blockNum)
            )
            peerNum = ack.id.peerNum
            verificationKey <- config.verificationKeys
                .get(peerNum)
                .liftTo[IO](UnexpectedPeer(peerNum))
            _ <- this.acks2.get(verificationKey).liftTo[IO](UnexpectedAck(blockNum, peerNum))
            newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
        } yield newRound

        override def isSaturated: Boolean =
            this.augmentedBlock.isDefined &&
                this.acks1.keySet == config.verificationKeys.values.toSet

        override def complete: IO[Either[(MajorBlockRoundTwo, AckBlock.Major2), Void]] = for {
            augBlock <- this.augmentedBlock.liftTo[IO](
              // This should never happen
              new IllegalStateException("Cannot complete without augmented block")
            )
            roundTwo = MajorBlockRoundTwo(
              blockNum = this.blockNum,
              augmentedBlock = augBlock,
              acks1 = this.acks1,
              acks2 = this.acks2
            )
            ownAck = this.acks2(config.verificationKeys(config.peerId))
        } yield Left((roundTwo, ownAck))
    }

    private object MajorBlockRoundOne:
        import CollectingError.*

        def apply(blockNum: Block.Number): IO[MajorBlockRoundOne] =
            IO.pure(
              MajorBlockRoundOne(
                blockNum,
                None,
                Map.empty,
                Map.empty
              )
            )

    final case class MajorBlockRoundTwo(
        override val blockNum: Block.Number,
        augmentedBlock: AugmentedBlock.Major,
        acks1: Map[VerificationKeyBytes, AckBlock.Major1],
        acks2: Map[VerificationKeyBytes, AckBlock.Major2]
    ) extends MajorBlockConsensus[MajorBlockRoundTwo] {
        import CollectingError.*

        override type AugBlockType = Void
        override type AckType = AckBlock.Major2
        override type NextRound = Void
        override type OwnAck = Void
        override type ResultType = BlockConfirmed.Major

        override def applyAugBlock(augBlock: Void): IO[MajorBlockRoundTwo] =
            IO.raiseError(
              new IllegalStateException("MajorBlockRoundTwo does not accept augmented blocks")
            )

        override def applyAck(ack: AckBlock.Major2): IO[MajorBlockRoundTwo] = for {
            blockNum <- IO.pure(ack.blockNum)
            _ <- IO.raiseWhen(this.blockNum != blockNum)(
              UnexpectedBlockNumber(this.blockNum, blockNum)
            )
            peerNum = ack.id.peerNum
            verificationKey <- config.verificationKeys
                .get(peerNum)
                .liftTo[IO](UnexpectedPeer(peerNum))
            _ <- this.acks2.get(verificationKey).liftTo[IO](UnexpectedAck(blockNum, peerNum))
            newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
        } yield newRound

        override def isSaturated: Boolean =
            this.acks2.keySet == config.verificationKeys.values.toSet

        override def complete: IO[Either[(Void, Void), BlockConfirmed.Major]] = {

            /** TODO:
              *   - verify signatures
              *   - produce the result
              */
            IO.pure(Right(???))
        }
    }

    sealed trait FinalBlockConsensus[T] extends BlockConsensus[T]

    final case class FinalBlockRoundOne(
        override val blockNum: Block.Number,
        augmentedBlock: Option[AugmentedBlock.Final],
        acks1: Map[VerificationKeyBytes, AckBlock.Final1],
        acks2: Map[VerificationKeyBytes, AckBlock.Final2]
    ) extends FinalBlockConsensus[FinalBlockRoundOne] {
        import CollectingError.*

        override type AugBlockType = AugmentedBlock.Final
        override type AckType = AckBlock.Final1 | AckBlock.Final2
        override type NextRound = FinalBlockRoundTwo
        override type OwnAck = AckBlock.Final2
        override type ResultType = Void

        override def applyAugBlock(augmentedBlock: AugmentedBlock.Final): IO[FinalBlockRoundOne] =
            for {
                blockNumber <- IO.pure(augmentedBlock.block.id)
                _ <- IO.raiseWhen(this.blockNum != blockNumber)(
                  UnexpectedBlockNumber(this.blockNum, blockNumber)
                )
                _ <- IO.raiseWhen(this.augmentedBlock.isDefined)(
                  UnexpectedAugmentedBlock(this.blockNum)
                )
                newRound = this.copy(augmentedBlock = Some(augmentedBlock))
            } yield newRound

        override def applyAck(ack: AckBlock.Final1 | AckBlock.Final2): IO[FinalBlockRoundOne] =
            ack match {
                case ack1: AckBlock.Final1 => applyAck1(ack1)
                case ack2: AckBlock.Final2 => applyAck2(ack2)
            }

        private def applyAck1(ack: AckBlock.Final1): IO[FinalBlockRoundOne] = for {
            blockNum <- IO.pure(ack.blockNum)
            _ <- IO.raiseWhen(this.blockNum != blockNum)(
              UnexpectedBlockNumber(this.blockNum, blockNum)
            )
            peerNum = ack.id.peerNum
            verificationKey <- config.verificationKeys
                .get(peerNum)
                .liftTo[IO](UnexpectedPeer(peerNum))
            _ <- this.acks1.get(verificationKey).liftTo[IO](UnexpectedAck(blockNum, peerNum))
            newRound = this.copy(acks1 = this.acks1 + (verificationKey -> ack))
        } yield newRound

        private def applyAck2(ack: AckBlock.Final2): IO[FinalBlockRoundOne] = for {
            blockNum <- IO.pure(ack.blockNum)
            _ <- IO.raiseWhen(this.blockNum != blockNum)(
              UnexpectedBlockNumber(this.blockNum, blockNum)
            )
            peerNum = ack.id.peerNum
            verificationKey <- config.verificationKeys
                .get(peerNum)
                .liftTo[IO](UnexpectedPeer(peerNum))
            _ <- this.acks2.get(verificationKey).liftTo[IO](UnexpectedAck(blockNum, peerNum))
            newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
        } yield newRound

        override def isSaturated: Boolean =
            this.augmentedBlock.isDefined &&
                this.acks1.keySet == config.verificationKeys.values.toSet

        override def complete: IO[Either[(FinalBlockRoundTwo, AckBlock.Final2), Void]] = for {
            augBlock <- this.augmentedBlock.liftTo[IO](
              new IllegalStateException("Cannot complete without augmented block")
            )
            roundTwo = FinalBlockRoundTwo(
              blockNum = this.blockNum,
              augmentedBlock = augBlock,
              acks1 = this.acks1,
              acks2 = this.acks2
            )
            // Own ack should always be present
            ownAck = this.acks2(config.verificationKeys(config.peerId))
        } yield Left((roundTwo, ownAck))
    }

    private object FinalBlockRoundOne:
        import CollectingError.*

        def apply(blockNum: Block.Number): IO[FinalBlockRoundOne] =
            IO.pure(
              FinalBlockRoundOne(
                blockNum,
                None,
                Map.empty,
                Map.empty
              )
            )

    final case class FinalBlockRoundTwo(
        override val blockNum: Block.Number,
        augmentedBlock: AugmentedBlock.Final,
        acks1: Map[VerificationKeyBytes, AckBlock.Final1],
        acks2: Map[VerificationKeyBytes, AckBlock.Final2]
    ) extends FinalBlockConsensus[FinalBlockRoundTwo] {
        import CollectingError.*

        override type AugBlockType = Void
        override type AckType = AckBlock.Final2
        override type NextRound = Void
        override type OwnAck = Void
        override type ResultType = BlockConfirmed.Final

        override def applyAugBlock(augBlock: Void): IO[FinalBlockRoundTwo] =
            IO.raiseError(
              new IllegalStateException("FinalBlockRoundTwo does not accept augmented blocks")
            )

        override def applyAck(ack: AckBlock.Final2): IO[FinalBlockRoundTwo] = for {
            blockNum <- IO.pure(ack.blockNum)
            _ <- IO.raiseWhen(this.blockNum != blockNum)(
              UnexpectedBlockNumber(this.blockNum, blockNum)
            )
            peerNum = ack.id.peerNum
            verificationKey <- config.verificationKeys
                .get(peerNum)
                .liftTo[IO](UnexpectedPeer(peerNum))
            _ <- this.acks2.get(verificationKey).liftTo[IO](UnexpectedAck(blockNum, peerNum))
            newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
        } yield newRound

        override def isSaturated: Boolean =
            this.acks2.keySet == config.verificationKeys.values.toSet

        override def complete: IO[Either[(Void, Void), BlockConfirmed.Final]] = {

            /** TODO:
              *   - verify signatures
              *   - produce the result
              */
            IO.pure(Right(???))
        }
    }
}
