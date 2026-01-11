package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.ledger.dapp.tx.{DeinitTx, FallbackTx, FinalizationTx, RefundTx, RolloutTx, SettlementTx, Tx}
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.types.AckBlock.HeaderSignature.given
import hydrozoa.multisig.protocol.types.AckBlock.{HeaderSignature, TxSignature}
import hydrozoa.multisig.protocol.types.{AckBlock, AugmentedBlock, Block, Peer}
import hydrozoa.{VerificationKeyBytes, attachWitnesses}
import scala.Function.tupled
import scala.util.control.NonFatal
import scalus.builtin.{ByteString, platform}
import scalus.cardano.ledger.{TransactionHash, VKeyWitness}

/** Round one own acks, wchich must be scheduled immediately upon receiving. There is no way to
  * separate own akcs from others' acks at the type level, but this alias is sthose alias is used
  * only for own acks that come from the joint ledger.
  */
type RoundOneOwnAck = AckBlock.Minor | AckBlock.Major1 | AckBlock.Final1

/** Round two akcs, that should be scheduled for announcing when the cell switches to round two.
  * There is no way to separate own akcs from others' acks at the type level, but those alias is
  * used only for own acks that come from the joint ledger.
  */
type RoundTwoOwnAck = AckBlock.Major2 | AckBlock.Final2

// ===================================
// Consensus cells - Top Level Traits
// ===================================

/** Represents the state of the consensus on any (except Initial) block [[blockNum]]. The content of
  * the consensus actors cells.
  */
sealed trait BlockConsensus[+T]():

    /** Type of the augmented block the cell can handle. May be Void if no augmented block is
      * expected.
      */
    type AugBlockType <: AugmentedBlock | Void

    /** Type of the acks the cell can handle */
    type AckType <: AckBlock

    // Invariant: NextRound + OwnAck | ResultType
    /** Next round */
    type NextRound <: BlockConsensus[?] | Void

    /** Type of the own ack that should be announced upon switching to the [[NextRound]] */
    type OwnAck <: RoundTwoOwnAck | Void

    /** Type of the final result */
    type ResultType

    def blockNum: Block.Number

    def applyAugBlock(augBlock: AugBlockType): IO[T]
    def applyAck(ack: AckType): IO[T]

    /** Whether the cell is ready to be completed */
    def isSaturated: Boolean

    /** Try to complete the cell producing either next round and own ack for it OR the result and
      * the postponed own ack for the next block
      */
    def complete: IO[Either[(NextRound, OwnAck), (ResultType, Option[AckBlock])]]

/** Trait for consensus cells that support postponing acks for the next block. This is applicable to
  * Minor blocks and Major blocks, but not Final blocks.
  */
trait PostponedAckSupport[Self] {
    self: BlockConsensus[Self] =>

    def postponedNextBlockOwnAck: Option[RoundOneOwnAck]

    /** Common implementation for applying postponed acks. Returns a new instance with the postponed
      * ack set.
      */
    protected def withPostponedAck(ack: RoundOneOwnAck): Self

    def applyPostponedAck(ack: RoundOneOwnAck): IO[Self] =
        postponedNextBlockOwnAck match {
            case Some(_) => IO.raiseError(new IllegalStateException("Postponed ack already set"))
            case None =>
                IO.raiseWhen(ack.blockNum != blockNum.increment)(
                  new IllegalStateException("Unexpected postponed ack")
                ) >>
                    IO.pure(withPostponedAck(ack))
        }
}

sealed trait MajorBlockConsensus[T] extends BlockConsensus[T] with PostponedAckSupport[T]

sealed trait FinalBlockConsensus[T] extends BlockConsensus[T]

/** Consensus actor:
  *
  * TODO:
  */
object ConsensusActor {

    final case class Config(
        /** Own peer number */
        peerId: Peer.Number,

        /** The mapping from head's peers to their verification keys. */
        verificationKeys: Map[Peer.Number, VerificationKeyBytes],

        /** Requests that haven't been handled, initially empty. */
        recoveredRequests: Seq[Request] = Seq.empty,

        // Actors
        // TODO: should be many - a liaison pre peer
        peerLiaison: PeerLiaison.PeerLiaisonRef,
        blockWeaver: BlockWeaver.BlockWeaverRef,
        cardanoLiaison: CardanoLiaison.CardanoLiaisonRef,
        eventSequencer: EventSequencer.EventSequencerRef,
    )

    type Request = AugmentedBlock.Next | AckBlock

    // ===================================
    // Actor's state
    // ===================================

    /** In the currently supported unanimous consensus, there might be no more than 2 blocks
      * involved in the worst-case scenario, but we decided to use map here. Currently, we don't
      * limit the size of the map. Every cell can contain a consensus object for a particular block.
      */
    type BlockConsensusAny = BlockConsensus[?]

    final case class State(
        /** Consensus cells */
        cells: Map[Block.Number, BlockConsensusAny]
    )

    object State:
        def mkInitialState: State = State(cells = Map.empty)

    def apply(config: Config): IO[ConsensusActor] = {
        for {
            stateRef <- Ref[IO].of(State.mkInitialState)
        } yield new ConsensusActor(config = config, stateRef = stateRef) {}
    }
}

class ConsensusActor(config: Config, stateRef: Ref[IO, ConsensusActor.State])
    extends Actor[IO, Request] {
    import ConsensusActor.*

    override def preStart: IO[Unit] =
        for {
            _ <- IO.traverse_(config.recoveredRequests)(receive)
        } yield ()

    override def receive: Receive[IO, Request] = {
        case augBlock: AugmentedBlock.Next => handleAugBlock(augBlock)
        case ack: AckBlock                 => handleAck(ack)
    }

    /** Since the ReqBlock messages are sent by the joint ledger actor directly, all we need to do
      * when receiving a new block is to store it in the proper cell. The consensus actor is
      * responsible for sending acks only.
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

    /** Handling acks, on the other hand, in addition to storing the ack in a cell, requires
      * checking whether it's an own acknowledgement and if so shcduling its announcing. This is
      * done differently for different types of acks:
      *   - for [[RoundOneOwnAck]] it's done here right when we receive them
      *   - for [[RoundTwoOwnAck] it's done in the corresponding cells
      *
      * TODO: shall we unify that?
      *
      * @param ack
      *   an arbitrary ack, whether own one or someone else's one
      */
    def handleAck(ack: AckBlock): IO[Unit] = for {
        state <- stateRef.get
        // This is a bit annoying but Scala cannot infer types unless we PM explicitly
        _ <- ack match {
            case minor: AckBlock.Minor =>
                state.withCellFor(minor)(_.applyAck(minor)) >>
                    IO.whenA(ack.id.peerNum == config.peerId)(state.scheduleOwnImmediateAck(minor))
            case major1: AckBlock.Major1 =>
                state.withCellFor(major1)(_.applyAck(major1)) >>
                    IO.whenA(ack.id.peerNum == config.peerId)(state.scheduleOwnImmediateAck(major1))
            case major2: AckBlock.Major2 =>
                state.withCellFor(major2) {
                    case round1: MajorBlockRoundOne => round1.applyAck(major2)
                    case round2: MajorBlockRoundTwo => round2.applyAck(major2)
                }
            case final1: AckBlock.Final1 =>
                state.withCellFor(final1)(_.applyAck(final1)) >>
                    IO.whenA(ack.id.peerNum == config.peerId)(state.scheduleOwnImmediateAck(final1))
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
    sealed trait BlockConfirmed {
        def augBlock: AugmentedBlock.Next
        final lazy val blockNum: Block.Number = augBlock match {
            case ab: AugmentedBlock.Minor => ab._1.id
            case ab: AugmentedBlock.Major => ab._1.id
            case ab: AugmentedBlock.Final => ab._1.id
        }
    }

    object BlockConfirmed:

        final case class Minor(
            override val augBlock: AugmentedBlock.Minor,
            // Verified header signatures
            headerSignatures: Set[HeaderSignature],
            // Fully signed txs
            postDatedRefundsSigned: List[RefundTx.PostDated],
            finalizationRequested: Boolean
        ) extends BlockConfirmed

        final case class Major(
            override val augBlock: AugmentedBlock.Major,
            // Fully signed txs
            fallbackSigned: FallbackTx,
            rolloutsSigned: List[RolloutTx],
            postDatedRefundsSigned: List[RefundTx.PostDated],
            settlementSigned: SettlementTx,
            finalizationRequested: Boolean
        ) extends BlockConfirmed

        final case class Final(
            override val augBlock: AugmentedBlock.Final,
            // Fully signed txs
            rolloutsSigned: List[RolloutTx],
            deinitSigned: Option[DeinitTx],
            finalizationSigned: FinalizationTx
        ) extends BlockConfirmed

    // ===================================
    // State extension methods
    // ===================================

    extension (state: State) {

        /**   - Gives access to a typed consensus cell or raise an error if it can't be done
          *   - Afterwards checks whether the cell is saturated and tries to complete it which leads
          *     the either a new round in the same cell or to the close of cell and the final
          *     result, so we can eliminate the cell.
          */
        def withCellFor(msg: AugmentedBlock.Next | AckBlock)(
            f: ConsensusFor[msg.type] => IO[ConsensusFor[msg.type]]
        ): IO[Unit] = {
            for {
                blockNum <- IO.pure(msgBlockNum(msg))
                consensus <-
                    state.cells.get(blockNum) match {
                        case Some(cell) => tryCastCell(msg)(cell)
                        case None       => spawnCell(msg)
                    }
                // Run asked actions
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
                                    // Ack2 can be safely sent when round 1 is finished (i.e. all first-round acks of
                                    // block N are received) because the peer liaisons guarantee that all acks of
                                    // block (N-1) are received (i.e. block N-1 is confirmed)
                                    // before the first-round acks of block N are received.
                                    // Sanity check: the consensus cell for block N-1 cannot exist now when cell N switches to round two.
                                    announceAck(ownAck.asInstanceOf[AckBlock]) >>
                                        updateCell(newRound.asInstanceOf[BlockConsensus[?]])
                                // Complete the consensus in the cell
                                case Right(result, mbAck) =>
                                    // This cast is sound since the only alternative is Void
                                    completeCell(
                                      result.asInstanceOf[BlockConfirmed],
                                      mbAck
                                    )
                            }
                        } yield ()
                    } else {
                        // Just update the state with the new consensus state
                        updateCell(consensus1)
                    }
            } yield ()
        }

        // ===================================
        // Round one acks handling
        // ===================================

        /** This function will broadcast a first-round ack as follows, depending on whether the
          * previous block's cell is absent:
          *   - If absent, broadcast the ack immediately.
          *   - Otherwise, postpone broadcasting the ack until the previous block's cell is
          *     complete.
          *
          * In the above check, the absence of the previous block's cell means that the previous
          * block has already been confirmed:
          *   - If the ack is for block 1, then the previous block's cell cannot exist because
          *     Hydrozoa heads always start with block 0 already confirmed.
          *   - If the ack is for a block N greater than 1, then two cases are possible:
          *     - (a) This peer is the leader for block N. In this case, when the consensus actor
          *       sees the peer's own ack for block N, it means that the joint ledger has completed
          *       the block. The joint ledger can only do this if it receives the CompleteBlock
          *       signal from the block weaver, and the block weaver will only send this signal when
          *       it receives the BlockConfirmed signal from the consensus actor for the previous
          *       block.
          *     - (b) This peer is a follower for block N. In this case, when the consensus sees the
          *       peer's own ack for block N, it means that the joint ledger has finished
          *       replicating and verifying the block. The peer liaison and block weaver guarantee
          *       that the joint ledger completes blocks in order (i.e. block N-1 before N).
          *       Therefore, the consensus actor would have received the peer's own ack for block
          *       N-1 before the peer's own ack for block N, spawning the cell for block N-1. Thus,
          *       the only way that cell N-1 can be absent is if block N-1 is confirmed.
          */
        def scheduleOwnImmediateAck(ack: RoundOneOwnAck): IO[Unit] = for {
            // Check again it's our own ack
            _ <- IO.raiseWhen(ack.id.peerNum != config.peerId)(Error.AlienAckAnnouncement)
            // The number of the block an ack may depend - the previous block
            prevBlockNum = ack.blockNum.decrement
            _ <- state.cells.get(prevBlockNum) match {
                case Some(cell) =>
                    cell match {
                        case minor: MinorBlockConsensus =>
                            minor
                                .applyPostponedAck(ack)
                                .flatMap(c => updateCell(c.asInstanceOf[BlockConsensus[?]]))
                        case major: MajorBlockConsensus[?] =>
                            major
                                .applyPostponedAck(ack)
                                .flatMap(c => updateCell(c.asInstanceOf[BlockConsensus[?]]))
                        case _ => IO.raiseError(Error.UnexpectedPreviousBlockCell)
                    }
                case None => announceAck(ack)
            }
        } yield ()

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

        private def updateCell(cell: BlockConsensus[?]): IO[Unit] =
            stateRef.set(state.copy(cells = state.cells.updated(cell.blockNum, cell)))

        private def completeCell(
            blockConfirmed: BlockConfirmed,
            mbAck: Option[AckBlock]
        ): IO[Unit] =
            for {
                // TODO: handle the block confirmed1
                // Announce the ack if present
                _ <- mbAck.traverse_(announceAck)
                // Remove the cell
                _ <- stateRef.set(state.copy(cells = state.cells - blockConfirmed.blockNum))
            } yield ()

        /** Broadcasts an ack to peer liaisons for distribution to other peers.
          *
          * Note: The actual broadcast mechanism depends on the peer liaison implementation.
          */
        private def announceAck(ack: AckBlock): IO[Unit] =
            // TODO: Implement actual broadcast logic via peer liaison
            // This will likely involve creating a NewMsgBatch with the ack
            // and sending it to config.peerLiaison
            IO.unit
    }

    enum Error extends Throwable:
        case MsgCellMismatch
        case AlienAckAnnouncement
        case UnexpectedPreviousBlockCell

    // ===================================
    // Consensus cells
    // ===================================

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

    /** TODO: find better place, it's used in all type of cells
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

    // ===================================
    // Minor cell
    // ===================================

    /** Minor block consensus, may start off by receiving a local augmented block (and local ack) or
      * someone else's acknowledgment.
      *
      * @param augBlock
      * @param acks
      */
    final case class MinorBlockConsensus(
        override val blockNum: Block.Number,
        augBlock: Option[AugmentedBlock.Minor],
        acks: Map[VerificationKeyBytes, AckBlock.Minor],
        postponedNextBlockOwnAck: Option[RoundOneOwnAck]
    ) extends BlockConsensus[MinorBlockConsensus]
        with PostponedAckSupport[MinorBlockConsensus] {
        import CollectingError.*

        override type AugBlockType = AugmentedBlock.Minor
        override type AckType = AckBlock.Minor
        override type NextRound = Void
        override type OwnAck = Void
        override type ResultType = BlockConfirmed.Minor

        override protected def withPostponedAck(ack: RoundOneOwnAck): MinorBlockConsensus =
            copy(postponedNextBlockOwnAck = Some(ack))

        override def applyAugBlock(
            augmentedBlock: AugmentedBlock.Minor
        ): IO[MinorBlockConsensus] =
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

        override def applyAck(ack: AckBlock.Minor): IO[MinorBlockConsensus] =
            for {
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
                _ <- IO.raiseWhen(this.acks.contains(verificationKey))(
                  UnexpectedAck(blockNum, peerNum)
                )
                newRound = copy(acks = acks + (verificationKey -> ack))
            } yield newRound

        override def isSaturated: Boolean =
            augBlock.isDefined && acks.keys == config.verificationKeys.values

        override def complete
            : IO[Either[(Void, Void), (BlockConfirmed.Minor, Option[AckBlock])]] = {

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
                augBlock <- IO.pure(augBlock.get)
                block = augBlock.block

                // 1. Verify header signatures
                validate = validateHeaderSignature(block.header.mkMessage)
                vksAndSigs <- IO.pure(acks.map((vk, ack) => vk -> ack.headerSignature).toList)
                _ <- IO.traverse_(vksAndSigs)((vk, sig) => validate(vk, sig))

                // 2. Zip post-dated refunds with signatures, verify them, and attach to the transactions
                refunds = augBlock.effects.postDatedRefunds
                witnessSets = acks
                    .map((vk, ack) => ack.postDatedRefunds.map(vk -> _))
                    .toList
                    .transpose
                postDatedRefundsSigned <- refunds
                    .zip(witnessSets)
                    .traverse(tupled(validateAndAttachTxSignature[RefundTx.PostDated]))

                // 3. Check whether someone requested the head to finalize
                finalizationRequested = acks.values
                    .foldLeft(false)(_ || _.finalizationRequested)
            } yield Right(
              BlockConfirmed.Minor(
                augBlock = augBlock,
                headerSignatures = vksAndSigs.map(_._2).toSet,
                postDatedRefundsSigned = postDatedRefundsSigned,
                finalizationRequested = finalizationRequested
              ) -> postponedNextBlockOwnAck
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
                Map.empty,
                None
              )
            )

    enum CollectingError extends Throwable:
        case UnexpectedBlockNumber(roundBlockNumber: Block.Number, blockNumber: Block.Number)
        case UnexpectedAck(blockNum: Block.Number, peerId: Peer.Number)
        case UnexpectedAugmentedBlock(blockNum: Block.Number)
        case UnexpectedPeer(peer: Peer.Number)
        case PostponedAckAlreadySet
        case UnexpectedPosponedAck

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
        case MissingDeinitSignature

    // ===================================
    // Major cells
    // ===================================

    final case class MajorBlockRoundOne(
        override val blockNum: Block.Number,
        augmentedBlock: Option[AugmentedBlock.Major],
        acks1: Map[VerificationKeyBytes, AckBlock.Major1],
        acks2: Map[VerificationKeyBytes, AckBlock.Major2],
        postponedNextBlockOwnAck: Option[RoundOneOwnAck]
    ) extends MajorBlockConsensus[MajorBlockRoundOne] {
        import CollectingError.*

        override type AugBlockType = AugmentedBlock.Major
        override type AckType = AckBlock.Major1 | AckBlock.Major2
        override type NextRound = MajorBlockRoundTwo
        override type OwnAck = AckBlock.Major2
        override type ResultType = Void

        override protected def withPostponedAck(ack: RoundOneOwnAck): MajorBlockRoundOne =
            copy(postponedNextBlockOwnAck = Some(ack))

        override def applyAugBlock(
            augmentedBlock: AugmentedBlock.Major
        ): IO[MajorBlockRoundOne] =
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

        override def applyAck(
            ack: AckBlock.Major1 | AckBlock.Major2
        ): IO[MajorBlockRoundOne] =
            ack match {
                case ack1: AckBlock.Major1 => applyAck1(ack1)
                case ack2: AckBlock.Major2 => applyAck2(ack2)
            }

        private def applyAck1(ack: AckBlock.Major1): IO[MajorBlockRoundOne] =
            for {
                blockNum <- IO.pure(ack.blockNum)
                _ <- IO.raiseWhen(this.blockNum != blockNum)(
                  UnexpectedBlockNumber(this.blockNum, blockNum)
                )
                peerNum = ack.id.peerNum
                verificationKey <- config.verificationKeys
                    .get(peerNum)
                    .liftTo[IO](UnexpectedPeer(peerNum))
                _ <- IO.raiseWhen(this.acks1.contains(verificationKey))(
                  UnexpectedAck(blockNum, peerNum)
                )
                newRound = this.copy(acks1 = this.acks1 + (verificationKey -> ack))
            } yield newRound

        private def applyAck2(ack: AckBlock.Major2): IO[MajorBlockRoundOne] =
            for {
                blockNum <- IO.pure(ack.blockNum)
                _ <- IO.raiseWhen(this.blockNum != blockNum)(
                  UnexpectedBlockNumber(this.blockNum, blockNum)
                )
                peerNum = ack.id.peerNum
                verificationKey <- config.verificationKeys
                    .get(peerNum)
                    .liftTo[IO](UnexpectedPeer(peerNum))
                _ <- IO.raiseWhen(this.acks2.contains(verificationKey))(
                  UnexpectedAck(blockNum, peerNum)
                )
                newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
            } yield newRound

        override def isSaturated: Boolean =
            this.augmentedBlock.isDefined &&
                this.acks1.keySet == config.verificationKeys.values.toSet

        override def complete
            : IO[Either[(MajorBlockRoundTwo, AckBlock.Major2), (Void, Option[AckBlock])]] = for {
            augBlock <- this.augmentedBlock.liftTo[IO](
              // This should never happen
              new IllegalStateException("Cannot complete without augmented block")
            )
            roundTwo = MajorBlockRoundTwo(
              blockNum = this.blockNum,
              augBlock = augBlock,
              acks1 = this.acks1,
              acks2 = this.acks2,
              postponedNextBlockOwnAck = postponedNextBlockOwnAck
            )
            ownAck <- config.verificationKeys
                .get(config.peerId)
                .flatMap(vkey => this.acks2.get(vkey))
                .liftTo[IO](
                  new IllegalStateException(s"Own Major2 ack not found for block $blockNum")
                )
        } yield Left((roundTwo, ownAck))
    }

    private object MajorBlockRoundOne:
        def apply(blockNum: Block.Number): IO[MajorBlockRoundOne] =
            IO.pure(
              MajorBlockRoundOne(
                blockNum,
                None,
                Map.empty,
                Map.empty,
                None
              )
            )

    final case class MajorBlockRoundTwo(
        override val blockNum: Block.Number,
        augBlock: AugmentedBlock.Major,
        acks1: Map[VerificationKeyBytes, AckBlock.Major1],
        acks2: Map[VerificationKeyBytes, AckBlock.Major2],
        postponedNextBlockOwnAck: Option[RoundOneOwnAck]
    ) extends MajorBlockConsensus[MajorBlockRoundTwo] {
        import CollectingError.*

        override type AugBlockType = Void
        override type AckType = AckBlock.Major2
        override type NextRound = Void
        override type OwnAck = Void
        override type ResultType = BlockConfirmed.Major

        override protected def withPostponedAck(ack: RoundOneOwnAck): MajorBlockRoundTwo =
            copy(postponedNextBlockOwnAck = Some(ack))

        override def applyAugBlock(augBlock: Void): IO[MajorBlockRoundTwo] =
            IO.raiseError(
              new IllegalStateException("MajorBlockRoundTwo does not accept augmented blocks")
            )

        override def applyAck(ack: AckBlock.Major2): IO[MajorBlockRoundTwo] =
            for {
                blockNum <- IO.pure(ack.blockNum)
                _ <- IO.raiseWhen(this.blockNum != blockNum)(
                  UnexpectedBlockNumber(this.blockNum, blockNum)
                )
                peerNum = ack.id.peerNum
                verificationKey <- config.verificationKeys
                    .get(peerNum)
                    .liftTo[IO](UnexpectedPeer(peerNum))
                _ <- IO.raiseWhen(this.acks2.contains(verificationKey))(
                  UnexpectedAck(blockNum, peerNum)
                )
                newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
            } yield newRound

        override def isSaturated: Boolean =
            this.acks2.keySet == config.verificationKeys.values.toSet

        override def complete: IO[Either[(Void, Void), (BlockConfirmed.Major, Option[AckBlock])]] =
            for {

                // 1.Fallback
                fallbackWitnessSet <- IO.pure(acks1.map((vk, ack) => vk -> ack.fallback).toList)
                fallbackTxSigned <- validateAndAttachTxSignature(
                  augBlock.effects.fallback,
                  fallbackWitnessSet
                )

                // 2.Rollouts
                rolloutWitnessSets = acks1
                    .map((vk, ack) => ack.rollouts.map(vk -> _))
                    .toList
                    .transpose
                rolloutsSigned <- augBlock.effects.rollouts
                    .zip(rolloutWitnessSets)
                    .traverse(tupled(validateAndAttachTxSignature[RolloutTx]))

                // 3. Post-dated refunds
                postDatedRefundsWitnessSets = acks1
                    .map((vk, ack) => ack.postDatedRefunds.map(vk -> _))
                    .toList
                    .transpose
                postDatedRefundsSigned <- augBlock.effects.postDatedRefunds
                    .zip(postDatedRefundsWitnessSets)
                    .traverse(tupled(validateAndAttachTxSignature[RefundTx.PostDated]))

                // 4. Settlement
                settlementWitnessSet <- IO.pure(acks2.map((vk, ack) => vk -> ack.settlement).toList)
                settlementTxSigned <- validateAndAttachTxSignature(
                  augBlock.effects.settlement,
                  settlementWitnessSet
                )

                // 5. Check whether someone requested the head to finalize
                finalizationRequested = acks1.values
                    .foldLeft(false)(_ || _.finalizationRequested)

            } yield Right(
              BlockConfirmed.Major(
                augBlock = augBlock,
                fallbackSigned = fallbackTxSigned,
                rolloutsSigned = rolloutsSigned,
                postDatedRefundsSigned = postDatedRefundsSigned,
                settlementSigned = settlementTxSigned,
                finalizationRequested = finalizationRequested
              ),
              postponedNextBlockOwnAck
            )
    }

    // ===================================
    // Final cells
    // ===================================

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

        private def applyAck1(ack: AckBlock.Final1): IO[FinalBlockRoundOne] =
            for {
                blockNum <- IO.pure(ack.blockNum)
                _ <- IO.raiseWhen(this.blockNum != blockNum)(
                  UnexpectedBlockNumber(this.blockNum, blockNum)
                )
                peerNum = ack.id.peerNum
                verificationKey <- config.verificationKeys
                    .get(peerNum)
                    .liftTo[IO](UnexpectedPeer(peerNum))
                _ <- IO.raiseWhen(this.acks1.contains(verificationKey))(
                  UnexpectedAck(blockNum, peerNum)
                )
                newRound = this.copy(acks1 = this.acks1 + (verificationKey -> ack))
            } yield newRound

        private def applyAck2(ack: AckBlock.Final2): IO[FinalBlockRoundOne] =
            for {
                blockNum <- IO.pure(ack.blockNum)
                _ <- IO.raiseWhen(this.blockNum != blockNum)(
                  UnexpectedBlockNumber(this.blockNum, blockNum)
                )
                peerNum = ack.id.peerNum
                verificationKey <- config.verificationKeys
                    .get(peerNum)
                    .liftTo[IO](UnexpectedPeer(peerNum))
                _ <- IO.raiseWhen(this.acks2.contains(verificationKey))(
                  UnexpectedAck(blockNum, peerNum)
                )
                newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
            } yield newRound

        override def isSaturated: Boolean =
            this.augmentedBlock.isDefined &&
                this.acks1.keySet == config.verificationKeys.values.toSet

        override def complete
            : IO[Either[(FinalBlockRoundTwo, AckBlock.Final2), (Void, Option[AckBlock])]] = for {
            augBlock <- this.augmentedBlock.liftTo[IO](
              new IllegalStateException("Cannot complete without augmented block")
            )
            roundTwo = FinalBlockRoundTwo(
              blockNum = this.blockNum,
              augBlock = augBlock,
              acks1 = this.acks1,
              acks2 = this.acks2
            )
            // Own ack should always be present
            ownAck <- config.verificationKeys
                .get(config.peerId)
                .flatMap(vkey => this.acks2.get(vkey))
                .liftTo[IO](
                  new IllegalStateException(s"Own Final2 ack not found for block $blockNum")
                )
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
        augBlock: AugmentedBlock.Final,
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

        override def applyAck(ack: AckBlock.Final2): IO[FinalBlockRoundTwo] =
            for {
                blockNum <- IO.pure(ack.blockNum)
                _ <- IO.raiseWhen(this.blockNum != blockNum)(
                  UnexpectedBlockNumber(this.blockNum, blockNum)
                )
                peerNum = ack.id.peerNum
                verificationKey <- config.verificationKeys
                    .get(peerNum)
                    .liftTo[IO](UnexpectedPeer(peerNum))
                _ <- IO.raiseWhen(this.acks2.contains(verificationKey))(
                  UnexpectedAck(blockNum, peerNum)
                )
                newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
            } yield newRound

        override def isSaturated: Boolean =
            this.acks2.keySet == config.verificationKeys.values.toSet

        override def complete: IO[Either[(Void, Void), (BlockConfirmed.Final, Option[AckBlock])]] =
            for {

                // 1.Rollouts
                rolloutWitnessSets <- IO.pure(
                  acks1
                      .map((vk, ack) => ack.rollouts.map(vk -> _))
                      .toList
                      .transpose
                )
                rolloutsSigned <- augBlock.effects.rollouts
                    .zip(rolloutWitnessSets)
                    .traverse(tupled(validateAndAttachTxSignature[RolloutTx]))

                // 2. Finalization
                finalizationWitnessSet <- IO.pure(
                  acks2.map((vk, ack) => vk -> ack.finalization).toList
                )
                finalizationSigned <- validateAndAttachTxSignature(
                  augBlock.effects.finalization,
                  finalizationWitnessSet
                )

                // 3. Optional deinit
                mbDeinitSigned <- augBlock.effects.deinit match {
                    case None => IO.pure(None)
                    case Some(deinit) =>
                        for {
                            deinitWitnessSet <- acks1.toList.traverse { case (vk, ack) =>
                                ack.deinit
                                    .liftTo[IO](CompletionError.MissingDeinitSignature)
                                    .map(vk -> _)
                            }
                            deinitSigned <- validateAndAttachTxSignature(deinit, deinitWitnessSet)
                        } yield Some(deinitSigned)
                }
            } yield Right(
              BlockConfirmed.Final(
                augBlock = augBlock,
                rolloutsSigned = rolloutsSigned,
                finalizationSigned = finalizationSigned,
                deinitSigned = mbDeinitSigned
              ),
              None
            )

    }
}
