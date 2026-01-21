package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.ledger.dapp.tx.{DeinitTx, RefundTx, RolloutTx, Tx}
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.types.AckBlock.HeaderSignature.given
import hydrozoa.multisig.protocol.types.AckBlock.{HeaderSignature, TxSignature}
import hydrozoa.multisig.protocol.types.{AckBlock, AugmentedBlock, Block, BlockEffectsSigned, Peer}
import hydrozoa.{VerificationKeyBytes, attachVKeyWitnesses}
import scala.Function.tupled
import scala.util.control.NonFatal
import scalus.builtin.{ByteString, platform}
import scalus.cardano.ledger.{TransactionHash, VKeyWitness}

/** Round one own acks, which must be scheduled immediately upon receiving. There is no way to
  * separate own akcs from others' acks at the type level, but this alias is those alias is used
  * only for own acks that come from the joint ledger.
  */
type RoundOneOwnAck = AckBlock.Minor | AckBlock.Major1 | AckBlock.Final1

/** Round two akcs, which should be scheduled for announcing when the cell switches to round two.
  * There is no way to separate own akcs from others' acks at the type level, but those alias is
  * used only for own acks that come from the joint ledger.
  */
type RoundTwoOwnAck = AckBlock.Major2 | AckBlock.Final2

// ===================================
// Consensus cells - Top Level Traits
// ===================================

// Traits are defined here to prevent having instance-bound types like ConsensusActor.this.type.

/** Represents the state of the consensus on any (except Initial) block [[blockNum]]. The content of
  * the consensus actors cells.
  */
sealed trait ConsensusCell[+T]():

    /** Type of the augmented block the cell can handle. May be Void if no augmented block is
      * expected in round two.
      */
    type AugBlockType <: AugmentedBlock | Void

    /** Types of the acks the cell can handle, both round one and round two. The reason we need to
      * have both is that round two akcs from some peers can come when the cell is still in the
      * round one.
      */
    type AckType <: AckBlock

    // Invariant: RoundTwoType + RoundTwoOwnAckType  | ResultType

    /** Next round */
    type RoundTwoType <: ConsensusCell[?] | Void

    /** Type of the own ack that should be announced upon switching to the [[RoundTwoType]] */
    type RoundTwoOwnAckType <: RoundTwoOwnAck | Void

    /** Type of the final result */
    type ResultType

    /** Every cell corresponds to a particular block.
      */
    def blockNum: Block.Number

    /** Handles an incoming augmented block, may throw.
      * @param augBlock
      * @return
      *   the updated cell
      */
    def applyAugBlock(augBlock: AugBlockType): IO[T]

    /** Handles an incoming ack, may throw.
      * @param ack
      * @return
      *   the updated cell
      */
    def applyAck(ack: AckType): IO[T]

    /** Whether the cell is ready to be [[complete]]d. */
    def isSaturated: Boolean

    /** Try to complete the cell producing either round two + own round two ack for it OR the result
      * and the optional postponed own round one ack for the next block.
      */
    def complete
        : IO[Either[(RoundTwoType, RoundTwoOwnAckType), (ResultType, Option[RoundOneOwnAck])]]

/** Trait for consensus cells that support postponing acks for the next block. This is applicable to
  * Minor blocks and Major blocks, but not Final blocks.
  */
trait PostponedAckSupport[Self] {
    self: ConsensusCell[Self] =>

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

sealed trait MajorConsensusCell[T] extends ConsensusCell[T] with PostponedAckSupport[T]

sealed trait FinalConsensusCell[T] extends ConsensusCell[T]

/** Consensus Actor - Coordinates multi-party consensus for blocks in Hydrozoa state channels.
  *
  * ==Overview==
  *
  * The ConsensusActor is the central coordinator for achieving unanimous agreement among multiple
  * parties on state channel blocks. It implements a round-based consensus protocol where peers
  * exchange acknowledgments (acks) with cryptographic signatures.
  *
  * ==Architecture==
  *
  * The actor maintains consensus cells (`ConsensusCell[T]`) for each block, where each cell tracks:
  *   - The augmented block containing L2 transactions and effects
  *   - Acknowledgments from all peers with their signatures
  *   - Progress through consensus rounds (1 or 2 rounds depending on block type)
  *
  * ===Block Types and Consensus Rounds===
  *
  * '''Minor Blocks''' (Single Round):
  *   - Announce own Minor ack
  *   - Collect header signatures and post-dated refund signatures
  *   - Complete when all peers have acknowledged
  *
  * '''Major Blocks''' (Two Rounds):
  *   - Round 1: Collect augmented block and Major1 acks with fallback/rollout signatures (though
  *     Major2 acks may come as well)
  *   - Switch between `MajorRoundOneCell` and `MajorRoundTwoCell`, announcing own Major2 ack
  *   - Round 2: Collect Major2 acks with settlement signatures
  *   - Complete when all round 2 acks are collected
  *
  * '''Final Blocks''' (Two Rounds):
  *   - Round 1: Collect augmented block and Final1 acks with rollout/deinit signatures (though
  *     Final2 acks may come as well)
  *   - Switch between `FinalRoundOneCell` and `FinalRoundTwoCell`, announcing own Final2 ack
  *   - Round 2: Collect Final2 acks with finalization signatures
  *   - Complete when all round 2 acks are collected
  *
  * ==Message Handling==
  *
  * The actor processes two types of messages:
  *
  * '''Augmented Blocks''' (`AugmentedBlock.Next`):
  *   - Received from the joint ledger when a new block is created
  *   - Stored in the appropriate consensus cell for the block number
  *   - Contains block and effects calculated by the joint ledger for a produced/validated block
  *
  * '''Acknowledgments''' (`AckBlock`):
  *   - Other peers' acks received from peer liaisons when peers acknowledge blocks
  *   - Contains cryptographic signatures over transactions
  *   - Own acks (from this peer) come from the joint ledger and handled differently
  *
  * ==Own Ack Scheduling==
  *
  * The actor handles own acknowledgments specially to ensure proper ordering:
  *
  *   - '''Round 1 acks''' (`RoundOneOwnAck`): Scheduled immediately upon receipt, as they indicate
  *     the joint ledger has finished processing the block
  *   - '''Round 2 acks''' (`RoundTwoOwnAck`): Scheduled when the cell switches from round 1 to
  *     round 2, ensuring all peers have completed round 1 first
  *
  * ==Postponed Acks==
  *
  * For Minor and Major blocks, acks for block N+1 can be "postponed" onto block N's cell. This
  * handles the case where a peer receives its own ack for block N+1 before block N is confirmed.
  * The postponed ack is announced once block N completes.
  *
  * ==Cell Lifecycle==
  *
  *   1. '''Spawn''': Cell created when first message (augmented block or ack) arrives for a block
  *   2. '''Collect''': Accumulate augmented block and acks from all peers
  *   3. '''Saturate''': Cell becomes saturated when all required data is present
  *   4. '''Complete''': Either switch to next round or produce final `BlockConfirmed` result
  *   5. '''Eliminate''': Remove cell from state after confirmation
  *
  * ==Signature Validation==
  *
  * When a cell is saturated, the actor validates signatures:
  *   - Header signatures for Minor blocks
  *   - Transaction signatures for all signed L1 transactions
  *   - Verifies Ed25519 signatures using peer verification keys
  *
  * ==Actor Communication==
  *
  * The ConsensusActor interacts with:
  *   - '''Peer Liaison''': Broadcasts own acks to other peers (TODO: implement)
  *   - '''Block Weaver''': Notifies when blocks are confirmed with finalization flags
  *   - '''Cardano Liaison''': Provides signed transactions for L1 submission (TODO: refactor)
  *   - '''Event Sequencer''': Reports confirmed blocks with their events (TODO: implement)
  *
  * ==Error Handling==
  *
  * Errors can occur during consensus:
  *   - `CollectingError`: Invalid ack, unexpected block number, duplicate peer acknowledgment
  *   - `CompletionError`: Signature verification failures, missing required signatures
  *   - `MsgCellMismatch`: Message type doesn't match the current cell type/round
  *   - `AlienAckAnnouncement`: Attempted to announce an ack not from this peer
  *   - `UnexpectedPreviousBlockCell`: Previous block cell unexpectedly exists
  *
  * ==State Recovery==
  *
  * The actor supports recovery from crashes via `recoveredRequests` in config:
  *   - Replayed requests are processed during `preStart`
  *   - Cells are reconstructed from replayed augmented blocks and acks
  *   - Normal operation resumes after recovery completes
  *
  * ==Invariants==
  *
  * The actor maintains these invariants:
  *   - At most 2 cells exist simultaneously (for consecutive blocks)
  *   - Cells complete in block number order (N before N+1)
  *   - Round 2 acks only announced after round 1 completes
  *   - Signatures verified before producing `BlockConfirmed` results
  *   - Own acks scheduled according to protocol timing rules
  *
  * @see
  *   [[ConsensusCell]] for consensus cell trait and implementations
  * @see
  *   [[PostponedAckSupport]] for postponed ack handling
  */
object ConsensusActor:

    final case class Config(
        /** Own peer number */
        peerId: Peer.Number,

        /** The mapping from head's peers to their verification keys. */
        verificationKeys: Map[Peer.Number, VerificationKeyBytes],

        /** Requests that haven't been handled, initially empty. */
        recoveredRequests: Seq[Request] = Seq.empty,

        // Actors
        // TODO: should be many - a liaison per peer
        peerLiaison: PeerLiaison.PeerLiaisonRef,
        blockWeaver: BlockWeaver.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        eventSequencer: EventSequencer.EventSequencerRef,
    )

    // ===================================
    // Actor's state
    // ===================================

    /** In the currently supported unanimous consensus, there might be no more than 2 blocks
      * involved in the worst-case scenario, but we decided to use map here. Currently, we don't
      * limit the size of the map. Every cell can contain a consensus object for a particular block.
      */
    final case class State(
        /** Consensus cells */
        cells: Map[Block.Number, ConsensusCell[?]]
    )

    object State:
        def mkInitialState: State = State(cells = Map.empty)

    // ===================================
    // Request + ActorRef + apply
    // ===================================

    type Request = AugmentedBlock.Next | AckBlock

    type Handle = ActorRef[IO, Request]

    def apply(config: Config): IO[ConsensusActor] =
        for {
            stateRef <- Ref[IO].of(State.mkInitialState)
        } yield new ConsensusActor(config = config, stateRef = stateRef)

end ConsensusActor

class ConsensusActor(
    config: ConsensusActor.Config,
    stateRef: Ref[IO, ConsensusActor.State]
) extends Actor[IO, ConsensusActor.Request]:
    import ConsensusActor.*
    import ConsensusCell.*

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
      * checking whether it's an own acknowledgement and if so scheduling its announcement. This is
      * done differently for different types of acks:
      *   - for [[RoundOneOwnAck]] it's done here right when we receive them
      *   - for [[RoundTwoOwnAck] it's done in the corresponding cells later on
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
                    case round1: MajorRoundOneCell => round1.applyAck(major2)
                    case round2: MajorRoundTwoCell => round2.applyAck(major2)
                }
            case final1: AckBlock.Final1 =>
                state.withCellFor(final1)(_.applyAck(final1)) >>
                    IO.whenA(ack.id.peerNum == config.peerId)(state.scheduleOwnImmediateAck(final1))
            case final2: AckBlock.Final2 =>
                state.withCellFor(final2) {
                    case round1: FinalRoundOneCell => round1.applyAck(final2)
                    case round2: FinalRoundTwoCell => round2.applyAck(final2)
                }
        }
    } yield ()

    // ===================================
    // Actor's internal results - confirmed blocks
    // ===================================
    sealed trait BlockConfirmed {
        def block: Block.Next
        def effects: BlockEffectsSigned.Next
        def finalizationRequested: Boolean

        final lazy val blockNum: Block.Number = block.blockNum

        final lazy val weaverConfirmation: BlockWeaver.BlockConfirmed =
            BlockWeaver.BlockConfirmed(
              blockNumber = blockNum,
              finalizationRequested = finalizationRequested
            )

        final lazy val mbCardanoLiaisonEffects
            : Option[CardanoLiaison.MajorBlockConfirmed | CardanoLiaison.FinalBlockConfirmed] =
            this match {
                case BlockConfirmed.Major(
                      _,
                      BlockEffectsSigned.Major(
                        _,
                        settlementSigned,
                        fallbackSigned,
                        rolloutsSigned,
                        _,
                      ),
                      _
                    ) =>
                    Some(
                      CardanoLiaison.MajorBlockConfirmed(
                        blockNum = blockNum,
                        settlementSigned = settlementSigned,
                        rolloutsSigned = rolloutsSigned,
                        fallbackSigned = fallbackSigned
                      )
                    )
                case BlockConfirmed.Final(
                      _,
                      BlockEffectsSigned.Final(
                        _,
                        rolloutsSigned,
                        deinitSigned,
                        finalizationSigned
                      ),
                      _
                    ) =>
                    Some(
                      CardanoLiaison.FinalBlockConfirmed(
                        blockNum = blockNum,
                        finalizationSigned = finalizationSigned,
                        rolloutsSigned = rolloutsSigned,
                        mbDeinitSigned = deinitSigned
                      )
                    )
                case _ => None
            }

        final lazy val mbFinalBlockConfirmation: Option[Unit] =
            this match {
                case _: BlockConfirmed.Final => Some(())
                case _                       => None
            }

        // TODO: types are not defined yet
        //  - blockNum
        //  - events with flags
        //  - post-dated refunds
        final lazy val sequencerEvents: EventSequencer.ConfirmBlock = ???
    }

    object BlockConfirmed:

        final case class Minor(
            override val block: Block.Minor,
            override val effects: BlockEffectsSigned.Minor,
            override val finalizationRequested: Boolean
        ) extends BlockConfirmed

        final case class Major(
            override val block: Block.Major,
            override val effects: BlockEffectsSigned.Major,
            override val finalizationRequested: Boolean
        ) extends BlockConfirmed

        final case class Final(
            override val block: Block.Final,
            override val effects: BlockEffectsSigned.Final,
            override val finalizationRequested: Boolean = false
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
                                        updateCell(newRound.asInstanceOf[ConsensusCell[?]])
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
                        case minor: MinorConsensusCell =>
                            minor
                                .applyPostponedAck(ack)
                                .flatMap(c => updateCell(c.asInstanceOf[ConsensusCell[?]]))
                        case major: MajorConsensusCell[?] =>
                            major
                                .applyPostponedAck(ack)
                                .flatMap(c => updateCell(c.asInstanceOf[ConsensusCell[?]]))
                        case _ => IO.raiseError(Error.UnexpectedPreviousBlockCell)
                    }
                case None => announceAck(ack)
            }
        } yield ()

        // ===================================
        // Private methods
        // ===================================

        private def msgBlockNum(msg: AugmentedBlock.Next | AckBlock): Block.Number = msg match {
            case augBlock: AugmentedBlock.Next => augBlock.blockAsNext.id
            case ack: AckBlock                 => ack.blockNum
        }

        private def spawnCell(msg: AugmentedBlock.Next | AckBlock): IO[ConsensusFor[msg.type]] = {
            for {
                blockNum <- IO.pure(msgBlockNum(msg))
                result <- msg match {
                    case _: (AugmentedBlock.Minor | AckBlock.Minor) =>
                        MinorConsensusCell.apply(blockNum)
                    case _: (AugmentedBlock.Major | AckBlock.Major1) =>
                        MajorRoundOneCell.apply(blockNum)
                    case _: AckBlock.Major2 =>
                        MajorRoundOneCell.apply(blockNum)
                    case _: (AugmentedBlock.Final | AckBlock.Final1) =>
                        FinalRoundOneCell.apply(blockNum)
                    case _: AckBlock.Final2 =>
                        FinalRoundOneCell.apply(blockNum)
                }
            } yield result.asInstanceOf[ConsensusFor[msg.type]]
        }

        private def tryCastCell(
            msg: AugmentedBlock.Next | AckBlock
        )(cell: ConsensusCell[?]): IO[ConsensusFor[msg.type]] = {
            import Error.*

            msg match {
                case _: (AugmentedBlock.Minor | AckBlock.Minor) =>
                    cell match {
                        case c: MinorConsensusCell =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case _ => IO.raiseError(MsgCellMismatch)
                    }
                case _: (AugmentedBlock.Major | AckBlock.Major1) =>
                    cell match {
                        case c: MajorRoundOneCell =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case _ => IO.raiseError(MsgCellMismatch)
                    }
                case _: AckBlock.Major2 =>
                    cell match {
                        case c: MajorRoundOneCell =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case c: MajorRoundTwoCell =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case _ => IO.raiseError(MsgCellMismatch)
                    }
                case _: (AugmentedBlock.Final | AckBlock.Final1) =>
                    cell match {
                        case c: FinalRoundOneCell =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case _ => IO.raiseError(MsgCellMismatch)
                    }
                case _: AckBlock.Final2 =>
                    cell match {
                        case c: FinalRoundOneCell =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case c: FinalRoundTwoCell =>
                            IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                        case _ => IO.raiseError(MsgCellMismatch)
                    }
            }
        }

        private def updateCell(cell: ConsensusCell[?]): IO[Unit] =
            stateRef.set(state.copy(cells = state.cells.updated(cell.blockNum, cell)))

        private def completeCell(
            blockConfirmed: BlockConfirmed,
            mbAck: Option[AckBlock]
        ): IO[Unit] =
            for {
                // Handle the confirmed block
                _ <- config.blockWeaver ! blockConfirmed.weaverConfirmation
                _ <- IO.traverse_(blockConfirmed.mbCardanoLiaisonEffects)(config.cardanoLiaison ! _)
                _ <- config.eventSequencer ! blockConfirmed.sequencerEvents
                // Announce the ack if present
                _ <- mbAck.traverse_(announceAck)
                // Signal the peer liaison if that is the final block confirmation
                // TODO: fill in the hole once we have a type for that
                _ <- IO.traverse_(blockConfirmed.mbFinalBlockConfirmation)(???)
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

    // Scala 3 match type is like Haskell closed type family
    type ConsensusFor[B] <: ConsensusCell[?] = B match {
        case AugmentedBlock.Minor => MinorConsensusCell
        case AckBlock.Minor       => MinorConsensusCell
        case AugmentedBlock.Major => MajorRoundOneCell
        case AckBlock.Major1      => MajorRoundOneCell
        case AckBlock.Major2      => MajorRoundOneCell | MajorRoundTwoCell
        case AugmentedBlock.Final => FinalRoundOneCell
        case AckBlock.Final1      => FinalRoundOneCell
        case AckBlock.Final2      => FinalRoundOneCell | FinalRoundTwoCell
    }

    // ===================================
    // Consensus cells
    // ===================================

    object ConsensusCell:

        /** Validates a set of witnesses and attaches them to any Tx.
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
                signedTx = attachVKeyWitnesses(tx, vkeyWitnesses)
            } yield txLens.replace(signedTx)(someTx)
        }

        // ===================================
        // Minor cell
        // ===================================

        /** Minor block consensus, may start off by receiving a local augmented block (and local
          * ack) or someone else's acknowledgment.
          *
          * @param augBlock
          * @param acks
          */
        final case class MinorConsensusCell(
            override val blockNum: Block.Number,
            augBlock: Option[AugmentedBlock.Minor],
            acks: Map[VerificationKeyBytes, AckBlock.Minor],
            postponedNextBlockOwnAck: Option[RoundOneOwnAck]
        ) extends ConsensusCell[MinorConsensusCell]
            with PostponedAckSupport[MinorConsensusCell] {
            import CollectingError.*

            override type AugBlockType = AugmentedBlock.Minor
            override type AckType = AckBlock.Minor
            override type RoundTwoType = Void
            override type RoundTwoOwnAckType = Void
            override type ResultType = BlockConfirmed.Minor

            override protected def withPostponedAck(ack: RoundOneOwnAck): MinorConsensusCell =
                copy(postponedNextBlockOwnAck = Some(ack))

            override def applyAugBlock(
                augmentedBlock: AugmentedBlock.Minor
            ): IO[MinorConsensusCell] =
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

            override def applyAck(ack: AckBlock.Minor): IO[MinorConsensusCell] =
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
                : IO[Either[(Void, Void), (BlockConfirmed.Minor, Option[RoundOneOwnAck])]] = {

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
                    block = block,
                    effects = BlockEffectsSigned.Minor(
                      blockNum = block.header.blockNum,
                      header = block.header,
                      headerSignatures = vksAndSigs.map(_._2).toSet,
                      postDatedRefundsSigned = postDatedRefundsSigned,
                    ),
                    finalizationRequested = finalizationRequested
                  ) -> postponedNextBlockOwnAck
                )
            }
        }

        object MinorConsensusCell:
            import CollectingError.*

            def apply(blockNum: Block.Number): IO[MinorConsensusCell] =
                IO.pure(
                  MinorConsensusCell(
                    blockNum,
                    None,
                    Map.empty,
                    None
                  )
                )

        // ===================================
        // Major cells
        // ===================================

        final case class MajorRoundOneCell(
            override val blockNum: Block.Number,
            augmentedBlock: Option[AugmentedBlock.Major],
            acks1: Map[VerificationKeyBytes, AckBlock.Major1],
            acks2: Map[VerificationKeyBytes, AckBlock.Major2],
            postponedNextBlockOwnAck: Option[RoundOneOwnAck]
        ) extends MajorConsensusCell[MajorRoundOneCell] {
            import CollectingError.*

            override type AugBlockType = AugmentedBlock.Major
            override type AckType = AckBlock.Major1 | AckBlock.Major2
            override type RoundTwoType = MajorRoundTwoCell
            override type RoundTwoOwnAckType = AckBlock.Major2
            override type ResultType = Void

            override protected def withPostponedAck(ack: RoundOneOwnAck): MajorRoundOneCell =
                copy(postponedNextBlockOwnAck = Some(ack))

            override def applyAugBlock(
                augmentedBlock: AugmentedBlock.Major
            ): IO[MajorRoundOneCell] =
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
            ): IO[MajorRoundOneCell] =
                ack match {
                    case ack1: AckBlock.Major1 => applyAck1(ack1)
                    case ack2: AckBlock.Major2 => applyAck2(ack2)
                }

            private def applyAck1(ack: AckBlock.Major1): IO[MajorRoundOneCell] =
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

            private def applyAck2(ack: AckBlock.Major2): IO[MajorRoundOneCell] =
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
                : IO[Either[(MajorRoundTwoCell, AckBlock.Major2), (Void, Option[RoundOneOwnAck])]] =
                for {
                    augBlock <- this.augmentedBlock.liftTo[IO](
                      // This should never happen
                      new IllegalStateException("Cannot complete without augmented block")
                    )
                    roundTwo = MajorRoundTwoCell(
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

        object MajorRoundOneCell:
            def apply(blockNum: Block.Number): IO[MajorRoundOneCell] =
                IO.pure(
                  MajorRoundOneCell(
                    blockNum,
                    None,
                    Map.empty,
                    Map.empty,
                    None
                  )
                )

        final case class MajorRoundTwoCell(
            override val blockNum: Block.Number,
            augBlock: AugmentedBlock.Major,
            acks1: Map[VerificationKeyBytes, AckBlock.Major1],
            acks2: Map[VerificationKeyBytes, AckBlock.Major2],
            postponedNextBlockOwnAck: Option[RoundOneOwnAck]
        ) extends MajorConsensusCell[MajorRoundTwoCell] {
            import CollectingError.*

            override type AugBlockType = Void
            override type AckType = AckBlock.Major2
            override type RoundTwoType = Void
            override type RoundTwoOwnAckType = Void
            override type ResultType = BlockConfirmed.Major

            override protected def withPostponedAck(ack: RoundOneOwnAck): MajorRoundTwoCell =
                copy(postponedNextBlockOwnAck = Some(ack))

            override def applyAugBlock(augBlock: Void): IO[MajorRoundTwoCell] =
                IO.raiseError(
                  new IllegalStateException("MajorBlockRoundTwo does not accept augmented blocks")
                )

            override def applyAck(ack: AckBlock.Major2): IO[MajorRoundTwoCell] =
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

            override def complete
                : IO[Either[(Void, Void), (BlockConfirmed.Major, Option[RoundOneOwnAck])]] =
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
                    settlementWitnessSet <- IO.pure(
                      acks2.map((vk, ack) => vk -> ack.settlement).toList
                    )
                    settlementTxSigned <- validateAndAttachTxSignature(
                      augBlock.effects.settlement,
                      settlementWitnessSet
                    )

                    // 5. Check whether someone requested the head to finalize
                    finalizationRequested = acks1.values
                        .foldLeft(false)(_ || _.finalizationRequested)

                } yield Right(
                  BlockConfirmed.Major(
                    block = augBlock.block,
                    effects = BlockEffectsSigned.Major(
                      blockNum = augBlock.block.header.blockNum,
                      settlementSigned = settlementTxSigned,
                      fallbackSigned = fallbackTxSigned,
                      rolloutsSigned = rolloutsSigned,
                      postDatedRefundsSigned = postDatedRefundsSigned,
                    ),
                    finalizationRequested = finalizationRequested
                  ),
                  postponedNextBlockOwnAck
                )
        }

        // ===================================
        // Final cells
        // ===================================

        final case class FinalRoundOneCell(
            override val blockNum: Block.Number,
            augmentedBlock: Option[AugmentedBlock.Final],
            acks1: Map[VerificationKeyBytes, AckBlock.Final1],
            acks2: Map[VerificationKeyBytes, AckBlock.Final2]
        ) extends FinalConsensusCell[FinalRoundOneCell] {
            import CollectingError.*

            override type AugBlockType = AugmentedBlock.Final
            override type AckType = AckBlock.Final1 | AckBlock.Final2
            override type RoundTwoType = FinalRoundTwoCell
            override type RoundTwoOwnAckType = AckBlock.Final2
            override type ResultType = Void

            override def applyAugBlock(
                augmentedBlock: AugmentedBlock.Final
            ): IO[FinalRoundOneCell] =
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

            override def applyAck(ack: AckBlock.Final1 | AckBlock.Final2): IO[FinalRoundOneCell] =
                ack match {
                    case ack1: AckBlock.Final1 => applyAck1(ack1)
                    case ack2: AckBlock.Final2 => applyAck2(ack2)
                }

            private def applyAck1(ack: AckBlock.Final1): IO[FinalRoundOneCell] =
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

            private def applyAck2(ack: AckBlock.Final2): IO[FinalRoundOneCell] =
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
                : IO[Either[(FinalRoundTwoCell, AckBlock.Final2), (Void, Option[RoundOneOwnAck])]] =
                for {
                    augBlock <- this.augmentedBlock.liftTo[IO](
                      new IllegalStateException("Cannot complete without augmented block")
                    )
                    roundTwo = FinalRoundTwoCell(
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

        object FinalRoundOneCell:
            import CollectingError.*

            def apply(blockNum: Block.Number): IO[FinalRoundOneCell] =
                IO.pure(
                  FinalRoundOneCell(
                    blockNum,
                    None,
                    Map.empty,
                    Map.empty
                  )
                )

        final case class FinalRoundTwoCell(
            override val blockNum: Block.Number,
            augBlock: AugmentedBlock.Final,
            acks1: Map[VerificationKeyBytes, AckBlock.Final1],
            acks2: Map[VerificationKeyBytes, AckBlock.Final2]
        ) extends FinalConsensusCell[FinalRoundTwoCell] {
            import CollectingError.*

            override type AugBlockType = Void
            override type AckType = AckBlock.Final2
            override type RoundTwoType = Void
            override type RoundTwoOwnAckType = Void
            override type ResultType = BlockConfirmed.Final

            override def applyAugBlock(augBlock: Void): IO[FinalRoundTwoCell] =
                IO.raiseError(
                  new IllegalStateException("FinalBlockRoundTwo does not accept augmented blocks")
                )

            override def applyAck(ack: AckBlock.Final2): IO[FinalRoundTwoCell] =
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

            override def complete
                : IO[Either[(Void, Void), (BlockConfirmed.Final, Option[RoundOneOwnAck])]] =
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
                                deinitSigned <- validateAndAttachTxSignature(
                                  deinit,
                                  deinitWitnessSet
                                )
                            } yield Some(deinitSigned)
                    }
                } yield Right(
                  (
                    BlockConfirmed.Final(
                      block = augBlock.block,
                      effects = BlockEffectsSigned.Final(
                        blockNum = augBlock.block.header.blockNum,
                        rolloutsSigned = rolloutsSigned,
                        mbDeinitSigned = mbDeinitSigned,
                        finalizationSigned = finalizationSigned
                      )
                    ),
                    None
                  )
                )
        }

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

    end ConsensusCell
end ConsensusActor
