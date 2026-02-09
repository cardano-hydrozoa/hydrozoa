package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.attachVKeyWitnesses
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.ack.{AckBlock, AckId}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockHeader.Minor.HeaderSignature
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader, BlockNumber}
import hydrozoa.multisig.ledger.dapp.tx.{RefundTx, RolloutTx, Tx, TxSignature}
import scala.Function.tupled
import scala.util.control.NonFatal
import scalus.builtin.{ByteString, platform}
import scalus.cardano.ledger.{TransactionHash, VKeyWitness}
import scalus.crypto.ed25519.VerificationKey

/** Round one own acks, which must be scheduled immediately upon receiving. There is no way to
  * separate own acks from others' acks at the type level, but this alias is those alias is used
  * only for own acks that come from the joint ledger.
  */
type RoundOneOwnAck = AckBlock.Minor | AckBlock.Major1 | AckBlock.Final1

/** Round two acks, which should be scheduled for announcing when the cell switches to round two.
  * There is no way to separate own acks from others' acks at the type level, but those alias is
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

    /** Type of the block the cell can handle. May be Void if no block is expected in round two.
      */
    type BlockType <: Block.Unsigned | Void

    /** Types of the acks the cell can handle, both round one and round two. The reason we need to
      * have both is that round two acks from some peers can come when the cell is still in the
      * round one.
      */
    type AckType <: AckBlock

    // Invariant: RoundTwoType + RoundTwoOwnAckType  | ResultType

    /** Next round */
    type RoundTwoType <: ConsensusCell[?] | Void

    /** Type of the own ack that should be announced upon switching to the [[RoundTwoType]] */
    type RoundTwoOwnAckType <: RoundTwoOwnAck | Void

    /** Type of the final result */
    type ResultType <: Block.MultiSigned.Next | Void

    /** Every cell corresponds to a particular block.
      */
    def blockNum: BlockNumber

    /** Handles an incoming block, may throw.
      * @param block
      * @return
      *   the updated cell
      */
    def applyBlock(block: BlockType): IO[T]

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

object ConsensusCell {
    sealed trait RoundOne[+T] extends ConsensusCell[T]
    sealed trait RoundTwo[+T] extends ConsensusCell[T]
}

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
  *   - The block containing L2 transactions and effects
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
  *   - Round 1: Collect block and Major1 acks with fallback/rollout signatures (though Major2 acks
  *     may come as well)
  *   - Switch between `MajorRoundOneCell` and `MajorRoundTwoCell`, announcing own Major2 ack
  *   - Round 2: Collect Major2 acks with settlement signatures
  *   - Complete when all round 2 acks are collected
  *
  * '''Final Blocks''' (Two Rounds):
  *   - Round 1: Collect block and Final1 acks with rollout signatures (though Final2 acks may come
  *     as well)
  *   - Switch between `FinalRoundOneCell` and `FinalRoundTwoCell`, announcing own Final2 ack
  *   - Round 2: Collect Final2 acks with finalization signatures
  *   - Complete when all round 2 acks are collected
  *
  * ==Message Handling==
  *
  * The actor processes two types of messages:
  *
  * '''Blocks''' (`Block.Unsigned.Next`):
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
  *   1. '''Spawn''': Cell created when first message (block or ack) arrives for a block
  *   2. '''Collect''': Accumulate block and acks from all peers
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
  *   - Cells are reconstructed from replayed blocks and acks
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

    type Config = OwnHeadPeerPublic.Section & HeadPeers.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        cardanoLiaison: CardanoLiaison.Handle,
        eventSequencer: EventSequencer.Handle,
        peerLiaisons: List[PeerLiaison.Handle],
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
        cells: Map[BlockNumber, ConsensusCell[?]]
    )

    object State:
        def mkInitialState: State = State(cells = Map.empty)

    // ===================================
    // Request + ActorRef + apply
    // ===================================

    type Request = Block.Unsigned.Next | AckBlock

    type Handle = ActorRef[IO, Request]

    def apply(
        config: Config,
        pendingConnections: MultisigRegimeManager.PendingConnections | ConsensusActor.Connections
    ): IO[ConsensusActor] =
        for {
            stateRef <- Ref[IO].of(State.mkInitialState)
        } yield new ConsensusActor(
          config = config,
          pendingConnections = pendingConnections,
          stateRef = stateRef
        )

end ConsensusActor

class ConsensusActor(
    config: ConsensusActor.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | ConsensusActor.Connections,
    stateRef: Ref[IO, ConsensusActor.State]
) extends Actor[IO, ConsensusActor.Request]:
    import ConsensusActor.*
    import ConsensusCell.*

    private val connections = Ref.unsafe[IO, Option[ConsensusActor.Connections]](None)

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error(
              "Consensus Actor is missing its connections to other actors."
            )
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                _connections <- x.get
                _ <- connections.set(
                  Some(
                    ConsensusActor.Connections(
                      blockWeaver = _connections.blockWeaver,
                      cardanoLiaison = _connections.cardanoLiaison,
                      eventSequencer = _connections.eventSequencer,
                      peerLiaisons = _connections.peerLiaisons
                    )
                  )
                )
            } yield ()
        case x: ConsensusActor.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] = initializeConnections

    override def receive: Receive[IO, Request] = {
        case block: Block.Unsigned.Next => handleBlock(block)
        case ack: AckBlock              => handleAck(ack)
    }

    /** Since the ReqBlock messages are sent by the joint ledger actor directly, all we need to do
      * when receiving a new block is to store it in the proper cell. The consensus actor is
      * responsible for sending acks only.
      */
    def handleBlock(block: Block.Unsigned.Next): IO[Unit] = for {
        // _ <- IO.println(s"---- CA ----: handleBlock: $block")
        // _ <- IO.println(s"---- CA ----: handleBlock")
        state <- stateRef.get
        // This is a bit annoying but Scala cannot infer types unless we PM explicitly
        _ <- block match {
            case minor: Block.Unsigned.Minor =>
                state.withCellFor(minor)(_.applyBlock(minor))
            case major: Block.Unsigned.Major =>
                state.withCellFor(major)(_.applyBlock(major))
            case final_ : Block.Unsigned.Final =>
                state.withCellFor(final_)(_.applyBlock(final_))
        }
    } yield ()

    /** Handling acks, on the other hand, in addition to storing the ack in a cell, requires
      * checking whether it's an own acknowledgement and if so scheduling its announcement. This is
      * done differently for different types of acks:
      *   - for [[RoundOneOwnAck]] it's done here right when we receive them
      *   - for [[RoundTwoOwnAck]] it's done in the corresponding cells later on
      *
      * @param ack
      *   an arbitrary ack, whether own one or someone else's one
      */
    def handleAck(ack: AckBlock): IO[Unit] = for {
        // _ <- IO.println(s"---- CA ----: handleAck: $ack")
        state <- stateRef.get
        // This is a bit annoying but Scala cannot infer types unless we PM explicitly
        _ <- ack match {
            case minor: AckBlock.Minor =>
                state.withCellFor(minor)(_.applyAck(minor)) >>
                    IO.whenA(ack.peerNum == config.ownHeadPeerNum)(
                      state.scheduleOwnImmediateAck(minor)
                    )
            case major1: AckBlock.Major1 =>
                state.withCellFor(major1)(_.applyAck(major1)) >>
                    IO.whenA(ack.peerNum == config.ownHeadPeerNum)(
                      state.scheduleOwnImmediateAck(major1)
                    )
            case major2: AckBlock.Major2 =>
                state.withCellFor(major2) {
                    case round1: MajorRoundOneCell => round1.applyAck(major2)
                    case round2: MajorRoundTwoCell => round2.applyAck(major2)
                }
            case final1: AckBlock.Final1 =>
                state.withCellFor(final1)(_.applyAck(final1)) >>
                    IO.whenA(ack.peerNum == config.ownHeadPeerNum)(
                      state.scheduleOwnImmediateAck(final1)
                    )
            case final2: AckBlock.Final2 =>
                state.withCellFor(final2) {
                    case round1: FinalRoundOneCell => round1.applyAck(final2)
                    case round2: FinalRoundTwoCell => round2.applyAck(final2)
                }
        }
    } yield ()

    // ===================================
    // State extension methods
    // ===================================

    // TODO: remove this, it's not safe
    extension (state: State) {

        /**   - Gives access to a typed consensus cell or raise an error if it can't be done
          *   - Afterwards checks whether the cell is saturated and tries to complete it which leads
          *     the either a new round in the same cell or to the close of cell and the final
          *     result, so we can eliminate the cell.
          */
        def withCellFor(msg: Block.Unsigned.Next | AckBlock)(
            f: ConsensusFor[msg.type] => IO[ConsensusFor[msg.type]]
        ): IO[Unit] = {
            for {
                blockNum <- IO.pure(msgBlockNum(msg))
                cell <-
                    state.cells.get(blockNum) match {
                        case Some(cell) => tryCastCell(msg)(cell)
                        case None       => spawnCell(msg)
                    }
                // Run asked actions
                cellUpdated <- f(cell)
                _ <-
                    if cellUpdated.isSaturated
                    then {
                        // Complete the round/cell
                        for {
                            // _ <- IO.println("---- CA ----: complete updated cell")
                            ret <- cellUpdated.complete
                            _ <- ret match {
                                // Switch the cell to the next round
                                case Left(nextRoundCell, ownAck) =>
                                    for {
                                        // _ <- IO.println("---- CA ----: complete updated cell: Left")

                                        // These casts are sound since the only alternative is Void
                                        // Ack2 can be safely sent when round 1 is finished (i.e. all first-round acks of
                                        // block N are received) because the peer liaisons guarantee that all acks of
                                        // block (N-1) are received (i.e. block N-1 is confirmed)
                                        // before the first-round acks of block N are received.
                                        // Sanity check: the consensus cell for block N-1 cannot exist now when cell N switches to round two.
                                        _ <- announceAck(ownAck.asInstanceOf[AckBlock])
                                        nextCell = nextRoundCell.asInstanceOf[ConsensusCell[?]]

                                        // Here is a little caveat:
                                        //  - we require own Ack2 to complete round one (since we have to announce it when
                                        //    switching to round two)
                                        //  - in the degenerate case of a head with just one node, it breaks:
                                        //     - First we receive own Ack1, and don't switch (we need own Ack2)
                                        //     - Second we receive own Ack2, and do switch to round two
                                        //     - nothing will happen, since only Ack2 is already here
                                        // So we have to try to go further immediately to cover that case.
                                        // TODO: originally the code (partially) assumed we may more than two rounds.
                                        //   But in practice whe have only two, so I am not going to do any
                                        //   recursive things there.
                                        _ <-
                                            if nextCell.isSaturated
                                            then
                                                for {
                                                    ret <- nextCell.complete
                                                    // This is safe as long as we have only two rounds (which unlikely will change)
                                                    Right(result, mbAck) = ret: @unchecked
                                                    // _ <- IO.println("---- CA ----: complete updated cell: Left-Right")
                                                    _ <- completeCell(
                                                      result.asInstanceOf[Block.MultiSigned.Next],
                                                      mbAck
                                                    )
                                                } yield ()
                                            else updateCell(nextCell)
                                    } yield ()
                                // Complete the consensus in the cell
                                case Right(result, mbAck) =>
                                    // IO.println("---- CA ----: complete updated cell: Right") >>
                                    // This cast is sound since the only alternative is Void
                                    completeCell(
                                      result.asInstanceOf[Block.MultiSigned.Next],
                                      mbAck
                                    )
                            }
                        } yield ()
                    } else {
                        // Just update the state with the new consensus state
                        updateCell(cellUpdated)
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
            _ <- IO.raiseWhen(ack.peerNum != config.ownHeadPeerNum)(Error.AlienAckAnnouncement)
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

        private def msgBlockNum(msg: Block.Unsigned.Next | AckBlock): BlockNumber = msg match {
            case block: Block.Unsigned.Next => block.blockNum
            case ack: AckBlock              => ack.blockNum
        }

        private def spawnCell(msg: Block.Unsigned.Next | AckBlock): IO[ConsensusFor[msg.type]] = {
            for {
                blockNum <- IO.pure(msgBlockNum(msg))
                // _ <- IO.println(s"spawnCell: $blockNum")
                result <- msg match {
                    case _: (Block.Unsigned.Minor | AckBlock.Minor) =>
                        MinorConsensusCell.apply(blockNum)
                    case _: (Block.Unsigned.Major | AckBlock.Major1) =>
                        MajorRoundOneCell.apply(blockNum)
                    case _: AckBlock.Major2 =>
                        MajorRoundOneCell.apply(blockNum)
                    case _: (Block.Unsigned.Final | AckBlock.Final1) =>
                        FinalRoundOneCell.apply(blockNum)
                    case _: AckBlock.Final2 =>
                        FinalRoundOneCell.apply(blockNum)
                }
            } yield result.asInstanceOf[ConsensusFor[msg.type]]
        }

        private def tryCastCell(
            msg: Block.Unsigned.Next | AckBlock
        )(cell: ConsensusCell[?]): IO[ConsensusFor[msg.type]] =
            // IO.println("tryCastCell") >>
            {
                import Error.*

                msg match {
                    case _: (Block.Unsigned.Minor | AckBlock.Minor) =>
                        cell match {
                            case c: MinorConsensusCell =>
                                IO.pure(c.asInstanceOf[ConsensusFor[msg.type]])
                            case _ => IO.raiseError(MsgCellMismatch)
                        }
                    case _: (Block.Unsigned.Major | AckBlock.Major1) =>
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
                    case _: (Block.Unsigned.Final | AckBlock.Final1) =>
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
            // IO.println(s"---- CA ----: updateCell for block ${cell.blockNum}") >>
            stateRef.update(s => s.copy(cells = s.cells.updated(cell.blockNum, cell)))

        private def completeCell(
            blockConfirmed: Block.MultiSigned.Next,
            mbAck: Option[AckBlock]
        ): IO[Unit] = {
            for {
                // _ <- IO.println(s"---- CA ----: completeCell: blockConfirmed: ${blockConfirmed.blockNum}")
                conn <- getConnections
                // Handle the confirmed block
                _ <- conn.blockWeaver ! blockConfirmed
                _ <- blockConfirmed match {
                    case x: (Block.MultiSigned.Major | Block.MultiSigned.Final) =>
                        // IO.println(s"---- CA ----: completeCell: sending multisigned block: ${x.blockNum}") >>
                        conn.cardanoLiaison ! x
                    case _ => IO.unit
                }
                _ <- conn.eventSequencer ! blockConfirmed
                _ <- (conn.peerLiaisons ! blockConfirmed).parallel
                // Announce the ack if present
                _ <- mbAck.traverse_(announceAck)
                // Remove the cell
                _ <- stateRef.update(s => s.copy(cells = s.cells - blockConfirmed.blockNum))
            } yield ()
        }

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
        case Block.Unsigned.Minor => MinorConsensusCell
        case AckBlock.Minor       => MinorConsensusCell
        case Block.Unsigned.Major => MajorRoundOneCell
        case AckBlock.Major1      => MajorRoundOneCell
        case AckBlock.Major2      => MajorRoundOneCell | MajorRoundTwoCell
        case Block.Unsigned.Final => FinalRoundOneCell
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
            vkeysAndSigs: List[(VerificationKey, TxSignature)]
        ): IO[T] = {
            import someTx.*
            for {
                txId <- IO.pure(tx.id)
                vkeyWitnesses <- IO.pure(
                  vkeysAndSigs.map((vk, sig) => VKeyWitness.apply(vk, sig))
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

        /** Minor block consensus, may start off by receiving a local block (and local ack) or
          * someone else's acknowledgment.
          *
          * @param block
          * @param acks
          */
        final case class MinorConsensusCell(
            override val blockNum: BlockNumber,
            block: Option[Block.Unsigned.Minor],
            acks: Map[VerificationKey, AckBlock.Minor],
            postponedNextBlockOwnAck: Option[RoundOneOwnAck]
        ) extends ConsensusCell[MinorConsensusCell]
            with PostponedAckSupport[MinorConsensusCell] {
            import CollectingError.*

            override type BlockType = Block.Unsigned.Minor
            override type AckType = AckBlock.Minor
            override type RoundTwoType = Void
            override type RoundTwoOwnAckType = Void
            override type ResultType = Block.MultiSigned.Minor

            override protected def withPostponedAck(ack: RoundOneOwnAck): MinorConsensusCell =
                copy(postponedNextBlockOwnAck = Some(ack))

            override def applyBlock(
                block: Block.Unsigned.Minor
            ): IO[MinorConsensusCell] =
                for {
                    blockNumber <- IO.pure(block.blockNum)
                    _ <- IO.raiseWhen(this.blockNum != blockNumber)(
                      UnexpectedBlockNumber(this.blockNum, blockNumber)
                    )
                    _ <- IO.raiseWhen(this.block.isDefined)(
                      UnexpectedBlock(this.blockNum)
                    )
                    newRound = this.copy(block = Some(block))
                } yield newRound

            override def applyAck(ack: AckBlock.Minor): IO[MinorConsensusCell] =
                for {
                    // Check block number
                    blockNum <- IO.pure(ack.blockNum)
                    _ <- IO.raiseWhen(this.blockNum != blockNum)(
                      UnexpectedBlockNumber(this.blockNum, blockNum)
                    )
                    // Check peer number
                    peerNum = ack.peerNum
                    verificationKey <- config
                        .headPeerVKey(peerNum)
                        .liftTo[IO](UnexpectedPeer(peerNum))
                    // Check whether ack already exists
                    _ <- IO.raiseWhen(this.acks.contains(verificationKey))(
                      UnexpectedAck(blockNum, peerNum)
                    )
                    newRound = copy(acks = acks + (verificationKey -> ack))
                } yield newRound

            override def isSaturated: Boolean =
                block.isDefined && acks.keys == config.headPeerVKeys

            override def complete
                : IO[Either[(Void, Void), (Block.MultiSigned.Minor, Option[RoundOneOwnAck])]] = {

                def validateHeaderSignature(
                    msg: BlockHeader.Minor.Onchain.Serialized
                )(vk: VerificationKey, sig: HeaderSignature): IO[Unit] =
                    IO.delay(platform.verifyEd25519Signature(vk, msg, sig))
                        .handleErrorWith {
                            case NonFatal(_) =>
                                IO.raiseError(CompletionError.WrongHeaderSignature(vk))
                            case e => IO.raiseError(e)
                        }
                        .void

                for {
                    block <- IO.pure(block.get)
                    blockBrief = block.blockBrief

                    // 1. Verify header signatures
                    validate = validateHeaderSignature(blockBrief.header.onchainMsg)
                    vksAndSigs <- IO.pure(acks.map((vk, ack) => vk -> ack.header).toList)
                    _ <- IO.traverse_(vksAndSigs)((vk, sig) => validate(vk, sig))

                    // 2. Zip post-dated refunds with signatures, verify them, and attach to the transactions
                    refunds = block.effects.postDatedRefundTxs
                    witnessSets = acks
                        .map((vk, ack) => ack.postDatedRefundTxs.map(vk -> _))
                        .toList
                        .transpose
                    postDatedRefundTxs <- refunds
                        .zip(witnessSets)
                        .traverse(tupled(validateAndAttachTxSignature[RefundTx.PostDated]))

                    // 3. Check whether someone requested the head to finalize
                    finalizationRequested = acks.values
                        .foldLeft(false)(_ || _.finalizationRequested)
                } yield Right(
                  Block.MultiSigned.Minor(
                    blockBrief = blockBrief,
                    effects = BlockEffects.MultiSigned.Minor(
                      headerSerialized = blockBrief.header.onchainMsg,
                      headerMultiSigned = vksAndSigs.map(_._2),
                      postDatedRefundTxs = postDatedRefundTxs,
                    ),
                    finalizationRequested = finalizationRequested
                  ) -> postponedNextBlockOwnAck
                )
            }
        }

        object MinorConsensusCell:
            import CollectingError.*

            def apply(blockNum: BlockNumber): IO[MinorConsensusCell] =
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
            override val blockNum: BlockNumber,
            block: Option[Block.Unsigned.Major],
            acks1: Map[VerificationKey, AckBlock.Major1],
            acks2: Map[VerificationKey, AckBlock.Major2],
            postponedNextBlockOwnAck: Option[RoundOneOwnAck]
        ) extends MajorConsensusCell[MajorRoundOneCell] {
            import CollectingError.*

            override type BlockType = Block.Unsigned.Major
            override type AckType = AckBlock.Major1 | AckBlock.Major2
            override type RoundTwoType = MajorRoundTwoCell
            override type RoundTwoOwnAckType = AckBlock.Major2
            override type ResultType = Void

            override protected def withPostponedAck(ack: RoundOneOwnAck): MajorRoundOneCell =
                copy(postponedNextBlockOwnAck = Some(ack))

            override def applyBlock(
                block: Block.Unsigned.Major
            ): IO[MajorRoundOneCell] =
                for {
                    blockNumber <- IO.pure(block.blockNum)
                    _ <- IO.raiseWhen(this.blockNum != blockNumber)(
                      UnexpectedBlockNumber(this.blockNum, blockNumber)
                    )
                    _ <- IO.raiseWhen(this.block.isDefined)(
                      UnexpectedBlock(this.blockNum)
                    )
                    newRound = this.copy(block = Some(block))
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
                    peerNum = ack.peerNum
                    verificationKey <- config
                        .headPeerVKey(peerNum)
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
                    peerNum = ack.peerNum
                    verificationKey <- config
                        .headPeerVKey(peerNum)
                        .liftTo[IO](UnexpectedPeer(peerNum))
                    _ <- IO.raiseWhen(this.acks2.contains(verificationKey))(
                      UnexpectedAck(blockNum, peerNum)
                    )
                    newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
                } yield newRound

            override def isSaturated: Boolean =
                this.block.isDefined &&
                    this.acks1.keySet == config.headPeerVKeys.iterator.toSet && this.acks2.contains(
                      config.ownHeadVKey
                    )

            override def complete
                : IO[Either[(MajorRoundTwoCell, AckBlock.Major2), (Void, Option[RoundOneOwnAck])]] =
                for {
                    block <- this.block.liftTo[IO](
                      // This should never happen due to checks in [[isSaturated]]
                      new IllegalStateException(
                        s"Cannot complete major round one without block $blockNum"
                      )
                    )
                    roundTwo = MajorRoundTwoCell(
                      blockNum = this.blockNum,
                      block = block,
                      acks1 = this.acks1,
                      acks2 = this.acks2,
                      postponedNextBlockOwnAck = postponedNextBlockOwnAck
                    )
                    ownAck <- this.acks2
                        .get(config.ownHeadVKey)
                        .liftTo[IO](
                          // This should never happen due to checks in [[isSaturated]]
                          new IllegalStateException(
                            s"Cannot complete major round one without own Major2 ack for block $blockNum"
                          )
                        )
                } yield Left((roundTwo, ownAck))
        }

        object MajorRoundOneCell:
            def apply(blockNum: BlockNumber): IO[MajorRoundOneCell] =
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
            override val blockNum: BlockNumber,
            block: Block.Unsigned.Major,
            acks1: Map[VerificationKey, AckBlock.Major1],
            acks2: Map[VerificationKey, AckBlock.Major2],
            postponedNextBlockOwnAck: Option[RoundOneOwnAck]
        ) extends MajorConsensusCell[MajorRoundTwoCell] {
            import CollectingError.*

            override type BlockType = Void
            override type AckType = AckBlock.Major2
            override type RoundTwoType = Void
            override type RoundTwoOwnAckType = Void
            override type ResultType = Block.MultiSigned.Major

            override protected def withPostponedAck(ack: RoundOneOwnAck): MajorRoundTwoCell =
                copy(postponedNextBlockOwnAck = Some(ack))

            override def applyBlock(block: Void): IO[MajorRoundTwoCell] =
                IO.raiseError(
                  new IllegalStateException("MajorBlockRoundTwo does not accept blocks")
                )

            override def applyAck(ack: AckBlock.Major2): IO[MajorRoundTwoCell] =
                for {
                    blockNum <- IO.pure(ack.blockNum)
                    _ <- IO.raiseWhen(this.blockNum != blockNum)(
                      UnexpectedBlockNumber(this.blockNum, blockNum)
                    )
                    peerNum = ack.peerNum
                    verificationKey <- config
                        .headPeerVKey(peerNum)
                        .liftTo[IO](UnexpectedPeer(peerNum))
                    _ <- IO.raiseWhen(this.acks2.contains(verificationKey))(
                      UnexpectedAck(blockNum, peerNum)
                    )
                    newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
                } yield newRound

            override def isSaturated: Boolean =
                this.acks2.keySet == config.headPeerVKeys.iterator.toSet

            override def complete
                : IO[Either[(Void, Void), (Block.MultiSigned.Major, Option[RoundOneOwnAck])]] =
                for {

                    // 1.Fallback
                    fallbackWitnessSet <- IO.pure(
                      acks1.map((vk, ack) => vk -> ack.fallbackTx).toList
                    )
                    fallbackTxSigned <- validateAndAttachTxSignature(
                      block.effects.fallbackTx,
                      fallbackWitnessSet
                    )

                    // 2.Rollouts
                    rolloutWitnessSets = acks1
                        .map((vk, ack) => ack.rolloutTxs.map(vk -> _))
                        .toList
                        .transpose
                    rolloutsSigned <- block.effects.rolloutTxs
                        .zip(rolloutWitnessSets)
                        .traverse(tupled(validateAndAttachTxSignature[RolloutTx]))

                    // 3. Post-dated refunds
                    postDatedRefundsWitnessSets = acks1
                        .map((vk, ack) => ack.postDatedRefundTxs.map(vk -> _))
                        .toList
                        .transpose
                    postDatedRefundsSigned <- block.effects.postDatedRefundTxs
                        .zip(postDatedRefundsWitnessSets)
                        .traverse(tupled(validateAndAttachTxSignature[RefundTx.PostDated]))

                    // 4. Settlement
                    settlementWitnessSet <- IO.pure(
                      acks2.map((vk, ack) => vk -> ack.settlementTx).toList
                    )
                    settlementTxSigned <- validateAndAttachTxSignature(
                      block.effects.settlementTx,
                      settlementWitnessSet
                    )

                    // 5. Check whether someone requested the head to finalize
                    finalizationRequested = acks1.values
                        .foldLeft(false)(_ || _.finalizationRequested)

                } yield Right(
                  Block.MultiSigned.Major(
                    blockBrief = block.blockBrief,
                    effects = BlockEffects.MultiSigned.Major(
                      settlementTx = settlementTxSigned,
                      fallbackTx = fallbackTxSigned,
                      rolloutTxs = rolloutsSigned,
                      postDatedRefundTxs = postDatedRefundsSigned,
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
            override val blockNum: BlockNumber,
            block: Option[Block.Unsigned.Final],
            acks1: Map[VerificationKey, AckBlock.Final1],
            acks2: Map[VerificationKey, AckBlock.Final2]
        ) extends FinalConsensusCell[FinalRoundOneCell] {
            import CollectingError.*

            override type BlockType = Block.Unsigned.Final
            override type AckType = AckBlock.Final1 | AckBlock.Final2
            override type RoundTwoType = FinalRoundTwoCell
            override type RoundTwoOwnAckType = AckBlock.Final2
            override type ResultType = Void

            override def applyBlock(
                block: Block.Unsigned.Final
            ): IO[FinalRoundOneCell] =
                for {
                    blockNumber <- IO.pure(block.blockNum)
                    _ <- IO.raiseWhen(this.blockNum != blockNumber)(
                      UnexpectedBlockNumber(this.blockNum, blockNumber)
                    )
                    _ <- IO.raiseWhen(this.block.isDefined)(
                      UnexpectedBlock(this.blockNum)
                    )
                    newRound = this.copy(block = Some(block))
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
                    peerNum = ack.peerNum
                    verificationKey <- config
                        .headPeerVKey(peerNum)
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
                    peerNum = ack.peerNum
                    verificationKey <- config
                        .headPeerVKey(peerNum)
                        .liftTo[IO](UnexpectedPeer(peerNum))
                    _ <- IO.raiseWhen(this.acks2.contains(verificationKey))(
                      UnexpectedAck(blockNum, peerNum)
                    )
                    newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
                } yield newRound

            override def isSaturated: Boolean =
                this.block.isDefined &&
                    this.acks1.keySet == config.headPeerVKeys.iterator.toSet &&
                    this.acks2.contains(config.ownHeadVKey)

            override def complete
                : IO[Either[(FinalRoundTwoCell, AckBlock.Final2), (Void, Option[RoundOneOwnAck])]] =
                for {
                    block <- this.block.liftTo[IO](
                      // This should never happen due to checks in [[isSaturated]]
                      new IllegalStateException(
                        s"Cannot complete final round one without block $blockNum"
                      )
                    )
                    roundTwo = FinalRoundTwoCell(
                      blockNum = this.blockNum,
                      block = block,
                      acks1 = this.acks1,
                      acks2 = this.acks2
                    )
                    // Own ack should always be present
                    ownAck <- this.acks2
                        .get(config.ownHeadVKey)
                        .liftTo[IO](
                          // This should never happen due to checks in [[isSaturated]]
                          new IllegalStateException(
                            s"Cannot complete final round one without own Final2 ack for block $blockNum"
                          )
                        )
                } yield Left((roundTwo, ownAck))
        }

        object FinalRoundOneCell:
            import CollectingError.*

            def apply(blockNum: BlockNumber): IO[FinalRoundOneCell] =
                IO.pure(
                  FinalRoundOneCell(
                    blockNum,
                    None,
                    Map.empty,
                    Map.empty
                  )
                )

        final case class FinalRoundTwoCell(
            override val blockNum: BlockNumber,
            block: Block.Unsigned.Final,
            acks1: Map[VerificationKey, AckBlock.Final1],
            acks2: Map[VerificationKey, AckBlock.Final2]
        ) extends FinalConsensusCell[FinalRoundTwoCell] {
            import CollectingError.*

            override type BlockType = Void
            override type AckType = AckBlock.Final2
            override type RoundTwoType = Void
            override type RoundTwoOwnAckType = Void
            override type ResultType = Block.MultiSigned.Final

            override def applyBlock(block: Void): IO[FinalRoundTwoCell] =
                IO.raiseError(
                  new IllegalStateException("FinalBlockRoundTwo does not accept blocks")
                )

            override def applyAck(ack: AckBlock.Final2): IO[FinalRoundTwoCell] =
                for {
                    blockNum <- IO.pure(ack.blockNum)
                    _ <- IO.raiseWhen(this.blockNum != blockNum)(
                      UnexpectedBlockNumber(this.blockNum, blockNum)
                    )
                    peerNum = ack.peerNum
                    verificationKey <- config
                        .headPeerVKey(peerNum)
                        .liftTo[IO](UnexpectedPeer(peerNum))
                    _ <- IO.raiseWhen(this.acks2.contains(verificationKey))(
                      UnexpectedAck(blockNum, peerNum)
                    )
                    newRound = this.copy(acks2 = this.acks2 + (verificationKey -> ack))
                } yield newRound

            override def isSaturated: Boolean =
                this.acks2.keySet == config.headPeerVKeys.iterator.toSet

            override def complete
                : IO[Either[(Void, Void), (Block.MultiSigned.Final, Option[RoundOneOwnAck])]] =
                for {

                    // 1.Rollouts
                    rolloutWitnessSets <- IO.pure(
                      acks1
                          .map((vk, ack) => ack.rolloutTxs.map(vk -> _))
                          .toList
                          .transpose
                    )
                    rolloutsSigned <- block.effects.rolloutTxs
                        .zip(rolloutWitnessSets)
                        .traverse(tupled(validateAndAttachTxSignature[RolloutTx]))

                    // 2. Finalization
                    finalizationWitnessSet <- IO.pure(
                      acks2.map((vk, ack) => vk -> ack.finalizationTx).toList
                    )
                    finalizationSigned <- validateAndAttachTxSignature(
                      block.effects.finalizationTx,
                      finalizationWitnessSet
                    )
                } yield Right(
                  (
                    Block.MultiSigned.Final(
                      blockBrief = block.blockBrief,
                      effects = BlockEffects.MultiSigned.Final(
                        rolloutTxs = rolloutsSigned,
                        finalizationTx = finalizationSigned
                      )
                    ),
                    None
                  )
                )
        }

        enum CollectingError extends Throwable:
            case UnexpectedBlockNumber(roundBlockNumber: BlockNumber, blockNumber: BlockNumber)
            case UnexpectedAck(blockNum: BlockNumber, peerId: HeadPeerNumber)
            case UnexpectedBlock(blockNum: BlockNumber)
            case UnexpectedPeer(peer: HeadPeerNumber)
            case PostponedAckAlreadySet
            case UnexpectedPostponedAck

            override def getMessage: String = this match {
                case UnexpectedBlockNumber(roundBlockNumber, blockNumber) =>
                    s"Unexpected block number $blockNumber in round for block number $roundBlockNumber"
                case UnexpectedAck(blockNum, peerId) =>
                    s"Round for block $blockNum already has an ack from peer $peerId"
                case UnexpectedBlock(blockNum) =>
                    s"Round for block $blockNum already has a block"
                case UnexpectedPeer(peer) =>
                    s"Unexpected peer number: $peer"
                case PostponedAckAlreadySet =>
                    "Postponed Ack is already set"
                case UnexpectedPostponedAck =>
                    "Unexpected postponed ack"
            }

        // TODO: add block numbers / peer numbers / getMessage
        enum CompletionError extends Throwable:
            case WrongHeaderSignature(vkey: ByteString)
            case WrongTxSignature(txId: TransactionHash, vkey: ByteString)
            case MissingDeinitSignature

    end ConsensusCell
end ConsensusActor
