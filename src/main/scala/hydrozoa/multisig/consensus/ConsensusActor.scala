package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.VerificationKeyBytes
import hydrozoa.multisig.consensus.ConsensusActor.{Config, Request}
import hydrozoa.multisig.ledger.dapp.tx.RefundTx
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.types.{AckBlock, AugmentedBlock, Block, Peer}
import scalus.builtin.ByteString
import scalus.cardano.ledger.TransactionHash

/** Consensus actor:
  */

object ConsensusActor {

    final case class Config(
        /** Own peer number */
        peerId: Peer.Number,

        /** The mapping from head's peers to their verification keys. */
        verificationKeys: Map[Peer.Number, VerificationKeyBytes],

        /** Requests that haven't been handled, intially empty. */
        recoveredRequests: Seq[Request],

        // Actors
        peerLiaison: PeerLiaison.PeerLiaisonRef,
        blockWeaver: BlockWeaver.BlockWeaverRef,
        cardanoLiaison: CardanoLiaison.CardanoLiaisonRef,
        eventSequencer: EventSequencer.EventSequencerRef,
    )

    type Request =
        AugmentedBlock.Minor | AugmentedBlock.Major | AugmentedBlock.Final | AckBlock.Minor |
            AckBlock.Major1 | AckBlock.Major2 | AckBlock.Final1 | AckBlock.Final2

    def apply(config: Config): IO[ConsensusActor] = {
        IO(new ConsensusActor(config = config) {})
    }
}

trait ConsensusActor(config: Config) extends Actor[IO, Request] {

    private val stateRef = Ref.unsafe[IO, State](State.mkInitialState)

    override def preStart: IO[Unit] =
        for {
            _ <- IO.traverse_(config.recoveredRequests)(receive)
        } yield ()

    override def receive: Receive[IO, Request] = {
        case augBlock: (AugmentedBlock.Minor | AugmentedBlock.Major | AugmentedBlock.Final) =>
            handleAugBlock(augBlock)
        case ack: (AckBlock.Minor | AckBlock.Major1 | AckBlock.Major2 | AckBlock.Final1 |
                AckBlock.Final2) =>
            handleAck(ack)
    }

    def handleAugBlock(
        augBlock: AugmentedBlock.Minor | AugmentedBlock.Major | AugmentedBlock.Final
    ): IO[Unit] = for {
        state <- stateRef.get
        // This is a bit annoying but Scala cannot infer types unless we PM explicitly
        _ <- augBlock match {
            case minor: AugmentedBlock.Minor =>
                state.withConsensusFor(minor)(_.applyAugBlock(minor))
            case major: AugmentedBlock.Major =>
                state.withConsensusFor(major)(_.applyAugBlock(major))
            case final_ : AugmentedBlock.Final =>
                state.withConsensusFor(final_)(_.applyAugBlock(final_))
        }
    } yield ()

    def handleAck(
        ack: AckBlock.Minor | AckBlock.Major1 | AckBlock.Major2 | AckBlock.Final1 | AckBlock.Final2
    ): IO[Unit] = for {
        state <- stateRef.get
        // This is a bit annoying but Scala cannot infer types unless we PM explicitly
        _ <- ack match {
            case minor: AckBlock.Minor =>
                state.withConsensusFor(minor)(_.applyAck(minor))
            case major1: AckBlock.Major1 =>
                state.withConsensusFor(major1)(_.applyAck(major1))
            case major2: AckBlock.Major2 =>
                state.withConsensusFor(major2)(_.applyAck(major2))
            case final1: AckBlock.Final1 =>
                state.withConsensusFor(final1)(_.applyAck(final1))
            case final2: AckBlock.Final2 =>
                state.withConsensusFor(final2)(_.applyAck(final2))
        }
    } yield ()

    // ===================================
    // Actor's internal results - confirmed blocks
    // ===================================
    sealed trait BlockConfirmed

    object BlockConfirmed:

        final case class Minor(
            augBlock: AugmentedBlock.Minor,
            headerSignatures: Set[ByteString],
            // Fully signed txs
            immediateRefundsSigned: List[RefundTx.Immediate],
            // Fully signed txs
            postDatedRefundsSigned: List[RefundTx.PostDated]
        ) extends BlockConfirmed

        final case class Major() extends BlockConfirmed

        final case class Final() extends BlockConfirmed

    // ===================================
    // Actor's state
    // ===================================

    /** In its simplest form of the currently supported unanimous consensus, there might be no more
      * than two block involved.
      *
      * Invariants:
      *   - If slotA is None then slotB must be None as well
      *   - if both are present, slotB.blockNum = slotA.blockNum + 1
      *   - if slotA is FinalBlockConsensus then slotB must be None
      *
      * @param blockA
      * @param blockB
      */
    final case class State(
        blockA: Option[BlockConsensus[_]],
        blockB: Option[BlockConsensus[_]]
    ) {

        /**   - Gives access to blockA/blockB consensus or raise an error if it can't be done
          *   - Afterwards checks whether the consensus is complete and tries to finalize it
          */
        def withConsensusFor(
            msg: AugmentedBlock.Minor | AugmentedBlock.Major | AugmentedBlock.Final |
                AckBlock.Minor | AckBlock.Major1 | AckBlock.Major2 | AckBlock.Final1 |
                AckBlock.Final2
        )(
            f: ConsensusFor[msg.type] => IO[ConsensusFor[msg.type]]
        ): IO[Unit] = {
            import Error.*

            def updateBlockA(c: BlockConsensus[?] | Void): State = c match {
                case bc: BlockConsensus[?] => this.copy(blockA = Some(bc))
                case _: Void               =>
                    // Void means no next round, this should not happen
                    throw new IllegalStateException("Cannot update with Void consensus")
            }
            def updateBlockB(c: BlockConsensus[?] | Void): State = c match {
                case bc: BlockConsensus[?] => this.copy(blockB = Some(bc))
                case _: Void               =>
                    // Void means no next round, this should not happen
                    throw new IllegalStateException("Cannot update with Void consensus")
            }

            def completeBlockA(c: BlockConfirmed | Void): IO[State] = for {
                // TODO handle the result
                _ <- IO.pure(())
            } yield this.copy(blockA = this.blockB, blockB = None)

            // I believe this is not possible
            def completeBlockB(c: BlockConfirmed | Void): IO[State] = ???

            for {
                blockNum <- IO.pure(msgBlockNum(msg))
                foo <-
                    (this.blockA.map(_.blockNum), this.blockB.map(_.blockNum)) match {
                        case (None, None) => IO.pure((spawn(msg), updateBlockA, completeBlockA))
                        case (Some(blockNumA), None) =>
                            blockNumA match {
                                case any if any == blockNum =>
                                    IO.pure((this.blockA.get, updateBlockA, completeBlockA))
                                case any if any.increment == blockNumA =>
                                    IO.pure((spawn(msg), updateBlockB, completeBlockB))
                                case _ => IO.raiseError(UnexpectedBlockNumber)
                            }
                        case (Some(_), Some(blockNumB)) =>
                            if blockNumB == blockNum
                            then IO.pure((this.blockB.get, updateBlockB, completeBlockB))
                            else IO.raiseError(UnexpectedBlockNumber)
                        case (None, Some(_)) => IO.raiseError(MalformedState)
                    }

                (consensus: ConsensusFor[msg.type], updateCallback, completeCallback) = foo

                consensus1 <- f(consensus)
                _ <-
                    if consensus1.isSaturated
                    then
                        for {
                            ret <- consensus1.complete
                            _ <- ret match {
                                case Left(newRound) => stateRef.set(updateCallback(newRound))
                                case Right(result) =>
                                    for {
                                        state1 <- completeCallback(result)
                                        _ <- stateRef.set(state1)
                                    } yield ()
                            }
                        } yield ()
                    else {
                        // Update the state, set back
                        stateRef.set(updateCallback(consensus1))
                    }
            } yield ()
        }

        private def msgBlockNum(
            msg: AugmentedBlock.Minor | AugmentedBlock.Major | AugmentedBlock.Final |
                AckBlock.Minor | AckBlock.Major1 | AckBlock.Major2 | AckBlock.Final1 |
                AckBlock.Final2
        ): Block.Number = msg match {
            case augBlock: (AugmentedBlock.Minor | AugmentedBlock.Major | AugmentedBlock.Final) =>
                augBlock.block.id
            case ack: (AckBlock.Minor | AckBlock.Major1 | AckBlock.Major2 | AckBlock.Final1 |
                    AckBlock.Final2) =>
                ack.blockNum
        }

        private def spawn(
            msg: AugmentedBlock.Minor | AugmentedBlock.Major | AugmentedBlock.Final |
                AckBlock.Minor | AckBlock.Major1 | AckBlock.Major2 | AckBlock.Final1 |
                AckBlock.Final2
        ): IO[ConsensusFor[msg.type]] = {
            import Error.*

            for {
                blockNum <- IO.pure(msgBlockNum(msg))
                result <- msg match {
                    case _: (AugmentedBlock.Minor | AckBlock.Minor) =>
                        IO.pure(MinorBlockConsensus.apply(blockNum))
                    case _: (AugmentedBlock.Major | AckBlock.Major1) =>
                        IO.pure(???)
                    case _: AckBlock.Major2 =>
                        IO.raiseError(UnexpectedMajor2)
                    case _: (AugmentedBlock.Final | AckBlock.Final1) =>
                        IO.pure(???)
                    case _: AckBlock.Final2 =>
                        IO.raiseError(UnexpectedFinal2)
                }
            } yield result.asInstanceOf[ConsensusFor[msg.type]]
        }

        enum Error extends Throwable:
            // Cannot spawn MajorBlockRoundTwo directly from Major2 ack
            case UnexpectedMajor2
            // Cannot spawn FinalBlockRoundTwo directly from Final2 ack
            case UnexpectedFinal2
            case UnexpectedBlockNumber
            case MalformedState
    }

    object State:
        def mkInitialState: State = State(None, None)

    /** Represents the state of the consensus on any (except Initial) block [[blockNum]].
      */
    sealed trait BlockConsensus[+T]:

        type AugBlockType
        type AckType
        type NextRound <: BlockConsensus[?] | Void
        type ResultType <: BlockConfirmed | Void

        def blockNum: Block.Number

        def applyAugBlock(augBlock: AugBlockType): IO[T]
        def applyAck(ack: AckType): IO[T]
        def isSaturated: Boolean
        def complete: IO[Either[NextRound, ResultType]]

    // Scala 3 match type is like Haskell closed type family
    type ConsensusFor[B] <: BlockConsensus[?] = B match {
        case AugmentedBlock.Minor => MinorBlockConsensus
        case AckBlock.Minor       => MinorBlockConsensus
        case AugmentedBlock.Major => MajorBlockRoundOne
        case AckBlock.Major1      => MajorBlockRoundOne
        case AckBlock.Major2      => MajorBlockRoundTwo
        case AugmentedBlock.Final => FinalBlockRoundOne
        case AckBlock.Final1      => FinalBlockRoundOne
        case AckBlock.Final2      => FinalBlockRoundTwo
    }

    object BlockNumX:
        def unapply(state: State): (Option[Block.Number], Option[Block.Number]) = ???

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

        override def complete: IO[Either[Void, BlockConfirmed.Minor]] = {

            /** TODO:
              *   - verify header signatures
              *   - zip immediate refunds with signatures and verify them
              *   - zip post-dated refunds with signatures and verify them
              *   - produce the result
              */
            ???
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

        // def apply(augmentedBlock: AugmentedBlock.Minor): IO[MinorBlockConsensus] =
        //    IO.pure(
        //      MinorBlockConsensus(
        //        augmentedBlock.block.id,
        //        Some(augmentedBlock),
        //        Map.empty
        //      )
        //    )
        //
        // def apply(ack: AckBlock.Minor): IO[MinorBlockConsensus] = for {
        //    peerNum <- IO.pure(ack.id.peerNum)
        //    verificationKey <- config.verificationKeys
        //        .get(peerNum)
        //        .liftTo[IO](UnexpectedPeer(peerNum))
        // } yield MinorBlockConsensus(
        //  ack.blockNum,
        //  None,
        //  Map(verificationKey -> ack)
        // )

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

    enum CompletionError:
        case WrongHeaderSignature(blockNum: Block.Number, peerId: Peer.Number)
        case WrongTxSignature(blockNum: Block.Number, peerId: Peer.Number, txId: TransactionHash)

    sealed trait MajorBlockConsensus[T] extends BlockConsensus[T]

    final case class MajorBlockRoundOne(
        override val blockNum: Block.Number,
        augmentedBlock: Option[AugmentedBlock.Major],
        acks1: Set[AckBlock.Major1]
    ) extends MajorBlockConsensus[MajorBlockRoundOne] {
        override type AugBlockType = AugmentedBlock.Major
        override type AckType = AckBlock.Major1
        override type NextRound = MajorBlockRoundTwo
        override type ResultType = Void

        override def applyAugBlock(augBlock: AugmentedBlock.Major): IO[MajorBlockRoundOne] = ???

        override def applyAck(ack: AckBlock.Major1): IO[MajorBlockRoundOne] = ???

        override def isSaturated: Boolean = ???

        override def complete: IO[Either[MajorBlockRoundTwo, Void]] = ???
    }

    final case class MajorBlockRoundTwo(
        override val blockNum: Block.Number,
        augmentedBlock: AugmentedBlock.Major,
        acks1: Set[AckBlock.Major1],
        acks2: Set[AckBlock.Major2]
    ) extends MajorBlockConsensus[MajorBlockRoundTwo] {
        override type AugBlockType = Void
        override type AckType = AckBlock.Major2
        override type NextRound = Void
        override type ResultType = BlockConfirmed.Major

        override def applyAugBlock(augBlock: Void): IO[MajorBlockRoundTwo] = ???

        override def applyAck(ack: AckBlock.Major2): IO[MajorBlockRoundTwo] = ???

        override def isSaturated: Boolean = ???

        override def complete: IO[Either[Void, BlockConfirmed.Major]] = ???
    }

    sealed trait FinalBlockConsensus[T] extends BlockConsensus[T]

    final case class FinalBlockRoundOne(
        override val blockNum: Block.Number,
        augmentedBlock: Option[AugmentedBlock.Final],
        acks1: Set[AckBlock.Final1]
    ) extends FinalBlockConsensus[FinalBlockRoundOne] {
        override type AugBlockType = AugmentedBlock.Final
        override type AckType = AckBlock.Final1
        override type NextRound = FinalBlockRoundTwo
        override type ResultType = Void

        override def applyAugBlock(augBlock: AugmentedBlock.Final): IO[FinalBlockRoundOne] = ???

        override def applyAck(ack: AckBlock.Final1): IO[FinalBlockRoundOne] = ???

        override def isSaturated: Boolean = ???

        override def complete: IO[Either[FinalBlockRoundTwo, Void]] = ???
    }

    final case class FinalBlockRoundTwo(
        override val blockNum: Block.Number,
        augmentedBlock: AugmentedBlock.Final,
        acks1: Set[AckBlock.Final1],
        acks2: Set[AckBlock.Final2]
    ) extends FinalBlockConsensus[FinalBlockRoundTwo] {
        override type AugBlockType = Void
        override type AckType = AckBlock.Final2
        override type NextRound = Void
        override type ResultType = BlockConfirmed.Final

        override def applyAugBlock(augBlock: Void): IO[FinalBlockRoundTwo] = ???

        override def applyAck(ack: AckBlock.Final2): IO[FinalBlockRoundTwo] = ???

        override def isSaturated: Boolean = ???

        override def complete: IO[Either[Void, BlockConfirmed.Final]] = ???
    }
}
