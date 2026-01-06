package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.consensus.ConsensusActor.{Config, Request}
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.types.{AckBlock, AugmentedBlock, Block, Peer}

/** Consensus actor:
  */

object ConsensusActor {

    final case class Config(
        /** Own peer number */
        peerId: Peer.Number,

        /** This is needed for acks custom wrappers.
          *
          * Invariant: >= 1
          *
          * TODO: likely we are going to move it to the head config
          */
        numberOfPeers: Int,

        // Actors
        // TODO: Should be a list ot peer liaisons?
        peerLiaison: PeerLiaison.PeerLiaisonRef,
        blockWeaver: BlockProducer.BlockProducerRef,
        cardanoLiaison: CardanoLiaison.CardanoLiaisonRef,
        // TODO: rename to EventSequencer?
        eventSequencer: TransactionSequencer.TransactionSequencerRef,
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
            // TODO: initialization code goes here
            _ <- IO.pure(???)
        } yield ()

    override def receive: Receive[IO, Request] = ???

    // ===================================
    // Actor's state
    // ===================================

    /** In its simplest form of the currently supported unanimous consensus, there might be no more
      * than two block involved.
      *
      * Invariants:
      *   - If slot1 is None then slot2 must be None as well
      *   - slot2.blockNum = slot1.blockNum + 1
      *   - if slot1 is FinalBlockConsensus then slot2 must be None -
      *
      * @param slot1
      * @param slot2
      */
    final case class State(
        slot1: Option[BlockConsensus],
        slot2: Option[BlockConsensus]
    )

    object State:
        def mkInitialState: State = State(None, None)

    def emptyAcks[T]: Map[Peer.Number, Option[T]] =
        Map.newBuilder(
          (1 to config.numberOfPeers)
              .map(Peer.Number(_))
              .filterNot(_ == config.peerId)
              .map(_ -> None)
        ).result()

    sealed trait BlockConsensus:
        def blockNum: Block.Number

    sealed trait MinorBlockConsensus extends BlockConsensus

    /** Minor block round, may start off by receiving a local augmented block or someone else's
      * acknowledgment.
      *
      * @param augmentedBlock
      * @param acks
      */
    final case class MinorBlockRound(
        override val blockNum: Block.Number,
        augmentedBlock: Option[AugmentedBlock.Minor],
        acks: Map[Peer.Number, Option[AckBlock.Minor]]
    ) extends MinorBlockConsensus

    object MinorBlockConsensus:
        import Error.*

        def apply(augmentedBlock: AugmentedBlock.Minor): MinorBlockRound = {
            MinorBlockRound(
              augmentedBlock.block.id,
              Some(augmentedBlock),
              emptyAcks[AckBlock.Minor]
            )
        }

        def apply(ack: AckBlock.Minor): MinorBlockRound =
            MinorBlockRound(
              ack.blockNum,
              None,
              emptyAcks[AckBlock.Minor]
            )

        extension (round: MinorBlockRound)

            def applyAugmentedBlock(augmentedBlock: AugmentedBlock.Minor): IO[MinorBlockRound] =
                for {
                    blockNumber <- IO.pure(augmentedBlock.block.id)
                    _ <- IO.raiseWhen(round.blockNum != blockNumber)(
                      UnexpectedBlockNumber(round.blockNum, blockNumber)
                    )
                    _ <- IO.raiseWhen(round.augmentedBlock.isDefined)(
                      UnexpectedAugmentedBlock(round.blockNum)
                    )
                    newRound = round.copy(augmentedBlock = Some(augmentedBlock))
                } yield newRound

            def applyAck(ack: AckBlock.Minor): IO[MinorBlockRound] = for {
                // Check block number
                blockNumber <- IO.pure(ack.blockNum)
                _ <- IO.raiseWhen(round.blockNum != blockNumber)(
                  UnexpectedBlockNumber(round.blockNum, blockNumber)
                )
                // Check peer num
                peerNum <- IO.pure(ack.id.peerNum)
                _ <- IO.raiseWhen(!round.acks.contains(peerNum))(UnexpectedPeer(ack.id.peerNum))
                // Check whether ack already exists
                _ <- IO.raiseWhen(round.acks(peerNum).isDefined)(
                  UnexpectedAck(blockNumber, peerNum)
                )
                newAcks = round.acks.updated(peerNum, Some(ack))
                newRound = round.copy(acks = newAcks)
            } yield newRound

    enum Error extends Throwable:
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
        }
    sealed trait MajorBlockConsensus extends BlockConsensus

    final case class MajorBlockRoundOne(
        override val blockNum: Block.Number,
        augmentedBlock: Option[AugmentedBlock.Major],
        acks1: Set[AckBlock.Major1]
    ) extends MajorBlockConsensus

    final case class MajorBlockRoundTwo(
        override val blockNum: Block.Number,
        augmentedBlock: AugmentedBlock.Major,
        acks1: Set[AckBlock.Major1],
        acks2: Set[AckBlock.Major2]
    ) extends MajorBlockConsensus

    sealed trait FinalBlockConsensus extends BlockConsensus

    final case class FinalBlockRoundOne(
        override val blockNum: Block.Number,
        augmentedBlock: Option[AugmentedBlock.Final],
        acks1: Set[AckBlock.Final1]
    ) extends MajorBlockConsensus

    final case class FinalBlockRoundTwo(
        override val blockNum: Block.Number,
        augmentedBlock: AugmentedBlock.Final,
        acks1: Set[AckBlock.Final1],
        acks2: Set[AckBlock.Final2]
    ) extends FinalBlockConsensus
}
