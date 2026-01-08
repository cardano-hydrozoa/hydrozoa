package hydrozoa.multisig.protocol.types

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef

import AckBlock.Fields.*

type AckBlockId = AckBlock.Id

enum AckBlock:
    def id: AckBlock.Id
    def blockNum: Block.Number

    case Minor(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val headerSignature: String,
        override val immediateRefunds: List[BlockEffect.Signature],
        override val postDatedRefunds: List[BlockEffect.Signature]
    ) extends AckBlock, MinorHeaderSignature, Refunds.Immediate, Refunds.PostDated

    case Major1(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val fallback: BlockEffect.Signature,
        override val rollouts: List[BlockEffect.Signature],
        override val immediateRefunds: List[BlockEffect.Signature],
        override val postDatedRefunds: List[BlockEffect.Signature]
    ) extends AckBlock, Rollouts, Fallback, Refunds.Immediate, Refunds.PostDated

    case Major2(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val settlement: BlockEffect.Signature
    ) extends AckBlock, Settlement

    case Final1(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val rollouts: List[BlockEffect.Signature],
        override val immediateRefunds: List[BlockEffect.Signature]
    ) extends AckBlock, Rollouts, Refunds.Immediate

    case Final2(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val finalization: BlockEffect.Signature
    ) extends AckBlock, Finalization

object AckBlock {
    type Id = Id.Id
    type Number = Number.Number
    type Subscriber = ActorRef[IO, AckBlock]

    object Fields {
        sealed trait MinorHeaderSignature {
            def headerSignature: String
        }

        sealed trait Settlement {
            def settlement: BlockEffect.Signature
        }

        sealed trait Rollouts {
            def rollouts: List[BlockEffect.Signature]
        }

        sealed trait Fallback {
            def fallback: BlockEffect.Signature
        }

        object Refunds {
            sealed trait Immediate {
                def immediateRefunds: List[BlockEffect.Signature]
            }

            sealed trait PostDated {
                def postDatedRefunds: List[BlockEffect.Signature]
            }
        }

        sealed trait Finalization {
            def finalization: BlockEffect.Signature
        }
    }

    object Id {
        opaque type Id = (Int, Int)

        def apply(peerId: Int, ackNum: Int): Id = (peerId, ackNum)

        def unapply(self: Id): (Peer.Number, Number) = (Peer.Number(self._1), Number(self._2))

        given Conversion[Id, (Int, Int)] = identity

        given Ordering[Id] with {
            override def compare(x: Id, y: Id): Int =
                x.compare(y)
        }

        extension (self: Id)
            def increment: Id = Id(self._1, self._2 + 1)
            // TODO: rename peerId (or rename all peerId to peerNum
            def peerNum: Peer.Number = Peer.Number(self._1)
            def ackNum: Number = Number(self._2)
    }

    object Number {
        opaque type Number = Int

        def apply(i: Int): Number = i

        given Conversion[Number, Int] = identity

        given Ordering[Number] with {
            override def compare(x: Number, y: Number): Int =
                x.compare(y)
        }

        extension (self: Number) def increment: Number = Number(self + 1)
    }
}
