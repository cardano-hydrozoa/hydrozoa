package hydrozoa.multisig.protocol.types

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.protocol.types.AckBlock.Fields.*
import scalus.builtin.ByteString
import scalus.cardano.ledger.VKeyWitness

type AckBlockId = AckBlock.Id

sealed trait AckBlock {
    def id: AckBlock.Id
    def blockNum: Block.Number
}

object AckBlock {

    final case class Minor(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val headerSignature: AckBlock.HeaderSignature,
        override val postDatedRefunds: List[AckBlock.TxSignature],
        override val finalizationRequested: Boolean,
    ) extends AckBlock
        with MinorHeaderSignature
        with Refunds.PostDated
        with FinalizationRequested
        with BlockAckSet {
        override def asList: List[AckBlock] = List(this)
    }

    final case class Major1(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val fallback: AckBlock.TxSignature,
        override val rollouts: List[AckBlock.TxSignature],
        override val postDatedRefunds: List[AckBlock.TxSignature],
        override val finalizationRequested: Boolean
    ) extends AckBlock
        with Rollouts
        with Fallback
        with Refunds.PostDated
        with FinalizationRequested

    final case class Major2(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val settlement: AckBlock.TxSignature
    ) extends AckBlock
        with Settlement

    final case class Final1(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val rollouts: List[AckBlock.TxSignature],
        override val deinit: Option[AckBlock.TxSignature]
    ) extends AckBlock
        with Rollouts
        with Deinit

    final case class Final2(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val finalization: AckBlock.TxSignature
    ) extends AckBlock
        with Finalization

    type Id = Id.Id
    type Number = Number.Number
    type Subscriber = ActorRef[IO, AckBlock]

    object Fields {
        sealed trait MinorHeaderSignature {
            def headerSignature: HeaderSignature
        }

        sealed trait Settlement {
            def settlement: TxSignature
        }

        sealed trait Rollouts {
            def rollouts: List[TxSignature]
        }

        sealed trait Fallback {
            def fallback: TxSignature
        }

        object Refunds {
            sealed trait PostDated {
                def postDatedRefunds: List[TxSignature]
            }
        }

        sealed trait Finalization {
            def finalization: TxSignature
        }

        sealed trait Deinit {
            def deinit: Option[TxSignature]
        }

        sealed trait FinalizationRequested {
            def finalizationRequested: Boolean
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

    // ===================================
    // Transactions signatures (Ed25519) - this type doesn't contain vkey as VKeyWitness does.
    // ===================================

    object TxSignature:
        opaque type TxSignature = IArray[Byte]

        def apply(signature: IArray[Byte]): TxSignature = signature

        def apply(witness: VKeyWitness): TxSignature =
            IArray.from(witness.signature.bytes)

        given Conversion[TxSignature, IArray[Byte]] = identity

        given Conversion[TxSignature, Array[Byte]] = sig => IArray.genericWrapArray(sig).toArray

        given Conversion[TxSignature, ByteString] = sig => ByteString.fromArray(sig)

        extension (signature: TxSignature) def untagged: IArray[Byte] = identity(signature)

    type TxSignature = TxSignature.TxSignature

    // ===================================
    // Minor block header signatures (also Ed25519)
    // ===================================

    // TODO: this is used in the rule-based regime as well, so maybe it should live in the "common" module
    object HeaderSignature:
        opaque type HeaderSignature = IArray[Byte]

        def apply(signature: IArray[Byte]): HeaderSignature = signature

        given Conversion[HeaderSignature, IArray[Byte]] = identity

        given Conversion[HeaderSignature, Array[Byte]] = sig => IArray.genericWrapArray(sig).toArray

        given Conversion[HeaderSignature, ByteString] = sig => ByteString.fromArray(sig)

        extension (signature: HeaderSignature) def untagged: IArray[Byte] = identity(signature)

    type HeaderSignature = HeaderSignature.HeaderSignature

    // ===================================
    // Ack sets
    // ===================================

    trait BlockAckSet:
        def asList: List[AckBlock]

    object BlockAckSet:
        case class Major(ack1: Major1, ack2: Major2) extends BlockAckSet {
            override def asList: List[AckBlock] = List(ack1, ack2)
        }
        case class Final(ack1: Final1, ack2: Final2) extends BlockAckSet {
            override def asList: List[AckBlock] = List(ack1, ack2)
        }

}
