package hydrozoa.multisig.protocol.types

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.protocol.types.AckBlock.Fields.*
import scalus.builtin.ByteString

type AckBlockId = AckBlock.Id

enum AckBlock:
    def id: AckBlock.Id
    def blockNum: Block.Number

    case Minor(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val headerSignature: AckBlock.HeaderSignature,
        override val postDatedRefunds: List[AckBlock.TxSignature],
        override val finalizationRequested: Boolean
    ) extends AckBlock, MinorHeaderSignature, Refunds.PostDated, FinalizationRequested

    case Major1(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val fallback: AckBlock.TxSignature,
        override val rollouts: List[AckBlock.TxSignature],
        override val postDatedRefunds: List[AckBlock.TxSignature],
        override val finalizationRequested: Boolean
    ) extends AckBlock, Rollouts, Fallback, Refunds.PostDated, FinalizationRequested

    case Major2(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val settlement: AckBlock.TxSignature
    ) extends AckBlock, Settlement

    case Final1(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val rollouts: List[AckBlock.TxSignature],
        override val deinit: Option[AckBlock.TxSignature]
    ) extends AckBlock, Rollouts, Deinit

    case Final2(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val finalization: AckBlock.TxSignature
    ) extends AckBlock, Finalization

object AckBlock {

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

        /** The given block will be confirmed when AckBlocks with this AckBlock.Number are received
          * from all peers. It is equal to the block number plus the major version number because:
          *   - Minor blocks each need only one ack and don't increment the major version.
          *   - Major and final blocks each need two acks and do increment the major version.
          */
        def neededToConfirm(block: Block.Next): Number =
            import block.header
            header.blockNum + header.blockVersion.major

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
}
