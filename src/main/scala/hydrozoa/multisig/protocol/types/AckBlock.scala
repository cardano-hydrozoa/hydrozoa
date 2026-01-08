package hydrozoa.multisig.protocol.types

import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.protocol.types.AckBlock.Fields.*
import hydrozoa.multisig.protocol.types.AckBlock.{HeaderSignature, TxSignature}
import scalus.cardano.ledger.VKeyWitness

type AckBlockId = AckBlock.Id

enum AckBlock:
    def id: AckBlock.Id
    def blockNum: Block.Number

    case Minor(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val headerSignature: HeaderSignature,
        override val postDatedRefunds: List[TxSignature]
    ) extends AckBlock, MinorHeaderSignature, Refunds.PostDated

    case Major1(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val fallback: TxSignature,
        override val rollouts: List[TxSignature],
        override val postDatedRefunds: List[TxSignature]
    ) extends AckBlock, Rollouts, Fallback, Refunds.PostDated

    case Major2(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val settlement: TxSignature
    ) extends AckBlock, Settlement

    case Final1(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val rollouts: List[TxSignature],
        override val deinit: Option[TxSignature]
    ) extends AckBlock, Rollouts, Deinit

    case Final2(
        override val id: AckBlock.Id,
        override val blockNum: Block.Number,
        override val finalization: TxSignature
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
    // Transactions signatures
    // ===================================

    case class TxSignature(
        signature: VKeyWitness
    )

    // ===================================
    // Ed25519 signatures (used for minor block headers)
    // ===================================

    type HeaderSignature = Ed25519SignatureHex.Ed25519SignatureHex

    object Ed25519SignatureHex:
        opaque type Ed25519SignatureHex = String

        def apply(signature: String): Ed25519SignatureHex = signature

        given Conversion[Ed25519SignatureHex, String] = identity

        extension (signature: Ed25519SignatureHex) def untagged: String = identity(signature)

    // TODO: this is used in the rule-based regime as well, so maybe it should live in the "bridge" module
    object Ed25519Signature:
        opaque type Ed25519Signature = IArray[Byte]

        def apply(signature: IArray[Byte]): Ed25519Signature = signature

        given Conversion[Ed25519Signature, IArray[Byte]] = identity

        given Conversion[Ed25519Signature, Array[Byte]] = sig => IArray.genericWrapArray(sig).toArray

        extension (signature: Ed25519Signature) def untagged: IArray[Byte] = identity(signature)

    type Ed25519Signature = Ed25519Signature.Ed25519Signature

}
