package hydrozoa.multisig.ledger.block

import hydrozoa.multisig.ledger.event.{RequestHash, RequestId}
import io.circe.Codec
import io.circe.generic.semiauto.*

import RequestId.ValidityFlag

trait BlockBody extends BlockBody.Section {
    def asUnsigned: this.type & BlockStatus.Unsigned =
        this.asInstanceOf[this.type & BlockStatus.Unsigned]
    def asHardConfirmed: this.type & BlockStatus.HardConfirmed =
        this.asInstanceOf[this.type & BlockStatus.HardConfirmed]
}

object BlockBody {
    case object Initial extends BlockBody, BlockType.Initial {
        override transparent inline def body: BlockBody.Initial.type = this
        override transparent inline def requests: List[(RequestId, RequestHash, ValidityFlag)] =
            List()
        override transparent inline def depositsAbsorbed: List[RequestId] = List()
        override transparent inline def depositsRejected: List[RequestId] = List()
    }

    given Codec[Minor] = deriveCodec[Minor]
    final case class Minor(
        override val requests: List[(RequestId, RequestHash, ValidityFlag)],
        override val depositsRejected: List[RequestId]
    ) extends BlockBody,
          BlockType.Minor {
        override transparent inline def body: BlockBody.Minor = this
        override transparent inline def depositsAbsorbed: List[RequestId] = List()
    }

    given Codec[Major] = deriveCodec[Major]
    final case class Major(
        override val requests: List[(RequestId, RequestHash, ValidityFlag)],
        override val depositsAbsorbed: List[RequestId],
        override val depositsRejected: List[RequestId]
    ) extends BlockBody,
          BlockType.Major {
        override transparent inline def body: BlockBody.Major = this
    }

    given Codec[Final] = deriveCodec[Final]
    final case class Final(
        override val requests: List[(RequestId, RequestHash, ValidityFlag)],
        override val depositsRejected: List[RequestId]
    ) extends BlockBody,
          BlockType.Final {
        override transparent inline def body: BlockBody.Final = this
        override transparent inline def depositsAbsorbed: List[RequestId] = List()
    }

    type Next = BlockBody & BlockType.Next
    type Intermediate = BlockBody & BlockType.Intermediate

    trait Section {
        def body: BlockBody

        /** The block's requests in the order the leader chose them: each request's id, the digest
          * of the body that id names ([[hydrozoa.multisig.consensus.UserRequestBody.hash]]), and
          * whether applying it succeeded.
          *
          * The digest is what ties an id to its bytes. Without it two peers holding different
          * payloads under the same id compare equal, because nothing else in a block names a
          * payload. It is carried and never trusted: a peer that holds the request re-derives the
          * digest from its own copy (see [[hydrozoa.multisig.ledger.joint.JointLedger]]), and only
          * a peer that does not hold it — a submitter checking a block for their request, a peer
          * seeded from a snapshot — reads the carried value.
          */
        def requests: List[(RequestId, RequestHash, ValidityFlag)]
        def depositsAbsorbed: List[RequestId]
        def depositsRejected: List[RequestId]
    }
}
