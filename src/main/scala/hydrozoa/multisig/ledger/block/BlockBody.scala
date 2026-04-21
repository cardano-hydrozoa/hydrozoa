package hydrozoa.multisig.ledger.block

import hydrozoa.multisig.ledger.event.RequestId
import io.circe.Codec
import io.circe.generic.semiauto.*

import RequestId.ValidityFlag

trait BlockBody extends BlockBody.Section {
    def asUnsigned: this.type & BlockStatus.Unsigned =
        this.asInstanceOf[this.type & BlockStatus.Unsigned]
    def asMultiSigned: this.type & BlockStatus.MultiSigned =
        this.asInstanceOf[this.type & BlockStatus.MultiSigned]
}

object BlockBody {
    case object Initial extends BlockBody, BlockType.Initial {
        override transparent inline def body: BlockBody.Initial.type = this
        override transparent inline def events: List[(RequestId, ValidityFlag)] = List()
        override transparent inline def depositsAbsorbed: List[RequestId] = List()
        override transparent inline def depositsRefunded: List[RequestId] = List()
    }

    given Codec[Minor] = deriveCodec[Minor]
    final case class Minor(
        override val events: List[(RequestId, ValidityFlag)],
        override val depositsRefunded: List[RequestId]
    ) extends BlockBody,
          BlockType.Minor {
        override transparent inline def body: BlockBody.Minor = this
        override transparent inline def depositsAbsorbed: List[RequestId] = List()
    }

    given Codec[Major] = deriveCodec[Major]
    final case class Major(
        override val events: List[(RequestId, ValidityFlag)],
        override val depositsAbsorbed: List[RequestId],
        override val depositsRefunded: List[RequestId]
    ) extends BlockBody,
          BlockType.Major {
        override transparent inline def body: BlockBody.Major = this
    }

    given Codec[Final] = deriveCodec[Final]
    final case class Final(
        override val events: List[(RequestId, ValidityFlag)],
        override val depositsRefunded: List[RequestId]
    ) extends BlockBody,
          BlockType.Final {
        override transparent inline def body: BlockBody.Final = this
        override transparent inline def depositsAbsorbed: List[RequestId] = List()
    }

    type Next = BlockBody & BlockType.Next
    type Intermediate = BlockBody & BlockType.Intermediate

    trait Section {
        def body: BlockBody
        def events: List[(RequestId, ValidityFlag)]
        def depositsAbsorbed: List[RequestId]
        def depositsRefunded: List[RequestId]
    }
}
