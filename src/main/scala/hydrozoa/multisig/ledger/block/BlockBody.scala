package hydrozoa.multisig.ledger.block

import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.protocol.types.LedgerEventId
import hydrozoa.multisig.protocol.types.LedgerEventId.ValidityFlag

trait BlockBody extends BlockBody.Section

object BlockBody {
    case object Initial extends BlockBody, BlockType.Initial {
        override transparent inline def body: BlockBody.Initial.type = this
        override transparent inline def events: List[(LedgerEventId, ValidityFlag)] = List()
        override transparent inline def depositsAbsorbed: List[LedgerEventId] = List()
        override transparent inline def depositsRefunded: List[LedgerEventId] = List()
    }

    final case class Minor(
        override val events: List[(LedgerEventId, ValidityFlag)],
        override val depositsRefunded: List[LedgerEventId]
    ) extends BlockBody,
          BlockType.Minor {
        override transparent inline def body: BlockBody.Minor = this
        override transparent inline def depositsAbsorbed: List[LedgerEventId] = List()

        transparent inline def asNextBlockBrief(
            previousHeader: BlockHeader,
            newStartTime: QuantizedInstant,
            newEndTime: QuantizedInstant,
            newKzgCommitment: KzgCommitment
        ): BlockBrief.Minor = BlockBrief.Minor(
          header = previousHeader.nextHeaderMinor(newStartTime, newEndTime, newKzgCommitment),
          body = this
        )
    }

    final case class Major(
        override val events: List[(LedgerEventId, ValidityFlag)],
        override val depositsAbsorbed: List[LedgerEventId],
        override val depositsRefunded: List[LedgerEventId]
    ) extends BlockBody,
          BlockType.Major {
        override transparent inline def body: BlockBody.Major = this

        transparent inline def asNextBlockBrief(
            previousHeader: BlockHeader,
            newStartTime: QuantizedInstant,
            newEndTime: QuantizedInstant,
            newKzgCommitment: KzgCommitment
        ): BlockBrief.Major = BlockBrief.Major(
          header = previousHeader.nextHeaderMajor(newStartTime, newEndTime, newKzgCommitment),
          body = this
        )
    }

    final case class Final(
        override val events: List[(LedgerEventId, ValidityFlag)],
        override val depositsRefunded: List[LedgerEventId]
    ) extends BlockBody,
          BlockType.Final {
        override transparent inline def body: BlockBody.Final = this
        override transparent inline def depositsAbsorbed: List[LedgerEventId] = List()

        transparent inline def asNextBlockBrief(
            previousHeader: BlockHeader,
            newStartTime: QuantizedInstant,
            newEndTime: QuantizedInstant
        ): BlockBrief.Final = BlockBrief.Final(
          header = previousHeader.nextHeaderFinal(newStartTime, newEndTime),
          body = this
        )
    }

    type Next = BlockBody & BlockType.Next
    type Intermediate = BlockBody & BlockType.Intermediate

    trait Section {
        def body: BlockBody
        def events: List[(LedgerEventId, ValidityFlag)]
        def depositsAbsorbed: List[LedgerEventId]
        def depositsRefunded: List[LedgerEventId]
    }
}
