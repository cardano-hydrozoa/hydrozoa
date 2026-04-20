package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import io.circe.*
import io.circe.generic.semiauto.*

sealed trait BlockBrief extends BlockBrief.Section {
    def asUnsigned: this.type & BlockStatus.Unsigned =
        this.asInstanceOf[this.type & BlockStatus.Unsigned]
    def asMultiSigned: this.type & BlockStatus.MultiSigned =
        this.asInstanceOf[this.type & BlockStatus.MultiSigned]
}

object BlockBrief {

    given blockBriefInitialEncoder: Encoder[BlockBrief.Initial] =
        deriveEncoder[BlockBrief.Initial]

    given blockBriefInitialDecoder(using CardanoNetwork.Section): Decoder[BlockBrief.Initial] =
        deriveDecoder[BlockBrief.Initial]

    final case class Initial(
        override val header: BlockHeader.Initial
    ) extends BlockBrief,
          BlockType.Initial {
        override transparent inline def blockBrief: BlockBrief.Initial = this
        override transparent inline def body: BlockBody.Initial.type = BlockBody.Initial
    }

    final case class Minor(
        override val header: BlockHeader.Minor,
        override val body: BlockBody.Minor
    ) extends BlockBrief,
          BlockType.Minor {
        override transparent inline def blockBrief: BlockBrief.Minor = this
    }

    final case class Major(
        override val header: BlockHeader.Major,
        override val body: BlockBody.Major
    ) extends BlockBrief,
          BlockType.Major {
        override transparent inline def blockBrief: BlockBrief.Major = this
    }

    final case class Final(
        override val header: BlockHeader.Final,
        override val body: BlockBody.Final
    ) extends BlockBrief,
          BlockType.Final {
        override transparent inline def blockBrief: BlockBrief.Final = this
    }

    type Next = BlockBrief & BlockType.Next
    type Intermediate = BlockBrief & BlockType.Intermediate
    type NonFinal = BlockBrief & BlockType.NonFinal

    trait Section extends BlockType, BlockHeader.Section, BlockBody.Section {
        import hydrozoa.multisig.ledger.event.RequestId
        import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
        import RequestId.ValidityFlag

        def blockBrief: BlockBrief

        override transparent inline def blockNum: BlockNumber = header.blockNum
        override transparent inline def blockVersion: BlockVersion.Full = header.blockVersion
        override transparent inline def startTime: BlockCreationStartTime = header.startTime
        override transparent inline def endTime: BlockCreationEndTime = header.endTime
        override transparent inline def kzgCommitment: KzgCommitment = header.kzgCommitment

        override transparent inline def events: List[(RequestId, ValidityFlag)] = body.events
        override transparent inline def depositsAbsorbed: List[RequestId] =
            body.depositsAbsorbed
        override transparent inline def depositsRefunded: List[RequestId] =
            body.depositsRefunded
    }

    object Section {
        type Next = Section & BlockType.Next
        type Intermediate = Section & BlockType.Intermediate
        type NonFinal = Section & BlockType.NonFinal
    }
}
