package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.event.RequestHash
import io.circe.*
import io.circe.generic.semiauto.*

sealed trait BlockBrief extends BlockBrief.Section {

    def asUnsigned: this.type & BlockStatus.Unsigned =
        this.asInstanceOf[this.type & BlockStatus.Unsigned]
    def asHardConfirmed: this.type & BlockStatus.HardConfirmed =
        this.asInstanceOf[this.type & BlockStatus.HardConfirmed]
    def asSoftConfirmed: this.type & BlockStatus.SoftConfirmed =
        this.asInstanceOf[this.type & BlockStatus.SoftConfirmed]
}

object BlockBrief {
    // N.B.: technically we only need the cardano network for the decoder.
    given (using cardanoNetwork: CardanoNetwork.Section): Codec[BlockBrief] =
        deriveCodec[BlockBrief]
    given (using cardanoNetwork: CardanoNetwork.Section): Codec[BlockBrief.Initial] =
        deriveCodec[BlockBrief.Initial]
    given bbMinorCodec(using CardanoNetwork.Section): Codec[BlockBrief.Minor] =
        deriveCodec[BlockBrief.Minor]
    given bbMajorCodec(using CardanoNetwork.Section): Codec[BlockBrief.Major] =
        deriveCodec[BlockBrief.Major]
    given (using CardanoNetwork.Section): Codec[BlockBrief.Final] = deriveCodec[BlockBrief.Final]

    final case class Initial(
        override val header: BlockHeader.Initial,
        override val blockHash: BlockHash
    ) extends BlockBrief,
          BlockType.Initial {
        override transparent inline def blockBrief: BlockBrief.Initial = this
        override transparent inline def body: BlockBody.Initial.type = BlockBody.Initial
    }

    object Initial {

        /** Build the brief, deriving its digest — what a producer does. The generated two-argument
          * `apply` keeps a digest that arrived with the brief instead, so a reader can compare the
          * claim against one it derived itself.
          */
        def apply(header: BlockHeader.Initial): Initial =
            new Initial(header, BlockHash(header, BlockBody.Initial))
    }

    final case class Minor(
        override val header: BlockHeader.Minor,
        override val body: BlockBody.Minor,
        override val blockHash: BlockHash
    ) extends BlockBrief,
          BlockType.Minor {
        override transparent inline def blockBrief: BlockBrief.Minor = this
    }

    object Minor {

        /** Build the brief, deriving its digest — see [[Initial.apply]]. */
        def apply(header: BlockHeader.Minor, body: BlockBody.Minor): Minor =
            new Minor(header, body, BlockHash(header, body))
    }

    final case class Major(
        override val header: BlockHeader.Major,
        override val body: BlockBody.Major,
        override val blockHash: BlockHash
    ) extends BlockBrief,
          BlockType.Major {
        override transparent inline def blockBrief: BlockBrief.Major = this
    }

    object Major {

        /** Build the brief, deriving its digest — see [[Initial.apply]]. */
        def apply(header: BlockHeader.Major, body: BlockBody.Major): Major =
            new Major(header, body, BlockHash(header, body))
    }

    final case class Final(
        override val header: BlockHeader.Final,
        override val body: BlockBody.Final,
        override val blockHash: BlockHash
    ) extends BlockBrief,
          BlockType.Final {
        override transparent inline def blockBrief: BlockBrief.Final = this
    }

    object Final {

        /** Build the brief, deriving its digest — see [[Initial.apply]]. */
        def apply(header: BlockHeader.Final, body: BlockBody.Final): Final =
            new Final(header, body, BlockHash(header, body))
    }

    type Next = BlockBrief & BlockType.Next
    type Intermediate = BlockBrief & BlockType.Intermediate
    type NonFinal = BlockBrief & BlockType.NonFinal

    trait Section extends BlockType, BlockHeader.Section, BlockBody.Section {
        import hydrozoa.multisig.ledger.event.RequestId
        import RequestId.ValidityFlag

        def blockBrief: BlockBrief

        /** This block's content digest — see [[BlockHash]].
          *
          * A brief is where header and body meet, which is why the digest lives here and not on
          * [[BlockHeader]]: `nextHeaderMinor` and its siblings derive block N+1's header from N's
          * header plus timing, *before* N+1's body exists, so a header field covering the body
          * could never be filled.
          *
          * **Stored, and never trusted.** The brief carries the value on the wire and into the
          * `Block` journal, but a stored digest is a claim: a peer that rebuilds the block derives
          * its own and compares (`JointLedger.panicOnMismatchWithExpectedBrief`). What makes the
          * claim worth anything is that every head peer's soft-ack signs it — the 32 digest bytes,
          * as [[hydrozoa.multisig.consensus.ack.SoftAck.Signature]].
          */
        def blockHash: BlockHash

        override transparent inline def blockNum: BlockNumber = header.blockNum
        override transparent inline def blockVersion: BlockVersion.Full = header.blockVersion
        override transparent inline def startTime: BlockCreationStartTime = header.startTime
        override transparent inline def endTime: BlockCreationEndTime = header.endTime

        override transparent inline def requests: List[(RequestId, RequestHash, ValidityFlag)] =
            body.requests
        override transparent inline def depositsAbsorbed: List[RequestId] =
            body.depositsAbsorbed
        override transparent inline def depositsRejected: List[RequestId] =
            body.depositsRejected
    }

    object Section {
        type Next = Section & BlockType.Next
        type Intermediate = Section & BlockType.Intermediate
        type NonFinal = Section & BlockType.NonFinal
    }
}
