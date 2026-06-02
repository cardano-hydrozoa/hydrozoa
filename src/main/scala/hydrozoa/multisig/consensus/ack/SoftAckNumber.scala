package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.ledger.block.BlockHeader
import io.circe.*

type SoftAckNumber = SoftAckNumber.SoftAckNumber

object SoftAckNumber {
    opaque type SoftAckNumber = Int

    given Codec[SoftAckNumber] = Codec.from(
      encodeA = Encoder.encodeInt,
      decodeA = Decoder.decodeInt
    )

    def apply(i: Int): SoftAckNumber = {
        require(i >= 0)
        i
    }

    val zero: SoftAckNumber = 0

    /** The given block is soft-confirmed when soft-acks with this (soft-)ack number are received
      * from all head peers. Equal to the block number plus the major version number because:
      *   - Minor blocks each need only one ack and don't increment the major version.
      *   - Major and final blocks each need two acks and do increment the major version.
      */
    def neededToConfirm(header: BlockHeader.Section): SoftAckNumber =
        header.blockNum + header.blockVersion.major

    given Conversion[SoftAckNumber, Int] = identity

    given Ordering[SoftAckNumber] with {
        override def compare(x: SoftAckNumber, y: SoftAckNumber): Int =
            x.compare(y)
    }

    extension (self: SoftAckNumber)
        def increment: SoftAckNumber = SoftAckNumber(self + 1)
        def decrement: SoftAckNumber = SoftAckNumber(self - 1)
}
