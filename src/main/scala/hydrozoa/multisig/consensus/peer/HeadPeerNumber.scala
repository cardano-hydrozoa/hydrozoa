package hydrozoa.multisig.consensus.peer

import cats.Order
import hydrozoa.lib.number.NonNegativeInt
import hydrozoa.multisig.ledger.block.BlockHeader
import io.circe.*

type HeadPeerNumber = HeadPeerNumber.HeadPeerNumber

object HeadPeerNumber {

    given headPeerNumberEncoder: Encoder[HeadPeerNumber] = Encoder.encodeInt

    given headPeerNumberDecoder: Decoder[HeadPeerNumber] = Decoder.decodeInt.emap(i =>
        Either.cond(
          i >= 0 && i < (1 << 8),
          right = i,
          left = s"Expected a number `i` such  that `i >= 0 && i < (1 << 8)`, but got $i"
        )
    )

    given headPeerNumberKeyEncoder: KeyEncoder[HeadPeerNumber] =
        KeyEncoder.encodeKeyInt

    given headPeerNumberKeyDecoder: KeyDecoder[HeadPeerNumber] with {
        override def apply(s: String): Option[HeadPeerNumber] = {

            for {
                i <- KeyDecoder.decodeKeyInt(s)
                pi <- NonNegativeInt(i)
            } yield i

        }
    }

    opaque type HeadPeerNumber = Int

    def apply(i: Int): HeadPeerNumber = {
        require(i >= 0 && i < (1 << 8))
        i
    }

    val zero: HeadPeerNumber = 0

    /** The given block will be confirmed when AckBlocks with this AckBlock.Number are received from
      * all peers. It is equal to the block number plus the major version number because:
      *   - Minor blocks each need only one ack and don't increment the major version.
      *   - Major and final blocks each need two acks and do increment the major version.
      */
    def neededToConfirm(header: BlockHeader): HeadPeerNumber =
        header.blockNum + header.blockVersion.major

    given Conversion[HeadPeerNumber, Int] = identity

    given Ordering[HeadPeerNumber] with {
        override def compare(x: HeadPeerNumber, y: HeadPeerNumber): Int =
            x.convert.compare(y.convert)
    }

    given Order[HeadPeerNumber] with {
        override def compare(x: HeadPeerNumber, y: HeadPeerNumber): Int =
            x.convert.compare(y.convert)
    }

    extension (self: HeadPeerNumber)
        def increment: HeadPeerNumber = HeadPeerNumber(self + 1)
        def decrement: HeadPeerNumber = HeadPeerNumber(self - 1)
}
