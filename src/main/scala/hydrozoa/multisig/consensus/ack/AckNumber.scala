package hydrozoa.multisig.consensus.ack

import hydrozoa.multisig.ledger.block.BlockHeader
import io.circe.*

// TODO(rename): AckNumber -> SoftAckNumber, to parallel HardAckNumber now that the slow
//   cycle exists. This is the FAST-cycle (soft-ack) per-peer cursor; "AckNumber" was
//   unambiguous before the fast/slow split but now reads as generic. A full rename touches
//   AckId / SoftAck / PeerLiaison / ConsensusActor / Codecs / tests, so it's deferred to a
//   dedicated sweep — left as a marker here and on the related comments below.
type AckNumber = AckNumber.AckNumber

object AckNumber {
    opaque type AckNumber = Int

    given Codec[AckNumber] = Codec.from(
      encodeA = Encoder.encodeInt,
      decodeA = Decoder.decodeInt
    )

    def apply(i: Int): AckNumber = {
        require(i >= 0)
        i
    }

    val zero: AckNumber = 0

    /** The given block is soft-confirmed when soft-acks with this (soft-)ack number are received
      * from all head peers. Equal to the block number plus the major version number because:
      *   - Minor blocks each need only one ack and don't increment the major version.
      *   - Major and final blocks each need two acks and do increment the major version.
      *
      * (TODO(rename): "AckNumber" -> "SoftAckNumber" — see the type-level note above.)
      */
    def neededToConfirm(header: BlockHeader.Section): AckNumber =
        header.blockNum + header.blockVersion.major

    given Conversion[AckNumber, Int] = identity

    given Ordering[AckNumber] with {
        override def compare(x: AckNumber, y: AckNumber): Int =
            x.compare(y)
    }

    extension (self: AckNumber)
        def increment: AckNumber = AckNumber(self + 1)
        def decrement: AckNumber = AckNumber(self - 1)
}
