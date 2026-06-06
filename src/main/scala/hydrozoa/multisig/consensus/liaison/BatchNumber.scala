package hydrozoa.multisig.consensus.liaison

import io.circe.*

/** Monotonic per-link batch counter. Every `GetMsgBatch` / `NewMsgBatch` round on a liaison link
  * carries one, increasing by one each round; the receiver matches a reply's `batchNum` against its
  * currently-outstanding request to discard stale duplicates.
  */
type BatchNumber = BatchNumber.BatchNumber

object BatchNumber {
    opaque type BatchNumber = Int

    given Codec[BatchNumber] = Codec.from(Decoder.decodeInt, Encoder.encodeInt)

    def apply(i: Int): BatchNumber = {
        require(i >= 0)
        i
    }

    val zero: BatchNumber = 0

    given Conversion[BatchNumber, Int] = identity

    given Ordering[BatchNumber] with {
        override def compare(x: BatchNumber, y: BatchNumber): Int = x.compare(y)
    }

    extension (self: BatchNumber) def increment: BatchNumber = BatchNumber(self + 1)
}
