package hydrozoa.multisig.ledger.stack

import io.circe.*
import scala.util.Try

/** Zero-based index of a partition within a stack — the position in the partition-indexed
  * [[StackEffects.Unsigned.Regular]] / hard-ack partition list (head-based partitions, see
  * [[StackPartition]]). So logs say *which* partition rather than a bare `Int`. Intra-partition
  * order is just list order (there is no `WithinPartitionIndex` — PR #446 review).
  */
type PartitionIndex = PartitionIndex.PartitionIndex

object PartitionIndex {
    given Encoder[PartitionIndex] = Encoder.encodeInt.contramap(identity)
    given Decoder[PartitionIndex] =
        Decoder.decodeInt.emap(i => Try(PartitionIndex(i)).toEither.left.map(_.getMessage))

    opaque type PartitionIndex = Int

    def apply(i: Int): PartitionIndex = {
        require(i >= 0)
        i
    }

    val zero: PartitionIndex = 0

    given Conversion[PartitionIndex, Int] = identity

    given Ordering[PartitionIndex] with {
        override def compare(x: PartitionIndex, y: PartitionIndex): Int = x.compare(y)
    }
}
