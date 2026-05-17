package hydrozoa.multisig.ledger.effects

import io.circe.*
import scala.util.Try

/** Zero-based index of a major-version partition within a stack (a stack is partitioned by major
  * version — see [[NecessaryEffectsPolicy]]). Keys the per-partition entries of a hard-ack payload
  * (settlement / fallback / finalization), so logs and maps say *which* partition rather than a
  * bare `Int`.
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

/** Zero-based index of an effect *within* a single partition — used for the rollout / refund
  * entries of a hard-ack payload, which are keyed `(PartitionIndex, WithinPartitionIndex)`.
  * Distinct opaque type from [[PartitionIndex]] so the two halves of the tuple can't be swapped by
  * accident.
  */
type WithinPartitionIndex = WithinPartitionIndex.WithinPartitionIndex

object WithinPartitionIndex {
    given Encoder[WithinPartitionIndex] = Encoder.encodeInt.contramap(identity)
    given Decoder[WithinPartitionIndex] =
        Decoder.decodeInt.emap(i => Try(WithinPartitionIndex(i)).toEither.left.map(_.getMessage))

    opaque type WithinPartitionIndex = Int

    def apply(i: Int): WithinPartitionIndex = {
        require(i >= 0)
        i
    }

    val zero: WithinPartitionIndex = 0

    given Conversion[WithinPartitionIndex, Int] = identity

    given Ordering[WithinPartitionIndex] with {
        override def compare(x: WithinPartitionIndex, y: WithinPartitionIndex): Int =
            x.compare(y)
    }
}
