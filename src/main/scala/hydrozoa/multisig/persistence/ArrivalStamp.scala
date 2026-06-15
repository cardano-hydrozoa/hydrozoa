package hydrozoa.multisig.persistence

import java.nio.ByteBuffer

/** The durable arrival stamp that orders persisted family entries across the `2 + 3N` streams at
  * replay time (§5.4). A `(generation, monotonicNanos)` pair, **ordered lexicographically**:
  *
  *   - `generation` — a per-process boot counter persisted in the store and bumped once at startup
  *     ([[Persistence.fromBackend]]). Entries written in a later process therefore always sort
  *     after earlier ones, so the order **survives restarts** (which a bare `IO.monotonic` would
  *     not — its zero resets every process).
  *   - `monotonicNanos` — `IO.monotonic` within the process: strictly increasing, nanosecond
  *     resolution (no ties), and never steps backward (unlike a wall clock).
  *
  * Encoded big-endian as `[generation : 4][monotonicNanos : 8]` (12 bytes), so raw byte order
  * matches `(generation, monotonicNanos)` order — the property the §5.4 merge relies on.
  */
final case class ArrivalStamp(generation: Int, monotonicNanos: Long):
    /** Big-endian `[generation : 4][monotonicNanos : 8]`. */
    def toBytes: Array[Byte] =
        ByteBuffer.allocate(ArrivalStamp.width).putInt(generation).putLong(monotonicNanos).array()

object ArrivalStamp:
    /** Encoded width in bytes: 4 (`generation`) + 8 (`monotonicNanos`). */
    val width: Int = 12

    /** Read an [[ArrivalStamp]] from the first [[width]] bytes of `bytes`. Throws on a short buffer
      * (treated as store corruption — fail safe).
      */
    def fromBytes(bytes: Array[Byte]): ArrivalStamp =
        if bytes.length < width then
            throw new IllegalArgumentException(
              s"arrival stamp too short: ${bytes.length} bytes (need $width)"
            )
        else
            val bb = ByteBuffer.wrap(bytes, 0, width)
            ArrivalStamp(bb.getInt, bb.getLong)

    /** Lexicographic `(generation, monotonicNanos)` order — matches the big-endian byte order. */
    given Ordering[ArrivalStamp] = Ordering.by(s => (s.generation, s.monotonicNanos))
