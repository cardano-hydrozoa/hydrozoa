package hydrozoa.multisig.persistence

import java.nio.ByteBuffer

/** The persistence-layer schema version.
  *
  * Bumped whenever the on-disk format (CF set, key layout, codecs) changes in a
  * backward-incompatible way. The store rejects opens that find a version it does not understand —
  * better fail-safe than silently misread (CR6 / §7 versioning note).
  */
object StoreVersion:
    /** Current on-disk schema version.
      *
      * v2: per-author satellite CF split (§7.1) — satellite CFs are now one-per-author with the
      * author embedded in the CF name and dropped from the key; incompatible with v1's combined
      * author-prefixed satellite CFs.
      *
      * v3: adds the `CoilStampMark` singleton CF (a hub's per-coil stamped-high-water marks, §6
      * `CoilAckSequencer`); the CF set differs from v2, so a v2 store cannot be opened with the v3
      * descriptor list.
      */
    val current: Int = 3

    /** The key under which the schema version is stored in [[Cf.Meta]]. */
    val key: Array[Byte] = "store_version".getBytes("UTF-8")

    /** Encode a version int as 4 big-endian bytes. */
    def encode(version: Int): Array[Byte] =
        ByteBuffer.allocate(4).putInt(version).array()

    /** Decode a version int from its 4-byte big-endian form; throws on malformed payload. */
    def decode(bytes: Array[Byte]): Int =
        if bytes.length != 4 then
            throw new IllegalArgumentException(
              s"store version: expected 4 bytes, got ${bytes.length}"
            )
        else ByteBuffer.wrap(bytes).getInt

    /** Outcome of the open-time version check. */
    enum Check:
        /** The store is fresh (no version present) — the caller writes the current version. */
        case Fresh

        /** The store's version matches `current` — proceed as normal. */
        case Compatible

        /** The store's version is something else — refuse to open. */
        case Incompatible(found: Int, expected: Int)
