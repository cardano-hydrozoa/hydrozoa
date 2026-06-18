package hydrozoa.multisig.persistence

import java.nio.ByteBuffer

/** The persistence-layer schema version.
  *
  * The store rejects opens that find a version it does not understand — better fail-safe than
  * silently misread (CR6 / §7 versioning note). The mechanism is in place from day one; bumping it
  * on backward-incompatible format changes (CF set, key layout, codecs) waits until the layout
  * stabilizes — see [[current]].
  */
object StoreVersion:
    /** Current on-disk schema version — held at **1**.
      *
      * The format still churns freely during development, so we do **not** track
      * backward-incompatible bumps yet: a format change just rebuilds the store. Versioning starts
      * (and the per-version deltas get recorded here) once the layout stabilizes and stores must
      * survive upgrades.
      */
    val current: Int = 1

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
