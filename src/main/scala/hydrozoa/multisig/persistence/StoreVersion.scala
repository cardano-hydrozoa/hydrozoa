package hydrozoa.multisig.persistence

import java.nio.ByteBuffer

/** The persistence-layer schema version: can this binary read this directory?
  *
  * The store rejects opens that find a version it does not understand — better fail-safe than
  * silently misread. A mismatch is a [[hydrozoa.lib.StartupRefusal]]: nothing about the world
  * changes what is on disk, so a restart re-derives the same verdict.
  *
  * One of the three versions a build carries, and the two beside it fail differently — see
  * `design/versioning.md`:
  *
  *   - [[hydrozoa.multisig.consensus.transport.ProtocolVersion]] is compared against a counterpart
  *     at every handshake; this one is compared against a local directory at every open.
  *   - The software version is compared against nothing at all.
  */
object StoreVersion:
    /** Current on-disk schema version — **5**.
      *
      * **Bump on any change to the column-family set, the key layout or a value codec.** A bump is
      * not a migration: no store of an earlier version is ever read, because a bump deploys by head
      * migration (`design/versioning.md`) and every peer of the new head starts from a fresh store.
      *
      * ⛔ A bump therefore cannot be answered by rebuilding the store of a running peer. A cold
      * store re-bootstraps stack 0 and never rejoins its head (GUM-312), so the rebuild that
      * settles a format change in development is not available on a live head.
      *
      *   - 2: the Request journals' values changed from the circe wire form to the canonical
      *     protobuf record (`proto/request_record.proto`).
      *   - 3: [[StoreIdentity]] arrived. Bumped rather than treating an unstamped store as fresh,
      *     which would bless whatever store the node happens to be pointed at on its first open —
      *     exactly the mistake the stamp exists to catch. Existing stores rebuild.
      *   - 4: content digests (`docs/spec/block-hash.md`). A `Request` record gains its
      *     `request_hash` field; a `Block` brief and every value carrying one — `SoftConfirmation`,
      *     `BlockResult` — gain `blockHash` and a per-request digest; a `SoftConfirmation`'s
      *     signature list is keyed `softAckSignatures`, and an SEC's `signatures`.
      *   - 5: L2 state certificates (`docs/spec/l2-state-certificate.md`). An SEC gains
      *     `l2StateHash` — both the offchain record and the on-chain bytes it serializes — and so
      *     does the multisig treasury datum, which every persisted settlement and treasury value
      *     carries.
      *   - 6: `Cf.DepositMap` is keyed by `blockNum` instead of holding one singleton blob, so the
      *     deposits map at any retained block is recoverable and servable rather than only the one
      *     at the tip. The value codec is unchanged; only the key is.
      */
    val current: Int = 7

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

    /** Compare a store's stamped version against [[current]]. `None` is a store with no version key
      * at all, which a writable open stamps and a read-only open refuses.
      */
    def check(stamped: Option[Int]): Check =
        stamped match {
            case None                    => Check.Fresh
            case Some(v) if v == current => Check.Compatible
            case Some(v)                 => Check.Incompatible(v, current)
        }
