package hydrozoa.multisig.persistence

import cats.effect.Resource

/** The **byte-level storage primitive** behind the persistence layer — a sorted key-value store
  * divided into [[Cf column families]], with atomic batched writes and range-scan iterators.
  *
  * This is *not* the actor-facing API. Actors use the typed [[Persistence]] trait, which sits on
  * top of a `BackendStore` and threads keys / values through the [[StoreKey]] codecs. Direct
  * `BackendStore` access is reserved for:
  *
  *   - the storage engine implementation (`rocksdb.RocksDbBackendStore`);
  *   - debugging / inspection tools that legitimately work at the byte level (e.g. [[StoreDump]]);
  *   - specialised modules that derive state from raw key scans (e.g. `Markers`, which uses
  *     `lastKey` / `lastKeyWithPrefix` to derive the four markers from their CFs).
  *
  * Values are opaque bytes at this layer; the typed view lives one layer up.
  *
  * See `design/persistence-and-crash-recovery.md` §7.
  */
trait BackendStore[F[_]]:
    /** Read the value at `(cf, key)`. `None` if the key is absent. */
    def get(cf: Cf, key: Array[Byte]): F[Option[Array[Byte]]]

    /** Write `value` at `(cf, key)`. Overwrites any existing value. */
    def put(cf: Cf, key: Array[Byte], value: Array[Byte]): F[Unit]

    /** Delete the entry at `(cf, key)`. No-op if the key is absent. */
    def delete(cf: Cf, key: Array[Byte]): F[Unit]

    /** Commit a [[RawWriteBatch]] atomically — all operations land or none, even across CFs. The
      * underlying durability barrier (CR4/CR6/CR8 per the spec).
      */
    def write(batch: RawWriteBatch): F[Unit]

    /** Open a range-scan cursor over `cf`, positioned at the first key `>= fromInclusive`. The
      * cursor must be closed via the [[Resource]]; once closed, all references to it are invalid.
      */
    def cursor(cf: Cf, fromInclusive: Array[Byte]): Resource[F, BackendStore.Cursor[F]]

    /** The highest-keyed entry in `cf`, or `None` if the CF is empty.
      *
      * Used by the `Markers` module to derive the confirmed-side markers (§5.2):
      *   - `softConfirmed = lastKey(Cf.SoftConfirmation)`
      *   - `hardConfirmed = lastKey(Cf.HardConfirmation)`
      *
      * Implementation note: RocksDB serves `seekToLast` in O(1) — the LSM index pinpoints the
      * largest key across the MemTable + every SSTable level without a full scan.
      */
    def lastKey(cf: Cf): F[Option[Array[Byte]]]

    /** The highest-keyed entry in `cf` whose encoded bytes start with `prefix`, or `None` if no
      * such entry exists.
      *
      * Used by the `Markers` module to derive the acked-side markers from the own-peer prefix of
      * `SoftAck` / `HardAck` (§5.2):
      *   - `softAcked = lastKeyWithPrefix(Cf.SoftAck, [ownPeer:1])`
      *   - `hardAcked = lastKeyWithPrefix(Cf.HardAck, [ownPeer:1])`
      *
      * Implementation note: backed by RocksDB `seekForPrev` over `prefix ++ 0xFF…`, which lands on
      * the largest key `≤` that upper bound in O(log n); a single byte-prefix check confirms we're
      * still inside the requested prefix.
      */
    def lastKeyWithPrefix(cf: Cf, prefix: Array[Byte]): F[Option[Array[Byte]]]

object BackendStore:
    /** A range-scan cursor over one column family. Stateful; pull entries with [[next]] until
      * `None`, then release via the owning [[Resource]].
      */
    trait Cursor[F[_]]:
        /** The current `(key, value)` if the cursor is positioned on a valid entry, then advance
          * one step. `None` once the cursor has walked past the end.
          */
        def next: F[Option[(Array[Byte], Array[Byte])]]
