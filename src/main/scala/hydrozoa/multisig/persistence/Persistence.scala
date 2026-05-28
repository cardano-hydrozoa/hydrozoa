package hydrozoa.multisig.persistence

import cats.effect.Resource

/** The persistence capability: a sorted key-value store, divided into [[Cf column families]], with
  * atomic batched writes and range-scan iterators.
  *
  * The interface is intentionally backend-agnostic — RocksDB is the chosen implementation, but any
  * sorted KV with multi-store atomic batching could sit behind it. Values are opaque bytes; callers
  * handle their own encoding (lane payloads reuse the wire codecs per §7).
  *
  * See `design/persistence-and-crash-recovery.md` §7.
  */
trait Persistence[F[_]]:
    /** Read the value at `(cf, key)`. `None` if the key is absent. */
    def get(cf: Cf, key: Array[Byte]): F[Option[Array[Byte]]]

    /** Write `value` at `(cf, key)`. Overwrites any existing value. */
    def put(cf: Cf, key: Array[Byte], value: Array[Byte]): F[Unit]

    /** Delete the entry at `(cf, key)`. No-op if the key is absent. */
    def delete(cf: Cf, key: Array[Byte]): F[Unit]

    /** Commit a [[WriteBatch]] atomically — all operations land or none, even across CFs. The
      * underlying durability barrier (CR4/CR6/CR8 per the spec).
      */
    def write(batch: WriteBatch): F[Unit]

    /** Open a range-scan cursor over `cf`, positioned at the first key `>= fromInclusive`. The
      * cursor must be closed via the [[Resource]]; once closed, all references to it are invalid.
      */
    def cursor(cf: Cf, fromInclusive: Array[Byte]): Resource[F, Persistence.Cursor[F]]

    /** The highest-keyed entry in `cf`, or `None` if the CF is empty.
      *
      * Backs the spine-shaped marker derivations (§5.2):
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
      * Backs satellite-lane own-marker derivations (§5.2):
      *   - `softAcked = lastKeyWithPrefix(Cf.SoftAck, [ownPeer:1])`
      *   - `hardAcked = lastKeyWithPrefix(Cf.HardAck, [ownPeer:1])`
      *
      * Implementation note: backed by RocksDB `seekForPrev` over `prefix ++ 0xFF…`, which lands on
      * the largest key `≤` that upper bound in O(log n); a single byte-prefix check confirms we're
      * still inside the requested prefix.
      */
    def lastKeyWithPrefix(cf: Cf, prefix: Array[Byte]): F[Option[Array[Byte]]]

    // ---- Typed convenience over LaneKey (for lane CFs only) ----

    /** Read the value at the given lane key. `None` if absent. */
    final def get(key: LaneKey): F[Option[Array[Byte]]] =
        get(key.laneId.cf, key.encode)

    /** Write `value` at the given lane key. */
    final def put(key: LaneKey, value: Array[Byte]): F[Unit] =
        put(key.laneId.cf, key.encode, value)

    /** Delete the entry at the given lane key. */
    final def delete(key: LaneKey): F[Unit] =
        delete(key.laneId.cf, key.encode)

object Persistence:
    /** A range-scan cursor over one column family. Stateful; pull entries with [[next]] until
      * `None`, then release via the owning [[Resource]].
      */
    trait Cursor[F[_]]:
        /** The current `(key, value)` if the cursor is positioned on a valid entry, then advance
          * one step. `None` once the cursor has walked past the end.
          */
        def next: F[Option[(Array[Byte], Array[Byte])]]
