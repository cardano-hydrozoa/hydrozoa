package hydrozoa.multisig.persistence

/** Byte-level batch — sibling of [[BackendStore]]: a group of writes committed atomically as one
  * transaction. The backend (e.g. RocksDB) lands all operations or none on a crash, regardless of
  * how many column families they touch.
  *
  * This is *not* the actor-facing batch. Actors build a typed [[WriteBatch]] over [[StoreKey]]s +
  * typed values; the typed batch lowers to a `RawWriteBatch` (via each key's codec) before hitting
  * the `BackendStore`. Direct `RawWriteBatch` use is reserved for the backend implementation and
  * the few internal modules that work at the byte level.
  */
final case class RawWriteBatch private (ops: Vector[RawWriteBatch.Op]):
    /** Add a put at `(cf, key)`. */
    def put(cf: Cf, key: Array[Byte], value: Array[Byte]): RawWriteBatch =
        copy(ops = ops :+ RawWriteBatch.Op.Put(cf, key, value))

    /** Add a delete at `(cf, key)`. */
    def delete(cf: Cf, key: Array[Byte]): RawWriteBatch =
        copy(ops = ops :+ RawWriteBatch.Op.Delete(cf, key))

    /** Add a delete-range at `cf` over `[fromInclusive, toExclusive)`. Half-open, RocksDB-style. */
    def deleteRange(cf: Cf, fromInclusive: Array[Byte], toExclusive: Array[Byte]): RawWriteBatch =
        copy(ops = ops :+ RawWriteBatch.Op.DeleteRange(cf, fromInclusive, toExclusive))

    /** True iff this batch contains no operations. */
    def isEmpty: Boolean = ops.isEmpty

object RawWriteBatch:
    /** The empty batch — starting point for building. */
    val empty: RawWriteBatch = RawWriteBatch(Vector.empty)

    /** One operation in a raw write batch. */
    enum Op:
        case Put(cf: Cf, key: Array[Byte], value: Array[Byte])
        case Delete(cf: Cf, key: Array[Byte])
        case DeleteRange(cf: Cf, fromInclusive: Array[Byte], toExclusive: Array[Byte])
