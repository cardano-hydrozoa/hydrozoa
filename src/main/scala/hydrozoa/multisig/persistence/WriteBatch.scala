package hydrozoa.multisig.persistence

/** A group of writes committed atomically as one transaction. The backend (e.g. RocksDB) lands all
  * operations or none on a crash, regardless of how many column families they touch.
  *
  * Build with [[WriteBatch.empty]] / the builder methods, e.g.:
  * {{{
  *   WriteBatch.empty
  *       .put(Cf.Block, blockKey, briefBytes)
  *       .put(Cf.SoftAck, ackKey, ackBytes)
  *       .put(Cf.BlockResult, blockResultKey, resultBytes)
  *       .put(Cf.DepositMap, depositsKey, depositsBytes)
  * }}}
  */
final case class WriteBatch private (ops: Vector[WriteBatch.Op]):
    /** Add a put at `(cf, key)`. */
    def put(cf: Cf, key: Array[Byte], value: Array[Byte]): WriteBatch =
        copy(ops = ops :+ WriteBatch.Op.Put(cf, key, value))

    /** Add a delete at `(cf, key)`. */
    def delete(cf: Cf, key: Array[Byte]): WriteBatch =
        copy(ops = ops :+ WriteBatch.Op.Delete(cf, key))

    /** Add a delete-range at `cf` over `[fromInclusive, toExclusive)`. Half-open, RocksDB-style. */
    def deleteRange(cf: Cf, fromInclusive: Array[Byte], toExclusive: Array[Byte]): WriteBatch =
        copy(ops = ops :+ WriteBatch.Op.DeleteRange(cf, fromInclusive, toExclusive))

    /** True iff this batch contains no operations. */
    def isEmpty: Boolean = ops.isEmpty

object WriteBatch:
    /** The empty batch — starting point for building. */
    val empty: WriteBatch = WriteBatch(Vector.empty)

    /** One operation in a write batch. */
    enum Op:
        case Put(cf: Cf, key: Array[Byte], value: Array[Byte])
        case Delete(cf: Cf, key: Array[Byte])
        case DeleteRange(cf: Cf, fromInclusive: Array[Byte], toExclusive: Array[Byte])
