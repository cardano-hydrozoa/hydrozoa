package hydrozoa.multisig.persistence

import hydrozoa.config.head.network.CardanoNetwork

/** The **typed, actor-facing** write batch — sibling of [[Persistence]]: a group of writes
  * committed atomically as one transaction.
  *
  * Build with [[WriteBatch.start]] and the typed builder methods, e.g.:
  * {{{
  *   WriteBatch.start
  *       .put(LaneKey.Block(blockNum))(brief)
  *       .put(LaneKey.SoftAck(ownPeer, softAckNum))(softAck)
  *       .put(StoreKey.BlockResult(blockNum))(blockResult)
  *       .put(StoreKey.DepositMap)(depositMap)
  * }}}
  *
  * Internally the batch records each typed op (key + path-dependent value); at
  * [[Persistence.write]] time each op runs its key's `encodeValue` and lowers to a
  * [[RawWriteBatch]] op before reaching the backend. Actors never see bytes.
  *
  * `put` takes a `using CardanoNetwork.Section`: the builder encodes eagerly so the implicit is
  * needed at builder-call time. Most actor code already has `Section` in scope (from `NodeConfig`
  * plumbing); if not, pass it explicitly.
  *
  * `deleteRange` stays byte-level — see [[RawWriteBatch.deleteRange]] — until the typed shape for
  * ack-pruning is settled (it spans peer-multiplexed keys, so the obvious typed range isn't
  * obvious).
  */
final case class WriteBatch private (private val ops: Vector[WriteBatch.Op]):
    /** Add a put at the given typed key. The value is `key.Value` — path-dependent. */
    def put(key: StoreKey)(value: key.Value)(using CardanoNetwork.Section): WriteBatch =
        copy(ops = ops :+ WriteBatch.Op.Put(key, key.encodeValue(value)))

    /** Add a delete at the given typed key. */
    def delete(key: StoreKey): WriteBatch =
        copy(ops = ops :+ WriteBatch.Op.Delete(key))

    /** True iff this batch contains no operations. */
    def isEmpty: Boolean = ops.isEmpty

    /** Number of operations in this batch. */
    def size: Int = ops.size

    /** Lower the typed batch to its byte-level form for the backend.
      *
      * Each typed op runs the key's encoders here so the backend stays byte-level. Internal —
      * called by `Persistence.write`.
      */
    private[persistence] def toRaw: RawWriteBatch =
        ops.foldLeft(RawWriteBatch.empty) { (acc, op) =>
            op match
                case WriteBatch.Op.Put(key, valueBytes) =>
                    acc.put(key.cf, key.encode, valueBytes)
                case WriteBatch.Op.Delete(key) =>
                    acc.delete(key.cf, key.encode)
        }

object WriteBatch:
    /** Starting point for building a typed batch — an empty batch ready for `put` / `delete`. */
    val start: WriteBatch = WriteBatch(Vector.empty)

    /** One typed op in a write batch. Value bytes for `Put` are produced eagerly at construction
      * time (via the key's `encodeValue`) so the batch can hold a homogeneous op vector instead of
      * carrying path-dependent values through to write time.
      */
    private enum Op:
        case Put(key: StoreKey, valueBytes: Array[Byte])
        case Delete(key: StoreKey)
