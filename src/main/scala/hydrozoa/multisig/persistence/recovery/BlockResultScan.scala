package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockResult}
import hydrozoa.multisig.persistence.{BackendStore, Cf, StoreKey}
import java.nio.ByteBuffer

/** Range-scan the [[Cf.BlockResult]] CF — the non-lane spine of JointLedger's per-block outputs —
  * from a block-number floor to the end, decoding each entry to its typed [[BlockResult]].
  *
  * Used by `StackComposer.recover` to rebuild `pending` from the blocks soft-acked since the last
  * closed stack: the range `(lastClosedBlockNum, softAcked]`. The highest persisted entry is the
  * `softAcked` block, since JointLedger writes exactly one `BlockResult` per own soft-ack, so a
  * scan to the end of the CF needs no explicit upper bound. `BlockResult` is a non-lane CF —
  * entries carry no arrival-stamp prefix, so the raw value bytes decode directly. See
  * `design/recovery-implementation-plan.md` R2-fast.
  */
object BlockResultScan:

    /** The persisted `BlockResult`s with `blockNum` strictly greater than `fromExclusive`, in
      * ascending block order.
      */
    def scanFrom(
        backend: BackendStore[IO],
        fromExclusive: BlockNumber
    )(using CardanoNetwork.Section): IO[List[BlockResult]] =
        val seek = StoreKey.BlockResult(BlockNumber((fromExclusive: Int) + 1)).encode
        backend.cursor(Cf.BlockResult, seek).use { cursor =>
            def loop(acc: List[BlockResult]): IO[List[BlockResult]] =
                cursor.next.flatMap {
                    case None => IO.pure(acc.reverse)
                    case Some((keyBytes, valueBytes)) =>
                        val key =
                            StoreKey.BlockResult(BlockNumber(ByteBuffer.wrap(keyBytes).getInt))
                        loop(key.decodeValue(valueBytes) :: acc)
                }
            loop(Nil)
        }
