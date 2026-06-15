package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import cats.syntax.traverse.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.{BlockNumber, BlockResult}
import hydrozoa.multisig.persistence.{Cf, FamilyKey, Persistence, StoreKey}
import java.nio.ByteBuffer

/** Range-scan the [[Cf.BlockResult]] CF — JointLedger's per-block deltas — from a block-number
  * floor to the end, reassembling each entry into a full [[BlockResult]] by rehydrating its `brief`
  * from the `Block` family (`FamilyKey.Block[blockNum]`), which already holds it durably (the
  * `BlockResult` value stores only the deltas — [[BlockResult.persisted]]).
  *
  * Used by `StackComposer.recover` to rebuild `pending` from the blocks soft-acked since the last
  * closed stack: the range `(lastClosedBlockNum, softAcked]`. JointLedger writes exactly one
  * `BlockResult` per own soft-ack, and the corresponding `Block` brief is durable before it (the
  * own-led write, or inbound `persistInbound` under CR8 — on head and coil peers alike), so the
  * brief join always resolves; a miss is store corruption (fail-safe throw via `getOrFail`). The
  * highest persisted entry is the `softAcked` block, so a scan to the end of the CF needs no
  * explicit upper bound. `BlockResult` is a non-family CF — entries carry no arrival-stamp prefix,
  * so the raw value bytes decode directly.
  */
object BlockResultScan:

    /** The persisted `BlockResult`s with `blockNum` strictly greater than `fromExclusive`, in
      * ascending block order, each with its `brief` rehydrated from the `Block` family.
      */
    def scanFrom(
        persistence: Persistence[IO],
        fromExclusive: BlockNumber
    )(using CardanoNetwork.Section): IO[List[BlockResult]] =
        val seek = StoreKey.BlockResult(BlockNumber((fromExclusive: Int) + 1)).encode
        val collect: IO[List[(BlockNumber, BlockResult.Persisted)]] =
            persistence.backend.cursor(Cf.BlockResult, seek).use { cursor =>
                def loop(
                    acc: List[(BlockNumber, BlockResult.Persisted)]
                ): IO[List[(BlockNumber, BlockResult.Persisted)]] =
                    cursor.next.flatMap {
                        case None => IO.pure(acc.reverse)
                        case Some((keyBytes, valueBytes)) =>
                            val key =
                                StoreKey.BlockResult(BlockNumber(ByteBuffer.wrap(keyBytes).getInt))
                            loop((key.num, key.decodeValue(valueBytes)) :: acc)
                    }
                loop(Nil)
            }
        collect.flatMap(_.traverse { (blockNum, deltas) =>
            persistence
                .getOrFail(FamilyKey.Block(blockNum))
                .map(brief => BlockResult.fromPersisted(brief.payload, deltas))
        })
