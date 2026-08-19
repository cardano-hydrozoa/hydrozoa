package hydrozoa.multisig.persistence.recovery

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.{Block, BlockNumber}
import hydrozoa.multisig.persistence.{Cf, Persistence, StoreKey}
import java.nio.ByteBuffer

/** Range-scan the [[Cf.SoftConfirmation]] CF — FastConsensusActor's per-block aggregate — from a
  * block-number floor to the end.
  *
  * The mirror of [[BlockResultScan]], and needed for the same reason. `StackComposer` pairs a
  * `BlockResult` with a `Block.SoftConfirmed` per block before that block can enter a stack, and
  * `StackComposer.State.recover` rebuilds the `BlockResult` half by scanning the store. The
  * soft-confirmed half used to be left to the replay tail, but that tail cannot carry it: the
  * block-spine replay floor is `softConfirmed + 1` (see [[ReplayCursors]]), whereas the blocks the
  * next stack needs run from the last CLOSED stack's `lastBlockNum` — a mark that trails
  * `softConfirmed` by a whole stack, and by far more than that once a stack round has stalled. So
  * every block in the range is below the replay floor and is never replayed, the composer can never
  * pair those blocks, and a follower waits in `tryCloseAsFollower`'s "not yet covered" branch
  * forever — for events that already happened and will never re-fire. Scanning the store makes the
  * pairing complete by construction, exactly as it already is for the `BlockResult` half.
  *
  * `SoftConfirmation` values are [[hydrozoa.multisig.persistence.Timestamped]], so the payload is
  * unwrapped here; the codec strips the stamp, unlike the raw-bytes `BlockResult` values.
  */
object SoftConfirmationScan:

    /** The persisted `Block.SoftConfirmed`s with `blockNum` strictly greater than `fromExclusive`,
      * in ascending block order.
      */
    def scanFrom(
        persistence: Persistence[IO],
        fromExclusive: BlockNumber
    )(using CardanoNetwork.Section): IO[List[Block.SoftConfirmed.Next]] =
        val seek = StoreKey.SoftConfirmation(BlockNumber((fromExclusive: Int) + 1)).encode
        CursorScan.cursorWalk(
          persistence.backend,
          Cf.SoftConfirmation,
          seek,
          keyBytes => StoreKey.SoftConfirmation(BlockNumber(ByteBuffer.wrap(keyBytes).getInt))
        )((key, valueBytes) => key.decodeValue(valueBytes).payload)
