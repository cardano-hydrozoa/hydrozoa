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
  * both halves must come from the store: the replay tail cannot supply the soft-confirmed one,
  * because the block-spine replay floor is `softConfirmed + 1` (see [[ReplayCursors]]) while the
  * blocks the next stack needs run from the last CLOSED stack's `lastBlockNum` — a mark trailing
  * `softConfirmed` by at least a stack, and by far more once a stack round has stalled. Every block
  * in that range therefore sits below the floor and is never replayed. Scanning makes the pairing
  * complete by construction, as it already is for the `BlockResult` half.
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
