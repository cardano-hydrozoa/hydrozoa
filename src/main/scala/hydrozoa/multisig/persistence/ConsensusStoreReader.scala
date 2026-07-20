package hydrozoa.multisig.persistence

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.stack.{StackEffects, StackNumber}
import hydrozoa.multisig.persistence.recovery.CursorScan
import java.nio.ByteBuffer

/** Read-only view over the consensus store for the user-facing HTTP API (the
  * [[hydrozoa.multisig.ledger.l2.EutxoL2LedgerReader]] pattern): block briefs from the block spine,
  * plus the confirmation records and reverse indices the block and request queries resolve through.
  * Never writes.
  */
trait ConsensusStoreReader[F[_]]:
    /** Every persisted block brief, in block order (the block spine; block 0 is config, not a spine
      * entry).
      */
    def blockBriefs: F[List[BlockBrief.Next]]

    /** One block's brief, if persisted. */
    def blockBrief(num: BlockNumber): F[Option[BlockBrief.Next]]

    /** This node's soft-confirmation record for a block: the aggregate plus the local confirmation
      * moment.
      */
    def softConfirmation(num: BlockNumber): F[Option[Timestamped[Block.SoftConfirmed.Next]]]

    /** The stack that hard-confirmed a block, if any. */
    def stackOf(num: BlockNumber): F[Option[StackNumber]]

    /** This node's hard-confirmation record for a stack: the multisigned effects plus the local
      * confirmation moment.
      */
    def hardConfirmation(num: StackNumber): F[Option[Timestamped[StackEffects.HardConfirmed]]]

object ConsensusStoreReader:

    /** The reader over a live [[Persistence]] instance. */
    def fromPersistence(
        persistence: Persistence[IO]
    )(using CardanoNetwork.Section): ConsensusStoreReader[IO] =
        new ConsensusStoreReader[IO]:
            def blockBriefs: IO[List[BlockBrief.Next]] =
                CursorScan.cursorWalk(
                  persistence.backend,
                  Cf.Block,
                  JournalKey.Block(BlockNumber(0)).encode,
                  keyBytes => JournalKey.Block(BlockNumber(ByteBuffer.wrap(keyBytes).getInt))
                )((key, valueBytes) => key.decodeValue(valueBytes).payload)

            def blockBrief(num: BlockNumber): IO[Option[BlockBrief.Next]] =
                persistence.get(JournalKey.Block(num)).map(_.map(_.payload))

            def softConfirmation(
                num: BlockNumber
            ): IO[Option[Timestamped[Block.SoftConfirmed.Next]]] =
                persistence.get(StoreKey.SoftConfirmation(num))

            def stackOf(num: BlockNumber): IO[Option[StackNumber]] =
                persistence.get(StoreKey.BlockStackIndex(num))

            def hardConfirmation(
                num: StackNumber
            ): IO[Option[Timestamped[StackEffects.HardConfirmed]]] =
                persistence.get(StoreKey.HardConfirmation(num))

    /** A reader over no data — every lookup is empty. For wiring the routes on a node whose store
      * is absent or irrelevant (test and harness setups).
      */
    def empty: ConsensusStoreReader[IO] =
        new ConsensusStoreReader[IO]:
            def blockBriefs: IO[List[BlockBrief.Next]] = IO.pure(Nil)
            def blockBrief(num: BlockNumber): IO[Option[BlockBrief.Next]] = IO.pure(None)
            def softConfirmation(
                num: BlockNumber
            ): IO[Option[Timestamped[Block.SoftConfirmed.Next]]] = IO.pure(None)
            def stackOf(num: BlockNumber): IO[Option[StackNumber]] = IO.pure(None)
            def hardConfirmation(
                num: StackNumber
            ): IO[Option[Timestamped[StackEffects.HardConfirmed]]] = IO.pure(None)
