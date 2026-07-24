package hydrozoa.multisig.persistence

import cats.effect.IO
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.consensus.UserRequestWithId
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.{StackBrief, StackEffects, StackNumber}
import hydrozoa.multisig.persistence.recovery.CursorScan
import java.nio.ByteBuffer
import java.time.Instant
import scalus.cardano.ledger.TransactionHash

/** Read-only view over the consensus store for the user-facing HTTP API (the
  * [[hydrozoa.multisig.ledger.l2.EutxoL2LedgerReader]] pattern): block briefs from the block spine,
  * plus the confirmation records and reverse indices the block and request queries resolve through.
  * Timestamps are surfaced as [[ArrivalStamp]]s; the wall-clock instant is derived on read via
  * [[wallClockOf]] (no wall clock is persisted). Never writes.
  */
trait ConsensusStoreReader[F[_]]:
    /** Every persisted block brief, in block order (the block spine; block 0 is config, not a spine
      * entry).
      */
    def blockBriefs: F[List[BlockBrief.Next]]

    /** One block's brief, if persisted. */
    def blockBrief(num: BlockNumber): F[Option[BlockBrief.Next]]

    /** This node's soft-confirmation record for a block: the aggregate plus the local confirmation
      * stamp.
      */
    def softConfirmation(num: BlockNumber): F[Option[Timestamped[Block.SoftConfirmed.Next]]]

    /** The stack that hard-confirmed a block, if any. */
    def stackOf(num: BlockNumber): F[Option[StackNumber]]

    /** This node's hard-confirmation record for a stack: the multisigned effects plus the local
      * confirmation stamp.
      */
    def hardConfirmation(num: StackNumber): F[Option[Timestamped[StackEffects.HardConfirmed]]]

    /** One stack's brief (its `[firstBlockNum, lastBlockNum]` range), if persisted. */
    def stackBrief(num: StackNumber): F[Option[StackBrief]]

    /** The stack whose hard-confirmation carries the effect with this l1TxId, if any (the effect →
      * stack reverse index).
      */
    def effectStack(l1TxId: TransactionHash): F[Option[StackNumber]]

    /** One head peer's assigned requests, in request-number order (that author's Request journal),
      * each paired with the arrival stamp it was recorded at (its receive time).
      */
    def requestsOf(peer: HeadPeerNumber): F[List[Timestamped[UserRequestWithId]]]

    /** One assigned request paired with its receive stamp, if persisted. */
    def request(id: RequestId): F[Option[Timestamped[UserRequestWithId]]]

    /** The block that locally processed a request, plus its validity verdict — absent while the
      * request is still unprocessed.
      */
    def requestBlock(id: RequestId): F[Option[RequestBlockEntry]]

    /** The major block that absorbed a deposit into the treasury — absent while the deposit is
      * unabsorbed (or the request is not a deposit).
      */
    def absorptionBlock(id: RequestId): F[Option[BlockNumber]]

    /** The wall-clock instant an arrival stamp was recorded at, via the store's per-generation
      * zero-time anchor. `None` for a stamp whose generation predates the anchor.
      */
    def wallClockOf(stamp: ArrivalStamp): F[Option[Instant]]

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

            def stackBrief(num: StackNumber): IO[Option[StackBrief]] =
                persistence.get(JournalKey.Stack(num)).map(_.map(_.payload))

            def effectStack(l1TxId: TransactionHash): IO[Option[StackNumber]] =
                persistence.get(StoreKey.EffectStackIndex(l1TxId))

            def requestsOf(peer: HeadPeerNumber): IO[List[Timestamped[UserRequestWithId]]] =
                CursorScan.cursorWalk(
                  persistence.backend,
                  Cf.Request(peer),
                  JournalKey.Request(peer, RequestNumber.zero).encode,
                  keyBytes =>
                      JournalKey.Request(peer, RequestNumber(ByteBuffer.wrap(keyBytes).getLong))
                )((key, valueBytes) =>
                    val jv = key.decodeValue(valueBytes)
                    Timestamped(jv.stamp, jv.payload)
                )

            def request(id: RequestId): IO[Option[Timestamped[UserRequestWithId]]] =
                persistence
                    .get(JournalKey.Request(id.peerNum, id.requestNum))
                    .map(_.map(jv => Timestamped(jv.stamp, jv.payload)))

            def requestBlock(id: RequestId): IO[Option[RequestBlockEntry]] =
                persistence.get(StoreKey.RequestBlockIndex(id))

            def absorptionBlock(id: RequestId): IO[Option[BlockNumber]] =
                persistence.get(StoreKey.DepositAbsorptionIndex(id))

            def wallClockOf(stamp: ArrivalStamp): IO[Option[Instant]] =
                persistence.wallClockOf(stamp)

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
            def stackBrief(num: StackNumber): IO[Option[StackBrief]] = IO.pure(None)
            def effectStack(l1TxId: TransactionHash): IO[Option[StackNumber]] = IO.pure(None)
            def requestsOf(peer: HeadPeerNumber): IO[List[Timestamped[UserRequestWithId]]] =
                IO.pure(Nil)
            def request(id: RequestId): IO[Option[Timestamped[UserRequestWithId]]] = IO.pure(None)
            def requestBlock(id: RequestId): IO[Option[RequestBlockEntry]] = IO.pure(None)
            def absorptionBlock(id: RequestId): IO[Option[BlockNumber]] = IO.pure(None)
            def wallClockOf(stamp: ArrivalStamp): IO[Option[Instant]] = IO.pure(None)
