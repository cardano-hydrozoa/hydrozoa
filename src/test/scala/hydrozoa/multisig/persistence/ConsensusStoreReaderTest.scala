package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.ledger.block.{Block, BlockBody, BlockBrief, BlockHeader, BlockNumber, BlockVersion}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** `softConfirmedAt` is the only soft-confirmation accessor the reader offers, so every caller —
  * present and future — gets block zero's derived moment rather than the `None` a stored read would
  * return for a block that never runs the fast cycle.
  */
class ConsensusStoreReaderTest extends AnyFunSuite:

    private val headConfig: HeadConfig =
        MultiNodeConfig.generateDefault.pureApply(Gen.Parameters.default, Seed(0L)).headConfig
    private given CardanoNetwork.Section = headConfig

    private val blockZeroEndTime: BlockCreationEndTime =
        headConfig.initialBlock.blockBrief.endTime

    private val stamp: ArrivalStamp = ArrivalStamp(generation = 0, monotonicNanos = 1L)

    test("block zero reports its creation end time, with nothing in the store") {
        val softAt = withReader((_, reader) => reader.softConfirmedAt(BlockNumber.zero))
        assert(softAt == Some(blockZeroEndTime.convert.instant))
    }

    test("a woven block with no stored record reports nothing") {
        val softAt = withReader((_, reader) => reader.softConfirmedAt(BlockNumber(1)))
        assert(softAt.isEmpty)
    }

    test("a woven block with a stored record reports that record’s wall clock") {
        withReader { (persistence, reader) =>
            for {
                now <- realTimeQuantizedInstant(headConfig.slotConfig)
                _ <- persistence.put(StoreKey.SoftConfirmation(BlockNumber(1)))(
                  Timestamped(stamp, softConfirmedMinor(1, now))
                )
                expected <- persistence.wallClockOf(stamp)
                softAt <- reader.softConfirmedAt(BlockNumber(1))
            } yield assert(softAt == Some(expected))
        }
    }

    /** Run `body` against a fresh in-memory store and the reader over it. */
    private def withReader[A](body: (Persistence[IO], ConsensusStoreReader[IO]) => IO[A]): A =
        val tracer = Slf4jTracer.sink.contramap(PersistenceEventFormat.humanFormat)
        InMemoryBackendStore
            .open(tracer)
            .use(backend =>
                for {
                    persistence <- Persistence.fromBackend(backend, tracer)
                    result <- body(
                      persistence,
                      ConsensusStoreReader.fromPersistence(persistence, blockZeroEndTime)
                    )
                } yield result
            )
            .unsafeRunSync()

    /** A minimal soft-confirmed Minor at `blockNum` — the payload's content is immaterial here,
      * only that a record exists to stamp.
      */
    private def softConfirmedMinor(blockNum: Int, now: QuantizedInstant): Block.SoftConfirmed.Next =
        val end = BlockCreationEndTime(now + 1.second)
        val fallback = headConfig.txTiming.newFallbackStartTime(end)
        Block.SoftConfirmed.Minor(
          BlockBrief.Minor(
            BlockHeader.Minor(
              blockNum = BlockNumber(blockNum),
              blockVersion = BlockVersion.Full(0, 0),
              startTime = BlockCreationStartTime(now),
              endTime = end,
              fallbackTxStartTime = fallback,
              forcedMajorBlockWakeupTime = headConfig.txTiming.forcedMajorBlockWakeupTime(fallback),
              mDepositDecisionWakeupTime = None
            ),
            BlockBody.Minor(requests = List.empty, depositsRejected = List.empty)
          ),
          headerMultiSigned = Nil,
          finalizationRequested = false
        )
