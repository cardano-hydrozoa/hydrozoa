package hydrozoa.lib.tracing

import cats.effect.{IO, Ref}
import java.util.concurrent.atomic.AtomicLong
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Protocol trace emitter for the Hydrozoa conformance checker.
  *
  * Each method corresponds to one event type in the trace schema. Events are emitted as JSONL lines
  * prefixed with `HTRACE|` to a dedicated SLF4J logger (`hydrozoa.trace`).
  *
  * Two implementations:
  *   - `ProtocolTracer.noop`: zero-cost no-op (default when tracing is disabled)
  *   - `ProtocolTracer.jsonLines(nodeId)`: writes JSONL to the `hydrozoa.trace` logger
  */
trait ProtocolTracer {
    def leaderStarted(blockNum: Long, peer: Int): IO[Unit]
    def briefProduced(
        blockNum: Long,
        peer: Int,
        blockType: String,
        vMajor: Int,
        vMinor: Int,
        eventCount: Int
    ): IO[Unit]
    def ack(blockNum: Long, peer: Int, ackType: String): IO[Unit]
    def roundComplete(blockNum: Long, blockType: String, round: Int): IO[Unit]
    def blockConfirmed(
        blockNum: Long,
        blockType: String,
        vMajor: Int,
        vMinor: Int
    ): IO[Unit]
    def eventProcessed(eventId: String, blockNum: Long, valid: Boolean): IO[Unit]
    def depositAbsorbed(depositId: String, blockNum: Long, amount: Long): IO[Unit]
    def balanceSnapshot(blockNum: Long, l2Total: Long, l1Treasury: Long): IO[Unit]
    def settlement(blockNum: Long, vMajor: Int): IO[Unit]
    def traceError(blockNum: Long, errorType: String, msg: String): IO[Unit]
}

object ProtocolTracer {

    /** No-op implementation — zero overhead when tracing is disabled. */
    val noop: ProtocolTracer = new ProtocolTracer {
        def leaderStarted(blockNum: Long, peer: Int): IO[Unit] = IO.unit
        def briefProduced(
            blockNum: Long,
            peer: Int,
            blockType: String,
            vMajor: Int,
            vMinor: Int,
            eventCount: Int
        ): IO[Unit] = IO.unit
        def ack(blockNum: Long, peer: Int, ackType: String): IO[Unit] = IO.unit
        def roundComplete(blockNum: Long, blockType: String, round: Int): IO[Unit] = IO.unit
        def blockConfirmed(
            blockNum: Long,
            blockType: String,
            vMajor: Int,
            vMinor: Int
        ): IO[Unit] = IO.unit
        def eventProcessed(eventId: String, blockNum: Long, valid: Boolean): IO[Unit] = IO.unit
        def depositAbsorbed(depositId: String, blockNum: Long, amount: Long): IO[Unit] = IO.unit
        def balanceSnapshot(blockNum: Long, l2Total: Long, l1Treasury: Long): IO[Unit] = IO.unit
        def settlement(blockNum: Long, vMajor: Int): IO[Unit] = IO.unit
        def traceError(blockNum: Long, errorType: String, msg: String): IO[Unit] = IO.unit
    }

    /** JSONL implementation that writes trace events to the `hydrozoa.trace` SLF4J logger.
      *
      * Each event is a single JSON line prefixed with `HTRACE|`. The Logback appender should use
      * `%msg%n` pattern (no Logback formatting) and `additivity="false"` to avoid stdout noise.
      *
      * @param nodeId
      *   node identity string (e.g. "head:0", "head:1")
      */
    def jsonLines(nodeId: String): IO[ProtocolTracer] = IO {
        val logger: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("hydrozoa.trace")
        val seqCounter = new AtomicLong(0L)

        new ProtocolTracer {
            private def emit(event: TraceEvent): IO[Unit] =
                logger.info(event.toJson)

            private def nextSeq(): Long = seqCounter.getAndIncrement()

            private def now(): Long = System.currentTimeMillis()

            def leaderStarted(blockNum: Long, peer: Int): IO[Unit] =
                emit(TraceEvent.LeaderStarted(nextSeq(), now(), nodeId, blockNum, peer))

            def briefProduced(
                blockNum: Long,
                peer: Int,
                blockType: String,
                vMajor: Int,
                vMinor: Int,
                eventCount: Int
            ): IO[Unit] =
                emit(
                  TraceEvent.BriefProduced(
                    nextSeq(),
                    now(),
                    nodeId,
                    blockNum,
                    peer,
                    blockType,
                    vMajor,
                    vMinor,
                    eventCount
                  )
                )

            def ack(blockNum: Long, peer: Int, ackType: String): IO[Unit] =
                emit(TraceEvent.Ack(nextSeq(), now(), nodeId, blockNum, peer, ackType))

            def roundComplete(blockNum: Long, blockType: String, round: Int): IO[Unit] =
                emit(TraceEvent.RoundComplete(nextSeq(), now(), nodeId, blockNum, blockType, round))

            def blockConfirmed(
                blockNum: Long,
                blockType: String,
                vMajor: Int,
                vMinor: Int
            ): IO[Unit] =
                emit(
                  TraceEvent.BlockConfirmed(
                    nextSeq(),
                    now(),
                    nodeId,
                    blockNum,
                    blockType,
                    vMajor,
                    vMinor
                  )
                )

            def eventProcessed(eventId: String, blockNum: Long, valid: Boolean): IO[Unit] =
                emit(TraceEvent.EventProcessed(nextSeq(), now(), nodeId, eventId, blockNum, valid))

            def depositAbsorbed(depositId: String, blockNum: Long, amount: Long): IO[Unit] =
                emit(
                  TraceEvent.DepositAbsorbed(nextSeq(), now(), nodeId, depositId, blockNum, amount)
                )

            def balanceSnapshot(blockNum: Long, l2Total: Long, l1Treasury: Long): IO[Unit] =
                emit(
                  TraceEvent.BalanceSnapshot(
                    nextSeq(),
                    now(),
                    nodeId,
                    blockNum,
                    l2Total,
                    l1Treasury
                  )
                )

            def settlement(blockNum: Long, vMajor: Int): IO[Unit] =
                emit(TraceEvent.Settlement(nextSeq(), now(), nodeId, blockNum, vMajor))

            def traceError(blockNum: Long, errorType: String, msg: String): IO[Unit] =
                emit(TraceEvent.TraceError(nextSeq(), now(), nodeId, blockNum, errorType, msg))
        }
    }

    /** Collecting implementation for tests — stores JSON lines in a Ref. */
    def collecting(nodeId: String): IO[(ProtocolTracer, Ref[IO, List[String]])] =
        for {
            ref <- Ref.of[IO, List[String]](List.empty)
        } yield {
            val seqCounter = new AtomicLong(0L)
            val tracer = new ProtocolTracer {
                private def emit(event: TraceEvent): IO[Unit] =
                    ref.update(lines => lines :+ event.toJson)

                private def nextSeq(): Long = seqCounter.getAndIncrement()
                private def now(): Long = System.currentTimeMillis()

                def leaderStarted(blockNum: Long, peer: Int): IO[Unit] =
                    emit(TraceEvent.LeaderStarted(nextSeq(), now(), nodeId, blockNum, peer))
                def briefProduced(
                    blockNum: Long,
                    peer: Int,
                    blockType: String,
                    vMajor: Int,
                    vMinor: Int,
                    eventCount: Int
                ): IO[Unit] =
                    emit(
                      TraceEvent.BriefProduced(
                        nextSeq(),
                        now(),
                        nodeId,
                        blockNum,
                        peer,
                        blockType,
                        vMajor,
                        vMinor,
                        eventCount
                      )
                    )
                def ack(blockNum: Long, peer: Int, ackType: String): IO[Unit] =
                    emit(TraceEvent.Ack(nextSeq(), now(), nodeId, blockNum, peer, ackType))
                def roundComplete(blockNum: Long, blockType: String, round: Int): IO[Unit] =
                    emit(
                      TraceEvent.RoundComplete(nextSeq(), now(), nodeId, blockNum, blockType, round)
                    )
                def blockConfirmed(
                    blockNum: Long,
                    blockType: String,
                    vMajor: Int,
                    vMinor: Int
                ): IO[Unit] =
                    emit(
                      TraceEvent.BlockConfirmed(
                        nextSeq(),
                        now(),
                        nodeId,
                        blockNum,
                        blockType,
                        vMajor,
                        vMinor
                      )
                    )
                def eventProcessed(eventId: String, blockNum: Long, valid: Boolean): IO[Unit] =
                    emit(
                      TraceEvent.EventProcessed(nextSeq(), now(), nodeId, eventId, blockNum, valid)
                    )
                def depositAbsorbed(depositId: String, blockNum: Long, amount: Long): IO[Unit] =
                    emit(
                      TraceEvent.DepositAbsorbed(
                        nextSeq(),
                        now(),
                        nodeId,
                        depositId,
                        blockNum,
                        amount
                      )
                    )
                def balanceSnapshot(blockNum: Long, l2Total: Long, l1Treasury: Long): IO[Unit] =
                    emit(
                      TraceEvent.BalanceSnapshot(
                        nextSeq(),
                        now(),
                        nodeId,
                        blockNum,
                        l2Total,
                        l1Treasury
                      )
                    )
                def settlement(blockNum: Long, vMajor: Int): IO[Unit] =
                    emit(TraceEvent.Settlement(nextSeq(), now(), nodeId, blockNum, vMajor))
                def traceError(blockNum: Long, errorType: String, msg: String): IO[Unit] =
                    emit(TraceEvent.TraceError(nextSeq(), now(), nodeId, blockNum, errorType, msg))
            }
            (tracer, ref)
        }
}
