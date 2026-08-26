package hydrozoa.multisig.metrics

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.{AtomicLong, AtomicReference, LongAdder}
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.*

/** Why a request was refused, split so the stats endpoints can answer "is backpressure firing?"
  * without log-diving.
  */
enum RejectionKind:
    case Screening, Backpressure

/** Process-lifetime, in-memory peer metrics behind `GET /head/stats` and `GET /head/metrics` (see
  * `docs/spec/peer-stats-endpoint.md`).
  *
  * The hot-path `on*` methods are lock-free side effects (a single atomic add). Each counter except
  * the peer-request map is written by exactly one actor — cats actors drain their mailbox serially
  * — so a plain [[java.util.concurrent.atomic.AtomicLong AtomicLong]] publishes safely to the HTTP
  * reader without [[java.util.concurrent.atomic.LongAdder LongAdder]]'s striping. Peer-request
  * ingestion fans in from multiple liaison actors, so those counters use `LongAdder`.
  *
  * All rates are top-style EWMA load averages (now / 1m / 5m / 15m), maintained by the single
  * [[sampler]] fiber and published through one
  * [[java.util.concurrent.atomic.AtomicReference AtomicReference]]. There are no fixed-window
  * rings.
  *
  * Counters are not persisted: they reset on restart.
  */
final class PeerMetrics private (startedAtMillis: Long, remotePeerNums: Vector[Int]):
    import PeerMetrics.*

    // ---- local requests (written by RequestSequencer) ----
    private val localAccepted = new AtomicLong(0)
    private val localRejScreening = new AtomicLong(0)
    private val localRejBackpressure = new AtomicLong(0)

    // ---- peer requests, per remote head peer (written by the peer liaisons, multi-writer) ----
    private val peerRequests: Map[Int, LongAdder] =
        remotePeerNums.map(_ -> new LongAdder).toMap

    // ---- blocks (written by FastConsensusActor) ----
    private val blocksMinor = new AtomicLong(0)
    private val blocksMajor = new AtomicLong(0)
    private val blockEventsSum = new AtomicLong(0)
    private val blockEventsMax = new AtomicLong(0)

    // ---- stacks (written by SlowConsensusActor) ----
    private val stacksTotal = new AtomicLong(0)
    private val lastStackNum = new AtomicLong(-1)
    private val lastStackMillis = new AtomicLong(0)
    private val stackGapSumMillis = new AtomicLong(0)
    private val stackBlocksSum = new AtomicLong(0)
    private val stackBlocksMax = new AtomicLong(0)

    // ---- block-lifecycle timings ----
    // Start clocks: blockNum -> startMillis. `leadStart`/`replayStart` (opened in BlockWeaver) close
    // when the JointLedger produces the brief; `cellStart` opens at that same brief-produced moment
    // and closes at soft-confirmation, so soft-consensus = brief produced -> soft-confirmed. Written
    // by one actor each, removed by one actor each — TrieMap keeps the cross-actor hand-off safe.
    private val leadStart = TrieMap.empty[Long, Long]
    private val replayStart = TrieMap.empty[Long, Long]
    private val cellStart = TrieMap.empty[Long, Long]
    private val leadTiming = new TimingAccumulator(perRequest = true)
    private val replayTiming = new TimingAccumulator(perRequest = true)
    private val softConsensusTiming = new TimingAccumulator(perRequest = false)

    // ---- gauges (last value) ----
    private val mempool = new AtomicLong(0)
    private val leaderMempoolDrain = new AtomicLong(0)
    private val seqHeadroom = new AtomicLong(0)
    private val equityLovelace = new AtomicLong(0)

    // ---- stack-composer phase (see StackComposerPhase) ----
    private val composerPhase = new AtomicReference[StackComposerPhase](StackComposerPhase.Deriving)
    private val composerPhaseSince = new AtomicLong(startedAtMillis)
    private val partitionsDone = new AtomicLong(0)
    private val partitionsTotal = new AtomicLong(0)

    // ---- derived rates (written only by the sampler fiber) ----
    private val rolling = new AtomicReference[Rolling](Rolling.empty(remotePeerNums))

    private def now(): Long = System.currentTimeMillis()

    /** Record a locally-sequenced request. */
    def onLocalAccepted(): Unit = { val _ = localAccepted.incrementAndGet() }

    /** Record a locally-rejected request, tagged by reason. */
    def onLocalRejected(kind: RejectionKind): Unit =
        val _ = kind match
            case RejectionKind.Screening    => localRejScreening.incrementAndGet()
            case RejectionKind.Backpressure => localRejBackpressure.incrementAndGet()

    /** Record `n` requests ingested from remote head peer `fromPeerNum`. */
    def onPeerRequests(fromPeerNum: Int, n: Int): Unit =
        peerRequests.get(fromPeerNum).foreach(_.add(n.toLong))

    /** `blockNum` soft-confirmed as the given type carrying `events` requests: bump the block
      * counters and close its soft-consensus clock (opened at [[onBlockProduced]]).
      */
    def onBlockConfirmed(blockNum: Long, isMajor: Boolean, events: Int): Unit =
        val _ = if isMajor then blocksMajor.incrementAndGet() else blocksMinor.incrementAndGet()
        blockEventsSum.addAndGet(events.toLong)
        updateMax(blockEventsMax, events.toLong)
        cellStart
            .remove(blockNum)
            .foreach(start => softConsensusTiming.record(blockNum, now() - start, 0L))

    /** Record a hard-confirmed stack absorbing `blocksAbsorbed` blocks, at wall-clock `nowMillis`.
      */
    def onStackConfirmed(stackNum: Int, blocksAbsorbed: Int, nowMillis: Long): Unit =
        val prev = lastStackMillis.getAndSet(nowMillis)
        if prev > 0 then { val _ = stackGapSumMillis.addAndGet(nowMillis - prev) }
        stacksTotal.incrementAndGet()
        lastStackNum.set(stackNum.toLong)
        stackBlocksSum.addAndGet(blocksAbsorbed.toLong)
        updateMax(stackBlocksMax, blocksAbsorbed.toLong)

    /** BlockWeaver became leader of `blockNum`: start its "lead" clock. */
    def onLeadStart(blockNum: Long): Unit = startClock(leadStart, blockNum)

    /** BlockWeaver received a brief to reproduce `blockNum` (follower): start its "replay" clock.
      */
    def onReplayStart(blockNum: Long): Unit = startClock(replayStart, blockNum)

    /** The JointLedger produced `blockNum`'s brief (this peer's own when leading, or a reproduction
      * when following): close whichever clock the local peer opened — `leadStart` if it led the
      * block, else `replayStart` — recording the elapsed time and the block's request count, and
      * open the soft-consensus clock (closed at [[onBlockConfirmed]]).
      */
    def onBlockProduced(blockNum: Long, requests: Int): Unit =
        val end = now()
        leadStart.remove(blockNum) match
            case Some(start) => leadTiming.record(blockNum, end - start, requests.toLong)
            case None =>
                replayStart
                    .remove(blockNum)
                    .foreach(start => replayTiming.record(blockNum, end - start, requests.toLong))
        startClock(cellStart, blockNum)

    /** BlockWeaver's current mempool depth (requests received, not yet packed into a block). */
    def onMempoolSize(size: Int): Unit = mempool.set(size.toLong)

    /** How many requests the leader just extracted from its mempool into the block it is starting.
      */
    def onLeaderMempoolDrain(n: Int): Unit = leaderMempoolDrain.set(n.toLong)

    /** How many more requests the RequestSequencer will admit right now before backpressure trips —
      * the free space in its window. (The window itself, `backpressureCoefficient *
      * maxRequestsPerBlock`, is derivable from config, so only the live headroom is reported.)
      */
    def onSequencerHeadroom(headroom: Long): Unit =
        seqHeadroom.set(math.max(0L, headroom))

    /** The equity the head holds beyond its L2 liabilities, in lovelace — the treasury utxo's own
      * `equity` field, one side of the double-entry identity `treasury.value == evacuation map
      * total + equity + beacon`. Reported by [[hydrozoa.multisig.consensus.StackComposer]] from the
      * initialization treasury at boot and from the rotated treasury on every stack close, so it is
      * the equity as of the last stack this peer closed.
      */
    def onEquity(lovelace: Long): Unit = equityLovelace.set(lovelace)

    /** The [[StackComposer]] entered a new phase. Re-entering the same phase does NOT restart the
      * clock, so `secondsInPhase` measures how long the composer has been stuck in it — the whole
      * point of the gauge, and `tryProgress` re-evaluates on every inbound event.
      */
    def onComposerPhase(phase: StackComposerPhase, nowMillis: Long): Unit =
        if composerPhase.getAndSet(phase) != phase then composerPhaseSince.set(nowMillis)

    /** Effect-derivation progress. `total` is set once when a stack starts deriving; `done`
      * advances per partition — a plain write, negligible beside the KZG commitment and tx building
      * each partition costs, so it is not sampled.
      */
    def onDerivationStarted(totalPartitions: Int): Unit =
        partitionsTotal.set(totalPartitions.toLong)
        partitionsDone.set(0L)

    def onPartitionDerived(done: Int): Unit = partitionsDone.set(done.toLong)

    private def startClock(m: TrieMap[Long, Long], blockNum: Long): Unit =
        m.update(blockNum, now())
        // Defensive: a block that never closes (crash) would leak a start entry; cap the in-flight
        // set so it cannot grow without bound.
        if m.size > PeerMetrics.InFlightCap then m.clear()

    /** A consistent, cheap read for the endpoints. `nowMillis` is the caller's wall-clock. */
    def snapshot(nowMillis: Long): PeerStats =
        val roll = rolling.get()
        val minor = blocksMinor.get()
        val major = blocksMajor.get()
        val blocksTotal = minor + major
        val eventsSum = blockEventsSum.get()
        val stacks = stacksTotal.get()
        PeerStats(
          uptimeSeconds = math.max(0L, (nowMillis - startedAtMillis) / 1000),
          localAccepted = localAccepted.get(),
          localRate = roll.localRate,
          localRejScreening = localRejScreening.get(),
          localRejBackpressure = localRejBackpressure.get(),
          peerRequests = remotePeerNums.map { p =>
              p -> CounterWithRate(
                peerRequests(p).sum(),
                roll.peerRates.getOrElse(p, RateView.zero)
              )
          }.toMap,
          blocks = BlockStats(
            minor = minor,
            major = major,
            avgEvents = if blocksTotal > 0 then eventsSum.toDouble / blocksTotal else 0.0,
            maxEvents = blockEventsMax.get(),
            blockRate = roll.blockRate,
            requestRate = roll.blockRequestRate
          ),
          stacks = StackStats(
            total = stacks,
            lastStackNumber = lastStackNum.get(),
            secondsSinceLastHardConfirm =
                if lastStackMillis.get() > 0 then (nowMillis - lastStackMillis.get()) / 1000
                else 0L,
            meanInterStackGapSeconds =
                if stacks > 1 then stackGapSumMillis.get().toDouble / (stacks - 1) / 1000.0
                else 0.0,
            avgBlocksAbsorbed = if stacks > 0 then stackBlocksSum.get().toDouble / stacks else 0.0,
            maxBlocksAbsorbed = stackBlocksMax.get()
          ),
          blockTimings = BlockTimingSet(
            lead = leadTiming.snapshot,
            replay = replayTiming.snapshot,
            softConsensus = softConsensusTiming.snapshot
          ),
          mempoolSize = mempool.get(),
          leaderMempoolDrain = leaderMempoolDrain.get(),
          sequencerHeadroom = seqHeadroom.get(),
          equityLovelace = equityLovelace.get(),
          runtime = PeerMetrics.runtimeSnapshot(),
          composer = ComposerStats(
            phase = composerPhase.get(),
            secondsInPhase = math.max(0L, (nowMillis - composerPhaseSince.get()) / 1000),
            partitionsDone = partitionsDone.get(),
            partitionsTotal = partitionsTotal.get()
          )
        )

    /** The 1 Hz sampler: feeds every rate's EWMAs from the cumulative counters, then publishes one
      * immutable [[Rolling]]. Run as a background fiber for the node's lifetime. The EWMA state is
      * fiber-local (single writer), so it needs no synchronization.
      */
    def sampler(period: FiniteDuration = 1.second): IO[Unit] =
        val ewmaLocal = mkEwmas()
        val ewmaPeer = remotePeerNums.map(_ -> mkEwmas()).toMap
        val ewmaBlocks = mkEwmas()
        val ewmaBlockReqs = mkEwmas()

        def loop(
            lastMillis: Long,
            lastLocal: Long,
            lastPeer: Map[Int, Long],
            lastBlocks: Long,
            lastBlockReqs: Long
        ): IO[Unit] =
            IO.sleep(period) >> IO.realTime.flatMap { now =>
                val nowMillis = now.toMillis
                val dt = math.max(1e-3, (nowMillis - lastMillis) / 1000.0)

                val curLocal = localAccepted.get()
                val localInst = (curLocal - lastLocal) / dt
                observe(ewmaLocal, localInst, dt)

                val curPeer = remotePeerNums.map(p => p -> peerRequests(p).sum()).toMap
                val peerInst = remotePeerNums.map { p =>
                    p -> (curPeer(p) - lastPeer.getOrElse(p, 0L)) / dt
                }.toMap
                remotePeerNums.foreach(p => observe(ewmaPeer(p), peerInst(p), dt))

                val curBlocks = blocksMinor.get() + blocksMajor.get()
                val blockInst = (curBlocks - lastBlocks) / dt
                observe(ewmaBlocks, blockInst, dt)

                val curBlockReqs = blockEventsSum.get()
                val blockReqInst = (curBlockReqs - lastBlockReqs) / dt
                observe(ewmaBlockReqs, blockReqInst, dt)

                rolling.set(
                  Rolling(
                    localRate = rateOf(ewmaLocal, localInst),
                    peerRates =
                        remotePeerNums.map(p => p -> rateOf(ewmaPeer(p), peerInst(p))).toMap,
                    blockRate = rateOf(ewmaBlocks, blockInst),
                    blockRequestRate = rateOf(ewmaBlockReqs, blockReqInst)
                  )
                )
                loop(nowMillis, curLocal, curPeer, curBlocks, curBlockReqs)
            }

        IO.realTime.flatMap(t0 =>
            loop(t0.toMillis, localAccepted.get(), remotePeerNums.map(_ -> 0L).toMap, 0L, 0L)
        )

object PeerMetrics:
    private val Taus: Vector[Double] = Vector(60.0, 300.0, 900.0) // 1m / 5m / 15m

    /** Read the whole-process resource gauges. Cheap enough for the 1 Hz sampler: counter reads,
      * one `MemoryMXBean` poll, and one attribute read for the fd count.
      *
      * Deliberately tolerant. `workStealingThreadPool` is `None` on a non-WSTP compute pool (the
      * test harnesses), the fd count is an `OperatingSystemMXBean` attribute that exists on Linux
      * and not everywhere, and none of this is worth failing a stats request over — so anything
      * unavailable reports as -1 rather than throwing.
      */
    def runtimeSnapshot(): RuntimeStats =
        val heap = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage
        val wstp = IORuntime.global.metrics.workStealingThreadPool
        RuntimeStats(
          fibersSuspended = wstp.map(_.suspendedFiberCount()).getOrElse(-1L),
          fibersQueuedLocal = wstp.map(_.localQueueFiberCount()).getOrElse(-1L),
          workerThreads = wstp.map(_.workerThreadCount()).getOrElse(-1),
          workersActive = wstp.map(_.activeThreadCount()).getOrElse(-1),
          workersSearching = wstp.map(_.searchingThreadCount()).getOrElse(-1),
          workersBlocked = wstp.map(_.blockedWorkerThreadCount()).getOrElse(-1),
          timersOutstanding = wstp
              .map(_.workerThreads.map(_.timerHeap.timersOutstandingCount().toLong).sum)
              .getOrElse(-1L),
          timersExecuted = wstp
              .map(_.workerThreads.map(_.timerHeap.totalTimersExecutedCount()).sum)
              .getOrElse(-1L),
          liveThreads = ManagementFactory.getThreadMXBean.getThreadCount,
          heapUsedBytes = heap.getUsed,
          heapCommittedBytes = heap.getCommitted,
          openFileDescriptors = openFdCount()
        )

    /** Open file descriptors, or -1 where the platform does not expose them. Socket exhaustion
      * presents as a box that has stopped responding, with nothing in the process reporting why.
      */
    private def openFdCount(): Long =
        try
            ManagementFactory.getPlatformMBeanServer
                .getAttribute(
                  new javax.management.ObjectName("java.lang:type=OperatingSystem"),
                  "OpenFileDescriptorCount"
                )
                .asInstanceOf[java.lang.Long]
                .longValue()
        catch case _: Throwable => -1L

    /** Upper bound on in-flight (unclosed) block clocks before we assume a leak and reset. */
    private val InFlightCap = 8192

    /** How many slowest blocks each timing category keeps (with their block numbers). */
    private val TopN = 10

    /** Build a registry that tracks inbound requests from `remotePeerNums` — the head peers other
      * than this one. Exclude the own peer number: a peer never sends requests to itself over the
      * network, so its `peerRequests` entry would be a constant zero.
      */
    def create(nowMillis: Long, remotePeerNums: Vector[Int]): PeerMetrics =
        new PeerMetrics(nowMillis, remotePeerNums)

    /** One timing category (lead / replay / soft-consensus). Written by exactly one actor and read
      * by the HTTP snapshot, so the running sum/count use plain atomics and the top-N list is
      * published through a single [[AtomicReference]] (single writer → the get-then-set is
      * race-free against itself; the reader only ever sees a fully-built list).
      */
    private final class TimingAccumulator(perRequest: Boolean):
        private val count = new AtomicLong(0)
        private val sumMillis = new AtomicLong(0)
        private val sumRequests = new AtomicLong(0)
        private val top = new AtomicReference[Vector[BlockTiming]](Vector.empty)

        def record(blockNumber: Long, millis: Long, requests: Long): Unit =
            count.incrementAndGet()
            sumMillis.addAndGet(millis)
            sumRequests.addAndGet(requests)
            top.set(
              (top.get() :+ BlockTiming(blockNumber, millis, requests)).sortBy(-_.millis).take(TopN)
            )

        def snapshot: TimingStats =
            val c = count.get()
            val reqs = sumRequests.get()
            TimingStats(
              count = c,
              avgMillis = if c > 0 then sumMillis.get().toDouble / c else 0.0,
              avgMillisPerRequest =
                  if perRequest && reqs > 0 then sumMillis.get().toDouble / reqs else 0.0,
              top = top.get().toList
            )

    /** One EWMA horizon; fiber-local, so a plain `var` is fine. */
    private final class Ewma(tauSeconds: Double):
        private var v: Double = 0.0
        def observe(ratePerSec: Double, dtSeconds: Double): Unit =
            val decay = math.exp(-dtSeconds / tauSeconds)
            v = v * decay + ratePerSec * (1.0 - decay)
        def get: Double = v

    private def mkEwmas(): Vector[Ewma] = Taus.map(new Ewma(_))
    private def observe(es: Vector[Ewma], rate: Double, dt: Double): Unit =
        es.foreach(_.observe(rate, dt))
    private def rateOf(es: Vector[Ewma], now: Double): RateView =
        RateView(now = now, load1m = es(0).get, load5m = es(1).get, load15m = es(2).get)

    private def updateMax(m: AtomicLong, v: Long): Unit =
        var cur = m.get()
        while v > cur && !m.compareAndSet(cur, v) do cur = m.get()

    /** The sampler's published output — all rates as EWMA load averages. */
    private final case class Rolling(
        localRate: RateView,
        peerRates: Map[Int, RateView],
        blockRate: RateView,
        blockRequestRate: RateView
    )
    private object Rolling:
        def empty(remotePeerNums: Vector[Int]): Rolling =
            Rolling(
              RateView.zero,
              remotePeerNums.map(_ -> RateView.zero).toMap,
              RateView.zero,
              RateView.zero
            )

// ---- snapshot data (plain, framework-free; ApiDto builds the JSON view, PrometheusFormat the text) ----

final case class RateView(now: Double, load1m: Double, load5m: Double, load15m: Double)
object RateView:
    val zero: RateView = RateView(0.0, 0.0, 0.0, 0.0)

final case class CounterWithRate(total: Long, rate: RateView)

final case class BlockStats(
    minor: Long,
    major: Long,
    avgEvents: Double,
    maxEvents: Long,
    blockRate: RateView,
    requestRate: RateView
)

final case class StackStats(
    total: Long,
    lastStackNumber: Long,
    secondsSinceLastHardConfirm: Long,
    meanInterStackGapSeconds: Double,
    avgBlocksAbsorbed: Double,
    maxBlocksAbsorbed: Long
)

/** One slow block in a timing category's top-N: its number, how long it took, and (for lead/replay)
  * how many requests it carried.
  */
final case class BlockTiming(blockNumber: Long, millis: Long, requests: Long)

/** Stats for one block-lifecycle timing category. `avgMillisPerRequest` is 0 for soft-consensus
  * (not request-normalized). `top` is the slowest blocks, most-severe first, with their numbers.
  */
final case class TimingStats(
    count: Long,
    avgMillis: Double,
    avgMillisPerRequest: Double,
    top: List[BlockTiming]
)

/** The three block-lifecycle timings: leading (become-leader → brief), replaying (brief received →
  * brief reproduced), and soft-consensus (cell spawned in FCA → soft-confirmed).
  */
final case class BlockTimingSet(lead: TimingStats, replay: TimingStats, softConsensus: TimingStats)

final case class PeerStats(
    uptimeSeconds: Long,
    localAccepted: Long,
    localRate: RateView,
    localRejScreening: Long,
    localRejBackpressure: Long,
    peerRequests: Map[Int, CounterWithRate],
    blocks: BlockStats,
    stacks: StackStats,
    blockTimings: BlockTimingSet,
    mempoolSize: Long,
    leaderMempoolDrain: Long,
    sequencerHeadroom: Long,
    equityLovelace: Long,
    composer: ComposerStats,
    runtime: RuntimeStats
)

/** Whole-process resource use, for answering one question about a run: is the resource curve
  * proportional to work IN FLIGHT, or to HISTORY?
  *
  * The discriminator is a slope against the right denominator, so these have to be read against the
  * cumulative counters already in [[PeerStats]] (`blocks`, `stacks`, `localAccepted`) and against a
  * deliberate load step-down: an in-flight-sized resource plateaus at constant load and comes back
  * DOWN when load drops; a history-sized one tracks the cumulative counter and does not.
  *
  * ⛔ Every axis here is a CONCURRENCY axis. A peer can hold all of them at their idle floor while
  * carrying a large backlog of unabsorbed deposits, unconfirmed blocks and queued resubmissions, so
  * these do not answer "has the node drained?" — only "is it running work right now?"
  *
  * `fibersSuspended` is a fiber census, not a fiber dump: `kill -USR1` costs seconds and megabytes,
  * this is two counter reads. `workersSearching` and `timersExecuted` are the cats-effect
  * scheduler-seizure signature — in that state `workersSearching` stays non-zero while
  * `timersExecuted` stops advancing, and the runtime cannot self-report it, because its own
  * starvation checker is an `IO.sleep` that fires once and then goes silent. All of these are read
  * from a plain snapshot rather than from a fiber, so a scraper outside the process still gets them
  * when the runtime is seized.
  */
final case class RuntimeStats(
    fibersSuspended: Long,
    fibersQueuedLocal: Long,
    workerThreads: Int,
    workersActive: Int,
    workersSearching: Int,
    workersBlocked: Int,
    timersOutstanding: Long,
    timersExecuted: Long,
    liveThreads: Int,
    heapUsedBytes: Long,
    heapCommittedBytes: Long,
    openFileDescriptors: Long
)

/** What the [[hydrozoa.multisig.consensus.StackComposer]] is doing, and for how long.
  *
  * Every path through `tryProgress` that is not `Deriving` returns `IO.unit` silently, so without
  * this a composer waiting on a peer and one with nothing to do are indistinguishable. While
  * `Deriving`, the partition counts show progress through a stack that may hold hundreds.
  */
final case class ComposerStats(
    phase: StackComposerPhase,
    secondsInPhase: Long,
    partitionsDone: Long,
    partitionsTotal: Long
)
