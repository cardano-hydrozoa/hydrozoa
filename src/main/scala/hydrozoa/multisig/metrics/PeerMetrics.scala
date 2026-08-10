package hydrozoa.multisig.metrics

import cats.effect.IO
import java.util.concurrent.atomic.{AtomicLong, AtomicReference, LongAdder}
import scala.concurrent.duration.*

/** Why a request was refused, split so the stats endpoints can answer "is backpressure firing?"
  * without log-diving.
  */
enum RejectionKind:
    case Screening, Backpressure

/** Process-lifetime, in-memory peer metrics behind `GET /head/stats` and `GET /head/metrics` (see
  * `design/peer-stats-endpoint.md`).
  *
  * The hot-path `on*` methods are lock-free side effects (a single atomic add). Each counter except
  * the peer-request map is written by exactly one actor — cats actors drain their mailbox serially
  * — so a plain [[java.util.concurrent.atomic.AtomicLong AtomicLong]] publishes safely to the HTTP
  * reader without [[java.util.concurrent.atomic.LongAdder LongAdder]]'s striping. Peer-request
  * ingestion fans in from multiple liaison actors, so those counters use `LongAdder`. The derived
  * rolling view (EWMA load averages + exact windows) is owned by the single [[sampler]] fiber and
  * published through one [[java.util.concurrent.atomic.AtomicReference AtomicReference]].
  *
  * Counters are not persisted: they reset on restart.
  */
final class PeerMetrics private (startedAtMillis: Long, headPeerNums: Vector[Int]):
    import PeerMetrics.*

    // ---- local requests (written by RequestSequencer) ----
    private val localAccepted = new AtomicLong(0)
    private val localRejScreening = new AtomicLong(0)
    private val localRejBackpressure = new AtomicLong(0)

    // ---- peer requests, per remote head peer (written by the peer liaisons, multi-writer) ----
    private val peerRequests: Map[Int, LongAdder] =
        headPeerNums.map(_ -> new LongAdder).toMap

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

    // ---- derived rolling view (written only by the sampler fiber) ----
    private val rolling = new AtomicReference[Rolling](Rolling.empty(headPeerNums))

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

    /** Record a soft-confirmed block of the given type carrying `events` requests. */
    def onBlockConfirmed(isMajor: Boolean, events: Int): Unit =
        val _ = if isMajor then blocksMajor.incrementAndGet() else blocksMinor.incrementAndGet()
        blockEventsSum.addAndGet(events.toLong)
        updateMax(blockEventsMax, events.toLong)

    /** Record a hard-confirmed stack absorbing `blocksAbsorbed` blocks, at wall-clock `nowMillis`.
      */
    def onStackConfirmed(stackNum: Int, blocksAbsorbed: Int, nowMillis: Long): Unit =
        val prev = lastStackMillis.getAndSet(nowMillis)
        if prev > 0 then { val _ = stackGapSumMillis.addAndGet(nowMillis - prev) }
        stacksTotal.incrementAndGet()
        lastStackNum.set(stackNum.toLong)
        stackBlocksSum.addAndGet(blocksAbsorbed.toLong)
        updateMax(stackBlocksMax, blocksAbsorbed.toLong)

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
          peerRequests = headPeerNums.map { p =>
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
            blocksPerWindow = roll.blocksPerWindow,
            requestsPerWindow = roll.requestsPerWindow
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
          )
        )

    /** The 1 Hz sampler: rolls the exact-window ring and updates the EWMA load averages, then
      * publishes one immutable [[Rolling]]. Run as a background fiber for the node's lifetime. The
      * ring and EWMA state are fiber-local (single writer), so they need no synchronization.
      */
    def sampler(period: FiniteDuration = 1.second): IO[Unit] =
        val blocksRing = Array.fill(RingSize)(0L)
        val eventsRing = Array.fill(RingSize)(0L)
        val ewmaLocal = mkEwmas()
        val ewmaPeer = headPeerNums.map(_ -> mkEwmas()).toMap

        def loop(
            tick: Long,
            lastMillis: Long,
            lastLocal: Long,
            lastPeer: Map[Int, Long]
        ): IO[Unit] =
            IO.sleep(period) >> IO.realTime.flatMap { now =>
                val nowMillis = now.toMillis
                val dt = math.max(1e-3, (nowMillis - lastMillis) / 1000.0)

                val curLocal = localAccepted.get()
                observe(ewmaLocal, (curLocal - lastLocal) / dt, dt)

                val curPeer = headPeerNums.map(p => p -> peerRequests(p).sum()).toMap
                headPeerNums.foreach { p =>
                    observe(ewmaPeer(p), (curPeer(p) - lastPeer.getOrElse(p, 0L)) / dt, dt)
                }

                val idx = (tick % RingSize).toInt
                blocksRing(idx) = blocksMinor.get() + blocksMajor.get()
                eventsRing(idx) = blockEventsSum.get()

                rolling.set(
                  Rolling(
                    localRate = rateOf(ewmaLocal, (curLocal - lastLocal) / dt),
                    peerRates = headPeerNums.map { p =>
                        p -> rateOf(ewmaPeer(p), (curPeer(p) - lastPeer.getOrElse(p, 0L)) / dt)
                    }.toMap,
                    blocksPerWindow = windows(blocksRing, blocksRing(idx), tick),
                    requestsPerWindow = windows(eventsRing, eventsRing(idx), tick)
                  )
                )
                loop(tick + 1, nowMillis, curLocal, curPeer)
            }

        IO.realTime.flatMap(t0 =>
            loop(0L, t0.toMillis, localAccepted.get(), headPeerNums.map(_ -> 0L).toMap)
        )

object PeerMetrics:
    /** Ring depth in seconds — 15 min, covering every window with headroom. */
    private val RingSize = 900
    private val Taus: Vector[Double] = Vector(60.0, 300.0, 900.0) // 1m / 5m / 15m

    def create(nowMillis: Long, headPeerNums: Vector[Int]): PeerMetrics =
        new PeerMetrics(nowMillis, headPeerNums)

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

    /** Windowed count from a cumulative ring: `cur - cumulative(tick - w)`; before enough history
      * has accrued, everything since boot.
      */
    private def windows(ring: Array[Long], cur: Long, tick: Long): WindowCounts =
        def ago(w: Int): Long =
            if tick < w then cur else cur - ring(((tick - w) % RingSize).toInt)
        WindowCounts(ago(10), ago(30), ago(60), ago(300), ago(600))

    /** The sampler's published output. */
    private final case class Rolling(
        localRate: RateView,
        peerRates: Map[Int, RateView],
        blocksPerWindow: WindowCounts,
        requestsPerWindow: WindowCounts
    )
    private object Rolling:
        def empty(headPeerNums: Vector[Int]): Rolling =
            Rolling(
              RateView.zero,
              headPeerNums.map(_ -> RateView.zero).toMap,
              WindowCounts.zero,
              WindowCounts.zero
            )

// ---- snapshot data (plain, framework-free; ApiDto builds the JSON view, PrometheusFormat the text) ----

final case class RateView(now: Double, load1m: Double, load5m: Double, load15m: Double)
object RateView:
    val zero: RateView = RateView(0.0, 0.0, 0.0, 0.0)

final case class WindowCounts(
    last10s: Long,
    last30s: Long,
    last60s: Long,
    last5m: Long,
    last10m: Long
)
object WindowCounts:
    val zero: WindowCounts = WindowCounts(0, 0, 0, 0, 0)

final case class CounterWithRate(total: Long, rate: RateView)

final case class BlockStats(
    minor: Long,
    major: Long,
    avgEvents: Double,
    maxEvents: Long,
    blocksPerWindow: WindowCounts,
    requestsPerWindow: WindowCounts
)

final case class StackStats(
    total: Long,
    lastStackNumber: Long,
    secondsSinceLastHardConfirm: Long,
    meanInterStackGapSeconds: Double,
    avgBlocksAbsorbed: Double,
    maxBlocksAbsorbed: Long
)

final case class PeerStats(
    uptimeSeconds: Long,
    localAccepted: Long,
    localRate: RateView,
    localRejScreening: Long,
    localRejBackpressure: Long,
    peerRequests: Map[Int, CounterWithRate],
    blocks: BlockStats,
    stacks: StackStats
)
