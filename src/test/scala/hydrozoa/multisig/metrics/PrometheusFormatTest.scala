package hydrozoa.multisig.metrics

import org.scalatest.funsuite.AnyFunSuite

class PrometheusFormatTest extends AnyFunSuite:

    private val sample = PeerStats(
      uptimeSeconds = 42,
      localAccepted = 1234,
      localRate = RateView(now = 3.7, load1m = 3.5, load5m = 2.0, load15m = 1.0),
      localRejScreening = 5,
      localRejBackpressure = 4210,
      peerRequests = Map(1 -> CounterWithRate(987, RateView(1.0, 1.0, 1.0, 1.0))),
      blocks = BlockStats(
        minor = 3860,
        major = 2,
        avgEvents = 1.5,
        maxEvents = 1000,
        blockRate = RateView(now = 2.0, load1m = 1.9, load5m = 1.5, load15m = 1.0),
        requestRate = RateView(now = 3.0, load1m = 2.8, load5m = 2.0, load15m = 1.2)
      ),
      stacks = StackStats(
        total = 10,
        lastStackNumber = 10,
        secondsSinceLastHardConfirm = 42,
        meanInterStackGapSeconds = 180.0,
        avgBlocksAbsorbed = 386.0,
        maxBlocksAbsorbed = 900
      ),
      blockTimings = BlockTimingSet(
        lead = TimingStats(
          count = 100,
          avgMillis = 12.5,
          avgMillisPerRequest = 0.4,
          top = List(BlockTiming(blockNumber = 228, millis = 23470, requests = 923))
        ),
        replay = TimingStats(count = 98, avgMillis = 8.0, avgMillisPerRequest = 0.3, top = Nil),
        softConsensus = TimingStats(
          count = 100,
          avgMillis = 3.0,
          avgMillisPerRequest = 0.0,
          top = List(BlockTiming(blockNumber = 228, millis = 150, requests = 0))
        )
      ),
      mempoolSize = 923,
      leaderMempoolDrain = 900,
      sequencerHeadroom = 12,
      equityLovelace = 4_500_000L,
      composer = ComposerStats(
        phase = StackComposerPhase.WaitingForPreviousHardConfirmation,
        secondsInPhase = 42,
        partitionsDone = 17,
        partitionsTotal = 300
      ),
      runtime = RuntimeStats(
        fibersSuspended = 27428,
        fibersQueuedLocal = 5654,
        workerThreads = 4,
        workersActive = 2,
        workersSearching = 1,
        workersBlocked = 0,
        timersOutstanding = 12,
        timersExecuted = 98765,
        liveThreads = 36,
        heapUsedBytes = 241172480,
        heapCommittedBytes = 536870912,
        // -1 is the "platform does not report this" sentinel, and it must survive rendering.
        openFileDescriptors = -1
      )
    )

    test("counters carry the _total suffix and a TYPE line"):
        val out = PrometheusFormat.render(sample)
        assert(
          out.contains("# TYPE hydrozoa_local_requests_total counter") &&
              out.contains("hydrozoa_local_requests_total 1234") &&
              out.contains("# TYPE hydrozoa_stacks_total counter") &&
              out.contains("hydrozoa_stacks_total 10"),
          out
        )

    test("dimensions are labels, not separate metric names"):
        val out = PrometheusFormat.render(sample)
        assert(
          out.contains("""hydrozoa_local_requests_rejected_total{reason="screening"} 5""") &&
              out.contains(
                """hydrozoa_local_requests_rejected_total{reason="backpressure"} 4210"""
              ) &&
              out.contains("""hydrozoa_peer_requests_total{peer="1"} 987""") &&
              out.contains("""hydrozoa_blocks_total{type="minor"} 3860""") &&
              out.contains("""hydrozoa_blocks_total{type="major"} 2"""),
          out
        )

    test("load averages (including block/request throughput) are gauges"):
        val out = PrometheusFormat.render(sample)
        assert(
          out.contains("""hydrozoa_local_requests_load{window="1m"} 3.5""") &&
              out.contains("hydrozoa_local_requests_per_second 3.7") &&
              out.contains("hydrozoa_blocks_per_second 2") &&
              out.contains("""hydrozoa_blocks_load{window="5m"} 1.5""") &&
              out.contains("hydrozoa_block_requests_per_second 3") &&
              out.contains("""hydrozoa_block_requests_load{window="1m"} 2.8"""),
          out
        )

    test("fmt renders clean plain decimals and tames NaN / Infinity"):
        val bad = sample.copy(
          localRate = RateView(Double.NaN, Double.PositiveInfinity, 0.0001, 12345.678)
        )
        val out = PrometheusFormat.render(bad)
        assert(
          out.contains("hydrozoa_local_requests_per_second 0") && // NaN -> 0
              out.contains("""hydrozoa_local_requests_load{window="1m"} 0""") && // Inf -> 0
              out.contains("""hydrozoa_local_requests_load{window="5m"} 0.0001""") &&
              out.contains("""hydrozoa_local_requests_load{window="15m"} 12345.678""") &&
              !out.contains("E-"), // no scientific notation
          out
        )

    test("the runtime gauges render, including the -1 unavailable sentinel"):
        val out = PrometheusFormat.render(sample)
        val _ = assert(out.contains("hydrozoa_fibers_suspended 27428"))
        val _ = assert(out.contains("hydrozoa_workers_searching 1"))
        val _ = assert(out.contains("hydrozoa_timers_executed_total 98765"))
        val _ = assert(out.contains("# TYPE hydrozoa_timers_executed_total counter"))
        assert(out.contains("hydrozoa_open_file_descriptors -1"))
