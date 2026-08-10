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
        blocksPerWindow = WindowCounts(1, 3, 6, 30, 60),
        requestsPerWindow = WindowCounts(2, 6, 12, 60, 120)
      ),
      stacks = StackStats(
        total = 10,
        lastStackNumber = 10,
        secondsSinceLastHardConfirm = 42,
        meanInterStackGapSeconds = 180.0,
        avgBlocksAbsorbed = 386.0,
        maxBlocksAbsorbed = 900
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

    test("windows and load averages are gauges"):
        val out = PrometheusFormat.render(sample)
        assert(
          out.contains("""hydrozoa_blocks_window{window="10s"} 1""") &&
              out.contains("""hydrozoa_block_requests_window{window="10m"} 120""") &&
              out.contains("""hydrozoa_local_requests_load{window="1m"} 3.5""") &&
              out.contains("hydrozoa_local_requests_per_second 3.7"),
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
