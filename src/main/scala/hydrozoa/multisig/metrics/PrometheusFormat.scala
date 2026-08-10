package hydrozoa.multisig.metrics

/** Renders a [[PeerStats]] snapshot as Prometheus text exposition (version 0.0.4).
  *
  * Canonical values are the monotonic `_total` counters — Prometheus computes rates from those with
  * `rate(...[5m])`. The EWMA load averages and exact windows are exposed as gauges for convenience
  * and to mirror the JSON view; both endpoints read the same snapshot, so they never disagree.
  */
object PrometheusFormat:

    /** The content type for the endpoint's response header. */
    val contentType: String = "text/plain; version=0.0.4; charset=utf-8"

    def render(s: PeerStats): String =
        val b = new StringBuilder

        def counter(name: String, help: String, value: Long, labels: String = ""): Unit =
            b.append(s"# HELP $name $help\n# TYPE $name counter\n$name$labels $value\n")
        def gauge(name: String, help: String, value: Long): Unit =
            b.append(s"# HELP $name $help\n# TYPE $name gauge\n$name $value\n")
        def gaugeD(name: String, help: String, value: Double): Unit =
            b.append(s"# HELP $name $help\n# TYPE $name gauge\n$name ${fmt(value)}\n")

        gauge(
          "hydrozoa_uptime_seconds",
          "Seconds since this peer's metrics started.",
          s.uptimeSeconds
        )

        counter(
          "hydrozoa_local_requests_total",
          "Requests this peer sequenced successfully.",
          s.localAccepted
        )
        b.append("# HELP hydrozoa_local_requests_rejected_total Locally-rejected requests.\n")
        b.append("# TYPE hydrozoa_local_requests_rejected_total counter\n")
        b.append(
          s"""hydrozoa_local_requests_rejected_total{reason="screening"} ${s.localRejScreening}\n"""
        )
        b.append(
          s"""hydrozoa_local_requests_rejected_total{reason="backpressure"} ${s.localRejBackpressure}\n"""
        )
        rate("hydrozoa_local_requests", "Local request rate (req/s).", s.localRate, b)

        b.append("# HELP hydrozoa_peer_requests_total Requests ingested from a remote head peer.\n")
        b.append("# TYPE hydrozoa_peer_requests_total counter\n")
        s.peerRequests.toVector.sortBy(_._1).foreach { case (peer, c) =>
            b.append(s"""hydrozoa_peer_requests_total{peer="$peer"} ${c.total}\n""")
        }

        b.append("# HELP hydrozoa_blocks_total Soft-confirmed blocks by type.\n")
        b.append("# TYPE hydrozoa_blocks_total counter\n")
        b.append(s"""hydrozoa_blocks_total{type="minor"} ${s.blocks.minor}\n""")
        b.append(s"""hydrozoa_blocks_total{type="major"} ${s.blocks.major}\n""")
        gaugeD("hydrozoa_block_events_avg", "Average events per block.", s.blocks.avgEvents)
        gauge("hydrozoa_block_events_max", "Maximum events in any block.", s.blocks.maxEvents)
        window(
          "hydrozoa_blocks_window",
          "Blocks in the trailing window.",
          s.blocks.blocksPerWindow,
          b
        )
        window(
          "hydrozoa_block_requests_window",
          "Requests in blocks in the trailing window.",
          s.blocks.requestsPerWindow,
          b
        )

        counter("hydrozoa_stacks_total", "Hard-confirmed stacks.", s.stacks.total)
        gauge(
          "hydrozoa_stack_last_number",
          "Most recent hard-confirmed stack number.",
          s.stacks.lastStackNumber
        )
        gauge(
          "hydrozoa_seconds_since_last_hard_confirm",
          "Seconds since the last stack hard-confirmed.",
          s.stacks.secondsSinceLastHardConfirm
        )
        gaugeD(
          "hydrozoa_stack_gap_seconds_mean",
          "Mean seconds between hard-confirmed stacks.",
          s.stacks.meanInterStackGapSeconds
        )
        gaugeD(
          "hydrozoa_stack_blocks_avg",
          "Average blocks absorbed per stack.",
          s.stacks.avgBlocksAbsorbed
        )
        gauge(
          "hydrozoa_stack_blocks_max",
          "Maximum blocks absorbed by any stack.",
          s.stacks.maxBlocksAbsorbed
        )

        b.toString

    private def rate(name: String, help: String, r: RateView, b: StringBuilder): Unit =
        b.append(s"# HELP ${name}_per_second $help\n# TYPE ${name}_per_second gauge\n")
        b.append(s"${name}_per_second ${fmt(r.now)}\n")
        b.append(s"# HELP ${name}_load $help (EWMA load average).\n# TYPE ${name}_load gauge\n")
        b.append(s"""${name}_load{window="1m"} ${fmt(r.load1m)}\n""")
        b.append(s"""${name}_load{window="5m"} ${fmt(r.load5m)}\n""")
        b.append(s"""${name}_load{window="15m"} ${fmt(r.load15m)}\n""")

    private def window(name: String, help: String, w: WindowCounts, b: StringBuilder): Unit =
        b.append(s"# HELP $name $help\n# TYPE $name gauge\n")
        b.append(s"""$name{window="10s"} ${w.last10s}\n""")
        b.append(s"""$name{window="30s"} ${w.last30s}\n""")
        b.append(s"""$name{window="60s"} ${w.last60s}\n""")
        b.append(s"""$name{window="5m"} ${w.last5m}\n""")
        b.append(s"""$name{window="10m"} ${w.last10m}\n""")

    /** Prometheus wants a plain decimal, not `1.0E-4` / `NaN`. Round to 4 dp and drop trailing
      * zeros; non-finite and zero collapse to `0`.
      */
    private def fmt(d: Double): String =
        if d.isNaN || d.isInfinite || d == 0.0 then "0"
        else
            val bd = BigDecimal(d).setScale(4, BigDecimal.RoundingMode.HALF_UP).bigDecimal
            if bd.signum() == 0 then "0" else bd.stripTrailingZeros().toPlainString()
