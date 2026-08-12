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
        rate("hydrozoa_blocks", "Block production rate (blocks/s).", s.blocks.blockRate, b)
        rate("hydrozoa_block_requests", "Requests-in-blocks rate (req/s).", s.blocks.requestRate, b)

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

        // ---- block-lifecycle timings (lead / replay / soft-consensus) ----
        val timings = Vector(
          "lead" -> s.blockTimings.lead,
          "replay" -> s.blockTimings.replay,
          "soft_consensus" -> s.blockTimings.softConsensus
        )
        b.append(
          "# HELP hydrozoa_block_timing_count Blocks measured, per lifecycle category.\n" +
              "# TYPE hydrozoa_block_timing_count gauge\n"
        )
        timings.foreach((c, ts) =>
            b.append(s"""hydrozoa_block_timing_count{category="$c"} ${ts.count}\n""")
        )
        b.append(
          "# HELP hydrozoa_block_timing_avg_millis Average lifecycle duration (ms), per category.\n" +
              "# TYPE hydrozoa_block_timing_avg_millis gauge\n"
        )
        timings.foreach((c, ts) =>
            b.append(s"""hydrozoa_block_timing_avg_millis{category="$c"} ${fmt(ts.avgMillis)}\n""")
        )
        b.append(
          "# HELP hydrozoa_block_timing_avg_millis_per_request Average duration per request " +
              "(ms/req); 0 for soft-consensus.\n" +
              "# TYPE hydrozoa_block_timing_avg_millis_per_request gauge\n"
        )
        timings.foreach((c, ts) =>
            b.append(
              s"""hydrozoa_block_timing_avg_millis_per_request{category="$c"} ${fmt(
                    ts.avgMillisPerRequest
                  )}\n"""
            )
        )
        b.append(
          "# HELP hydrozoa_block_timing_top_millis Slowest measured blocks (ms), by category and " +
              "block number.\n# TYPE hydrozoa_block_timing_top_millis gauge\n"
        )
        timings.foreach((c, ts) =>
            ts.top.foreach(t =>
                b.append(
                  s"""hydrozoa_block_timing_top_millis{category="$c",block="${t.blockNumber}"} ${t.millis}\n"""
                )
            )
        )

        gauge("hydrozoa_mempool_size", "Requests held in the BlockWeaver mempool.", s.mempoolSize)
        gauge(
          "hydrozoa_leader_mempool_drain",
          "Requests the last lead pulled from the mempool into its block.",
          s.leaderMempoolDrain
        )
        gauge(
          "hydrozoa_sequencer_headroom",
          "Requests the sequencer will admit now before backpressure trips.",
          s.sequencerHeadroom
        )

        b.toString

    private def rate(name: String, help: String, r: RateView, b: StringBuilder): Unit =
        b.append(s"# HELP ${name}_per_second $help\n# TYPE ${name}_per_second gauge\n")
        b.append(s"${name}_per_second ${fmt(r.now)}\n")
        b.append(s"# HELP ${name}_load $help (EWMA load average).\n# TYPE ${name}_load gauge\n")
        b.append(s"""${name}_load{window="1m"} ${fmt(r.load1m)}\n""")
        b.append(s"""${name}_load{window="5m"} ${fmt(r.load5m)}\n""")
        b.append(s"""${name}_load{window="15m"} ${fmt(r.load15m)}\n""")

    /** Prometheus wants a plain decimal, not `1.0E-4` / `NaN`. Round to 4 dp and drop trailing
      * zeros; non-finite and zero collapse to `0`.
      */
    private def fmt(d: Double): String =
        if d.isNaN || d.isInfinite || d == 0.0 then "0"
        else
            val bd = BigDecimal(d).setScale(4, BigDecimal.RoundingMode.HALF_UP).bigDecimal
            if bd.signum() == 0 then "0" else bd.stripTrailingZeros().toPlainString()
