package hydrozoa.multisig.consensus.limiter

import scala.concurrent.duration.FiniteDuration

/** The downstream-backlog gate: turns "how much work downstream has yet to absorb" into a
  * multiplier on the lane's release rate.
  *
  * ⛔ **Opt-in.** Without a gate there is no counting, no ticking and no multiplier, so a lane that
  * is never sent [[LimiterControl.DownstreamDrained]] cannot ratchet its period toward infinity.
  *
  * The spacing gate alone bounds the floor on cycle time; this bounds the ceiling on outstanding
  * work. Neither subsumes the other.
  */
final case class LimiterGate(
    /** Backlog (in released messages per downstream cycle) below which the gate is fully open. Set
      * above healthy steady-state accumulation and the controller does nothing; the sizing rule is
      * on the config knob that supplies it.
      */
    backlogSoftLimit: Int,

    /** Backlog at which the multiplier reaches [[floor]]. */
    backlogHardLimit: Int,

    /** The slowest the lane is ever shaped to, as a fraction of the configured rate. ⛔ Never 0:
      * upstream is self-clocked by what this lane releases, so releasing nothing stops the clock
      * that would later reopen the gate. Same reason TCP always sends a full segment eventually.
      */
    floor: Double,

    /** EWMA weight on the newest cycle's residual. Below 1.0 the controller acts on a filtered
      * count rather than the raw one.
      */
    smoothing: Double,

    /** Longest single sleep the limiter will commit to before re-reading its mailbox. Bounds how
      * stale the multiplier can be while a hold is outstanding.
      */
    slice: FiniteDuration
):
    require(backlogHardLimit > backlogSoftLimit, "backlogHardLimit must exceed backlogSoftLimit")
    require(floor > 0.0 && floor <= 1.0, "gate floor must be in (0, 1]")
    require(smoothing > 0.0 && smoothing <= 1.0, "gate smoothing must be in (0, 1]")

    /** Headroom, 1.0 fully open through 0.0 fully loaded. */
    def headroom(backlog: Double): Double =
        val span = (backlogHardLimit - backlogSoftLimit).toDouble
        ((backlogHardLimit - backlog) / span).max(0.0).min(1.0)

    /** The rate multiplier for a filtered backlog.
      *
      * `1 - (1 - h)^2` is flat near full headroom and steepens as it runs out. ⛔ Not `h^2`, whose
      * highest gain sits where the system is healthy: the loop's dead time is a full downstream
      * cycle, and high gain across long dead time produces a limit cycle.
      */
    def multiplier(backlog: Double): Double =
        val h = headroom(backlog)
        val open = 1.0 - (1.0 - h) * (1.0 - h)
        floor + (1.0 - floor) * open

object LimiterGate:

    /** The gate's running state. Owned by the limiter actor and read by nothing else — the copy
      * published to `PeerMetrics` is for the stats endpoints and is never read back.
      *
      * @param released
      *   throttled messages released since the last drain signal. This is the live count.
      * @param residual
      *   the filtered per-cycle residual the multiplier is derived from. Updated only on a drain
      *   signal, from the count accumulated over the cycle just ended, so between signals the
      *   multiplier is constant.
      */
    final case class State(released: Long, residual: Double, multiplier: Double, drains: Long)

    val Open: State = State(released = 0L, residual = 0.0, multiplier = 1.0, drains = 0L)

    extension (gate: LimiterGate)
        /** Fold one completed downstream cycle into the filter and re-derive the multiplier. */
        def observeDrain(s: State): State =
            val residual =
                gate.smoothing * s.released.toDouble + (1.0 - gate.smoothing) * s.residual
            State(
              released = 0L,
              residual = residual,
              multiplier = gate.multiplier(residual),
              drains = s.drains + 1
            )
