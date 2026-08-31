package hydrozoa.multisig.consensus.limiter

/** Control messages a [[Limiter]] consumes for its own account. Never forwarded downstream, so the
  * lane's downstream actor needs no case for them.
  *
  * Messages rather than a shared [[cats.effect.Ref]]: a `Ref` read inside a sleep loop is invisible
  * to deterministic replay.
  */
sealed trait LimiterControl

object LimiterControl:

    /** Downstream has absorbed the backlog this limiter released — on the block lane, one stack
      * hard-confirmed. Reopens the gate.
      *
      * Hard confirmation rather than the peer's own hard ack: it is one event per downstream cycle
      * and strictly later, so it subsumes the ack, and where a coil lags the extra conservatism is
      * what we want. It also cannot deadlock against the gate — hard confirmation needs hard acks
      * and the stack clock, never new blocks — so throttling the block lane all the way to its
      * floor can never suppress the event that lifts the throttle.
      */
    case object DownstreamDrained extends LimiterControl

    /** Self-tick: re-evaluate an outstanding hold.
      *
      * A hold is a chain of short sleeps each followed by one of these, not a single `IO.sleep` for
      * the whole wait, so the mailbox drains between slices and [[DownstreamDrained]] is acted on
      * at most one slice late. The handler recomputes from current state, so a stale or duplicated
      * tick is harmless.
      */
    case object Tick extends LimiterControl
