package hydrozoa.multisig.ledger.remote

/** Typed events emitted by [[RemoteL2Screener]]. Pure data; formatters in
  * [[RemoteL2ScreenerEventFormat]] decide how each variant is rendered to a particular sink.
  */
sealed trait RemoteL2ScreenerEvent

object RemoteL2ScreenerEvent:

    /** The endpoint screened a deposit and rejected it (its `reason` is surfaced to the user as the
      * [[hydrozoa.multisig.ledger.l2.L2ScreenError]] message).
      */
    final case class DepositRejected(reason: Option[String]) extends RemoteL2ScreenerEvent

    /** The endpoint screened a transaction and rejected it (its `reason` is surfaced to the user as
      * the [[hydrozoa.multisig.ledger.l2.L2ScreenError]] message).
      */
    final case class TxRejected(reason: Option[String]) extends RemoteL2ScreenerEvent

    /** The screening call failed at the transport level (connection refused, timeout, non-2xx,
      * undecodable body). Screening fails open — the request proceeds unscreened, exactly as if no
      * screener were configured — because a screening pass is advisory: the remote ledger re-runs
      * the same checks authoritatively at submission, and an unreachable screener must not block
      * the request path.
      */
    final case class ScreenerUnavailable(cause: Throwable) extends RemoteL2ScreenerEvent
