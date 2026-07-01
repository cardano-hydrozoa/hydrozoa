package hydrozoa.integration.fallbackhandoff

import cats.effect.IO
import cats.implicits.*
import hydrozoa.integration.harness.MultiPeerHeadHarness.Event as HEvent
import hydrozoa.integration.harness.{Plugin, Signal}
import hydrozoa.multisig.CommonChildEvent
import hydrozoa.multisig.LifecycleEvent
import hydrozoa.multisig.consensus.CardanoLiaisonEvent
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Per-peer signals for the fallback handoff scenario.
  *
  *   - [[fallbackEnteredByPeer]] — fires on the first `FallbackToRuleBasedDispatched` from each
  *     peer's CL.
  *   - [[ruleBasedStartedByPeer]] — fires on the first `RuleBasedRegimeStarted` from each
  *     peer's HMRM. The event itself does not yet exist (TDD target on the src side).
  */
object Plugins:

    /** One [[Signal]] per peer; fires when that peer's CL dispatches `FallbackToRuleBased`. */
    def fallbackEnteredByPeer(
        peers: Seq[HeadPeerNumber],
    ): IO[Map[HeadPeerNumber, Signal[Unit]]] =
        peers.toList
            .traverse(p => signalForPeer(p)(matchFallbackDispatched).map(p -> _))
            .map(_.toMap)

    /** One [[Signal]] per peer; fires when that peer's HMRM surfaces `RuleBasedRegimeStarted`.
      * The matched event does not yet exist — placeholder until the handoff is implemented.
      */
    def ruleBasedStartedByPeer(
        peers: Seq[HeadPeerNumber],
    ): IO[Map[HeadPeerNumber, Signal[Unit]]] =
        peers.toList
            .traverse(p => signalForPeer(p)(matchRuleBasedStarted).map(p -> _))
            .map(_.toMap)

    /** Plugin wrapper folding both signal maps into the one tracer the harness consumes. */
    final case class HandoffPlugins(
        fallbackByPeer: Map[HeadPeerNumber, Signal[Unit]],
        ruleBasedByPeer: Map[HeadPeerNumber, Signal[Unit]],
    ):
        def tracer = Plugin.tracerOf(
          (fallbackByPeer.values.toSeq ++ ruleBasedByPeer.values.toSeq)*
        )

    /** Build a per-peer signal: the predicate is called with each `HEvent`; the event is
      * filtered to the named peer's head side, then the inner matcher decides whether to fire.
      */
    private def signalForPeer(target: HeadPeerNumber)(
        matcher: HeadPeerNumber => HEvent => Option[Unit],
    ): IO[Signal[Unit]] =
        Signal.make[Unit](e => IO.pure(matcher(target)(e)))

    private def matchFallbackDispatched(target: HeadPeerNumber)(event: HEvent): Option[Unit] =
        event match
            case HEvent.Head(
                  p,
                  CommonChildEvent.CardanoLiaison(
                    CardanoLiaisonEvent.FallbackToRuleBasedDispatched(_)
                  ),
                ) if p == target =>
                Some(())
            case _ => None

    /** TDD target: the matched lifecycle event does not exist yet. */
    private def matchRuleBasedStarted(target: HeadPeerNumber)(event: HEvent): Option[Unit] = ???

end Plugins
