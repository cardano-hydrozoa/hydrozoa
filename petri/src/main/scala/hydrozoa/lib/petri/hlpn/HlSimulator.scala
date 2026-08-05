package hydrozoa.lib.petri.hlpn

/** An HLPN firing driver: an [[HlNet]] paired with a [[ModeSelector]]. The net supplies the *rules*
  * (Concepts 23/24 via [[HlNet.fire]]); the selector supplies the *search* — which candidate modes
  * to try — and [[fire]] fires the first one the net accepts (the selection policy of Concept 7).
  *
  * It deliberately exposes *no* "enabled set" query. Any such query would be *selector-relative*:
  * `selector.candidates(net, tid)` is a search strategy, free to be incomplete (a greedy `Collect`
  * selector yields one batch; an arbitrary selector may propose nothing for a transition), so "the
  * selector finds an enabled mode" is not "the transition is enabled". The truthful, selector-free
  * primitive is [[HlNet.isModeEnabled]] on a *specific* mode. A caller that wants the enabled set
  * composes `selector.candidates(net, tid).filter(net.isModeEnabled(tid, _))` itself and owns the
  * completeness — which is the selector's burden (see [[ModeSelector]]), not the net's.
  *
  * HLPN firing is mode-relative: [[fire]] returns the mode that fired as part of the result, so
  * this is its own driver rather than a plain mode-less simulator.
  */
final case class HlSimulator[PlaceId, TransitionId, C](
    net: HlNet[PlaceId, TransitionId, C],
    selector: ModeSelector[PlaceId, TransitionId, C]
) {

    /** Fire `tid` under the first candidate the net accepts, returning the advanced simulator and
      * the mode that fired. Candidates that fail the net's own checks are skipped, not errors.
      */
    def fire(
        tid: TransitionId
    ): Either[HlSimulator.Error, (HlSimulator[PlaceId, TransitionId, C], Binding)] =
        if !net.transitionIds.contains(tid) then Left(HlSimulator.Error.TransitionNotFound(tid))
        else
            selector
                .candidates(net, tid)
                .map(mode => net.fire(tid, mode).toOption.map(_ -> mode))
                .collectFirst { case Some(ok) => ok }
                .map((fired, mode) => (copy(net = fired), mode))
                .toRight(HlSimulator.Error.NoEnabledMode(tid))
}

object HlSimulator {

    /** Why the simulator could not fire. */
    enum Error {

        /** The requested transition ID is not present in the net. */
        case TransitionNotFound[TransitionId](transition: TransitionId) extends Error

        /** No selector candidate enabled the transition — either it is genuinely not enabled, or
          * the selector is incomplete for it.
          */
        case NoEnabledMode[TransitionId](transition: TransitionId) extends Error
    }
}
