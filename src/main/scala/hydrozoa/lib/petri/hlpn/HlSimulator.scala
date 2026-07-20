package hydrozoa.lib.petri.hlpn

/** An HLPN simulator: an [[HlNet]] paired with a [[ModeSelector]]. The net supplies the *rules*
  * (Concepts 23/24 via [[HlNet.fire]]); the selector supplies the *search* — which candidate modes
  * to try. This mirrors ISO Concept 6/7's split: the enabling function `E` is evaluated over the
  * selector's candidates, and firing selects the first enabled one (the selector's preference order
  * is the selection policy of Concept 7, step 2).
  *
  * Like [[HlNet]], this does not extend the framework's mode-less
  * [[hydrozoa.lib.petri.net.Simulator]] — HLPN firing is mode-relative, and the chosen mode is part
  * of the result.
  */
final case class HlSimulator[PlaceId, TransitionId, C](
    net: HlNet[PlaceId, TransitionId, C],
    selector: ModeSelector[PlaceId, TransitionId, C]
) {

    /** The selector's candidates that enable `tid` at the current marking, in preference order. */
    def enabledModes(tid: TransitionId): LazyList[Binding] =
        selector.candidates(net, tid).filter(net.isModeEnabled(tid, _))

    /** Whether the selector finds any mode enabling `tid`. */
    def isEnabled(tid: TransitionId): Boolean = enabledModes(tid).nonEmpty

    /** Every transition the selector finds an enabled mode for. */
    def enabledTransitions: Set[TransitionId] = net.transitionIds.filter(isEnabled)

    /** Fire `tid` under the first enabled candidate, returning the advanced simulator and the mode
      * that fired. Candidates that fail the net's own checks are skipped, not errors.
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
