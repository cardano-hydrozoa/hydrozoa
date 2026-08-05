package hydrozoa.integration.rbr.mbt

import hydrozoa.integration.rbr.model.petri.hlpn.RBRHlNet.{RBRPlaceId, RBRTransitionId}
import hydrozoa.lib.petri.hlpn.{HlNet, HlSimulator, ModeSelector}
import scala.annotation.tailrec

/** Drives the model `RBRHlNet` from its fallback seed to the all-evacuated terminal, mirroring the
  * head's autonomous dispute: every peer votes, the voting deadline passes, the ballot-box linked
  * list folds pairwise to the single fully-tallied `(0, 0)` box, resolution reveals the winning
  * version, and evacuation drains its whole committed batch.
  *
  * Fully driven by the [[ModeSelector.unifying]] simulator and parametric in the peer count. The
  * tally's `continuing.key < removed.key` fold guard makes the contraction confluent — key 0 (the
  * public box) is never removed, so it always survives to the `(0, 0)` terminal — which means
  * firing each phase to saturation reaches the same marking regardless of `nHeadPeers`, the vote
  * pattern, or the fold order the selector happens to pick. `ObservableMarking` projects the
  * winning version away, so any resolved version yields the same observable terminal.
  */
object NetDriver:

    private type Net = HlNet[RBRPlaceId, RBRTransitionId, Any]
    private type Sim = HlSimulator[RBRPlaceId, RBRTransitionId, Any]

    def driveToEvacuated(net: Net): Net =
        import RBRTransitionId.*
        val voted = fireToSaturation(HlSimulator(net, ModeSelector.unifying), List(Vote))
        val closed = fireOnce(voted, VotingDeadline)
        val tallied = fireToSaturation(closed, List(TallyRemovedWins, TallyContinuingWins))
        val resolved = fireOnce(tallied, Resolution)
        fireToSaturation(resolved, List(Evacuation)).net

    /** Fire `tid` once via the unifying selector, surfacing the simulator error (rather than a bare
      * `None.get`) if it cannot fire.
      */
    private def fireOnce(sim: Sim, tid: RBRTransitionId): Sim =
        sim.fire(tid) match
            case Right((next, _)) => next
            case Left(err) => throw RuntimeException(s"driveToEvacuated: cannot fire $tid: $err")

    /** Fire whichever of `tids` is currently enabled, repeatedly, until none can fire. */
    @tailrec
    private def fireToSaturation(sim: Sim, tids: List[RBRTransitionId]): Sim =
        tids.iterator.flatMap(sim.fire(_).toOption).nextOption() match
            case Some((next, _)) => fireToSaturation(next, tids)
            case None            => sim
