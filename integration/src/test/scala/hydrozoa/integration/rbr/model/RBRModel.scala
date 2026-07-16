package hydrozoa.integration.rbr.model

import hydrozoa.integration.rbr.model.petri.net.{RBRNet, RBRPlaceId}
import hydrozoa.integration.rbr.model.petri.net.Transitions.RBRTransitionId
import scala.annotation.tailrec

/** Runs an [[RBRNet]] to quiescence and exposes the terminal marking as `Map[RBRPlaceId, Int]`.
  *
  * Firing strategy: repeatedly pick the first `RBRTransitionId` (by declared enum order) that is
  * currently enabled and fire it. This yields a deterministic trajectory the sibling MBT suite can
  * compare against a SUT snapshot. The RBR net is expected to be confluent up to marking, i.e.
  * every firing order reaches the same terminal — that assumption will be property-tested in PR1b.
  *
  * A `MaxSteps` guard bounds the loop so a mistake in arc weights (or a future cyclic transition)
  * does not lock a test up.
  */
object RBRModel {

    /** Upper bound on the number of firings the driver will perform before giving up. Chosen so
      * that a reasonable RBR run (e.g. 20 head peers, 1000 evacs, batch 63 ⇒ ~1050 firings)
      * completes comfortably, while an unbounded cycle terminates the test loudly.
      */
    val MaxSteps: Int = 10_000

    /** Failure to reach quiescence within [[MaxSteps]] — indicates either a broken net or a
      * misconfigured `Params`.
      */
    case class QuiescenceLimitExceeded(steps: Int, lastMarking: Map[RBRPlaceId, Int])
        extends RuntimeException(
          s"RBR net did not quiesce within $steps steps; last marking = $lastMarking"
        )

    /** Fire enabled transitions until none remain, or raise [[QuiescenceLimitExceeded]]. */
    @throws[QuiescenceLimitExceeded]("if the net does not quiesce within MaxSteps firings")
    def runToQuiescence(net: RBRNet.Type): RBRNet.Type = {
        val order: List[RBRTransitionId] = RBRTransitionId.values.toList

        @tailrec
        def loop(current: RBRNet.Type, steps: Int): RBRNet.Type =
            if steps >= MaxSteps then throw QuiescenceLimitExceeded(steps, marking(current))
            else
                order.iterator
                    .flatMap(t => current.fire(t).toOption.iterator)
                    .nextOption() match
                    case Some(next) => loop(next, steps + 1)
                    case None       => current

        loop(net, 0)
    }

    /** Snapshot the current marking of every [[RBRPlaceId]] as an `Int`. Missing places (which
      * should not occur in a well-built net) default to 0 so this is safe to call before
      * validation.
      */
    def marking(net: RBRNet.Type): Map[RBRPlaceId, Int] =
        RBRPlaceId.values.toList.map { pid =>
            val count = net.getPlaceSemantics(pid).toOption.fold(0)(_.marking.toInt)
            pid -> count
        }.toMap

    /** Convenience: build the net for `params`, run to quiescence, return terminal marking. */
    def terminal(params: RBRNet.Params): Map[RBRPlaceId, Int] =
        marking(runToQuiescence(RBRNet(params)))
}
