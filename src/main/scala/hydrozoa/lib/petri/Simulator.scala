package hydrozoa.lib.petri

import hydrozoa.lib.number.PositiveInt
import scala.annotation.tailrec
import scala.util.Random

object Simulator {
    enum FiringError[TransitionId]:
        case TransitionNotEnabled(id: TransitionId)

}

/** A simulator needs all data _except_ presentation data.
  * @tparam Self
  *   This is F-Bounded polymorphism. It is what enables a recursive type signature. See
  *   https://www.youtube.com/watch?v=Wki2B6iW1oA
  */
trait Simulator[NetId, ArcId, PlaceId, TransitionId, Sim <: Simulator[
  NetId,
  ArcId,
  PlaceId,
  TransitionId,
  Sim
]] extends net.Id[NetId, ArcId, PlaceId, TransitionId],
      net.Topology[ArcId, PlaceId, TransitionId],
      net.Configuration[ArcId, PlaceId, TransitionId],
      net.Simulation[ArcId, PlaceId, TransitionId] { self: Sim =>

    // You should override this -- there is probably a more efficient way
    lazy val getEnabledTransitions: Set[TransitionId] = transitions.filter(isEnabled)
    def isEnabled(t: TransitionId): Boolean

    protected def fireTransitionUnsafe(id: TransitionId): Sim

    final def fireTransition(id: TransitionId): Either[Simulator.FiringError[TransitionId], Sim] =
        if isEnabled(id)
        then Right(fireTransitionUnsafe(id))
        else Left(Simulator.FiringError.TransitionNotEnabled(id))

    // TODO:
    //   - This currently serializes transitions. Make it pick from a set of non-conflicting transitions
    //     and execute them simultaneously
    //   - Return the execution trace log
    //   - This does not check if the net has cycles. If it does, it will run forever.
    @tailrec
    final def autoFireEnabledTransitions(
        maxSteps: Option[PositiveInt],
        // TODO: Apparently cats effect also has a random, its probably better to use that.
        random: Random = Random(0)
    ): Sim =
        if getEnabledTransitions.isEmpty
        then self
        else {
            val index = random.nextInt(getEnabledTransitions.size)
            val t =
                getEnabledTransitions.toIndexedSeq(index)
            val fired = fireTransitionUnsafe(t)
            maxSteps match {
                case None =>
                    fired.autoFireEnabledTransitions(None, random)
                case Some(p) if p.convert == 1 => fired
                case Some(p) =>
                    fired.autoFireEnabledTransitions(
                      Some(PositiveInt.unsafeApply(p.convert - 1)),
                      random
                    )

            }
        }
}
