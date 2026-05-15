//package hydrozoa.lib.petri
//
//import hydrozoa.lib.number.PositiveInt
//
//import scala.annotation.tailrec
//import scala.util.{Random, Try}
//
//object Simulator {
//    enum FiringError[TransitionId](id: TransitionId):
//        case TransitionNotEnabled(id: TransitionId) extends FiringError(id)
//        case FireUnsafeFailure(id: TransitionId, error: Throwable) extends FiringError(id)
//
//}
//
///** A simulator needs all data _except_ presentation data.
//  * @tparam Self
//  *   This is F-Bounded polymorphism. It is what enables a recursive type signature. See
//  *   https://www.youtube.com/watch?v=Wki2B6iW1oA
//  */
//trait Simulator[NetId, ArcId, PlaceId, TransitionId, Sim <: Simulator[
//  NetId,
//  ArcId,
//  PlaceId,
//  TransitionId,
//  Sim
//]] extends Net.Id[NetId, ArcId, PlaceId, TransitionId] { self: Sim =>
//
//    lazy val getEnabledTransitions: Set[TransitionId] = transitions.filter(isEnabled)
//    
//    // Making a temporary choice here -- in order to determine whether a transition is enabled, we basically just do a 
//    // `test fire`. This means that `fireTransition` must check _all_ semantics (including arc, place, transition, and
//    // data). In the future, there might be a more structured way to check these one-by-one.
//    final def isEnabled(t: TransitionId): Boolean = fireTransition(t).isRight
//
//    def fireTransition(id: TransitionId): Either[Simulator.FiringError[TransitionId], Sim] 
//
//    // TODO:
//    //   - This currently serializes transitions. Make it pick from a set of non-conflicting transitions
//    //     and execute them simultaneously
//    //   - Return the execution trace log
//    //   - This does not check if the net has cycles. If it does, it will run forever.
//    @tailrec
//    final def autoFireEnabledTransitions(
//        maxSteps: Option[PositiveInt],
//        // TODO: Apparently cats effect also has a random, its probably better to embed that in a simulator monad stack.
//        random: Random = Random(0)
//    ): Sim =
//        if getEnabledTransitions.isEmpty
//        then self
//        else {
//            val index = random.nextInt(getEnabledTransitions.size)
//            val t =
//                getEnabledTransitions.toIndexedSeq(index)
//            val Right(fired) = fireTransition(t) : @unchecked
//            maxSteps match {
//                case None =>
//                    fired.autoFireEnabledTransitions(None, random)
//                case Some(p) if p.convert == 1 => fired
//                case Some(p) =>
//                    fired.autoFireEnabledTransitions(
//                      Some(PositiveInt.unsafeApply(p.convert - 1)),
//                      random
//                    )
//
//            }
//        }
//}
