package hydrozoa.lib.actor

import cats.effect.{Deferred, IO}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.{ActorSystem, InternalActorRef, PoisonPill}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Stop an actor's whole subtree, children before parents, and wait until all of it has terminated.
  *
  * cats-actors (2.1.0, and upstream `main` as of 2.2.0) does not do this for us. A stopped actor
  * sends `Terminate` to each of its children, but through the child's ref (`ActorRef.stop`) rather
  * than the cell's `stop(child)` (`FaultHandling.terminate`). Only the latter marks the child as
  * dying, so the parent never waits for any child: it runs its own `postStop`, retires its mailbox
  * to dead letters, and tells its watchers it has terminated at once, while its children are still
  * running. Each child's `DeathWatchNotification` to its parent then lands in dead letters, and
  * whoever watched the parent carries on while the subtree is still doing work.
  *
  * Stopping the leaves first gives every actor a live parent to report its death to, so no death
  * reaches dead letters, and the watch on the subtree's root fires only once nothing below it is
  * running. Messages the subtree's actors were still sending one another, or that arrive from
  * outside it, can still reach an actor that has stopped: a stop can't prevent those.
  */
object SubtreeStop:

    /** Stop `ref` and its subtree, children first, and wait until every actor in it has terminated.
      * Fails, rather than hangs, if that takes longer than `within`.
      */
    def stopAndAwait(
        system: ActorSystem[IO],
        ref: NoSendActorRef[IO],
        within: FiniteDuration = 1.minute,
    ): IO[Unit] =
        stopEachAndAwait(system, List(ref), within)

    /** Stop each of `refs` and its subtree, children first, and wait until every actor in them has
      * terminated. Siblings are stopped together, as cats-actors stops a parent's children. Fails,
      * rather than hangs, if that takes longer than `within`.
      */
    def stopEachAndAwait(
        system: ActorSystem[IO],
        refs: List[NoSendActorRef[IO]],
        within: FiniteDuration,
    ): IO[Unit] =
        refs
            .parTraverse(ref => stopDescendants(system, ref).tupleLeft(ref))
            .flatMap(stopAllAndAwait(system, _))
            .timeoutTo(
              within,
              IO.raiseError(
                new IllegalStateException(
                  s"${refs.mkString(", ")} did not terminate within $within"
                )
              )
            )

    /** Stop everything below `ref`, which keeps running, with no children once this returns.
      * Returns whether it had any.
      *
      * An actor's children are stopped together, as cats-actors stops them: watches on all of them
      * first, then every stop at once. Stopping them one after another would give those still
      * running longer to send to those already gone. Children spawned while their siblings stop (a
      * handoff can spawn one) are stopped in a further round.
      */
    private def stopDescendants(system: ActorSystem[IO], ref: NoSendActorRef[IO]): IO[Boolean] =
        childrenOf(ref).flatMap {
            case Nil => IO.pure(false)
            case children =>
                children
                    .parTraverse(child => stopDescendants(system, child).tupleLeft(child))
                    .flatMap(stopAllAndAwait(system, _)) >>
                    awaitDeathsTakenIn(ref, children.toSet) >>
                    stopDescendants(system, ref).as(true)
        }

    /** Stop each of `refs`, whose children have all terminated, and wait for each one's death.
      *
      * A leaf is stopped at once, with `Terminate`, which drops whatever work it still had queued,
      * as a crash would. A parent is stopped with `PoisonPill`, which it takes in turn after what
      * is already in its mailbox: that is where the `Terminated` of each child it watched is
      * waiting (see [[awaitDeathsTakenIn]]), and a `Terminate` would drop those into dead letters.
      */
    private def stopAllAndAwait(
        system: ActorSystem[IO],
        refs: List[(NoSendActorRef[IO], Boolean)],
    ): IO[Unit] =
        for
            live <- refs.filterA((ref, _) => isTerminated(ref).map(!_))
            deaths <- live.traverse((ref, _) => watch(system, ref))
            _ <- live.traverse_((ref, isParent) => if isParent then ref !* PoisonPill else ref.stop)
            _ <- deaths.sequence_
        yield ()

    /** Wait until `parent` has taken in the deaths of `children`, which have all terminated.
      *
      * A child's death reaches its parent as a system message, and only when the parent handles it
      * does it drop the child and queue the child's `Terminated`, if it watched it, as an ordinary
      * message. A stop sent to the parent before then would overtake those `Terminated`s. The
      * parent handles them between messages, so this waits in short steps rather than on a signal:
      * cats-actors gives no other way to see that a parent has caught up.
      */
    private def awaitDeathsTakenIn(
        parent: NoSendActorRef[IO],
        children: Set[NoSendActorRef[IO]],
    ): IO[Unit] =
        childrenOf(parent).flatMap(current =>
            IO.whenA(current.exists(children.contains))(
              IO.sleep(1.millis) >> awaitDeathsTakenIn(parent, children)
            )
        )

    /** Watch `ref`, and return what waits for its death. The watch is placed before this returns
      * (the watcher's `preStart` runs inside `actorOf`), so a stop sent after it cannot be missed.
      *
      * cats-actors never answers a watch that reaches an actor already terminating: the watch goes
      * to dead letters and nothing fires. Callers skip actors already terminated, so that is left
      * to an actor that stops itself in the instant between that check and this watch, and the
      * caller's timeout reports it.
      */
    private def watch(system: ActorSystem[IO], ref: NoSendActorRef[IO]): IO[IO[Unit]] =
        for
            terminated <- Deferred[IO, Unit]
            _ <- system.actorOf(new Actor[IO, Unit] {
                override def preStart: IO[Unit] = context.watch(ref, ()).void
                override def receive: Receive[IO, Unit] =
                    PartialFunction.fromFunction(_ => terminated.complete(()) >> context.self.stop)
            })
        yield terminated.get

    private[actor] def childrenOf(ref: NoSendActorRef[IO]): IO[List[NoSendActorRef[IO]]] = ref match
        case r: InternalActorRef[IO, ?, ?] => r.assertCellActiveAndDo(_.children)
        case other => IO.raiseError(new IllegalArgumentException(s"not a local actor: $other"))

    private[actor] def isTerminated(ref: NoSendActorRef[IO]): IO[Boolean] = ref match
        case r: InternalActorRef[IO, ?, ?] => r.assertCellActiveAndDo(_.isTerminated)
        case other => IO.raiseError(new IllegalArgumentException(s"not a local actor: $other"))
