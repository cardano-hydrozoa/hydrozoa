package hydrozoa.integration.harness

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.DeadLetter
import com.suprnation.actor.event.Debug
import hydrozoa.lib.actor.HydrozoaActorSystem
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** A crash-restart stops a regime manager's whole subtree and then reuses its store, so the stop
  * must be orderly: nothing in the subtree may still be running once it returns, no actor may
  * report its death to a parent that has already gone, and no parent may lose the death of a child
  * it watched.
  */
class SubtreeStopTest extends AnyFunSuite {

    /** Records its name once its `postStop` has finished, after `stopDelay`, and spawns and watches
      * `children` in its `preStart`, as the regime managers do. It takes longer over each message
      * than `stopDelay`, as a busy manager might, so the `Terminated` of the slow child is still in
      * its mailbox when the stop reaches it.
      */
    private final class Node(
        name: String,
        children: List[Node],
        stopped: Ref[IO, List[String]],
        stopDelay: Boolean = false,
    ) extends Actor[IO, Unit] {
        override def preStart: IO[Unit] =
            children.traverse_(child =>
                context.actorOf(child, child.nodeName).flatMap(context.watch(_, ()))
            )
        override def postStop: IO[Unit] =
            IO.sleep(100.millis).whenA(stopDelay) >> stopped.update(name :: _)
        override def receive: Receive[IO, Unit] =
            PartialFunction.fromFunction(_ => IO.sleep(300.millis))
        def nodeName: String = name
    }

    test("stopping a subtree terminates every actor in it before returning, with no dead letters") {
        val (deadLetters, stoppedBeforeReturn) =
            (for
                deadLetters <- Ref[IO].of(List.empty[Any])
                result <- HydrozoaActorSystem
                    .withoutRoot(
                      "subtree-stop",
                      {
                          case Debug(_, _, dl: DeadLetter[?]) => deadLetters.update(dl.message :: _)
                          case _                              => IO.unit
                      }
                    )
                    .use { system =>
                        for
                            stopped <- Ref[IO].of(List.empty[String])
                            // A parent with two children, one slow to stop and one with a child of
                            // its own: the shape of a regime manager with a nested manager.
                            tree = Node(
                              "parent",
                              List(
                                Node("slow", Nil, stopped, stopDelay = true),
                                Node("middle", List(Node("grandchild", Nil, stopped)), stopped),
                              ),
                              stopped,
                            )
                            parent <- system.actorOf(tree, "parent")
                            _ <- SubtreeStop.stopAndAwait(system, parent, within = 10.seconds)
                            stoppedBeforeReturn <- stopped.get
                            // Anything still running in the subtree has long finished by now, and
                            // the event stream's listener has logged what it did.
                            _ <- IO.sleep(1.second)
                            letters <- deadLetters.get
                        yield (letters, stoppedBeforeReturn.toSet)
                    }
            yield result).unsafeRunSync()

        val stillRunning = Set("parent", "slow", "middle", "grandchild") -- stoppedBeforeReturn
        val problems = List(
          // Nothing here sends a message, so a dead letter can only be an actor's death reported
          // to a parent that had already gone (`DeathWatchNotification`), or a death its parent
          // watched for, dropped from the parent's mailbox (`Terminated`).
          Option.when(deadLetters.nonEmpty)(s"dead letters: $deadLetters"),
          Option.when(stillRunning.nonEmpty)(
            s"still running when the stop returned: ${stillRunning.toList.sorted.mkString(", ")}"
          ),
        ).flatten
        assert(problems.isEmpty, problems.mkString("; "))
    }
}
