package hydrozoa.lib.actor

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.DeadLetter
import com.suprnation.actor.event.Debug
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.lib.logging.Level
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.DurationInt

/** What a hydrozoa process relies on its actor system for: any actor failure, of any kind, stops
  * the system within a bound and hands back the failure; and the system's events are logged, not
  * printed.
  */
class HydrozoaActorSystemTest extends AnyFunSuite {

    /** Throws whatever it is sent. */
    private final class Thrower extends Actor[IO, Throwable] {
        override def receive: Receive[IO, Throwable] = { case t => IO.raiseError(t) }
    }

    /** A parent with the regime managers' rules, whose one child throws whatever it is sent. */
    private final class EscalatingParent extends Actor[IO, Throwable] {
        override def supervisorStrategy = HydrozoaActorSystem.escalateAll
        override def receive: Receive[IO, Throwable] = { case t =>
            context.actorOf(new Thrower, "thrower").flatMap(_ ! t)
        }
    }

    /** Send `thrown` to a fresh actor under the root, or under an escalating parent under the root,
      * and return the failure the system reports once it has stopped, failing if it doesn't stop
      * within five seconds.
      */
    private def failureOf(thrown: Throwable, nested: Boolean): Option[Throwable] =
        HydrozoaActorSystem("failure-test")
            .use { actors =>
                for
                    ref <-
                        if nested then actors.actorOf(new EscalatingParent, "parent")
                        else actors.actorOf(new Thrower, "thrower")
                    _ <- ref ! thrown
                    failure <- actors.waitForTermination.timeout(5.seconds)
                yield failure
            }
            .unsafeRunSync()

    // `Error`s and bare `Throwable`s are what the user guardian's rules miss: with those, a failure
    // left the JVM running with a dead actor system. `new Throwable` is the case the lint rule
    // forbids in our own code; a library can still throw one.
    private val kinds: List[(String, () => Throwable)] = List(
      "a RuntimeException" -> (() => new RuntimeException("boom")),
      "an AssertionError, as from `assert`" -> (() => new AssertionError("boom")),
      "a NotImplementedError, as from `???`" -> (() => new NotImplementedError("boom")),
      "a bare Throwable" -> (() => new Throwable("boom")) // scalafix:ok
    )

    for (kind, make) <- kinds; nested <- List(false, true) do {
        val where = if nested then "a grandchild of the root" else "a child of the root"
        test(s"$kind thrown by $where stops the system and is reported") {
            val thrown = make()
            assert(failureOf(thrown, nested).contains(thrown))
        }
    }

    test("a system stopped without a failure reports none") {
        val failure = HydrozoaActorSystem("stop-test")
            .use(actors =>
                actors.system.terminate(None) >> actors.waitForTermination.timeout(5.seconds)
            )
            .unsafeRunSync()
        assert(failure.isEmpty)
    }

    test("the root's children are under the guardian, where waitForIdle looks for actors") {
        val paths = HydrozoaActorSystem("idle-test")
            .use(actors =>
                actors.actorOf(new Thrower, "child") >>
                    actors.system.allChildren.map(_.map(_.path.toString))
            )
            .unsafeRunSync()
        assert(paths.exists(_.endsWith("/user/hydrozoa/child")), paths)
    }

    test("a dead letter is a WARN while the system runs, and names its message and recipient") {
        val (event, logged) = (for
            seen <- Deferred[IO, Any]
            result <- HydrozoaActorSystem
                .withoutRoot(
                  "dead-letter-test",
                  onEvent = {
                      case e @ Debug(_, _, _: DeadLetter[?]) => seen.complete(e).void
                      case _                                 => IO.unit
                  }
                )
                .use(system =>
                    for
                        ref <- system.actorOf(new Thrower, "stopped")
                        _ <- ref.stop
                        _ <- IO.sleep(200.millis)
                        _ <- ref ! new RuntimeException("late")
                        event <- seen.get.timeout(5.seconds)
                    yield (event, ActorSystemEvents.toLogEvent(event, stopping = false))
                )
        yield result).unsafeRunSync()
        val _ = assert(logged.level == Level.Warn)
        val _ = assert(logged.routingKey.contains(ActorSystemEvents.DeadLetterLogger))
        val message = logged.render.value.msg
        val _ = assert(message.contains("java.lang.RuntimeException to "), message)
        val _ = assert(message.contains("/user/stopped"), message)
        assert(ActorSystemEvents.toLogEvent(event, stopping = true).level == Level.Debug)
    }
}
