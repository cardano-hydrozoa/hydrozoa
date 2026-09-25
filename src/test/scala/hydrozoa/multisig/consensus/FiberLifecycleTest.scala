package hydrozoa.multisig.consensus

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, FiberIO, IO, Ref}
import java.nio.file.{Files, Path}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Guards a fiber-lifecycle bug class this codebase keeps reproducing: a long-running fiber stored
  * with `Ref.set`, which drops whatever the slot already held WITHOUT cancelling it. The orphan
  * outlives its purpose — in the `PeerLiaison*` actors it is a `foreverM` that resends for the life
  * of the process.
  *
  * Cancel-on-replace has to be local to the store site. It cannot rest on the actor's own teardown:
  * cats-actors runs `preRestart` (which calls `postStop`) on a restart, but `faultRecreate` SKIPS
  * `preRestart` entirely when the actor is `FailedFatally`, so that ordering is not guaranteed.
  *
  * The first test pins the behaviour the fix depends on, with a control that fails without it. The
  * second is a source guard: this bug is cheap to reintroduce and nearly invisible in review, so
  * the codebase itself is what is worth asserting on.
  */
class FiberLifecycleTest extends AnyFunSuite with Matchers:

    test("Ref.set orphans a previously stored fiber; getAndSet + cancel does not"):
        // `survived` can only be observed if the FIRST fiber outlives replacement -- i.e. only if
        // the buggy pattern leaked it. Without this control the test would assert nothing.
        def scenario(cancelPrevious: Boolean): IO[Boolean] = for {
            slot <- Ref.of[IO, Option[FiberIO[Unit]]](None)
            ticked <- Deferred[IO, Unit]
            first <- (IO.sleep(100.millis) >> ticked.complete(()).attempt.void).foreverM.void.start
            _ <- slot.set(Some(first))
            second <- IO.never[Unit].start
            _ <-
                if cancelPrevious then
                    slot.getAndSet(Some(second)).flatMap(_.fold(IO.unit)(_.cancel))
                else slot.set(Some(second))
            _ <- ticked.tryGet // drain any pre-replacement tick
            survived <- IO.sleep(400.millis) >> ticked.tryGet.map(_.isDefined)
            _ <- second.cancel >> first.cancel
        } yield survived

        val buggy = scenario(cancelPrevious = false).unsafeRunSync()
        val fixed = scenario(cancelPrevious = true).unsafeRunSync()
        withClue("control: `set` must leave the old fiber running, else this test proves nothing"):
            val _ = buggy shouldBe true
        withClue("fix: replacing via getAndSet must cancel the previous fiber"):
            fixed shouldBe false

    test("no long-running fiber is stored with Ref.set outside teardown"):
        // `<something>Fiber.set(Some(...))` is the exact shape of the bug. Teardown paths use
        // `getAndSet(None)`, which this deliberately does not match.
        val root = Path.of("src", "main", "scala")
        val offenders = Files
            .walk(root)
            .iterator
            .asScala
            .filter(p => p.toString.endsWith(".scala"))
            .flatMap { p =>
                Files
                    .readAllLines(p)
                    .asScala
                    .filter(l => l.matches(""".*[Ff]iber\.set\(Some.*"""))
                    .map(l => s"$p: ${l.trim}")
            }
            .toList
        withClue(s"stored without cancelling the previous fiber: ${offenders.mkString("; ")}"):
            offenders shouldBe empty
