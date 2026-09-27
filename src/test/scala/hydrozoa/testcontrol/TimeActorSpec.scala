package hydrozoa.testcontrol

import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.suprnation.typelevel.actors.syntax.*
import hydrozoa.lib.actor.HydrozoaActorSystem
import java.time.Instant
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration.DurationInt

class TimeActorSpec extends AnyFlatSpec with Matchers {

    "TimeActor" should "use virtual time in TestControl" in {
        val program = TestControl.executeEmbed {
            HydrozoaActorSystem.withoutRoot("test-system").use { system =>
                for {
                    // Spawn actor inside TestControl
                    actorRef <- system.actorOf(new TimeActor, "time-actor")
                    _ <- system.actorOf(new TimeActor, "time-actor_")

                    // Get initial time (should be epoch 0)
                    reply1 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(reply1)
                    time1 <- reply1.get

                    _ <- IO(time1.toEpochMilli shouldBe 0)

                    // Advance virtual time by 1 hour
                    _ <- IO.sleep(1.hour)

                    // Get time again
                    reply2 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(reply2)
                    time2 <- reply2.get

                    _ <- IO {
                        val elapsed = time2.toEpochMilli - time1.toEpochMilli
                        elapsed shouldBe 1.hour.toMillis
                    }

                } yield ()
            }
        }
        program.unsafeRunSync() // Executes instantly!
    }

    "TimeActor" should "use virtual time in TestControl with ActorSystem.use" in {
        val program = TestControl.executeEmbed {
            HydrozoaActorSystem.withoutRoot("test-system").use { system =>
                for {
                    // Spawn actor inside TestControl
                    actorRef <- system.actorOf(new TimeActor, "time-actor")
                    _ <- system.actorOf(new TimeActor, "time-actor_")

                    // Get initial time (should be epoch 0)
                    reply1 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(reply1)
                    time1 <- reply1.get

                    _ <- IO(time1.toEpochMilli shouldBe 0)

                    // Advance virtual time by 1 hour
                    _ <- IO.sleep(1.hour)

                    // Get time again
                    reply2 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(reply2)
                    time2 <- reply2.get

                    _ <- IO {
                        val elapsed = time2.toEpochMilli - time1.toEpochMilli
                        elapsed shouldBe 1.hour.toMillis
                    }

                } yield ()
            }
        }

        program.unsafeRunSync() // Executes instantly!
    }

    "TimeActor" should "handle delayed work instantly" in {
        val program = TestControl.executeEmbed {
            HydrozoaActorSystem.withoutRoot("test-system").use { system =>
                for {
                    actorRef <- system.actorOf(new TimeActor, "time-actor")

                    // Tell actor to wait 20 minutes
                    _ <- actorRef ! Wait(20.minutes)

                    // Get current time to verify it advanced
                    reply <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(reply)
                    // _ <- IO.sleep(1.milli)
                    finalTime <- reply.get

                    _ <- IO {
                        finalTime.toEpochMilli shouldBe 20.minutes.toMillis
                    }

                } yield ()
            }
        }

        program.unsafeRunSync()
    }

    "TimeActor" should "handle multiple time queries" in {
        val program = TestControl.executeEmbed {
            HydrozoaActorSystem.withoutRoot("test-system").use { system =>
                for {
                    actorRef <- system.actorOf(new TimeActor, "time-actor")

                    // Query time at T=0
                    r1 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(r1)
                    t1 <- r1.get

                    // Advance 30 seconds
                    _ <- IO.sleep(30.seconds)

                    // Query time at T=30s
                    r2 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(r2)
                    t2 <- r2.get

                    // Advance 45 seconds more
                    _ <- IO.sleep(45.seconds)

                    // Query time at T=75s
                    r3 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(r3)
                    t3 <- r3.get

                    _ <- (
                      IO { t1.toEpochMilli shouldBe 0 },
                      IO { t2.toEpochMilli shouldBe 30.seconds.toMillis },
                      IO { t3.toEpochMilli shouldBe 75.seconds.toMillis }
                    ).sequence

                } yield ()
            }
        }

        program.unsafeRunSync()
    }

    "TimeActor" should "record the virtual time at each request" in {
        val program = TestControl.executeEmbed {
            HydrozoaActorSystem.withoutRoot("test-system").use { system =>
                for {
                    actorRef <- system.actorOf(new TimeActor, "time-actor")
                    recorded <- Ref[IO].of(Vector.empty[Instant])

                    // Record at T=0
                    _ <- actorRef ! RecordTime(recorded)
                    _ <- IO.sleep(1.milli)

                    // Advance and record at T=1h
                    _ <- IO.sleep(1.hour)
                    _ <- actorRef ! RecordTime(recorded)

                    // Advance and record at T=2h
                    _ <- IO.sleep(1.hour)
                    _ <- actorRef ! RecordTime(recorded)

                    // This requires `import com.suprnation.typelevel.actors.syntax.*`
                    _ <- system.waitForIdle()
                    times <- recorded.get
                    _ <- IO {
                        times.map(_.toEpochMilli) shouldBe Vector(
                          0L,
                          (1.milli + 1.hour).toMillis,
                          (1.milli + 2.hours).toMillis
                        )
                    }
                } yield ()
            }
        }

        program.unsafeRunSync()
    }
}
