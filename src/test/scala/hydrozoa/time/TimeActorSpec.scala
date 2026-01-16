package hydrozoa.time

import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import com.suprnation.actor.ActorSystem
import java.time.Instant
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration.DurationInt

class TimeActorSpec extends AnyFlatSpec with Matchers {

    "TimeActor" should "use virtual time in TestControl" in {
        val program = TestControl.executeEmbed {
            ActorSystem[IO]("test-system").use { system =>
                for {
                    // Spawn actor inside TestControl
                    actorRef <- system.actorOf(new TimeActor, "time-actor")

                    // Get initial time (should be epoch 0)
                    reply1 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(reply1)
                    _ <- IO.sleep(1.milli) // Let actor process message
                    time1 <- reply1.get

                    _ <- IO(time1.toEpochMilli shouldBe 0)

                    // Advance virtual time by 1 hour
                    _ <- IO.sleep(1.hour)

                    // Get time again
                    reply2 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(reply2)
                    _ <- IO.sleep(1.milli)
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

    "TimeActor" should "handle delayed prints instantly" in {
        val program = TestControl.executeEmbed {
            ActorSystem[IO]("test-system").use { system =>
                for {
                    actorRef <- system.actorOf(new TimeActor, "time-actor")

                    // Tell actor to wait 10 minutes then print
                    _ <- actorRef ! WaitAndPrint(10.minutes)

                    // Advance time (instant in TestControl)
                    _ <- IO.sleep(10.minutes)

                    // Give actor time to process
                    _ <- IO.sleep(10.millis)

                    // Get current time to verify it advanced
                    reply <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(reply)
                    _ <- IO.sleep(1.milli)
                    finalTime <- reply.get

                    _ <- IO {
                        finalTime.toEpochMilli shouldBe 10.minutes.toMillis + 11.millis.toMillis
                    }

                } yield ()
            }
        }

        program.unsafeRunSync()
    }

    "TimeActor" should "handle multiple time queries" in {
        val program = TestControl.executeEmbed {
            ActorSystem[IO]("test-system").use { system =>
                for {
                    actorRef <- system.actorOf(new TimeActor, "time-actor")

                    // Query time at T=0
                    r1 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(r1)
                    _ <- IO.sleep(1.milli)
                    t1 <- r1.get

                    // Advance 30 seconds
                    _ <- IO.sleep(30.seconds)

                    // Query time at T=30s
                    r2 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(r2)
                    _ <- IO.sleep(1.milli)
                    t2 <- r2.get

                    // Advance 45 seconds more
                    _ <- IO.sleep(45.seconds)

                    // Query time at T=75s
                    r3 <- Deferred[IO, Instant]
                    _ <- actorRef ! GetTime(r3)
                    _ <- IO.sleep(1.milli)
                    t3 <- r3.get

                    _ <- IO {
                        t1.toEpochMilli shouldBe 0
                        t2.toEpochMilli shouldBe 30.seconds.toMillis + 1.milli.toMillis
                        t3.toEpochMilli shouldBe 75.seconds.toMillis + 2.millis.toMillis
                    }

                } yield ()
            }
        }

        program.unsafeRunSync()
    }

    "TimeActor" should "print times during test (visible in console)" in {
        val program = TestControl.executeEmbed {
            ActorSystem[IO]("test-system").use { system =>
                for {
                    actorRef <- system.actorOf(new TimeActor, "time-actor")

                    // Print at T=0
                    _ <- actorRef ! PrintTime()
                    _ <- IO.sleep(1.milli)

                    // Advance and print at T=1h
                    _ <- IO.sleep(1.hour)
                    _ <- actorRef ! PrintTime()
                    _ <- IO.sleep(1.milli)

                    // Advance and print at T=2h
                    _ <- IO.sleep(1.hour)
                    _ <- actorRef ! PrintTime()
                    _ <- IO.sleep(1.milli)

                } yield ()
            }
        }

        program.unsafeRunSync()
    }
}
