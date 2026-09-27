package hydrozoa.testcontrol

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.*
import hydrozoa.lib.actor.HydrozoaActorSystem
import java.time.Instant
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Properties}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object TestControlScalaCheck extends Properties("TestControl/ScalaCheck") {

    // TestControl's virtual clock starts at the epoch and advances only by the program's sleeps.
    private def after(elapsed: FiniteDuration): Instant =
        Instant.EPOCH.plusMillis(elapsed.toMillis)

    val _ = property("minimal TestControl test") = {
        val program = TestControl.executeEmbed {
            IO.sleep(1.hour) >> IO.realTimeInstant
        }
        val now = program.unsafeRunSync()
        (now == after(1.hour)) :| s"virtual time after a 1-hour sleep was $now"
    }

    val _ = property("minimal TestControl + ActorSystem test") = {
        val program = TestControl.executeEmbed {
            HydrozoaActorSystem.withoutRoot("test-system").use { _ =>
                IO.sleep(1.hour) >> IO.realTimeInstant
            }
        }
        val now = program.unsafeRunSync()
        (now == after(1.hour)) :| s"virtual time after a 1-hour sleep was $now"
    }

    val _ = property("absolute minimum test") = {
        forAll(Gen.const(())) { _ =>
            val program = TestControl.executeEmbed {
                IO.sleep(30000.day) >>
                    HydrozoaActorSystem.withoutRoot("test-system").use { _ =>
                        IO.sleep(1.day) >> IO.realTimeInstant
                    }
            }
            val now = program.unsafeRunSync()
            (now == after(30001.days)) :| s"virtual time after 30001 days of sleeps was $now"
        }
    }
}
