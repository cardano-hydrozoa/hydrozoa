package hydrozoa.testcontrol

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.*
import hydrozoa.lib.actor.HydrozoaActorSystem
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop, Properties}
import scala.concurrent.duration.DurationInt

object TestControlScalaCheck extends Properties("TestControl/ScalaCheck") {

    val _ = property("minimal TestControl test") = {
        val program = TestControl.executeEmbed {
            IO.sleep(1.hour) >> IO.realTimeInstant.flatMap(t => IO.println(s"time=$t"))
        }
        program.unsafeRunSync()
        Prop.proved
    }

    val _ = property("minimal TestControl + ActorSystem test") = {
        val program = TestControl.executeEmbed {
            HydrozoaActorSystem.withoutRoot("test-system").use { _ =>
                IO.sleep(1.hour) >> IO.realTimeInstant.flatMap(t => IO.println(s"time=$t"))
            }
        }
        program.unsafeRunSync()
        Prop.proved
    }

    val _ = property("absolute minimum test") = {
        forAll(Gen.const(())) { _ =>
            val program = TestControl.executeEmbed {
                IO.sleep(30000.day) >>
                    HydrozoaActorSystem.withoutRoot("test-system").use { _ =>
                        IO.sleep(1.day) >> IO.realTimeInstant.flatMap(t => IO.println(s"time=$t"))
                    }
            }
            program.unsafeRunSync()
            Prop.proved
        }
    }
}
