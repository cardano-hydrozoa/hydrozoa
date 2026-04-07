package hydrozoa.testcontrol

import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.*
import com.suprnation.actor.ActorSystem
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
            ActorSystem[IO]("test-system").use { system =>
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
                    ActorSystem[IO]("test-system").use { system =>
                        IO.sleep(1.day) >> IO.realTimeInstant.flatMap(t => IO.println(s"time=$t"))
                    }
            }
            program.unsafeRunSync()
            Prop.proved
        }
    }
}
