package hydrozoa.l2.consensus.network.mailbox

import munit.{FunSuite, ScalaCheckSuite}
import ox.channels.ActorRef
import ox.{sleep, supervised}

import scala.concurrent.duration.DurationInt

class ActorWatchdogSpec extends ScalaCheckSuite:

    test("Watchdog works"):

        class WatchdogCounter extends Watchdog:
            var counter = 0L
            override def wakeUp(): Unit = counter += 1

        supervised {
            given timer: WatchdogTimeoutSeconds = WatchdogTimeoutSeconds(1)
            val actor: ActorRef[WatchdogCounter] = ActorWatchdog.create(WatchdogCounter())

            sleep(2500.millis)
            assertEquals(actor.ask(_.counter), 2L)
        }
