package hydrozoa.l2.consensus.network.mailbox

import munit.{FunSuite, ScalaCheckSuite}
import ox.channels.ActorRef
import ox.{sleep, supervised}

import scala.concurrent.duration.DurationInt

class ActorWatchdogSpec extends ScalaCheckSuite:

    class WatchdogCounter extends Watchdog:
        private var counter = 0L
        override def wakeUp() = counter = counter + 1
        def getCounter: Long = counter

    test("Watchdog works"):

        supervised {
            given timer: WatchdogTimeoutSeconds = WatchdogTimeoutSeconds(1)
            val actor: ActorRef[WatchdogCounter] = ActorWatchdog.create(WatchdogCounter())

            sleep(2500.millis)
            assertEquals(actor.ask(_.getCounter), 2L)
        }
