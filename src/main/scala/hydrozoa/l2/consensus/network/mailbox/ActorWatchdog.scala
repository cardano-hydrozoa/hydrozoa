package hydrozoa.l2.consensus.network.mailbox

import ox.*
import ox.channels.{Actor, ActorRef, BufferCapacity}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Newtype for representing watchdog timeout in the scope.
  */
opaque type WatchdogTimeout = FiniteDuration

object WatchdogTimeout:
    def apply(d: FiniteDuration): WatchdogTimeout = {
        assert(d > 0.seconds, "Watchdog timeouts must be positive")
        d
    }
    given Conversion[WatchdogTimeout, FiniteDuration] = identity

/** An interface that actors with a watchdog timer should provide.
  */
trait Watchdog:
    /** Called with `tell` every [[WatchdogTimeout]] in scope, exceptions must be handled by the
      * wrapped actor
      */
    def wakeUp(): Unit

object ActorWatchdog:
    /** Creates a new actor and watchdog thread that wakes it up every [[WatchdogTimeout]] in scope.
      */
    def create[ErrorType, T <: Watchdog](logic: T, close: Option[T => Unit] = None)(using
        ox: Ox,
        sc: BufferCapacity,
        timeout: WatchdogTimeout
    ): ActorRef[T] =
        val ref = Actor.create(logic, close)
        forkDiscard {
            forever {
                sleep(timeout)
                ref.tell(_.wakeUp())
            }
        }
        ref
    end create
end ActorWatchdog
