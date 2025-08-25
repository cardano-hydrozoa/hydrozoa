package hydrozoa.l2.consensus.network.mailbox

import ox.*
import ox.channels.{Actor, ActorRef, BufferCapacity}

import scala.concurrent.duration.DurationInt

// Newtype for representing watchdog timeout in the scope, seconds.
opaque type WatchdogTimeoutSeconds = Int

object WatchdogTimeoutSeconds:
    def apply(n: Int): WatchdogTimeoutSeconds = {
        assert(n > 0, "Watchdog timeouts must be positive")
        n
    }

extension (self: WatchdogTimeoutSeconds) {
    def toInt: Int = self
}

/** An interface that actors with a watchdog timer should provide.
  */
trait Watchdog:
    /** Called with `tell` every [[WatchdogTimeoutSeconds]] in scope, exceptions are not handled.
      */
    def wakeUp(): Either[Throwable, Unit]

object ActorWatchdog:
    /** Creates a new actor and watchdog thread that wakes it up every [[WatchdogTimeoutSeconds]] in
      * scope.
      */
    def create[ErrorType, T <: Watchdog](logic: T, close: Option[T => Unit] = None)(using
        ox: Ox,
        sc: BufferCapacity,
        timeout: WatchdogTimeoutSeconds
    ): ActorRef[T] =
        val ref = Actor.create(logic, close)
        forkDiscard {
            forever {
                sleep(timeout.toInt.seconds)
                ref.tellDiscard(_.wakeUp())
            }
        }
        ref
    end create
end ActorWatchdog
