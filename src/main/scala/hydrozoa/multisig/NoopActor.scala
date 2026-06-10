package hydrozoa.multisig

import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}

/** An actor behavior that silently discards every message it receives.
  *
  * Fills a shared `Connections` slot a node never routes to — e.g. a coil peer's `RequestSequencer`
  * slot: a coil peer authors no user requests, so the reused actors resolve the field but nothing
  * is ever sent to it. Spawning this is lighter than running a real actor purely to occupy the
  * slot.
  */
object NoopActor {

    /** A fresh no-op behavior for message type `R`, to hand to `actorOf`. */
    def apply[R]: Actor[IO, R] = new Actor[IO, R] {
        override def receive: Receive[IO, R] = PartialFunction.fromFunction((_: R) => IO.unit)
    }
}
