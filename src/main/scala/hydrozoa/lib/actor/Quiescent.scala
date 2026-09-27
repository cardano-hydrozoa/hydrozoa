package hydrozoa.lib.actor

import cats.effect.IO

/** An actor that must stop starting new work before it is stopped: one that owns a timer or a
  * fiber, or a manager whose inputs must close first. [[OrderlyShutdown]] finds these by walking
  * the actors it is about to stop and calls [[quiesce]] on each.
  */
trait Quiescent:

    /** Stop originating new work, and keep handling what arrives. Called from outside the actor, on
      * another fiber, so it may only touch thread-safe state and send messages. An actor that owns
      * a timer or a fiber sends itself [[Quiesce]] and cancels them in its handler, so they are
      * cancelled after whatever was already in its mailbox.
      */
    def quiesce: IO[Unit]

/** Sent by a [[Quiescent]] actor to itself. From its handler on, the actor arms no timer and starts
  * no fiber, and treats a timer's message still in its mailbox as a no-op.
  */
case object Quiesce
