
● Here's the bug report:

  ---
  TerminateActorSystem hangs when an actor raises a Throwable that is not an Exception

  cats-actors version: 2.0.1
  cats-effect version: 3.x

  Summary

  When an actor created via system.actorOf raises a Throwable that does not extend Exception (e.g. a custom sealed trait Foo extends Throwable), system.waitForTermination never resolves. The system hangs instead of terminating.

  Root cause

  TerminateActorSystem — the supervision strategy installed on UserGuardian — inherits defaultDecider unchanged:

  // FaultHandling.scala
  final val defaultDecider: Decider = {
      case _: ActorInitializationException[?] => Stop
      case _: ActorKilledException            => Stop
      case _: DeathPactException[?]           => Stop
      case _: Exception                       => Restart   // only matches Exception subtypes
      // falls through to:
  }
  final val escalateDefault: Decider = (_: Throwable) => Escalate

  A Throwable that is not an Exception matches escalateDefault and produces Escalate. SupervisionStrategy.handleFailure then returns false, which causes FaultHandling.handleFailure to re-raise the cause via Concurrent[F].raiseError(f.cause).

  systemInvoke.recoverWith catches this and calls handleInvokeFailure(Nil, cause) on the guardian itself. Inside handleInvokeFailure, the guardian tries to send Failed to its parent:

  // ActorCell.scala line 104
  @inline override def parent: NoSendActorRef[F] = _parent.get  // _parent = None for guardian

  _parent.get throws NoSuchElementException: None.get. This cascades into the "emergency stop" fallback path in handleInvokeFailure.recoverWith, which calls finishTerminate on the guardian but never completes systemShutdownHook. Consequently, system.waitForTermination blocks
  indefinitely.

  The fix should be in TerminateActorSystem itself — it should override decider to catch all Throwable (not just Exception), since processFailure already ignores the restart flag and always calls system.terminate():

  // Suggested fix:
  case class TerminateActorSystem[F[+_]: Concurrent: Parallel](system: ActorSystem[F])
      extends SupervisionStrategy[F]:
    override def decider: Decider = { case _: Throwable => Restart }  // catch all, not just Exception
    override def processFailure(...): F[Unit] =
      Concurrent[F].pure(cause.get.printStackTrace()) >> system.terminate(cause)
    ...

  Minimal verifiable example

  import cats.effect.*
  import cats.effect.std.Console
  import com.suprnation.actor.Actor.Actor
  import com.suprnation.actor.ActorSystem

  import scala.concurrent.duration.DurationInt

  object CatsActorsBug extends IOApp.Simple:

    // A Throwable that is NOT an Exception — this is the trigger.
    case object MyError extends Throwable

    val run: IO[Unit] =
      ActorSystem[IO]("bug-demo").use { system =>
        val failingActor: Actor[IO, Unit] = new Actor[IO, Unit]:
          override def receive = { case () => IO.raiseError(MyError) }

        for
          ref <- system.actorOf(failingActor)
          _   <- ref ! ()        // trigger MyError
          // Expected: resolves quickly once the actor fails and guardian terminates the system
          // Actual:   hangs indefinitely (until killed externally)
          _   <- system.waitForTermination.timeoutTo(
                   5.seconds,
                   Console[IO].println("BUG: system did not terminate after actor raised Throwable")
                 )
        yield ()
      }

  Expected output: system terminates within a second of the actor receiving ().

  Actual output (with timeoutTo added to avoid hanging the process):
  [EventBus] => Error(java.util.NoSuchElementException: None.get,
    kukku://bug-demo@localhost/user,
    class com.suprnation.actor.ActorSystem$UserGuardian,
    emergency stop: exception in failure handling for class CatsActorsBug$MyError$
    ...)
  BUG: system did not terminate after actor raised Throwable

  Workaround

  Ensure all custom actor error types extend Exception rather than Throwable directly. defaultDecider then maps them to Restart, which routes through TerminateActorSystem.processFailure and correctly calls system.terminate().

  // Before (broken):
  sealed trait Unrecoverable extends Throwable

  // After (workaround):
  sealed trait Unrecoverable extends Exception

✻ Cogitated for 47s

