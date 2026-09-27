package hydrozoa.lib.actor

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.ActorRef.NoSendActorRef
import com.suprnation.actor.{ActorSystem, InternalActorRef}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Stop a set of actors that message one another without losing the messages between them.
  *
  * Stopping an actor turns whatever is still in its mailbox, and whatever reaches it later, into
  * dead letters. The multisig actors message each other in cycles (the block and stack pipelines,
  * and every liaison to and from the consensus actors), so no order of stopping them avoids that.
  * Instead, [[run]] makes the traffic end first:
  *
  *   1. Quiesce: every [[Quiescent]] actor among the targets and their descendants, each before its
  *      children, stops starting new work. A regime manager closes its inputs; an actor that owns a
  *      timer or a fiber cancels it. Every actor goes on handling what arrives.
  *   1. Wait until every actor in the targets' subtrees is idle. With the inputs closed and the
  *      timers gone, the only traffic left is reactions to messages already sent, and it dies out.
  *   1. Stop the subtrees, leaves first ([[SubtreeStop]]).
  *
  * Each phase has a bound, and the next phase starts when it elapses, so a handler that never
  * returns delays shutdown by the bounds and no more. Traffic still in flight then is
  * dead-lettered, as it would be without this.
  */
object OrderlyShutdown:

    /** How long each phase may take. */
    final case class Bounds(quiesce: FiniteDuration, idle: FiniteDuration, stop: FiniteDuration)

    /** What [[run]] achieved.
      *
      * @param quiesced
      *   how many [[Quiescent]] actors were asked to quiesce
      * @param failures
      *   the errors raised by `quiesce` calls, which don't stop the other phases
      * @param quiescedInTime
      *   every `quiesce` call returned within its bound
      * @param idleInTime
      *   every actor in the subtrees was idle before the bound; if not, whatever was still in
      *   flight when they stopped is dead-lettered
      * @param stoppedInTime
      *   every actor in the subtrees terminated before the bound
      */
    final case class Outcome(
        quiesced: Int,
        failures: List[Throwable],
        quiescedInTime: Boolean,
        idleInTime: Boolean,
        stoppedInTime: Boolean,
    ):
        def complete: Boolean = failures.isEmpty && quiescedInTime && idleInTime && stoppedInTime

        /** One line for a log: what was done, and each phase that ran out of time or failed. */
        def describe: String =
            List(
              Some(s"quiesced $quiesced actors"),
              Option.unless(quiescedInTime)("quiescing ran out of time"),
              Option.unless(idleInTime)("not idle in time"),
              Option.unless(stoppedInTime)("not stopped in time"),
            ).flatten.mkString(", ") + failures.map(e => s"; $e").mkString

    /** Quiesce `targets` and everything below them, wait until all of it is idle, then stop it,
      * leaves first. Never fails, and returns within the sum of `bounds`.
      */
    def run(
        system: ActorSystem[IO],
        targets: List[NoSendActorRef[IO]],
        bounds: Bounds,
    ): IO[Outcome] =
        for
            quiesced <- Ref[IO].of(0)
            failures <- Ref[IO].of(List.empty[Throwable])
            failed = (e: Throwable) => failures.update(e :: _).as(false)
            inTime = (phase: IO[Unit], bound: FiniteDuration) =>
                phase.as(true).timeoutTo(bound, IO.pure(false)).handleErrorWith(failed)
            quiescedInTime <- inTime(quiesceAll(targets, quiesced, failures), bounds.quiesce)
            idleInTime <- inTime(awaitIdle(targets), bounds.idle)
            // Enforces the bound itself, failing when it elapses.
            stoppedInTime <- SubtreeStop
                .stopEachAndAwait(system, targets, bounds.stop)
                .as(true)
                .handleErrorWith(failed)
            n <- quiesced.get
            errors <- failures.get
        yield Outcome(n, errors.reverse, quiescedInTime, idleInTime, stoppedInTime)

    /** Consecutive all-idle sweeps that count as idle. A sweep reads one actor after another, so an
      * actor can hand a message to one already read and go idle before it is read itself; one
      * all-idle sweep can therefore miss a message in flight, and several in a row are needed.
      */
    private val QuietSweeps = 3

    private val SweepEvery = 10.millis

    private def quiesceAll(
        refs: List[NoSendActorRef[IO]],
        quiesced: Ref[IO, Int],
        failures: Ref[IO, List[Throwable]],
    ): IO[Unit] =
        refs.parTraverse_(ref =>
            SubtreeStop
                .isTerminated(ref)
                .ifM(
                  IO.unit,
                  actorOf(ref).flatMap {
                      case Some(actor: Quiescent) =>
                          actor.quiesce.attempt.flatMap {
                              case Right(())  => quiesced.update(_ + 1)
                              case Left(fail) => failures.update(fail :: _)
                          }
                      case _ => IO.unit
                  } >> SubtreeStop.childrenOf(ref).flatMap(quiesceAll(_, quiesced, failures))
                )
        )

    private def awaitIdle(targets: List[NoSendActorRef[IO]]): IO[Unit] =
        def loop(quietSweeps: Int): IO[Unit] =
            IO.whenA(quietSweeps < QuietSweeps)(
              IO.sleep(SweepEvery) >> allIdle(targets).flatMap(idle =>
                  loop(if idle then quietSweeps + 1 else 0)
              )
            )
        loop(0)

    private def allIdle(refs: List[NoSendActorRef[IO]]): IO[Boolean] =
        refs.forallM(ref =>
            SubtreeStop
                .isTerminated(ref)
                .ifM(
                  IO.pure(true),
                  isIdle(ref).flatMap(idle =>
                      if idle then SubtreeStop.childrenOf(ref).flatMap(allIdle) else IO.pure(false)
                  )
                )
        )

    private def actorOf(ref: NoSendActorRef[IO]): IO[Option[Any]] = ref match
        case r: InternalActorRef[IO, ?, ?] => r.assertCellActiveAndDo(_.actorOp)
        case other => IO.raiseError(new IllegalArgumentException(s"not a local actor: $other"))

    private def isIdle(ref: NoSendActorRef[IO]): IO[Boolean] = ref match
        case r: InternalActorRef[IO, ?, ?] => r.assertCellActiveAndDo(_.isIdle)
        case other => IO.raiseError(new IllegalArgumentException(s"not a local actor: $other"))
