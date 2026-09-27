package hydrozoa.lib.actor

import cats.effect.{Deferred, IO, Ref, Resource}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.{ActorRef, NoSendActorRef}
import com.suprnation.actor.SupervisorStrategy.{Decider, Escalate, Stop}
import com.suprnation.actor.event.{Debug, Error as ActorError, Info, Warning}
import com.suprnation.actor.utils.IdGen
import com.suprnation.actor.{ActorContext, ActorSystem, ChildRestartStats, DeadLetter, Envelope, EnvelopeWithDeferred, OneForOneStrategy, SupervisionStrategy}
import hydrozoa.lib.logging.{Level, LogEvent, Slf4jTracer}
import scala.collection.immutable
import scala.concurrent.duration.DurationInt

/** The actor system a hydrozoa process runs its actors in: cats-actors' own, with two changes.
  *
  *   - Every actor this process starts is a child of a root of ours, `/user/hydrozoa`, which turns
  *     any failure that reaches it into a shutdown of the whole system, and keeps the failure. The
  *     user guardian's rules (cats-actors' `TerminateActorSystem`) cover only `Exception`s:
  *     anything else escalates from the guardian to a parent it doesn't have, throws `None.get`
  *     inside the failure handling, and leaves a live JVM with a dead actor system. Ours cover
  *     every `Throwable`, and never escalate. The root sits under the guardian rather than beside
  *     it so that everything walking the guardian's subtree, `waitForIdle` included, still sees our
  *     actors.
  *   - The system's event stream goes to SLF4J (see [[ActorSystemEvents]]) instead of stdout.
  *   - Releasing it stops the actors in order first ([[shutdown]]). cats-actors' own release stops
  *     them all at once and runs no actor's `postStop`.
  *
  * Start actors with [[actorOf]], not `system.actorOf`, so their failures reach the root.
  */
final class HydrozoaActorSystem private (
    val system: ActorSystem[IO],
    rootContext: ActorContext[IO, Any, Any],
    failure: Deferred[IO, Throwable],
    expected: ActorSystemEvents.Expected,
    orderlyShutdown: IO[Unit],
):

    /** Start `props` as a child of the root. */
    def actorOf[Request](props: => Actor[IO, Request], name: => String): IO[ActorRef[IO, Request]] =
        rootContext.actorOf(props, name)

    /** Start `props` as a child of the root, under a generated name. */
    def actorOf[Request](props: => Actor[IO, Request]): IO[ActorRef[IO, Request]] =
        rootContext.actorOf(props)

    /** Start the actor `props` builds as a child of the root, under `name` if given. */
    def actorOf[Request](
        props: IO[Actor[IO, Request]],
        name: => String = IdGen.newId()
    ): IO[ActorRef[IO, Request]] =
        rootContext.actorOf(props, name)

    /** Wait until the system terminates, then return the failure that terminated it, if one did. A
      * system released without a failure, as on SIGTERM, returns `None`.
      */
    def waitForTermination: IO[Option[Throwable]] =
        system.waitForTermination >> failure.tryGet

    /** For a test that stops an actor on purpose while messages may still reach it, as a simulated
      * crash does, or a negative control: a message to `ref` or its subtree from now on is expected
      * to go nowhere, so its dead letter is logged at DEBUG as stopped on purpose, not counted as
      * lost while the system runs.
      */
    def expectDeadLetters(ref: NoSendActorRef[IO]): IO[Unit] =
        expected.stoppedOnPurpose.update(_ + ref.path.toString)

    /** Stop every actor under the root with [[OrderlyShutdown]]: quiesce them, wait until they are
      * idle, then stop them, leaves first, within [[ShutdownBounds]] in all. Runs once: later
      * calls, including the one releasing this system makes, wait for the first. Does nothing if
      * the system has already terminated.
      *
      * Releasing the system runs it, so call it only when something else must outlive the actors: a
      * transport their liaisons use, or a store they write to, released before the system.
      */
    def shutdown: IO[Unit] = orderlyShutdown

object HydrozoaActorSystem:

    /** The root's name; its path is `/user/hydrozoa`. */
    val RootName: String = "hydrozoa"

    /** [[HydrozoaActorSystem.shutdown]]'s bounds. Their sum must stay well inside the time a signal
      * gives the whole process to exit (`Main.runtimeConfig`), which also has to close the
      * transports and the store.
      */
    val ShutdownBounds: OrderlyShutdown.Bounds =
        OrderlyShutdown.Bounds(quiesce = 3.seconds, idle = 5.seconds, stop = 5.seconds)

    /** Rules for an actor whose children's failures must reach the root: escalate every one. */
    def escalateAll: SupervisionStrategy[IO] =
        OneForOneStrategy[IO](maxNrOfRetries = 0, withinTimeRange = 1.minute)(
          PartialFunction.fromFunction(_ => Escalate)
        )

    /** A system with the root, whose events go to SLF4J and then to `onEvent`. `onEvent` is for a
      * test harness that must see every event: the system's listener is the only reader of its
      * event stream, so a second reader would split the events between the two.
      */
    def apply(
        name: String,
        onEvent: Any => IO[Unit] = _ => IO.unit,
    ): Resource[IO, HydrozoaActorSystem] =
        for
            built <- logged(name, onEvent)
            (system, expected) = built
            failure <- Resource.eval(Deferred[IO, Throwable])
            started <- Resource.eval(Deferred[IO, ActorContext[IO, Any, Any]])
            _ <- Resource.eval(system.actorOf(Root(failure, started), RootName))
            rootContext <- Resource.eval(started.get)
            shutdown <- Resource.eval(orderlyShutdown(system, rootContext.self, expected).memoize)
            // Acquired after the root, so released before the system: its actors stop in order
            // while the system still runs.
            _ <- Resource.onFinalize(shutdown)
        yield new HydrozoaActorSystem(system, rootContext, failure, expected, shutdown)

    /** Stop the root's children in order. The system counts as stopping from the start, so a dead
      * letter this cannot prevent is logged as a shutdown's dead letters always were.
      */
    private def orderlyShutdown(
        system: ActorSystem[IO],
        root: NoSendActorRef[IO],
        expected: ActorSystemEvents.Expected,
    ): IO[Unit] =
        system.isTerminated.ifM(
          IO.unit,
          expected.stopping.set(true) >>
              IO.race(
                system.waitForTermination,
                SubtreeStop
                    .childrenOf(root)
                    .flatMap(OrderlyShutdown.run(system, _, ShutdownBounds))
              ).flatMap {
                  case Right(outcome) => Slf4jTracer.sink.traceWith(shutdownLogEvent(outcome))
                  case Left(())       => IO.unit
              }
        )

    private def shutdownLogEvent(outcome: OrderlyShutdown.Outcome): LogEvent =
        LogEvent(
          if outcome.complete then Level.Info else Level.Warn,
          s"stopped the actors in order: ${outcome.describe}",
          cause = outcome.failures.headOption,
          routingKey = Some(ActorSystemEvents.SystemLogger)
        )

    /** A plain cats-actors system whose events go to SLF4J and then to `onEvent`, with no root: for
      * tests of single actors that want the system's own guardian.
      */
    def withoutRoot(
        name: String,
        onEvent: Any => IO[Unit] = _ => IO.unit,
    ): Resource[IO, ActorSystem[IO]] =
        logged(name, onEvent).map(_._1)

    private def logged(
        name: String,
        onEvent: Any => IO[Unit],
    ): Resource[IO, (ActorSystem[IO], ActorSystemEvents.Expected)] =
        for
            expected <- Resource.eval(ActorSystemEvents.Expected())
            // Uncancelable, `onEvent` first: the system cancels its listener as it terminates, which
            // is exactly when the failure that terminated it is being handled here.
            system <- ActorSystem[IO](
              name,
              event => (onEvent(event) >> ActorSystemEvents.log(expected)(event)).uncancelable
            )
            // Acquired after the system, so released before it: a dead letter logged from here on
            // was in flight when the system was told to stop.
            _ <- Resource.onFinalize(expected.stopping.set(true))
        yield (system, expected)

    /** The root. It receives nothing: everything it does, it does as its children's supervisor.
      * Having no parent to escalate to, it must never fail itself, so its handler is total and does
      * nothing.
      */
    private final class Root(
        failure: Deferred[IO, Throwable],
        started: Deferred[IO, ActorContext[IO, Any, Any]],
    ) extends Actor[IO, Any]:
        override def preStart: IO[Unit] = started.complete(context).void

        override def supervisorStrategy: SupervisionStrategy[IO] = StopSystemOnFailure(failure)

        override def receive: Receive[IO, Any] = PartialFunction.fromFunction(_ => IO.unit)

    /** Any failure of a child stops the system: publish it as cats-actors does (so the event
      * stream, and a test harness reading it, see the failure), keep the first one for
      * [[HydrozoaActorSystem.waitForTermination]], and terminate. It never escalates, and never
      * throws: if recording the failure fails, the system still terminates.
      */
    private final class StopSystemOnFailure(failure: Deferred[IO, Throwable])
        extends SupervisionStrategy[IO]:

        // Consulted by nothing here: `handleFailure` is overridden. Total, and never `Escalate`,
        // in case cats-actors starts consulting it elsewhere.
        override def decider: Decider = PartialFunction.fromFunction(_ => Stop)

        override def handleFailure(
            context: ActorContext[IO, ?, ?],
            child: NoSendActorRef[IO],
            cause: Throwable,
            stats: ChildRestartStats[IO],
            children: immutable.List[ChildRestartStats[IO]]
        ): IO[Boolean] =
            (context.system.eventStream.offer(
              ActorError(
                cause,
                child.path.toString,
                getClass,
                s"[Message: ${cause.getMessage}] stopping the actor system"
              )
            ) >> failure.complete(cause).void)
                .guarantee(context.system.terminate(Some(cause)))
                .attempt
                .as(true)

        override def processFailure(
            context: ActorContext[IO, ?, ?],
            restart: Boolean,
            child: NoSendActorRef[IO],
            cause: Option[Throwable],
            stats: ChildRestartStats[IO],
            children: immutable.List[ChildRestartStats[IO]]
        ): IO[Unit] = IO.unit

        override def handleChildTerminated(
            context: ActorContext[IO, ?, ?],
            child: NoSendActorRef[IO],
            children: Iterable[NoSendActorRef[IO]]
        ): IO[Unit] = IO.unit

/** An actor system's events, as SLF4J log lines.
  *
  * cats-actors' default listener prints every event to stdout, dead letters included, at a few
  * hundred characters each. Here each goes to a logger at its own level: `ActorSystem` for the
  * system's own events, and `DeadLetters` for messages that reached no actor. A dead letter is WARN
  * while the system is running, because a message nobody received can be lost work; and DEBUG when
  * it is expected to go nowhere: once the system is stopping, with messages still in flight, or
  * when a test stopped its recipient on purpose ([[HydrozoaActorSystem.expectDeadLetters]]).
  */
object ActorSystemEvents:

    /** When a dead letter is expected: the system is stopping, or its recipient is in a subtree a
      * test stopped on purpose (the paths of those subtrees' roots).
      */
    final class Expected private (
        val stopping: Ref[IO, Boolean],
        val stoppedOnPurpose: Ref[IO, Set[String]]
    )

    object Expected:
        def apply(): IO[Expected] =
            for
                stopping <- Ref[IO].of(false)
                stoppedOnPurpose <- Ref[IO].of(Set.empty[String])
            yield new Expected(stopping, stoppedOnPurpose)

    def log(expected: Expected)(event: Any): IO[Unit] =
        for
            stopping <- expected.stopping.get
            onPurpose <- expected.stoppedOnPurpose.get
            _ <- Slf4jTracer.sink.traceWith(toLogEvent(event, stopping, onPurpose))
        yield ()

    def toLogEvent(event: Any, stopping: Boolean, onPurpose: Set[String] = Set.empty): LogEvent =
        event match
            case Debug(_, _, dl: DeadLetter[?]) => deadLetter(dl, stopping, onPurpose)
            case e: ActorError =>
                LogEvent(
                  Level.Error,
                  s"${e.logSource}: ${e.message}",
                  cause = Option.when(e.cause != ActorError.NoCause)(e.cause),
                  routingKey = Some(SystemLogger)
                )
            case Warning(source, _, message) => system(Level.Warn, source, message)
            case Info(source, _, message)    => system(Level.Info, source, message)
            case Debug(source, _, message)   => system(Level.Debug, source, message)
            case other                       => system(Level.Debug, "event stream", other)

    val SystemLogger: String = "ActorSystem"
    val DeadLetterLogger: String = "DeadLetters"

    private def system(level: Level, source: String, message: Any): LogEvent =
        LogEvent(level, s"$source: $message", routingKey = Some(SystemLogger))

    private def deadLetter(dl: DeadLetter[?], stopping: Boolean, onPurpose: Set[String]): LogEvent =
        // The message cats-actors wraps is often an `Envelope` around the real one; name the real
        // one, and keep its full text for TRACE-level reading of the file log.
        val inner = dl.message match
            case e: Envelope[?, ?]             => e.message
            case e: EnvelopeWithDeferred[?, ?] => e.envelope.message
            case m                             => m
        val recipient = dl.recipient.actorRef.path.toString
        val stoppedOnPurpose =
            onPurpose.exists(r => recipient == r || recipient.startsWith(r + "/"))
        LogEvent(
          if stopping || stoppedOnPurpose then Level.Debug else Level.Warn,
          s"${inner.getClass.getName} to $recipient" +
              s"${dl.sender.fold("")(s => s" from ${s.path}")}" +
              (if stopping then " (system stopping)"
               else if stoppedOnPurpose then " (recipient stopped on purpose)"
               else " (system running)"),
          ctx = Map.empty,
          routingKey = Some(DeadLetterLogger)
        )
