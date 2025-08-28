package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.mailbox.AskTell.{Ask, Tell}
import munit.Assertions.assertEquals
import munit.ScalaCheckSuite
import ox.*
import ox.channels.{BufferCapacity, Channel, Sink, Source}

import java.util.concurrent.CompletableFuture
import scala.annotation.unused
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionException, TimeoutException}
import scala.util.control.NonFatal

// Here are no more events, for observability we use messages actors pass each other.
class DataActorsSpec extends ScalaCheckSuite:

    private val log = Logger(getClass)

    // This test demonstrates how to write a test flow which is driven by
    // observable messages.
    test("reactive test"):
        val msgSink = Channel.unlimited[AMsg]

        supervised {
            val actorB = DataActor.create(DActorB(), msgSink)
            val actorA = DataActor.create(DActorA(actorB), msgSink)
            actorA.tell(AMsgFoo())

            // Blocks until we see `xyz` in the msgSink or timeout is up
            val _ = FastForward(msgSink)(2.seconds)(_ == AMsgFoo())

            // Act upon the result is needed, make assertions
            // ...

            // Blocks until we see `xyz` in the msgSink or timeout is up
            val _ = FastForward(msgSink)(2.seconds)(_ == AMsgBar())

            // Act upon the result is needed, make assertions
//            xyz match {
//                case _: AMsgBar => println("xyz is AMsgBar")
//                case _ => println("xyz is not AMsgBar")
//            }
        }

    // If we don't want/need to wait for any messages we can just run and
    // do some after-run cold checks
    test("passive test"):
        val msgSink = Channel.unlimited[AMsg]

        supervised {
            // setup
            val actorB = DataActor.create(DActorB(), msgSink)
            val actorA = DataActor.create(DActorA(actorB), msgSink)

            // test actions
            actorA.tell(AMsgFoo())
            sleep(2.second)
        }

        // Stop the channel and get all msgs
        msgSink.done()
        val msgs = msgSink.toList

        log.info(s"Got messages: $msgs")

        // verify that a list of "invariants" is hold (very naively)
        checkMsgTraceAssertions(
          msgs,
          List(
            MsgIsPresent(AMsgFoo()),
            FirstThisThenThat(AMsgFoo(), AMsgBar())
          )
        )

    // We can combine both approaches, we decided to teach FastForward to pass messages it
    // skips over so we can get the full list of messages after the run.
    test("mixed test"):
        val msgSink = Channel.unlimited[AMsg]
        val msgTrace = Channel.unlimited[AMsg]

        supervised {

            // setup
            val actorB = DataActor.create(DActorB(), msgSink)
            val actorA = DataActor.create(DActorA(actorB), msgSink)

            // test actions
            actorA.tell(AMsgFoo())

            val _ = FastForward(msgSink, Some(msgTrace))(2.seconds)(_ == AMsgFoo())

            // Blocks until we see `xyz` in the msgSink or timeout is up
            val _ = FastForward(msgSink, Some(msgTrace))(2.seconds)(_ == AMsgBar())

            sleep(2.second)
        }

        msgTrace.done()
        msgSink.done()
        val msgs = msgTrace.toList ++ msgSink.toList

        log.info(s"Got messages: $msgs")

        checkMsgTraceAssertions(
          msgs,
          List(
            MsgIsPresent(AMsgFoo()),
            FirstThisThenThat(AMsgFoo(), AMsgBar())
          )
        )

    // If we need to use .ask - use CallableDataActor and just ask :-)
    test("callable actors"):
        val msgSink = Channel.unlimited[AMsg]

        supervised {
            val callableActor = CallableDataActor.create(ExampleCallableActor(), msgSink)

            callableActor.tell(AMsgFoo())
            val ret = callableActor.ask(AMsgFoo())

            assertEquals(ret, 42)

            // One for .tell and one for .ask
            val _ = FastForward(msgSink)(2.seconds)(_ == AMsgFoo())
            val _ = FastForward(msgSink)(2.seconds)(_ == AMsgFoo())
        }

/** Fixture for waiting a msg that suits the predicate and getting it back.
  */
object FastForward:
    def apply[E](msgSink: Source[E], msgTrace: Option[Sink[E]] = None)(
        timeout: FiniteDuration
    )(pred: E => Boolean, predText: Option[String] = None)(using ox: Ox): E =
        // Function to optionally dump to the trace
        val mbSend: E => Unit = msgTrace match {
            case None       => _ => ()
            case Some(sink) => sink.send
        }
        // try to receive the target during the allowed time
        timeoutOption(timeout) {
            var msg = msgSink.receive()
            mbSend(msg)
            while !pred(msg) do {
                msg = msgSink.receive()
                mbSend(msg)
            }
            msg
        }.getOrElse(
          throw new TimeoutException(
            s"timeout fast forwarding with predicate: ${predText}"
          )
        )

// utility to run invariants check
def checkMsgTraceAssertions(msgs: List[AMsg], checks: List[MsgTraceAssertion]) =
    checks.foreach(_ match {
        case MsgIsPresent(msg) =>
            // TODO: contains!
            assertEquals(msgs.contains(msg), true, s"msg $msg is not present in msgs sink")
        case FirstThisThenThat(msgA, msgB) =>
            assertEquals(
              msgs.indexOf(msgA) < msgs.indexOf(msgB),
              true,
              s"msg $msgB does not go after msg $msgA"
            )
    })

// Hierarchy of invariants...
sealed trait MsgTraceAssertion
case class MsgIsPresent(msg: AMsg) extends MsgTraceAssertion
case class FirstThisThenThat(msgA: AMsg, msgB: AMsg) extends MsgTraceAssertion

// Hierarchy of messages - now we need to have them explicitly
sealed trait AMsg
sealed trait AMsgA extends AMsg
sealed trait AMsgB extends AMsg
final case class AMsgFoo() extends AMsgA
final case class AMsgBar() extends AMsgB

// Data actor, only tell (which is handled by receive). Or shoudl we call it listen? ;-)
trait DataActor:
    type Msg
    def receive(msg: Msg): Unit

// This is for .ask, for now it's necessary for db writer only.
trait CallableDataActor extends DataActor:
    type Resp
    def respond(msg: Msg): Resp

// sample actor for .tell
class DActorA(actorB: DataActorRef[AMsgB]) extends DataActor:

    type Msg = AMsgA

    private val log = Logger(getClass)

    def receive(msg: Msg): Unit = msg match {
        case arg: AMsgFoo => foo(arg)
    }

    def foo(_arg: AMsgFoo): Unit =
        log.info("foo!")
        sleep(1.second)
        actorB.tell(AMsgBar())

// sample actor for .tell
class DActorB extends DataActor:

    type Msg = AMsgB

    private val log = Logger(getClass)

    def receive(msg: Msg): Unit = msg match {
        case arg: AMsgBar => bar(arg)
    }

    private def bar(@unused arg: AMsgBar): Unit = {
        log.info("bar!")
    }

// An example actor for .ask
class ExampleCallableActor() extends CallableDataActor:

    type Msg = AMsgA

    type Resp = Int

    private val log = Logger(getClass)

    def receive(msg: Msg): Unit = msg match {
        // TODO: NB: Let's use .discard not `: Unit` everywhere!
        case arg: AMsgFoo => foo(arg).discard
    }

    def respond(msg: Msg): Int = msg match {
        case arg: AMsgFoo => foo(arg)
    }

    def foo(_arg: AMsgFoo): Int =
        log.info("foo!")
        sleep(1.second)
        42

// Reference to a data actor
class DataActorRef[M](c: Sink[M], msgSink: Sink[M]):
    def tell(msg: M): Unit =
        c.send(msg)
        msgSink.send(msg)
end DataActorRef

// Wrapper for messages to use with [[CallableDataActorRef]]
enum AskTell[M, R]:
    // Future is used to pass back the result
    case Ask(msg: M, cf: CompletableFuture[R])
    case Tell(msg: M)

// Reference to a data actor with .ask support
class CallableDataActorRef[M, R](c: Sink[AskTell[M, R]], msgSink: Sink[M]):
    def ask(msg: M): R =
        val cf = new CompletableFuture[R]()
        c.send(Ask(msg, cf))
        // TODO: move it to the companion object, add Ask/Tell tag (with no future)
        msgSink.send(msg)
        unwrapExecutionException(cf.get())
    end ask

    def tell(msg: M): Unit =
        c.send(Tell(msg))
        // TODO: move it to the companion object, add Ask/Tell tag (with no future)
        msgSink.send(msg)

end CallableDataActorRef

// Companion object for a data actor
// TODO: (Ilia) I would pass msgSing using "using" since we likely want it to be only one for all actors
object DataActor:
    def create(logic: DataActor, msgSink: Sink[logic.Msg], close: Option[DataActor => Unit] = None)(
        using
        ox: Ox,
        sc: BufferCapacity
    ): DataActorRef[logic.Msg] =
        val c = BufferCapacity.newChannel[logic.Msg]
        val ref = DataActorRef[logic.Msg](c, msgSink)
        forkDiscard {
            try
                forever {
                    val m = c.receive()
                    try logic.receive(m)
                    catch
                        case t: Throwable =>
                            c.error(t)
                            throw t
                }
            finally close.foreach(c => uninterruptible(c(logic)))
        }
        ref
    end create
end DataActor

// Companion object for a callable data actor
// TODO: (Ilia) I would pass msgSing using "using" since we likely want it to be only one for all actors
object CallableDataActor:
    def create(
        logic: CallableDataActor,
        msgSink: Sink[logic.Msg],
        close: Option[DataActor => Unit] = None
    )(using
        ox: Ox,
        sc: BufferCapacity
    ): CallableDataActorRef[logic.Msg, logic.Resp] =
        val c = BufferCapacity.newChannel[AskTell[logic.Msg, logic.Resp]]
        val ref = CallableDataActorRef[logic.Msg, logic.Resp](c, msgSink)
        forkDiscard {
            try
                forever {
                    val m = c.receive()

                    m match {
                        case Tell(msg) =>
                            try logic.receive(msg)
                            catch
                                case t: Throwable =>
                                    c.error(t)
                                    throw t
                        case Ask(msg, cf) =>
                            try
                                val res = logic.respond(msg)
                                cf.complete(res).discard
                            catch
                                case NonFatal(e) =>
                                    // since this is an ask, only propagating the exception to the caller, not to the scope
                                    cf.completeExceptionally(e).discard
                                case t: Throwable =>
                                    // fatal exceptions are propagated to the scope (e.g. InterruptedException)
                                    cf.completeExceptionally(t).discard
                                    c.error(t)
                                    throw t
                    }
                }
            finally close.foreach(c => uninterruptible(c(logic)))
        }
        ref
    end create
end CallableDataActor

private inline def unwrapExecutionException[T](f: => T): T =
    try f
    catch
        case e: ExecutionException => throw causeWithSelfAsSuppressed(e)
        case e: Throwable          => throw e

private inline def causeWithSelfAsSuppressed(e: ExecutionException): Throwable =
    val cause = e.getCause
    // adding the original as suppressed, so that no context is lost
    // we cannot simply throw the EE, as it might wrap e.g. boundary-break, which has to be thrown unchanged
    cause.addSuppressed(e)
    cause
