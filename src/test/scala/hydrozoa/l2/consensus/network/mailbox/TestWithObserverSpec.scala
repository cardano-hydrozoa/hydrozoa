package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import munit.ScalaCheckSuite
import org.scalacheck.Test as ScalaCheckTest
import ox.*
import ox.channels.{BufferCapacity, Channel, Sink}
import ox.logback.InheritableMDC

import java.util.concurrent.CompletableFuture
import scala.annotation.unused
import scala.concurrent.ExecutionException
import scala.util.control.NonFatal

class TestWithObserverSpec extends ScalaCheckSuite:

    private val log = Logger(getClass)
    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        /* NOTE: This is very low for sufficient test coverage, but we're keeping it low for now because
       most of these tests contain race conditions. We'll often need to poll things like databases to wait for
       messages to appear, which will slow down the tests
         */
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(10)
    }

    override def beforeEach(context: BeforeEach): Unit =
        InheritableMDC.init

    test("baz"):
        log.info("42")
        supervised {
            val eventSink = Channel.unlimited[Event1]
            val actorB = ActorRefObservable.create[ActorB, Event1](ActorB(), eventSink)
            @unused
            val actorA = ActorRefObservable.create[ActorA, Event1](ActorA(Some(actorB)), eventSink)

            actorA.tell(_.foo())

            log.info(s"${eventSink.receive()}")
            log.info(s"${eventSink.receive()}")
        }

sealed trait Event1
sealed trait Event1A extends Event1
sealed trait Event1B extends Event1
case class Event1Foo() extends Event1A
case class Event1Bar(ref: Event1Foo) extends Event1B

class ActorA(@unused actorB: Option[ActorRefObservable[ActorB, Event1]] = None):

    private val log = Logger(getClass)

    def foo(): Event1Foo = {
        log.info("foo!")
        // some calculations
        val ret = Event1Foo()
        actorB.foreach(_.tell(_.bar((), ret)))
        ret
    }

class ActorB:
    private val log = Logger(getClass)

    def bar(payload: Unit, eventA: Event1Foo): Event1Bar = {
        log.info("bar!")
        // some calculations
        Event1Bar(eventA)
    }

class ActorRefObservable[T, E](c: Sink[T => Unit], eventSink: Sink[E]):
    def ask(f: T => E): E =
        val cf = new CompletableFuture[E]()
        c.send { t =>
            try {
                val ret = f(t)
                eventSink.send(ret)
                cf.complete(ret).discard
            } catch
                case NonFatal(e) =>
                    // since this is an ask, only propagating the exception to the caller, not to the scope
                    cf.completeExceptionally(e).discard
                case t: Throwable =>
                    // fatal exceptions are propagated to the scope (e.g. InterruptedException)
                    cf.completeExceptionally(t).discard
                    throw t
        }
        unwrapExecutionException(cf.get())
    end ask

    def tell(f: T => E): Unit =
        c.send(t => {
            val ret = f(t)
            eventSink.send(ret)
        })
end ActorRefObservable

object ActorRefObservable:
    def create[T, E](logic: T, eventSink: Sink[E], close: Option[T => Unit] = None)(using
        ox: Ox,
        sc: BufferCapacity
    ): ActorRefObservable[T, E] =
        val c = BufferCapacity.newChannel[T => Unit]
        val ref = ActorRefObservable[T, E](c, eventSink)
        forkDiscard {
            try
                forever {
                    val m = c.receive()
                    try m(logic)
                    catch
                        case t: Throwable =>
                            c.error(t)
                            throw t
                }
            finally close.foreach(c => uninterruptible(c(logic)))
        }
        ref
    end create
end ActorRefObservable

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
