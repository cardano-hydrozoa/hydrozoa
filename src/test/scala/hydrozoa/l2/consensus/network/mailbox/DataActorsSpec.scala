package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import munit.Assertions.assertEquals
import munit.ScalaCheckSuite
import ox.*
import ox.channels.*

import scala.annotation.unused
import scala.concurrent.TimeoutException
import scala.concurrent.duration.{DurationInt, FiniteDuration}

// There are no more events, for observability we use messages actors pass each other.
class DataActorsSpec extends ScalaCheckSuite:

    /** This test demonstrates how to write a test flow which is driven by observable messages.
      */
    test("example reactive test"):
        val msgSink = BroadcastSink[AMsg]()
        val msgSource = BroadcastSource(msgSink)

        supervised {
            given Option[BroadcastSink[AMsg]] = Some(msgSink)

            val actorB = DataActor.create(DActorB())
            val actorA = DataActor.create(DActorA(actorB))
            actorA.tell(AMsgFoo())

            // Blocks until we see `AMsgFoo()` in the msgSink or timeout is up
            val _ = FastForward(msgSource)(2.seconds)(_ == AMsgFoo())

            // Blocks until we see `AMsgBar()` in the msgSink or timeout is up
            val _ = FastForward(msgSource)(2.seconds)(_ == AMsgBar())
        }

    /* If we need to use .ask - use CallableDataActor and just ask :-)
     */
    test("example callable actors"):
        val msgSink = BroadcastSink[AMsg]()
        val msgSource = BroadcastSource(msgSink)

        supervised {
            given Option[BroadcastSink[AMsg]] = Some(msgSink)

            val callableActor = CallableDataActor.create(ExampleCallableActor())

            callableActor.tell(AMsgFoo())
            val ret = callableActor.ask(AMsgFoo())

            assertEquals(ret, 42)

            // One for .tell and one for .ask
            val _ = FastForward(msgSource)(2.seconds)(_ == AMsgFoo())
            val _ = FastForward(msgSource)(2.seconds)(_ == AMsgFoo())
        }

    test("support multiple consumers"):

        /** Tries to read from the channel, returns None if it can't after 1 second. */
        class ChannelReadActor(source: BroadcastSource[AMsg]):
            def read: Option[AMsg] =
                timeoutOption(1.seconds) {
                    source.readChan
                }

        val msgSink = BroadcastSink[AMsg]()
        val msgSource1 = BroadcastSource(msgSink)
        val msgSource2 = msgSource1.dupe
        val msgSource3 = BroadcastSource(msgSink)

        supervised {
            // setup
            given Option[BroadcastSink[AMsg]] = Some(msgSink)

            val dataActor = DataActor.create(DActorB())
            val readActorA = Actor.create(ChannelReadActor(msgSource1))
            val readActorB = Actor.create(ChannelReadActor(msgSource2))
            val readActorC = Actor.create(ChannelReadActor(msgSource3))

            // Put a message on the sink.
            dataActor.tell(AMsgBar())

            assert(readActorA.ask(_.read).contains(AMsgBar()))
            assert(readActorB.ask(_.read).contains(AMsgBar()))
            assert(readActorC.ask(_.read).contains(AMsgBar()))
        }

/** Fixture for waiting a msg that suits the predicate and getting it back.
  */
object FastForward:
    def apply[E](msgSource: BroadcastSource[E])(
        timeout: FiniteDuration
    )(pred: E => Boolean, predText: Option[String] = None)(using ox: Ox): E =
        // try to receive the target during the allowed time
        timeoutOption(timeout) {
            var msg = msgSource.readChan
            while !pred(msg) do {
                msg = msgSource.readChan
            }
            msg
        }.getOrElse(
          throw new TimeoutException(
            s"timeout fast forwarding with predicate: ${predText}"
          )
        )

/** Example hierarchy of messages - with data actors you need to have them defined explicitly.
  */
sealed trait AMsg
sealed trait AMsgA extends AMsg
sealed trait AMsgB extends AMsg
final case class AMsgFoo() extends AMsgA
final case class AMsgBar() extends AMsgB

/** An example actor for `.tell` that should call another actor [[actorB]] by contract.
  * @param actorB
  */
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

/** An example actor for `.tell` that do nothing, though we want to observe the incoming message in
  * out tests.
  */
class DActorB extends DataActor:

    type Msg = AMsgB

    private val log = Logger(getClass)

    def receive(msg: Msg): Unit = msg match {
        case arg: AMsgBar => bar(arg)
    }

    private def bar(@unused arg: AMsgBar): Unit = {
        log.info("bar!")
    }

/** A callable actor for showcasing how `.ask` works.
  */
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
