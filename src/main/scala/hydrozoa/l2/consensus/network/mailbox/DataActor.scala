package hydrozoa.l2.consensus.network.mailbox

import hydrozoa.l2.consensus.network.mailbox.AskTell.{Ask, Tell}
import ox.*
import ox.channels.{BufferCapacity, Sink}

import java.util.concurrent.CompletableFuture
import scala.concurrent.ExecutionException
import scala.util.control.NonFatal

/** Data actor, only tell (which is handled by [[receive]]).
  */
trait DataActor:
    type Msg
    def receive(msg: Msg): Unit

/** This is for .ask, for now it's necessary for db writer only.
  */
trait CallableDataActor extends DataActor:
    type Resp
    def respond(msg: Msg): Resp

/** Reference to a data actor.
  *
  * @param c
  * @param msgSink
  * @tparam M
  */
class DataActorRef[M](c: Sink[M]):
    def tell(msg: M): Unit = c.send(msg)

/** Wrapper for messages to use with [[CallableDataActorRef]].
  * @tparam M
  * @tparam R
  */
enum AskTell[M, R]:
    // Future is used to pass back the result
    case Ask(msg: M, cf: CompletableFuture[R])
    case Tell(msg: M)

/** Reference to a data actor with `.ask` support.
  * @param c
  * @param msgSink
  * @tparam M
  * @tparam R
  */
class CallableDataActorRef[M, R](c: Sink[AskTell[M, R]]):
    def ask(msg: M): R =
        val cf = new CompletableFuture[R]()
        c.send(Ask(msg, cf))
        unwrapExecutionException(cf.get())
    end ask

    def tell(msg: M): Unit = c.send(Tell(msg))

/** Companion object for a data actor. A [[msgSink]] broadcast sink is used to collect all messages.
  * Can be None for some actors, likely for actors that dispatch collected messages like
  * [[PubSubActor]].
  */
object DataActor:
    def create(
        logic: DataActor,
        close: Option[DataActor => Unit] = None
    )(using
        ox: Ox,
        sc: BufferCapacity,
        msgSink: Option[BroadcastSink[logic.Msg]]
    ): DataActorRef[logic.Msg] =
        val c = BufferCapacity.newChannel[logic.Msg]
        val ref = DataActorRef(c)
        forkDiscard {
            try
                forever {
                    val msg = c.receive()
                    msgSink.foreach(_.writeChan(msg))
                    try logic.receive(msg)
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

/** Companion object for a callable data actor. A [[msgSink]] broadcast sink is used to collect all
  * messages. Can be None for some actors, likely for actors that dispatch collected messages like
  * [[PubSubActor]].
  */
object CallableDataActor:
    def create(
        logic: CallableDataActor,
        close: Option[DataActor => Unit] = None
    )(using
        ox: Ox,
        sc: BufferCapacity,
        msgSink: Option[BroadcastSink[logic.Msg]]
    ): CallableDataActorRef[logic.Msg, logic.Resp] =
        val c = BufferCapacity.newChannel[AskTell[logic.Msg, logic.Resp]]
        val ref = CallableDataActorRef(c)
        forkDiscard {
            try
                forever {
                    val tellOrAsk = c.receive()
                    tellOrAsk match {
                        case Tell(msg) =>
                            try
                                logic.receive(msg)
                                msgSink.foreach(_.writeChan(msg))
                            catch
                                case t: Throwable =>
                                    c.error(t)
                                    throw t
                        case Ask(msg, cf) =>
                            try
                                val res = logic.respond(msg)
                                cf.complete(res).discard
                                msgSink.foreach(_.writeChan(msg))
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
