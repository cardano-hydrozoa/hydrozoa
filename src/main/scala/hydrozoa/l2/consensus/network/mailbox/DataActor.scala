package hydrozoa.l2.consensus.network.mailbox

import hydrozoa.l2.consensus.network.mailbox.AskTell.{Ask, Tell}
import ox.{Ox, discard, forever, forkDiscard, uninterruptible}
import ox.channels.{BufferCapacity, Sink}

import java.util.concurrent.CompletableFuture
import scala.concurrent.ExecutionException
import scala.util.control.NonFatal

// Data actor, only tell (which is handled by receive). Or should we call it listen? ;-)
trait DataActor:
    type Msg
    def receive(msg: Msg): Unit

// This is for .ask, for now it's necessary for db writer only.
trait CallableDataActor extends DataActor:
    type Resp
    def respond(msg: Msg): Resp

// Reference to a data actor
class DataActorRef[M](c: Sink[M], msgSink: Option[BroadcastSink[M]] = None):
    def tell(msg: M): Unit =
        c.send(msg)
        msgSink.foreach(_.writeChan(msg))
end DataActorRef

// Wrapper for messages to use with [[CallableDataActorRef]]
enum AskTell[M, R]:
    // Future is used to pass back the result
    case Ask(msg: M, cf: CompletableFuture[R])
    case Tell(msg: M)

// Reference to a data actor with .ask support
class CallableDataActorRef[M, R](c: Sink[AskTell[M, R]], msgSink: BroadcastSink[M]):
    def ask(msg: M): R =
        val cf = new CompletableFuture[R]()
        c.send(Ask(msg, cf))
        // TODO: move it to the companion object, add Ask/Tell tag (with no future)
        msgSink.writeChan(msg)
        unwrapExecutionException(cf.get())
    end ask

    def tell(msg: M): Unit =
        c.send(Tell(msg))
        // TODO: move it to the companion object, add Ask/Tell tag (with no future)
        msgSink.writeChan(msg)

end CallableDataActorRef

// Companion object for a data actor
// TODO: (Ilia) I would pass msgSing using "using" since we likely want it to be only one for all actors
object DataActor:
    def create(
        logic: DataActor,
        msgSink: Option[BroadcastSink[logic.Msg]],
        close: Option[DataActor => Unit] = None
    )(using
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
    def create(logic: DataActor, msgSink: BroadcastSink[logic.Msg])(using
        ox: Ox,
        sc: BufferCapacity
    ): DataActorRef[logic.Msg] =
        create(logic, Some(msgSink), None)
end DataActor

// Companion object for a callable data actor
// TODO: (Ilia) I would pass msgSing using "using" since we likely want it to be only one for all actors
object CallableDataActor:
    def create(
        logic: CallableDataActor,
        msgSink: BroadcastSink[logic.Msg],
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
