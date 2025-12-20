package hydrozoa.lib.actor

import cats.Monad
import cats.effect.{Concurrent, Deferred}
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef

/** An envelope for a synchronous request to an actor. We use this instead of
  * cats.actor.ReplyingActor so that each request type can (optionally) have its own response type.
  *
  * The envelope contains the request, plus an empty [[Deferred]] through which the sender can
  * receive the actor's response. The actor should provide an effectful handler function to the
  * envelope's [[handleRequest]] method, which will then complete the [[Deferred]] value with the
  * result of applying the function to the request contained in the envelope.
  *
  * The constructor for [[SyncRequest]] is private, so it can't be used directly to attach a
  * deferred response to a request and send to an actor. Instead, the request's case class should
  * define a method (conventionally called [[?:]]) that aliases to [[SyncRequest.send]], which sends
  * the request as a [[SyncRequest]] with a newly initialized [[Deferred]] response to a given
  * actor.
  *
  * @tparam F
  *   The effect in which the request/computation/response runs
  * @tparam Response
  *   The type of the response
  * @param request
  *   The request being sent synchronously.
  * @param dResponse
  *   An empty [[Deferred]] placeholder for the response, created by the sender and sent to the
  *   receiver. The receiver should populate this via [[handleRequest]].
  *
  * {{{
  * import cats.effect.{IO, IOApp, ExitCode, Resource}
  * import com.suprnation.actor.ActorSystem
  * import com.suprnation.actor.Actor.{Actor, Receive}
  *
  * case class Ping(i: Int) {
  *     def ?:(a: ActorRef[IO, SyncRequest[IO, Ping, Pong]]): IO[Pong] =
  *         SyncRequest.send(a, this)
  * }
  *
  * case class Pong(i: Int)
  *
  * case class PingPongActor() extends Actor[IO, SyncRequest[IO, Ping, Pong]] {
  *     override def receive: Receive[IO, SyncRequest[IO, Ping, Pong]] = {
  *         case r: SyncRequest[IO, Ping, Pong] =>
  *             r.handleRequest(ping => IO.pure(Pong(ping.i)))
  *     }
  * }
  *
  * object PingPongApp extends IOApp {
  *     override def run(args: List[String]): IO[ExitCode] = {
  *         val actorSystemResource: Resource[IO, ActorSystem[IO]] =
  *             ActorSystem[IO]("ping pong system")
  *
  *         actorSystemResource.use { system =>
  *             for {
  *                 pingPongActor <- system.actorOf(PingPongActor(), "ping pong actor")
  *                 pong: Pong <- pingPongActor ?: Ping(42)
  *             } yield ExitCode.Success
  *         }
  *     }
  * }
  * }}}
  */
final case class SyncRequest[
    F[+_]: Monad,
    Request,
    Response
] private (
    request: Request,
    dResponse: Deferred[F, Response]
) {

    /** Handle a synchronous request with a function, ensuring that the deferred result is completed
      * with the function's result.
      *
      * This should be used by the receiver of this request.
      * @param f
      *   the handler function
      */
    def handleRequest(f: Request => F[Response]): F[Unit] =
        for {
            result <- f(request)
            _ <- dResponse.complete(result)
        } yield ()

    override def equals(other: Any): Boolean = other match {
        case x: SyncRequest[F, Request, Response] => x.request == this.request
    }
}

object SyncRequest {

    /** Send a synchronous request to an actor that can handle it, via some concurrent monad.
      * Conventionally, it should be aliased as the `?:` method of the request's case class.
      *
      * @tparam F
      *   The concurrent monad type
      * @tparam Request
      *   The request type
      * @tparam Response
      *   The response type
      */
    def send[F[+_]: Concurrent, Request, Response](
        actorRef: ActorRef[F, SyncRequest[F, Request, Response]],
        self: Request
    ): F[Response] =
        for {
            dResponse <- Deferred[F, Response]
            syncRequest = SyncRequest(self, dResponse)
            _ <- actorRef ! syncRequest
            eResponse <- syncRequest.dResponse.get
        } yield eResponse
}
