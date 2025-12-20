package hydrozoa.lib.actor

import cats.Monad
import cats.data.EitherT
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
  * @tparam Request
  *   The type of the request
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
  *     def ?: : SyncRequest.Send[IO, Ping, Pong] =
  *         SyncRequest.send(_, this)
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

    /** Handle a synchronous request with a handler function, ensuring that the deferred result is
      * completed with the function's result. The handler function can have one of the following
      * type signatures:
      *
      *   - `Request => F[Response]`
      *   - `Request => EitherT[F, L, R]`, where `Either[L,R] =:= Response`
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

    /** Handle a synchronous request with a handler function, ensuring that the deferred result is
      * completed with the function's result. The handler function can have one of the following
      * type signatures:
      *
      *   - `Request => F[Response]`
      *   - `Request => EitherT[F, L, R]`, where `Either[L,R] =:= Response`
      *
      * @param f
      *   the handler function
      * @param ev
      *   evidence that the [[Response]] type is an [[Either]]
      * @tparam L
      *   the left type in the response's [[Either]]
      * @tparam R
      *   the right type in the response's [[Either]]
      */
    def handleRequest[L, R](
        f: Request => EitherT[F, L, R]
    )(using ev: Either[L, R] =:= Response): F[Unit] =
        for {
            result <- f(request).value
            _ <- dResponse.complete(result)
        } yield ()
}

/** A [[SyncRequest]] specialized with a response type that is an [[Either]]. Its handler function
  * (provided to its [[SyncRequest.handleRequest]] method) should have the type signature
  * `Request => EitherT[F, L, R]`.
  *
  * Type parameters:
  *   - `F` the effect in which the request/computation/response runs
  *   - `Request` the type of the request
  *   - `Err` the left type in the response's [[Either]]
  *   - `Response` the right type in the response's [[Either]]
  */
type SyncRequestE[F[+_], Request, Err, Response] = SyncRequest[F, Request, Either[Err, Response]]

object SyncRequest {
    type Send[F[+_], Request, Response] =
        (actorRef: ActorRef[F, SyncRequest[F, Request, Response]]) => F[Response]

    type SendE[F[+_], Request, Err, Response] = Send[F, Request, Either[Err, Response]]

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
