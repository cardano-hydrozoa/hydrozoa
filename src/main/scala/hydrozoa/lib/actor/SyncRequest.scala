package hydrozoa.lib.actor

import cats.Monad
import cats.data.EitherT
import cats.effect.{Concurrent, Deferred}
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import scala.Any as ScalaAny

/** A synchronous request to an actor. We use this instead of cats.actor.ReplyingActor so that each
  * request type can (optionally) have its own response type.
  *
  * When sending a request synchronously, it gets placed into an envelope that also contains an
  * empty [[Deferred]] through which the sender can receive the actor's response. The actor should
  * provide an effectful handler function to the request's [[handleSync]] method, which will then
  * complete the [[Deferred]] value with the result of applying the function to the request
  * contained in the envelope.
  *
  * The constructor for [[SyncRequest.Envelope]] is private, so it can't be used directly to attach
  * a deferred response to a request and send to an actor. Instead, the request's case class should
  * define a method (conventionally called [[?:]]) that aliases to [[send]], which sends the request
  * as a [[SyncRequest.Envelope]] with a newly initialized [[Deferred]] response to a given actor.
  *
  * To allow a given request type to be sent synchronously, it should extend the [[SyncRequest]]
  * trait. If the request type is a case class, it should define a [[Sync]] type alias as the
  * corresponding [[Sync.Envelope]] for the request and re-export that [[Sync]] alias within the
  * case class. If the request type is a case object, then the [[Sync]] type alias should be defined
  * within the case object. This [[Sync]] case object keeps the envelope type aligned between the
  * [[SyncRequest]] methods and the actors that want to receive this request type synchronously. See
  * the example code, below.
  *
  * @tparam F
  *   The effect in which the request/computation/response runs
  * @tparam Request
  *   The type of the request
  * @tparam Response
  *   The type of the response
  *   {{{
  * import cats.effect.{IO, IOApp, ExitCode, Resource}
  * import com.suprnation.actor.ActorSystem
  * import com.suprnation.actor.Actor.{Actor, Receive}
  *
  * case class Ping(i: Int) extends SyncRequest[IO, Ping, Pong] {
  *     export Ping.Sync
  *     def ?: : this.Send = SyncRequest.send(_, this)
  * }
  *
  * object Ping {
  *     type Sync = SyncRequest.Envelope[IO, Ping, Pong]
  * }
  *
  * case class Pong(i: Int)
  *
  * case class PingPongActor() extends Actor[IO, Ping.Sync] {
  *     override def receive: Receive[IO, Ping.Sync] = { case req: SyncRequest.Any =>
  *         req.request match {
  *             case r: Ping => r.handleSync(req, ping => IO.pure(Pong(ping.i)))
  *         }
  *
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
  *   }}}
  */
trait SyncRequest[F[+_]: Concurrent, Request, Response] {
    type Sync <: SyncRequest.Envelope[F, Request, Response]
    type Respondent = ActorRef[F, Sync]
    type Send = (actorRef: Respondent) => F[Response]

    def ?: : Send

    /** Handle a synchronous request with a handler function, ensuring that the deferred result is
      * completed with the function's result. The handler function can have one of the following
      * type signatures:
      *
      *   - `Request => F[Response]`
      *   - `Request => EitherT[F, L, R]`, where `Either[L,R] =:= Response`
      *
      * This should be used by the receiver of this request.
      *
      * @param envelope
      *   the envelope in which this request was received
      * @param handler
      *   the handler function
      */
    def handleSync(envelope: SyncRequest.Any, handler: Request => F[Response]): F[Unit] =
        envelope.asInstanceOf[Sync].handleRequest(handler)

    /** Handle a synchronous request with a handler function, ensuring that the deferred result is
      * completed with the function's result. The handler function can have one of the following
      * type signatures:
      *
      *   - `Request => F[Response]`
      *   - `Request => EitherT[F, L, R]`, where `Either[L,R] =:= Response`
      *
      * @param envelope
      *   the envelope in which this request was received
      * @param handler
      *   the handler function
      * @param ev
      *   evidence that the [[Response]] type is an [[Either]]
      * @tparam L
      *   the left type in the response's [[Either]]
      * @tparam R
      *   the right type in the response's [[Either]]
      */
    def handleSync[L, R](
        envelope: SyncRequest.Any,
        handler: Request => EitherT[F, L, R]
    )(using ev: Either[L, R] =:= Response): F[Unit] =
        envelope.asInstanceOf[Sync].handleRequest(handler)(using ev)
}

type SyncRequestE[F[+_], Request, Err, Response] = SyncRequest[F, Request, Either[Err, Response]]

object SyncRequest {

    /** The parent of all [[SyncRequest]] envelopes. It has no type parameters, which makes it
      * convenient in Scala pattern matching, which cannot see envelope's type parameters due to JVM
      * type erasure.
      */
    sealed trait Any {
        def request: ScalaAny
        def dResponse: ScalaAny
    }

    /** The envelope for a [[SyncRequest]].
      * @param request
      *   The request being sent synchronously.
      * @param dResponse
      *   An empty [[Deferred]] placeholder for the response, created by the sender and sent to the
      *   receiver.
      */
    final case class Envelope[
        F[+_]: Monad,
        Request,
        Response
    ] private[actor] (
        override val request: Request,
        override val dResponse: Deferred[F, Response]
    ) extends Any {

        private[actor] def handleRequest(f: Request => F[Response]): F[Unit] =
            for {
                result <- f(request)
                _ <- dResponse.complete(result)
            } yield ()

        private[actor] def handleRequest[L, R](
            f: Request => EitherT[F, L, R]
        )(using ev: Either[L, R] =:= Response): F[Unit] =
            for {
                result <- f(request).value
                _ <- dResponse.complete(result)
            } yield ()
    }

    /** A [[Envelope]] specialized with a response type that is an [[Either]]. Its handler function
      * (provided to its [[Envelope.handleRequest]] method) should have the type signature
      * `Request => EitherT[F, L, R]`.
      *
      * Type parameters:
      *   - `F` the effect in which the request/computation/response runs
      *   - `Request` the type of the request
      *   - `Err` the left type in the response's [[Either]]
      *   - `Response` the right type in the response's [[Either]]
      */
    type EnvelopeE[F[+_], Request, Err, Response] = Envelope[F, Request, Either[Err, Response]]

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
        actorRef: ActorRef[F, Envelope[F, Request, Response]],
        self: Request
    ): F[Response] =
        for {
            dResponse <- Deferred[F, Response]
            syncRequest = Envelope.apply(self, dResponse)
            _ <- actorRef ! syncRequest
            eResponse <- syncRequest.dResponse.get
        } yield eResponse
}
