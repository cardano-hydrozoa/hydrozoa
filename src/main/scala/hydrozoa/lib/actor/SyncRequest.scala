package hydrozoa.lib.actor

import cats.effect.{Concurrent, Deferred, MonadCancelThrow}
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef
import scala.reflect.ClassTag

/*
Notes (Peter, 2025-12-09):

1.) Should any/all of these fields be marked `final`?
  - A: probably

2.) When I was first trying to use this class, I was switching from a regular Request to this SyncRequest trait.
I wasn't using "handleRequest" and was instead manually extracting the dResponse, which I forgot to `_.complete`.

If we wrap the Deferred into an opaque type/class and provide constructors, we can expose _only_ the methods in this
trait as the only eliminators; only `handleRequest` and `?:`. This would ensure that the deferred is always filled
when the request is handled.

3.) Since the requests need to be issued in IO, we usually have to do something like

for {
  (...)
  req <- MyReqConstructor
  res <- myActor ?: req
  (...)
} yield (...)

Does it make sense to expose a function (that initialized the request and issues the request at the same time, so
that we can just do

for {
  (...)
  res <- myActor ?~: MyReqConstructor
  (...)
} yield (...)

?

4.) What are the trade-offs of doing this in `EitherT` vs in F with a throwable?
 */

/** A type-safe trait to handle synchronous requests between actors. We use this instead of
  * ReplyingActor so that each request type can (optionally) have its own response type.
  *
  * The responses are stored in a [[Deferred]] that is passed along with the request. Think of it
  * like a "return envelope".
  *
  * To use this, the sender of the request:
  *   - Creates a Deferred value and places inside a value of a type that implements this trait
  *     (usually via a _.apply)
  *   - Issues the request to the receiving actor
  *   - Blocks on the dResponse
  *
  * @tparam F
  *   The effect in which the request/computation/response runs
  * @tparam E
  *   A [[Throwable]] error type associated with the computation
  * @tparam Response
  *   The type of the response
  *
  * @param request
  *   The request...
  * @param dResponse
  *   The [[Deferred]] cell created by the sender and sent to the receiver. The receiver should
  *   populate this cell, via [[handleRequest]] or otherwise.
  */
final case class SyncRequest[
    F[+_]: MonadCancelThrow,
    E <: Throwable: ClassTag,
    Request,
    Response
] protected (
    request: Request,
    dResponse: Deferred[F, Either[E, Response]]
) {

    /** Handle a synchronous request with a function, ensuring that the deferred result is completed
      * with the function's result.
      *
      * This should be used by the receiver of this request.
      * @param f
      * @return
      */
    def handleRequest(f: Request => F[Response]): F[Unit] =
        for {
            eResult <- f(request).attemptNarrow
            _ <- dResponse.complete(eResult)
        } yield ()
}

object SyncRequest {
    trait Send[F[+_]: Concurrent, E <: Throwable: ClassTag, Request, Response] {

        /** Send the request to the actor and block until the receiver completes the deferred
          * response.
          *
          * This should be used by the sender of this request.
          *
          * @param actorRef
          * @return
          */
        def ?:(
            actorRef: ActorRef[F, SyncRequest[F, E, Request, Response]]
        ): F[Either[E, Response]] = {
            val self = this.asInstanceOf[Request]
            for {
                dResponse <- Deferred[F, Either[E, Response]]
                syncRequest = SyncRequest(self, dResponse)
                _ <- actorRef ! syncRequest
                eResponse <- syncRequest.dResponse.get
            } yield eResponse
        }
    }
}
