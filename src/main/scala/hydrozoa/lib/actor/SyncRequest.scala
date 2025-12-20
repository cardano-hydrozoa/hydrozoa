package hydrozoa.lib.actor

import cats.MonadError
import cats.effect.Deferred
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
  */
trait SyncRequest[F[+_], E <: Throwable, Response](implicit
    F: MonadError[F, Throwable],
    tag: ClassTag[E]
) {

    /** The [[Deferred]] cell created by the sender and sent to the receiver. The receiver should
      * populate this cell, via [[handleRequest]] or otherwise.
      */
    def dResponse: Deferred[F, Either[E, Response]]

    /** The default implementation of this method runs the handler function and completes the
      * deferred with the result.
      *
      * This should be used by the receiver of this request.
      * @param f
      * @return
      */
    final def handleRequest(f: this.type => F[Response]): F[Unit] =
        for {
            eResult <- f(this).attemptNarrow
            _ <- dResponse.complete(eResult)
        } yield ()

    /** The default implementation of this function messages the actor with the request and blocks
      * on the response.
      *
      * This should be used by the sender of this request.
      * @param actorRef
      * @return
      */
    final def ?:(actorRef: ActorRef[F, this.type]): F[Either[E, Response]] =
        for {
            _ <- actorRef ! this
            eResponse <- this.dResponse.get
        } yield eResponse
}
