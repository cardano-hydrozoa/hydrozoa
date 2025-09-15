package hydrozoa.lib.handle.actor

import cats.MonadError
import cats.effect.{Deferred, GenConcurrent}
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef

object ActorHandle {
    import hydrozoa.lib.handle.Handle.*

    sealed trait ActorRequestSync[F[+_], +RequestS <: RequestSync](
        val request: RequestS,
        private val deferredResponse: Deferred[F, Either[Throwable, request.Response]]
    )(implicit F: MonadError[F, Throwable]) {
        def receiveSync(f: RequestS => F[request.Response]): F[Unit] =
            for {
                eResult <- f(request).attempt
                _ <- deferredResponse.complete(eResult)
            } yield ()

        def complete(response: request.Response): F[Unit] =
            deferredResponse.complete(Right(response)).void
    }

    object ActorRequestSync {
        def apply[F[+_], RequestS <: RequestSync](
            request: RequestS,
            deferredResponse: Deferred[F, Either[Throwable, request.Response]]
        )(implicit F: MonadError[F, Throwable]): ActorRequestSync[F, RequestS] =
            new ActorRequestSync(request, deferredResponse) {}
    }

    sealed trait ActorHandleAsync[F[+_], -RequestA <: RequestAsync]
        extends HandleAsync[F, RequestA] {
        def asyncActorRef: ActorRef[F, RequestA]

        override def !(request: RequestA): F[Unit] =
            this.asyncActorRef ! request
    }

    object ActorHandleAsync {
        def apply[F[+_], RequestA <: RequestAsync](
            a: ActorRef[F, RequestA]
        ): ActorHandleAsync[F, RequestA] =
            new ActorHandleAsync {
                override val asyncActorRef: ActorRef[F, RequestA] = a
            }
    }

    sealed trait ActorHandleSyncOnly[F[+_], -RequestS <: RequestSync](implicit
        F: GenConcurrent[F, Throwable]
    ) extends HandleOnlySync[F, RequestS] {
        def syncActorRef: ActorHandleSyncOnly.Ref[F, RequestS]

        override def ?(request: RequestS): F[request.Response] =
            for {
                deferredResponse <- Deferred[F, Either[Throwable, request.Response]]
                actorRequest = ActorRequestSync(request, deferredResponse)
                _ <- this.syncActorRef ! actorRequest
                response <- deferredResponse.get.rethrow
            } yield response
    }

    object ActorHandleSyncOnly {
        type Ref[F[+_], -RequestS <: RequestSync] = ActorRef[F, Request[F, RequestS]]
        type Request[F[+_], +RequestS <: RequestSync] = ActorRequestSync[F, RequestS]

        def apply[F[+_], RequestS <: RequestSync](
            a: Ref[F, RequestS]
        )(implicit F: GenConcurrent[F, Throwable]): ActorHandleSyncOnly[F, RequestS] =
            new ActorHandleSyncOnly {
                override val syncActorRef: ActorRef[F, ActorRequestSync[F, RequestS]] = a
            }
    }

    sealed trait ActorHandle[F[+_], -RequestA <: RequestAsync, -RequestS <: RequestSync]
        extends Handle[F, RequestA, RequestS],
          ActorHandleAsync[F, RequestA],
          ActorHandleSyncOnly[F, RequestS] {
        def actorRef: ActorHandle.Ref[F, RequestA, RequestS]

        override def asyncActorRef: ActorRef[F, RequestA] = actorRef

        override def syncActorRef: ActorRef[F, ActorRequestSync[F, RequestS]] = actorRef
    }

    object ActorHandle {
        type Ref2[F[+_], -RequestS <: RequestSync] = Ref[F, RequestS, RequestS]

        type Ref[F[+_], -RequestA <: RequestAsync, -RequestS <: RequestSync] =
            ActorRef[F, Request[F, RequestA, RequestS]]

        type Request[F[+_], +RequestA <: RequestAsync, +RequestS <: RequestSync] =
            RequestA | ActorRequestSync[F, RequestS]

        def apply[F[+_], RequestA <: RequestSync, RequestS <: RequestSync](
            a: Ref[F, RequestA, RequestS]
        )(implicit F: GenConcurrent[F, Throwable]): ActorHandle[F, RequestA, RequestS] =
            new ActorHandle {
                override val actorRef: Ref[F, RequestA, RequestS] = a
            }
    }
}
