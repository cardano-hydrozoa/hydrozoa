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

        def fromRequestSync[F[+_], RequestS <: RequestSync](request: RequestS)(implicit
            F: GenConcurrent[F, Throwable]
        ): F[(ActorRequestSync[F, RequestS], Deferred[F, Either[Throwable, request.Response]])] =
            for {
                deferredResponse <- Deferred[F, Either[Throwable, request.Response]]
                actorRequest = new ActorRequestSync(request, deferredResponse) {}
            } yield (actorRequest, deferredResponse)

        def unapply[F[+_], RequestS <: RequestSync](a: ActorRequestSync[F, RequestS]): RequestS =
            a.request
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

    sealed trait ActorHandleSyncOnly[F[+_], -RequestS <: RequestSync, -ActorRequestS >: ActorRequestSync[F, RequestS]](implicit
        F: GenConcurrent[F, Throwable]
    ) extends HandleOnlySync[F, RequestS] {
        def syncActorRef: ActorRef[F, ActorRequestS]

        override def ?(request: RequestS): F[request.Response] =
            for {
                x <- ActorRequestSync.fromRequestSync(request)
                _ <- this.syncActorRef ! x._1
                response <- x._2.get.rethrow
            } yield response
    }

    object ActorHandleSyncOnly {
        def apply[F[+_], RequestS <: RequestSync, ActorRequestS >: ActorRequestSync[F, RequestS]](
            a: ActorRef[F, ActorRequestS]
        )(implicit F: GenConcurrent[F, Throwable]): ActorHandleSyncOnly[F, RequestS, ActorRequestS] =
            new ActorHandleSyncOnly {
                override val syncActorRef: ActorRef[F, ActorRequestS] = a
            }
    }

    sealed trait ActorHandle[F[+_], -RequestA <: RequestAsync, -RequestS <: RequestSync, -ActorRequestS >: ActorRequestSync[F, RequestS]]
        extends Handle[F, RequestA, RequestS],
          ActorHandleAsync[F, RequestA],
          ActorHandleSyncOnly[F, RequestS, ActorRequestS] {
        def actorRef: ActorRef[F, RequestA | ActorRequestS]

        override def asyncActorRef: ActorRef[F, RequestA] = actorRef

        override def syncActorRef: ActorRef[F, ActorRequestS] = actorRef
    }

    object ActorHandle {
        def apply[F[+_], RequestA <: RequestAsync, RequestS <: RequestSync, ActorRequestS >: ActorRequestSync[F, RequestS]](
            a: ActorRef[F, RequestA | ActorRequestS]
        )(implicit F: GenConcurrent[F, Throwable]): ActorHandle[F, RequestA, RequestS, ActorRequestS] =
            new ActorHandle {
                override val actorRef: ActorRef[F, RequestA | ActorRequestS] = a
            }
    }
}
