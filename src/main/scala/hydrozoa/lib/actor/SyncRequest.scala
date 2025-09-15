package hydrozoa.lib.actor

import cats.MonadError
import cats.effect.Deferred
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef

trait SyncRequest[F[+_], Response](implicit F: MonadError[F, Throwable]) {
    def dResponse: Deferred[F, Either[Throwable, Response]]
    def handleRequest(f: this.type => F[Response]): F[Unit] =
        for {
            eResult <- f(this).attempt
            _ <- dResponse.complete(eResult)
        } yield ()

    def ?:(actorRef: ActorRef[F, this.type]): F[Either[Throwable, Response]] =
        for {
            _ <- actorRef ! this
            eResponse <- this.dResponse.get
        } yield eResponse
}

object SyncRequest {
    type DeferredResponse[F[+_], R] = Deferred[F, Either[Throwable, R]]
}
