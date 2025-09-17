package hydrozoa.lib.actor

import cats.MonadError
import cats.effect.Deferred
import cats.syntax.all.*
import com.suprnation.actor.ActorRef.ActorRef

import scala.reflect.ClassTag

trait SyncRequest[F[+_], E <: Throwable, Response](implicit
    F: MonadError[F, Throwable],
    tag: ClassTag[E]
) {
    def dResponse: Deferred[F, Either[E, Response]]
    def handleRequest(f: this.type => F[Response]): F[Unit] =
        for {
            eResult <- f(this).attemptNarrow
            _ <- dResponse.complete(eResult)
        } yield ()

    def ?:(actorRef: ActorRef[F, this.type]): F[Either[E, Response]] =
        for {
            _ <- actorRef ! this
            eResponse <- this.dResponse.get
        } yield eResponse
}
