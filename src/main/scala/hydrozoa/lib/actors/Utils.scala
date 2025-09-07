package hydrozoa.lib.actors

import cats._
import cats.effect.Ref
import cats.syntax.all._
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.actor.ReplyingActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps

object Utils {
    /**
     * "Get and tell" a message to an actor inside a Ref-Option, yielding Unit if the option is empty.
     *
     * @param ref
     * @param msg
     * @tparam F
     * @tparam Request
     * @return
     */
    def gat[F[+_] : Monad, Request](ref: Ref[F, Option[ActorRef[F, Request]]], msg: Request): F[Unit] =
        for {
            mbActor <- ref.get
            _ <- mbActor match {
                case Some(actor) => actor ! msg
                case None => Monad[F].pure(())
            }
        } yield ()

    /**
     * "Get and ask" a message to an actor inside a Ref-Option, yielding Unit if the option is empty.
     *
     * @param ref
     * @param msg
     * @tparam F
     * @tparam Request
     * @return
     */
    def gask[F[+_] : Monad, Request, Response](ref: Ref[F, Option[ReplyingActorRef[F, Request, Response]]], msg: Request): F[Option[Response]] =
        for {
            mbActor <- ref.get
            mbMsg <- mbActor match {
                case Some(actor) => (actor ? msg).map(Some(_))
                case None => Monad[F].pure(None)
            }
        } yield mbMsg

    /**
     * "get and broadcast (tell)" (in parallel)
     * @param ref
     * @param msg
     * @tparam F
     * @tparam Request
     * @tparam Response
     * @return
     */
    def gab[F[+_] : Monad : Parallel, G[+_] : Traverse, Request](ref: Ref[F, Option[G[ActorRef[F, Request]]]], msg: Request): F[Unit] =
        for {
            mbActor <- ref.get
            _ <- mbActor match {
                case Some(actor) => (actor ! msg).parallel
                case None => Monad[F].pure(())
            }
        } yield ()


}
