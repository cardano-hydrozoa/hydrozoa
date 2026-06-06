package hydrozoa.multisig.consensus.liaison

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.implicits.*
import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** Unit tests for the [[Puller]] / [[Server]] orchestration engines, with `Int`-cursor fake batch
  * types so the cross-lane state machine is exercised in isolation from real lanes.
  */
class BatchLinkTest extends AnyFunSuite {
    private given Logger[IO] = NoOpLogger[IO]

    final case class Get(batchNum: BatchNumber, cursor: Int)
    final case class New(batchNum: BatchNumber, payload: Option[Int])

    test("Puller advances on a matching reply, drops a stale one, rejects a gap") {
        val cursor = Ref.unsafe[IO, Int](0)
        val sent = Ref.unsafe[IO, List[Get]](Nil)
        val dispatched = Ref.unsafe[IO, List[Int]](Nil)

        def buildGet(bn: BatchNumber): IO[Get] = cursor.get.map(Get(bn, _))
        def accept(n: New): IO[Either[String, Unit]] = n.payload match
            case Some(v) =>
                cursor.get.flatMap(c =>
                    if v == c then cursor.set(c + 1).as(Right(()))
                    else IO.pure(Left(s"gap $v != $c"))
                )
            case None => IO.pure(Right(()))
        def dispatch(n: New): IO[Unit] = n.payload.traverse_(v => dispatched.update(_ :+ v))

        val puller = new Puller[Get, New](
          initialGet = Get(BatchNumber.zero, 0),
          buildGet = buildGet,
          accept = accept,
          dispatch = dispatch,
          getBatchNum = _.batchNum,
          newBatchNum = _.batchNum
        )(g => sent.update(_ :+ g))

        puller.start.unsafeRunSync()
        assert(sent.get.unsafeRunSync() == List(Get(BatchNumber.zero, 0)))

        puller.handleReply(New(BatchNumber.zero, Some(0))).unsafeRunSync()
        assert(cursor.get.unsafeRunSync() == 1)
        assert(dispatched.get.unsafeRunSync() == List(0))
        assert(sent.get.unsafeRunSync().last == Get(BatchNumber(1), 1))

        // Stale: batch 0 while the outstanding request is batch 1 -> dropped, no change.
        puller.handleReply(New(BatchNumber.zero, Some(1))).unsafeRunSync()
        assert(cursor.get.unsafeRunSync() == 1)

        // Gap: payload 2 but cursor is 1 -> rejected, no advance, no new request.
        val before = sent.get.unsafeRunSync().size
        puller.handleReply(New(BatchNumber(1), Some(2))).unsafeRunSync()
        assert(cursor.get.unsafeRunSync() == 1)
        assert(sent.get.unsafeRunSync().size == before)
    }

    test("Server stashes an empty pull and re-answers after an append") {
        val queue = Ref.unsafe[IO, List[Int]](Nil)
        val sent = Ref.unsafe[IO, List[New]](Nil)

        def serve(g: Get): IO[BatchLink.Served[New]] = queue.get.map { q =>
            q.find(_ >= g.cursor) match
                case Some(v) => BatchLink.Served.Reply(New(g.batchNum, Some(v)))
                case None    => BatchLink.Served.Empty
        }
        val server = new Server[Get, New](serve)(n => sent.update(_ :+ n))

        server.handleGet(Get(BatchNumber.zero, 5)).unsafeRunSync() // nothing yet -> stash
        assert(sent.get.unsafeRunSync().isEmpty)
        queue.set(List(5)).unsafeRunSync()
        server.afterAppend.unsafeRunSync() // content arrived -> answer
        assert(sent.get.unsafeRunSync() == List(New(BatchNumber.zero, Some(5))))
    }
}
