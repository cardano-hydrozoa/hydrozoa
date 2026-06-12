package hydrozoa.multisig.consensus.liaison

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import org.scalatest.funsuite.AnyFunSuite

/** Unit tests for the [[Server]] serve-side state machine, with `Int`-cursor fake batch types so it
  * is exercised in isolation from real lanes.
  */
class ServerTest extends AnyFunSuite {

    final case class Get(batchNum: BatchNumber, cursor: Int)
    final case class New(batchNum: BatchNumber, payload: Option[Int])

    test("Server stashes an empty pull and re-answers after an append") {
        val queue = Ref.unsafe[IO, List[Int]](Nil)
        val sent = Ref.unsafe[IO, List[New]](Nil)

        def serve(g: Get): IO[Server.Served[New]] = queue.get.map { q =>
            q.find(_ >= g.cursor) match
                case Some(v) => Server.Served.Reply(New(g.batchNum, Some(v)))
                case None    => Server.Served.Empty
        }
        val server = new Server[Get, New](serve)(n => sent.update(_ :+ n))

        server.handleGet(Get(BatchNumber.zero, 5)).unsafeRunSync() // nothing yet -> stash
        val _ = assert(sent.get.unsafeRunSync().isEmpty)
        queue.set(List(5)).unsafeRunSync()
        server.afterAppend.unsafeRunSync() // content arrived -> answer
        assert(sent.get.unsafeRunSync() == List(New(BatchNumber.zero, Some(5))))
    }
}
