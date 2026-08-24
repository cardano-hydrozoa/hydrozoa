package hydrozoa.multisig.consensus.transport

import cats.effect.std.Queue
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import org.http4s.client.websocket.{WSConnection, WSFrame}
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt

/** [[WsDuplex]]'s liveness contract, on a virtual clock.
  *
  * A dialer re-dials only when `run` completes, so `run` completing is the *only* way a wedged link
  * is ever recovered. These pin the three outcomes: silence fails, keep-alive traffic alone keeps a
  * quiet link open, and a close ends it cleanly.
  */
class WsDuplexTest extends AnyFunSuite {

    /** A connection that emits `frames` on the given schedule, then goes silent forever. */
    private def scheduled(
        frames: List[(scala.concurrent.duration.FiniteDuration, WSFrame)],
        sent: Ref[IO, List[WSFrame]]
    ): WSConnection[IO] =
        new WSConnection[IO] {
            private val remaining =
                Ref.unsafe[IO, List[(scala.concurrent.duration.FiniteDuration, WSFrame)]](frames)
            override def send(wsf: WSFrame): IO[Unit] = sent.update(_ :+ wsf)
            override def sendMany[G[_]: cats.Foldable, A <: WSFrame](wsfs: G[A]): IO[Unit] =
                cats.Foldable[G].foldM(wsfs, ())((_, f) => send(f))
            override def receive: IO[Option[WSFrame]] =
                remaining
                    .modify {
                        case Nil          => (Nil, None)
                        case (d, f) :: tl => (tl, Some((d, f)))
                    }
                    .flatMap {
                        // Nothing left: never completes, modelling a half-open socket.
                        case None         => IO.never
                        case Some((d, f)) => IO.sleep(d).as(Some(f))
                    }
            override def subprotocol: Option[String] = None
        }

    test("a silent connection fails once the read deadline elapses, so the dialer can redial") {
        val prog = for {
            sent <- Ref.of[IO, List[WSFrame]](Nil)
            outbox <- Queue.unbounded[IO, String]
            // No frames at all — the half-open case.
            c = scheduled(Nil, sent)
            outcome <- WsDuplex.run(c, outbox, _ => IO.unit, readIdleTimeout = 30.seconds).attempt
        } yield outcome
        val result = TestControl
            .executeEmbed(prog)
            .unsafeRunSync()(using
              cats.effect.unsafe.implicits.global
            )
        assert(result.isLeft && result.left.exists(_.isInstanceOf[TimeoutException]))
    }

    test("the peer's keep-alive Ping holds a quiet link open past the deadline, and is Pong'd") {
        // Pings every 10s, as NodeWsServer.defaultKeepAlivePing sends. No data frames at all: this
        // is exactly the quiet-but-live link a data-only deadline would wrongly kill.
        val pings =
            List.fill(6)(10.seconds -> (WSFrame.Ping(scodec.bits.ByteVector.empty): WSFrame))
        val prog = for {
            sent <- Ref.of[IO, List[WSFrame]](Nil)
            outbox <- Queue.unbounded[IO, String]
            c = scheduled(pings, sent)
            outcome <- WsDuplex.run(c, outbox, _ => IO.unit, readIdleTimeout = 30.seconds).attempt
            pongs <- sent.get.map(_.count(_.isInstanceOf[WSFrame.Pong]))
        } yield (outcome, pongs)
        val (outcome, pongs) = TestControl
            .executeEmbed(prog)
            .unsafeRunSync()(using
              cats.effect.unsafe.implicits.global
            )
        // It survived all six pings (60s > the 30s deadline) and only failed in the silence after.
        assert(outcome.isLeft && pongs == 6)
    }

    test("fragmented text is reassembled into one line before dispatch") {
        val frames = List(
          1.second -> (WSFrame.Text("he", false): WSFrame),
          1.second -> (WSFrame.Text("ll", false): WSFrame),
          1.second -> (WSFrame.Text("o", true): WSFrame)
        )
        val prog = for {
            sent <- Ref.of[IO, List[WSFrame]](Nil)
            lines <- Ref.of[IO, List[String]](Nil)
            outbox <- Queue.unbounded[IO, String]
            c = scheduled(frames, sent)
            _ <- WsDuplex
                .run(c, outbox, l => lines.update(_ :+ l), readIdleTimeout = 30.seconds)
                .attempt
            got <- lines.get
        } yield got
        val got = TestControl
            .executeEmbed(prog)
            .unsafeRunSync()(using
              cats.effect.unsafe.implicits.global
            )
        assert(got == List("hello"))
    }
}
