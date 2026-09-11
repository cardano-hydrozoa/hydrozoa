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
  * is ever recovered. These pin both ways it can complete — an error (silence past the deadline)
  * and a normal return (the peer closed) — plus the case that must NOT complete: a quiet link kept
  * alive by keep-alive traffic alone.
  */
class WsDuplexTest extends AnyFunSuite {

    /** A connection driven by a schedule of `(delay, frame)`.
      *
      * `Some(frame)` delivers it after `delay`; `None` delivers end-of-stream, which is what a peer
      * closing its receiving side looks like. Running off the end of the schedule blocks forever,
      * modelling the half-open socket the read deadline exists to catch — so the two endings stay
      * distinguishable in tests rather than collapsing into one.
      */
    private def scheduled(
        frames: List[(scala.concurrent.duration.FiniteDuration, Option[WSFrame])],
        sent: Ref[IO, List[WSFrame]]
    ): WSConnection[IO] =
        new WSConnection[IO] {
            private val remaining =
                Ref.unsafe[IO, List[(scala.concurrent.duration.FiniteDuration, Option[WSFrame])]](
                  frames
                )
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
                        // Off the end of the schedule: never completes — a half-open socket.
                        case None         => IO.never
                        case Some((d, f)) => IO.sleep(d).as(f)
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
            List.fill(6)(10.seconds -> Some(WSFrame.Ping(scodec.bits.ByteVector.empty): WSFrame))
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
          1.second -> Some(WSFrame.Text("he", false): WSFrame),
          1.second -> Some(WSFrame.Text("ll", false): WSFrame),
          1.second -> Some(WSFrame.Text("o", true): WSFrame)
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

    test("a peer closing ends the run normally, and the close is echoed") {
        // The only path that produces DialerDisconnected: `run` must COMPLETE, not fail — a dialer
        // redials on completion, so this ending is as load-bearing as the timeout one.
        val frames = List(
          1.second -> Some(WSFrame.Text("hello", true): WSFrame),
          1.second -> Some(WSFrame.Close(1000, "bye"): WSFrame),
          1.second -> Option.empty[WSFrame] // receiving side closed
        )
        val prog = for {
            sent <- Ref.of[IO, List[WSFrame]](Nil)
            lines <- Ref.of[IO, List[String]](Nil)
            outbox <- Queue.unbounded[IO, String]
            c = scheduled(frames, sent)
            outcome <- WsDuplex
                .run(c, outbox, l => lines.update(_ :+ l), readIdleTimeout = 30.seconds)
                .attempt
            echoed <- sent.get.map(_.count(_.isInstanceOf[WSFrame.Close]))
            got <- lines.get
        } yield (outcome, echoed, got)
        val (outcome, echoed, got) = TestControl
            .executeEmbed(prog)
            .unsafeRunSync()(using cats.effect.unsafe.implicits.global)
        assert(outcome == Right(()) && echoed == 1 && got == List("hello"))
    }

    test("a peer that never ends a fragment run is abandoned rather than buffered forever") {
        // Unbounded accumulation is the failure mode being closed here: fail, so the dialer redials.
        val frames =
            List.fill(WsDuplex.maxTextFragments + 2)(
              1.second -> Some(WSFrame.Text("x", false): WSFrame)
            )
        val prog = for {
            sent <- Ref.of[IO, List[WSFrame]](Nil)
            outbox <- Queue.unbounded[IO, String]
            c = scheduled(frames, sent)
            outcome <- WsDuplex.run(c, outbox, _ => IO.unit, readIdleTimeout = 30.seconds).attempt
        } yield outcome
        val outcome = TestControl
            .executeEmbed(prog)
            .unsafeRunSync()(using cats.effect.unsafe.implicits.global)
        assert(outcome.left.exists(_.isInstanceOf[IllegalStateException]))
    }
}
