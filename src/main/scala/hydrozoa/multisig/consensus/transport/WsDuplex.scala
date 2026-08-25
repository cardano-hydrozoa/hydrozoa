package hydrozoa.multisig.consensus.transport

import cats.data.Chain
import cats.effect.IO
import cats.effect.std.Queue
import fs2.Stream
import org.http4s.client.websocket.{WSConnection, WSFrame}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** The read+write loop for one established WS connection (the dialer side, shared by the head-mesh
  * and coil-uplink transports). Writer drains the outbox; reader hands each inbound text line to
  * `onLine`. Read and write run in parallel; whichever completes first cancels the other.
  *
  * Runs on the **low-level** [[WSConnection]] rather than `connectHighLevel`, because the
  * high-level connection swallows control frames: it auto-replies `Pong` and then recurses past the
  * `Ping` without surfacing it, and its `send` accepts data frames only. A dialer built on it
  * therefore cannot see the peer's keep-alive traffic, which is the only thing arriving on a
  * live-but-quiet link. In exchange this loop takes on what the high level did — `Pong` replies,
  * echoing `Close`, and defragmenting `Text` by the `last` flag.
  */
object WsDuplex {

    /** Fail the connection after this long with **no inbound frame of any kind**.
      *
      * A dialer has no other liveness signal: it re-dials only when [[run]] completes, so without a
      * deadline a half-open socket parks the reader forever while `send` keeps succeeding into a
      * buffer the peer will never read — the connection looks up and carries nothing, and no timer
      * anywhere breaks the tie (the writer's own traffic keeps TCP and any proxy idle-timer alive).
      * Unlike a server, a dialing peer has no `idleTimeout` catching it from the other side.
      *
      * Sized against `NodeWsServer.defaultKeepAlivePing` (10s): the peer pings well inside this
      * window, and a *quiet* link is therefore not a *silent* one. Keep this comfortably above the
      * ping interval so a missed ping does not by itself drop a healthy connection.
      */
    val defaultReadIdleTimeout: FiniteDuration = 30.seconds

    /** Fail the connection once this many `Text` fragments accumulate without a `last = true`.
      *
      * Defragmentation buffers until the peer marks the final frame, so a peer that never does
      * grows the accumulator without bound. `connectHighLevel` had the same exposure, but this is
      * our code now. Failing fits the same logic as the read deadline — the dialer redials, and a
      * bounded reconnect beats an unbounded heap.
      *
      * Generous on purpose: the liaison protocols send one line per frame, so exceeding this means
      * the peer is misbehaving, not that a message is merely large.
      */
    val maxTextFragments: Int = 1024

    def run(
        conn: WSConnection[IO],
        outbox: Queue[IO, String],
        onLine: String => IO[Unit],
        readIdleTimeout: FiniteDuration = defaultReadIdleTimeout
    ): IO[Unit] = {
        val writer: IO[Unit] =
            Stream
                .fromQueueUnterminated(outbox)
                .evalMap(line => conn.send(WSFrame.Text(line)))
                .compile
                .drain

        // `receive` yields None once the receiving side is closed, ending the stream and so the
        // race. The deadline rides on each read: any frame — including the peer's keep-alive
        // `Ping` — resets it, so it measures silence rather than idleness.
        val reader: IO[Unit] =
            Stream
                .repeatEval(conn.receive.timeout(readIdleTimeout))
                .unNoneTerminate
                // The accumulator carries pending `Text` fragments; only a `last` frame flushes it.
                .evalMapAccumulate(Chain.empty[String]) { (partial, frame) =>
                    frame match {
                        case WSFrame.Ping(data) => conn.send(WSFrame.Pong(data)).as((partial, ()))
                        // Echo the peer's close, as the high-level connection did; the next
                        // `receive` then yields None and the stream ends.
                        // `.attempt`: if the peer dropped TCP rather than just the WS half, echoing
                        // fails — and a clean close would then be reported as a DialerFailed,
                        // undoing the very distinction this logging draws.
                        case close: WSFrame.Close => conn.send(close).attempt.as((partial, ()))
                        case WSFrame.Text(text, true) =>
                            onLine((partial :+ text).toList.mkString).as((Chain.empty[String], ()))
                        case WSFrame.Text(text, false) =>
                            val next = partial :+ text
                            IO.raiseWhen(next.size > maxTextFragments)(
                              new IllegalStateException(
                                s"peer sent over $maxTextFragments Text fragments without a final" +
                                    " frame; abandoning the connection"
                              )
                            ).as((next, ()))
                        // Pong (our own keep-alive answered) and Binary: both protocols are
                        // text-only, and either still counts as evidence the peer is alive.
                        case _ => IO.pure((partial, ()))
                    }
                }
                .compile
                .drain

        IO.race(writer, reader).void
    }
}
