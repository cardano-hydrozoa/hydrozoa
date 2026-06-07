package hydrozoa.multisig.consensus.transport

import cats.effect.IO
import cats.effect.std.Queue
import fs2.Stream
import org.http4s.client.websocket.{WSConnectionHighLevel, WSFrame}

/** The read+write loop for one established high-level WS connection (the dialer side, shared by the
  * head-mesh and coil-uplink transports). Writer drains the outbox; reader hands each inbound text
  * line to `onLine`. Read and write run in parallel; whichever completes first cancels the other.
  */
object WsDuplex {

    def run(
        conn: WSConnectionHighLevel[IO],
        outbox: Queue[IO, String],
        onLine: String => IO[Unit]
    ): IO[Unit] = {
        val writer: IO[Unit] =
            Stream
                .fromQueueUnterminated(outbox)
                .evalMap(line => conn.send(WSFrame.Text(line)))
                .compile
                .drain

        val reader: IO[Unit] =
            conn.receiveStream
                .evalMap {
                    case WSFrame.Text(s, _) => onLine(s)
                    case _                  => IO.unit
                }
                .compile
                .drain

        IO.race(writer, reader).void
    }
}
