package hydrozoa.timingviz

import cats.effect.std.Queue
import cats.effect.{IO, Resource}
import fs2.Stream
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.timingviz.Codecs.given
import io.circe.syntax.*
import org.http4s.Uri
import org.http4s.client.websocket.{WSClient, WSFrame, WSRequest}
import org.http4s.jdkhttpclient.JdkWSClient

/** Tiny WebSocket client that opens a connection to a running `hydrozoa.timingviz.Server` and
  * exposes a `Command => IO[Unit]` sink. Use with [[VizTracer.io]] to publish events from a running
  * Hydrozoa node:
  *
  * {{{
  *   VizClient.open(uri"ws://localhost:8765/ws").use { send =>
  *     val tr = VizTracer.io(send)
  *     // combine `tr` with your existing root MRM tracer via Monoid[ContraTracer[IO, MRMEvent]]
  *     ???
  *   }
  *   .void
  * }}}
  *
  * Failures during send are logged to stderr and swallowed — a tracing failure should never crash
  * the node it's observing.
  */
object VizClient:

    def open(
        uri: Uri
    )(using CardanoNetwork.Section): Resource[IO, Command => IO[Unit]] =
        for
            wsClient <- Resource.eval(JdkWSClient.simple[IO])
            sink <- openWith(wsClient, uri)
        yield sink

    /** Same as [[open]] but reuses a caller-supplied [[WSClient]]. */
    def openWith(
        client: WSClient[IO],
        uri: Uri
    )(using CardanoNetwork.Section): Resource[IO, Command => IO[Unit]] =
        for
            outbox <- Resource.eval(Queue.unbounded[IO, Command])
            wsConn <- client.connectHighLevel(WSRequest(uri))
            sendLoop = Stream
                .fromQueueUnterminated(outbox)
                .evalMap(cmd =>
                    wsConn
                        .send(WSFrame.Text(cmd.asJson.noSpaces))
                        .handleErrorWith(t => IO.println(s"VizClient send error: ${t.getMessage}"))
                )
            _ <- sendLoop.compile.drain.background
        yield (cmd: Command) => outbox.offer(cmd)
