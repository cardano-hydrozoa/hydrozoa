package hydrozoa.timingviz

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.concurrent.Topic
import fs2.{Pipe, Stream}
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.timingviz.Codecs.given
import io.circe.parser as JsonParser
import io.circe.syntax.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.CORS
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.{HttpRoutes, Response}

/** HTTP+WebSocket front-end for the timing visualizer.
  *
  *   - `GET /frame` — current state as a JSON `Frame`.
  *   - `POST /replay` — body is a JSON array of `Command`s; applies all sequentially and returns
  *     the final `Frame`.
  *   - `GET /ws` — bi-directional channel. Server sends a `Frame` on connect and after every state
  *     change; client sends `Command` JSON messages.
  *
  * CORS is permissive so a frontend on a different origin (vite dev server) can connect.
  */
object Server extends IOApp.Simple:

    private val network: CardanoNetwork = CardanoNetwork.Preview
    given CardanoNetwork.Section = network

    def run: IO[Unit] =
        val slotConfig = network.slotConfig
        val cfg = TxTiming.default(slotConfig)
        for
            now0 <- IO.realTimeInstant.map(_.quantize(slotConfig))
            stateRef <- Ref.of[IO, TimingVisualizerState](
              TimingVisualizerState.empty(cfg, now0)
            )
            topic <- Topic[IO, Frame]
            _ <- IO.println(
              s"timing-viz server starting on http://localhost:8765 (initial now=$now0)"
            )
            _ <- EmberServerBuilder
                .default[IO]
                .withHost(host"0.0.0.0")
                .withPort(port"8765")
                .withHttpWebSocketApp(ws =>
                    CORS.policy.withAllowOriginAll(routes(stateRef, topic, ws)).orNotFound
                )
                .build
                .useForever
        yield ()

    private def routes(
        stateRef: Ref[IO, TimingVisualizerState],
        topic: Topic[IO, Frame],
        ws: WebSocketBuilder2[IO]
    ): HttpRoutes[IO] =
        HttpRoutes.of[IO] {
            case GET -> Root / "frame" =>
                stateRef.get.map(Presentation.render).flatMap(f => Ok(f.asJson))

            case req @ POST -> Root / "replay" =>
                for
                    cmds <- req.as[List[io.circe.Json]].map(_.flatMap(parseCommandJson))
                    _ <- cmds.traverse_(runCommand(_, stateRef, topic))
                    f <- stateRef.get.map(Presentation.render)
                    resp <- Ok(f.asJson)
                yield resp

            case GET -> Root / "ws" => websocketRoute(stateRef, topic, ws)
        }

    private def websocketRoute(
        stateRef: Ref[IO, TimingVisualizerState],
        topic: Topic[IO, Frame],
        ws: WebSocketBuilder2[IO]
    ): IO[Response[IO]] =
        val snapshot: Stream[IO, WebSocketFrame] =
            Stream.eval(stateRef.get).map(s => textFrame(Presentation.render(s)))
        val updates: Stream[IO, WebSocketFrame] =
            topic.subscribe(64).map(textFrame)
        val send: Stream[IO, WebSocketFrame] = snapshot ++ updates
        val recv: Pipe[IO, WebSocketFrame, Unit] = _.evalMap {
            case WebSocketFrame.Text(text, _) =>
                JsonParser.parse(text).flatMap(_.as[Command]) match
                    case Right(cmd) => runCommand(cmd, stateRef, topic).void
                    case Left(err)  => IO.println(s"ws: bad command JSON: ${err.getMessage}")
            case _ => IO.unit
        }
        ws.build(send, recv)

    private def textFrame(f: Frame): WebSocketFrame =
        WebSocketFrame.Text(f.asJson.noSpaces)

    private def parseCommandJson(j: io.circe.Json): List[Command] =
        j.as[Command].toOption.toList

    private def runCommand(
        cmd: Command,
        stateRef: Ref[IO, TimingVisualizerState],
        topic: Topic[IO, Frame]
    ): IO[TransitionOutput[Unit]] =
        for
            outAndNew <- stateRef.modify { s =>
                val (s2, out) = TimingVisualizer(cmd).run(s).value
                (s2, (out, s2))
            }
            (out, newState) = outAndNew
            _ <- topic.publish1(Presentation.render(newState))
        yield out
