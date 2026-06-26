package hydrozoa.timingviz

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.quantize
import hydrozoa.timingviz.Codecs.given
import io.circe.parser as JsonParser
import io.circe.syntax.*
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** Batch-replay driver. Reads a JSON file containing an array of `Command`s, folds them through a
  * fresh `TimingVisualizerState`, prints the final `Frame` to stdout.
  *
  * Usage: `sbt "timingViz/runMain hydrozoa.timingviz.Replay path/to/commands.json"`
  */
object Replay extends IOApp:

    private val network: CardanoNetwork = CardanoNetwork.Preview
    given CardanoNetwork.Section = network

    def run(args: List[String]): IO[ExitCode] = args match
        case path :: Nil => replayFile(path)
        case _ =>
            IO.println("usage: Replay <commands.json>").as(ExitCode.Error)

    private def replayFile(path: String): IO[ExitCode] =
        val slotConfig = network.slotConfig
        val cfg = TxTiming.default(slotConfig)
        // Replay starts at epoch 0 so any forward-going AdvanceClock in the log is valid.
        val now0 = java.time.Instant.EPOCH.quantize(slotConfig)
        for
            text <- IO.blocking(
              new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8)
            )
            cmds <- IO.fromEither(JsonParser.parse(text).flatMap(_.as[List[Command]]))
            initial = TimingVisualizerState.empty(cfg, now0)
            (finalSt, rejections) = cmds.foldLeft((initial, List.empty[Rejection])) {
                case ((s, rs), cmd) =>
                    val (s2, out) = TimingVisualizer(cmd).run(s).value
                    val rs2 = out.result match
                        case Left(r) => r :: rs
                        case _       => rs
                    (s2, rs2)
            }
            _ <- IO.println(Presentation.render(finalSt).asJson.spaces2)
            _ <- IO.whenA(rejections.nonEmpty)(
              IO.println(s"\n${rejections.size} rejections during replay:")
                  *> rejections.reverse.traverse_(r => IO.println(s"  - $r"))
            )
        yield ExitCode.Success
