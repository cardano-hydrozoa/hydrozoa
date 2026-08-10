package hydrozoa.multisig.ledger.remote

import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.Stream
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.l2.{L2CommandNumber, L2LedgerCommand, L2LedgerResponse}
import io.circe.syntax.*
import org.http4s.HttpRoutes
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.Server
import org.http4s.websocket.WebSocketFrame
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.Duration

/** Behavioral tests for [[RemoteL2Ledger]]'s response handling, driven against a fake WebSocket
  * server that replies with a crafted frame. Covers the per-command **narrowing**: a method returns
  * only its own [[L2LedgerResponse.Applied]] descendant (plus the shared coordination branches),
  * and **fail-stops** on anything else the remote could say — a wrong `Applied` variant, a response
  * echoing the wrong command number, or an undecodable frame. Those are protocol violations, not
  * one of the four verdicts, so they must raise rather than be returned (turning one into a verdict
  * would diverge this peer). The command driven is a no-op `ApplyDepositDecisions` (empty lists),
  * the simplest to construct; the server ignores the request payload and replies with the fixture.
  */
class RemoteL2LedgerTest extends AnyFunSuite:

    private val config: CardanoNetwork.Section =
        MultiNodeConfig.generateDefault
            .map(_.nodeConfigs(HeadPeerNumber.zero))
            .pureApply(Gen.Parameters.default, org.scalacheck.rng.Seed(0L))

    import RemoteL2LedgerCodecs.given

    private val noopTracer: ContraTracer[IO, RemoteL2LedgerEvent] =
        ContraTracer[IO, RemoteL2LedgerEvent](_ => IO.unit)

    private val noop: L2LedgerCommand.ApplyDepositDecisions =
        L2LedgerCommand.ApplyDepositDecisions(
          blockNumber = BlockNumber(1),
          blockCreationEndTime = BigInt(1),
          absorbedDeposits = Nil,
          rejectedDeposits = Nil
        )

    /** An OS-assigned free port (closed immediately; a fresh Ember server rebinds it below). */
    private def freePort: Int =
        val socket = new java.net.ServerSocket(0)
        try socket.getLocalPort
        finally socket.close()

    /** A WebSocket server on `/l2` that answers each incoming frame with the fixed `reply` text. */
    private def wsServer(port: Int, reply: String): Resource[IO, Server] =
        EmberServerBuilder
            .default[IO]
            .withHost(host"127.0.0.1")
            .withPort(Port.fromInt(port).get)
            .withShutdownTimeout(Duration.Zero)
            .withHttpWebSocketApp(wsb =>
                HttpRoutes
                    .of[IO] { case GET -> Root / "l2" =>
                        Queue.unbounded[IO, WebSocketFrame].flatMap { outbox =>
                            wsb.build(
                              Stream.fromQueueUnterminated(outbox),
                              _.evalMap {
                                  case _: WebSocketFrame.Text =>
                                      outbox.offer(WebSocketFrame.Text(reply))
                                  case _ => IO.unit
                              }
                            )
                        }
                    }
                    .orNotFound
            )
            .build

    private def encode(response: L2LedgerResponse): String = response.asJson.noSpaces

    /** Drive `applyDepositDecisions(commandNumber, noop)` against a server replying with `reply`.
      */
    private def drive(
        reply: String,
        commandNumber: L2CommandNumber
    ): Either[Throwable, L2LedgerResponse] =
        val port = freePort
        (wsServer(port, reply) *> RemoteL2Ledger.create(
          s"ws://127.0.0.1:$port/l2",
          config,
          noopTracer
        ))
            .use(_.applyDepositDecisions(commandNumber, noop))
            .attempt
            .unsafeRunSync()

    test(
      "applyDepositDecisions returns its narrowed Applied when the remote sends the matching variant"
    ) {
        val cn = L2CommandNumber(1L)
        val reply = encode(L2LedgerResponse.Applied.ApplyDepositDecisions(cn, Vector.empty))
        assert(
          drive(reply, cn).exists(_.isInstanceOf[L2LedgerResponse.Applied.ApplyDepositDecisions])
        )
    }

    test("applyDepositDecisions fail-stops when the remote answers with a wrong Applied variant") {
        val cn = L2CommandNumber(1L)
        val reply = encode(L2LedgerResponse.Applied.RegisterDeposit(cn))
        assert(drive(reply, cn).isLeft)
    }

    test("a response echoing the wrong command number fail-stops") {
        val reply = encode(
          L2LedgerResponse.Applied.ApplyDepositDecisions(L2CommandNumber(99L), Vector.empty)
        )
        assert(drive(reply, L2CommandNumber(1L)).isLeft)
    }

    test("an undecodable response frame fail-stops") {
        assert(drive("not a response", L2CommandNumber(1L)).isLeft)
    }
