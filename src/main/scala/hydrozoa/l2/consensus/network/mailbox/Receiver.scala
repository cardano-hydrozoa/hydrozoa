package hydrozoa.l2.consensus.network.mailbox

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import com.typesafe.scalalogging.Logger
import hydrozoa.infra.Piper
import hydrozoa.l2.consensus.network.mailbox.MsgBatch.msgBatchSchema
import ox.*
import ox.channels.*
import ox.flow.Flow
import sttp.capabilities.WebSockets
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.server.netty.sync.OxStreams.Pipe
import sttp.tapir.server.netty.sync.{NettySyncServer, OxStreams}
import sttp.ws.WebSocketFrame

import scala.concurrent.duration.DurationInt

/** Likely, not an actor but something else that physically receives messages from multiple
  * [[TransmitterActor]]s and passes them to the [[InboxActor]] or [[OutboxActor]].
  */
abstract class Receiver(outboxActor: ActorRef[OutboxActor], inboxActor: ActorRef[InboxActor]):

    final def handleAppendEntries(from: PeerId, batch: MsgBatch): Unit =
        inboxActor.tell(_.appendEntries(from, batch))

    final def handleConfirmMatchIndex(from: PeerId, matchIndex: MatchIndex): Unit =
        outboxActor.tell(_.confirmMatchIndex(from, matchIndex))

/** A short-circuit receiver.
  *
  * @param outboxActor
  * @param inboxActor
  */
final class LocalReceiver(outboxActor: ActorRef[OutboxActor], inboxActor: ActorRef[InboxActor])
    extends Receiver(outboxActor, inboxActor)

/** This reveiver starts Netty server that serves `ws://0.0.0.0:<port>/ws` endpoint.
  *
  * @param outboxActor
  * @param inboxActor
  * @param port
  * @param ox
  */
class WSReceiver(
    outboxActor: ActorRef[OutboxActor],
    inboxActor: ActorRef[InboxActor],
    port: Int
)(using ox: Ox)
    extends Receiver(outboxActor, inboxActor):

    private val log = Logger(getClass)

    given msgBatchWSFCodec: Codec[WebSocketFrame, MsgBatch, CodecFormat.Json] =
        new Codec[WebSocketFrame, MsgBatch, CodecFormat.Json] {

            override def decode(wsf: WebSocketFrame): DecodeResult[MsgBatch] =
                wsf match
                    case text: WebSocketFrame.Text =>
                        val batch: MsgBatch = readFromString(text.payload)
                        DecodeResult.Value.apply(batch)
                    case _ =>
                        DecodeResult.Error(
                          "<string representation is not available>",
                          new RuntimeException("Unsupported web socket frame type")
                        )

            def encode(msgBatch: MsgBatch): WebSocketFrame =
                writeToString(msgBatch) |> WebSocketFrame.text

            override def rawDecode(l: WebSocketFrame): DecodeResult[MsgBatch] = ???

            override def schema: Schema[MsgBatch] = msgBatchSchema

            override def format: CodecFormat.Json = CodecFormat.Json()
        }

    given voidSchema: Schema[Void] = Schema.binary[Void]

    given voidWSFCodec: Codec[WebSocketFrame, Void, CodecFormat.Json] =
        new Codec[WebSocketFrame, Void, CodecFormat.Json] {

            override def decode(wsf: WebSocketFrame): DecodeResult[Void] = ???

            def encode(msgBatch: Void): WebSocketFrame = ???

            override def rawDecode(l: WebSocketFrame): DecodeResult[Void] = ???

            override def schema: Schema[Void] = voidSchema

            override def format: CodecFormat.Json = ???
        }

    // The WebSocket server endpoint type and definition
    type FullWs = Full[
      Unit, // _SECURITY_INPUT
      Unit, // _PRINCIPAL
      Unit, // _INPUT
      Unit, // _ERROR_OUTPUT
      Flow[MsgBatch] => Flow[Void], // _OUTPUT
      OxStreams & WebSockets, // R
      Identity // F
    ]

    def wsInboxEndpoint(): FullWs =
        endpoint.get
            .in("ws")
            .out(
              // TODO: what should we use in lieu of MsgBatch for responses? We don't want responses at all...
              webSocketBody[MsgBatch, CodecFormat.Json, Void, CodecFormat.Json](OxStreams)
                  .concatenateFragmentedFrames(false)
                  .ignorePong(true)
                  .autoPongOnPing(true)
                  .decodeCloseRequests(false)
                  .decodeCloseResponses(false)
                  .autoPing(Some((10.seconds, WebSocketFrame.Ping("ping-content".getBytes))))
            )
            .handleSuccess(_ => mkPipe())

    // This is called for every new incoming connection
    def mkPipe(): Pipe[MsgBatch, Void] = {
        log.info("mkPipe")
        in =>
            in.tap(batch => inboxActor.tell(_.appendEntries(PeerId("Alice"), batch)))
                .drain()
    }

    // Run the server
    val _ = useInScope(
      NettySyncServer()
          .host("0.0.0.0")
          .port(port)
          .addEndpoint(wsInboxEndpoint())
          .start()
    )(_.stop())
