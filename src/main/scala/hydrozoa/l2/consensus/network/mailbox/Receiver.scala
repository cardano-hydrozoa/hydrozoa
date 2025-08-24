package hydrozoa.l2.consensus.network.mailbox

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import com.typesafe.scalalogging.Logger
import hydrozoa.infra.Piper
import ox.*
import ox.channels.*
import ox.flow.Flow
import sttp.capabilities.WebSockets
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.server.netty.sync.{NettySyncServer, OxStreams}
import sttp.ws.WebSocketFrame

import scala.concurrent.duration.DurationInt

/** Likely, not an actor but something else that physically receives messages from multiple
  * [[TransmitterActor]]s and passes them to the [[InboxActor]] or [[OutboxActor]].
  */
abstract class Receiver(outboxActor: ActorRef[OutboxActor], inboxActor: ActorRef[InboxActor]):

    final def handleAppendEntries(batchMsg: BatchMsg): Unit =
        inboxActor.tell(_.appendEntries(batchMsg._1, batchMsg._2))

    final def handleConfirmMatchIndex(matchIndexMsg: MatchIndexMsg): Unit =
        outboxActor.tell(_.confirmMatchIndex(matchIndexMsg._1, matchIndexMsg._2))

/** A short-circuit receiver.
  *
  * @param outboxActor
  * @param inboxActor
  */
final class LocalReceiver(outboxActor: ActorRef[OutboxActor], inboxActor: ActorRef[InboxActor])
    extends Receiver(outboxActor, inboxActor)

// TODO: move away?
given batchMsgWSFCodec: Codec[WebSocketFrame, BatchMsg, CodecFormat.Json] =
    new Codec[WebSocketFrame, BatchMsg, CodecFormat.Json] {

        override def decode(wsf: WebSocketFrame): DecodeResult[BatchMsg] =
            wsf match
                case text: WebSocketFrame.Text =>
                    val batch: BatchMsg = readFromString(text.payload)
                    DecodeResult.Value.apply(batch)
                case _ =>
                    DecodeResult.Error(
                      "<string representation is not available>",
                      new RuntimeException("Unsupported web socket frame type")
                    )

        def encode(msgBatch: BatchMsg): WebSocketFrame =
            writeToString(msgBatch) |> WebSocketFrame.text

        override def rawDecode(l: WebSocketFrame): DecodeResult[BatchMsg] = ???

        override def schema: Schema[BatchMsg] = batchMsgSchema

        override def format: CodecFormat.Json = CodecFormat.Json()
    }

// TODO: move away?
given matchIndexMsgWSFCodec: Codec[WebSocketFrame, MatchIndexMsg, CodecFormat.Json] =
    new Codec[WebSocketFrame, MatchIndexMsg, CodecFormat.Json] {

        override def decode(wsf: WebSocketFrame): DecodeResult[MatchIndexMsg] =
            wsf match
                case text: WebSocketFrame.Text =>
                    val matchIndexMsg: MatchIndexMsg = readFromString(text.payload)
                    DecodeResult.Value.apply(matchIndexMsg)
                case _ =>
                    DecodeResult.Error(
                      "<string representation is not available>",
                      new RuntimeException("Unsupported web socket frame type")
                    )

        def encode(msgBatch: MatchIndexMsg): WebSocketFrame =
            writeToString(msgBatch) |> WebSocketFrame.text

        override def rawDecode(l: WebSocketFrame): DecodeResult[MatchIndexMsg] = ???

        override def schema: Schema[MatchIndexMsg] = matchIndexMsgSchema

        override def format: CodecFormat.Json = CodecFormat.Json()
    }

/** This receiver starts Netty server that serves `ws://0.0.0.0:<port>/ws` endpoint.
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

    // The WebSocket server endpoint type and definition
    type FullWs = Full[
      Unit, // _SECURITY_INPUT
      Unit, // _PRINCIPAL
      Unit, // _INPUT
      Unit, // _ERROR_OUTPUT
      Flow[BatchMsg] => Flow[Void], // _OUTPUT
      OxStreams & WebSockets, // R
      Identity // F
    ]

    val wsInboxEndpoint: FullWs =
        endpoint.get
            .in("ws")
            .out(
              // TODO: what should we use in lieu of MsgBatch for responses? We don't want responses at all...
              webSocketBody[BatchMsg, CodecFormat.Json, Void, CodecFormat.Json](OxStreams)
                  .concatenateFragmentedFrames(false)
                  .ignorePong(true)
                  .autoPongOnPing(true)
                  .decodeCloseRequests(false)
                  .decodeCloseResponses(false)
                  .autoPing(Some((10.seconds, WebSocketFrame.Ping("ping-content".getBytes))))
            )
            .handleSuccess(_ => {
                log.info("new ws channel established")
                in =>
                    in.tap(batchMsg => inboxActor.tell(_.appendEntries(batchMsg._1, batchMsg._2)))
                        .drain()
            })

    // TODO: move away?
    given voidSchema: Schema[Void] = Schema.binary[Void]

    // TODO: move away?
    given voidWSFCodec: Codec[WebSocketFrame, Void, CodecFormat.Json] =
        new Codec[WebSocketFrame, Void, CodecFormat.Json] {

            override def decode(wsf: WebSocketFrame): DecodeResult[Void] = ???

            def encode(msgBatch: Void): WebSocketFrame = ???

            override def rawDecode(l: WebSocketFrame): DecodeResult[Void] = ???

            override def schema: Schema[Void] = voidSchema

            override def format: CodecFormat.Json = ???
        }

    // Run the server
    val _ = useInScope(
      NettySyncServer()
          .host("0.0.0.0")
          .port(port)
          .addEndpoint(wsInboxEndpoint)
          .start()
    )(_.stop())
