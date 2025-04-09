package hydrozoa.l2.consensus.network.transport

import com.github.plokhotnyuk.jsoniter_scala.core.{
    JsonValueCodec,
    JsonWriter,
    readFromString,
    writeToString
}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.typesafe.scalalogging.Logger
import hydrozoa.infra.Piper
import hydrozoa.l2.consensus.network.*
import hydrozoa.node.TestPeer
import ox.*
import ox.channels.*
import ox.flow.Flow
import sttp.capabilities.WebSockets
import sttp.client4.*
import sttp.client4.impl.ox.ws.asSourceAndSink
import sttp.client4.ws.SyncWebSocket
import sttp.client4.ws.sync.*
import sttp.model.Uri
import sttp.shared.Identity
import sttp.tapir.DecodeResult.Value
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.server.netty.sync.OxStreams.Pipe
import sttp.tapir.server.netty.sync.{NettySyncServer, OxStreams}
import sttp.ws.WebSocketFrame

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

import hydrozoa.l2.consensus.network.ReqVerKey.given
import hydrozoa.l2.consensus.network.AckVerKey.given
import hydrozoa.l2.consensus.network.{reqVerKeySchema, ackVerKeySchema, testPeerSchema}

trait HeadPeerNetworkTransport:

    /** Sends a message to all peers, and don't wait for a response.
      * @param msg
      */
    def broadcastMessage(req: Req): Long

    def broadcastMessage(replyTo: Long)(ack: Ack): Long

    /** Sends a message to all peers, and get back a source channel with replies that may get back.
      * @param req
      *   request
      * @tparam R
      *   type of request
      * @return
      *   source of dependent responses
      */
    def broadcastAndCollect[R <: Req](req: R): Source[req.ackType]

end HeadPeerNetworkTransport

/** An interface to handle incoming messages that the node should provide.
  */
trait IncomingDispatcher:
    def dispatchMessage(payload: Msg, reply: Ack => Long): Unit

sealed trait Aux

case class ReqAux(sender: TestPeer, seq: Long) extends Aux

given reqAuxSchema: Schema[ReqAux] =
    Schema.derived[ReqAux]

case class AckAux(sender: TestPeer, seq: Long, replyTo: Long) extends Aux

given ackAuxSchema: Schema[AckAux] =
    Schema.derived[AckAux]

trait HasMsgId:
    def msgId: Long

enum AnyMsg:
    case ReqVerKeyMsg(content: ReqVerKey, aux: ReqAux)
    case AckVerKeyMsg(content: AckVerKey, aux: AckAux)

    def msgId: Long = this match
        case ReqVerKeyMsg(_, aux) => aux.seq
        case AckVerKeyMsg(_, aux) => aux.seq

    def asAck: Option[(Long, Ack)] = this match
        case AckVerKeyMsg(content, aux) => Some(aux.replyTo, content)
        case _                          => None

    def asMsg: Msg = this match
        case ReqVerKeyMsg(content, _) => content
        case AckVerKeyMsg(content, _) => content

object AnyMsg:
    def apply[A <: Aux](msg: Req, aux: ReqAux): AnyMsg = msg match
        case content: ReqVerKey => ReqVerKeyMsg(content, aux)

    def apply[A <: Aux](msg: Ack, aux: AckAux): AnyMsg = msg match
        case content: AckVerKey => AckVerKeyMsg(content, aux)

given anyMsgCodec: JsonValueCodec[AnyMsg] =
    JsonCodecMaker.make

given anyMsgSchema: Schema[AnyMsg] =
    Schema.derived[AnyMsg]

class HeadPeerNetworkTransportWS(
    ownPeer: TestPeer,
    ownPort: Int,
    peers: Map[TestPeer, Uri],
    handler: IncomingDispatcher
) extends HeadPeerNetworkTransport:

    private val log = Logger(getClass)

    // Global channel for incoming messages
    private val incoming: Channel[AnyMsg] = Channel.unlimited

    // Global channel for outgoing messages
    private val outgoing: Channel[AnyMsg] = Channel.unlimited

    // A list of private channels for every peer we are talking to
    private val outgoingChannels: mutable.Buffer[Channel[AnyMsg]] = mutable.Buffer.empty

    private val subscriptions: mutable.Map[Long, Channel[Ack]] = mutable.Map.empty

    private var counter: Long = 0 // FIXME: make thread-safe?

    def nextMsgNumber(): Long =
        val ret = counter + 1
        counter = ret
        ret

    // Creates a new channel and returns a flow from it
    private def mkOutgoingFlowCopy(): Flow[AnyMsg] =
        val newChannel: Channel[AnyMsg] = Channel.unlimited
        outgoingChannels.append(newChannel)
        Flow.fromSource(newChannel)

    private val serverPeers = peers.filter((k, _) => ownPeer.compareTo(k) > 0)
    log.info(s"peers to connect: $serverPeers")

    // "Server" component

    // Requests and responses can be treated separately despite the Pipe representation which is Flow[A] => Flow[B]
    // This is called for every new incoming connection
    def mkPipe(outgoingFlow: Flow[AnyMsg]): Pipe[AnyMsg, AnyMsg] =
        in =>
            in.tap(incoming.send)
                .drain()
                .merge(outgoingFlow)

    type FullWs = Full[
      Unit,
      Unit,
      Unit,
      Unit,
      Flow[AnyMsg] => Flow[AnyMsg],
      OxStreams & WebSockets,
      Identity
    ]

    // The WebSocket endpoint
    def wsConsensusEndpoint(): FullWs =
        endpoint.get
            .in("ws")
            .out(
              webSocketBody[AnyMsg, CodecFormat.Json, AnyMsg, CodecFormat.Json](OxStreams)
                  .concatenateFragmentedFrames(false)
                  .ignorePong(true)
                  .autoPongOnPing(true)
                  .decodeCloseRequests(false)
                  .decodeCloseResponses(false)
                  .autoPing(Some((10.seconds, WebSocketFrame.Ping("ping-content".getBytes))))
            )
            .handleSuccess(_ => mkPipe(mkOutgoingFlowCopy()))

    // "Client" component

    def wsConsensusClient()(ws: SyncWebSocket): Unit =
        supervised:
            val (wsSource, wsSink) = asSourceAndSink(ws)

            forkDiscard:
                mkOutgoingFlowCopy()
                    .runToChannel()
                    .map(anyMsg1WSFCodec.encode)
                    .pipeTo(wsSink, propagateDone = false)

            wsSource.foreach { wsf =>
                anyMsg1WSFCodec.decode(wsf) match
                    case DecodeResult.Value(v) => incoming.send(v)
                    case DecodeResult.Error(err, throwable) =>
                        log.error(err)
                        throw throwable
            }

    def run(): Unit =
        supervised {

            // Outgoing fanout thread
            forkDiscard {
                log.info("running outgoing fanout thread")
                Flow.fromSource(outgoing)
                    .runForeach(msg => outgoingChannels.foreach(ch => ch.send(msg)))
            }

            // Incoming channel reader
            forkDiscard {
                log.info("Running incoming reader...")
                Flow.fromSource(incoming)
                    .runForeach(anyMsg =>
                        log.info(s"Handling incoming msg: $anyMsg")
                        val msgId =
                            anyMsg.msgId // If an ack -- pass to possible subscription channel
                        anyMsg.asAck match
                            case Some(replyTo -> ack) =>
                                subscriptions.get(replyTo).foreach(_.send(ack))
                            case _ => ()
                        // Always pass to the common dispatcher
                        handler.dispatchMessage(anyMsg.asMsg, broadcastMessage(msgId))
                    )
            }

            // Server
            val _ = useInScope(
              NettySyncServer()
                  .port(ownPort)
                  .addEndpoint(wsConsensusEndpoint())
                  .start()
            )(_.stop())

            // Connections to "server" peers
            serverPeers.toList.foreachPar(serverPeers.size) { (peer, uri) =>
                log.info(s"connecting to $peer")

                // FIXME: do we need it?
                val backend = DefaultSyncBackend()
                try
                    basicRequest
                        .get(uri)
                        .response(asWebSocket(wsConsensusClient()))
                        .send(backend)
                        .discard
                finally
                    backend.close()
            }

            never
        }

    def broadcastMessage(req: Req): Long =
        val next = nextMsgNumber()
        val aux = ReqAux(ownPeer, next)
        outgoing.send(AnyMsg(req, aux))
        next

    def broadcastMessage(replyTo: Long)(ack: Ack): Long =
        val next = nextMsgNumber()
        val aux = AckAux(ownPeer, next, replyTo)
        outgoing.send(AnyMsg(ack, aux))
        next

    def broadcastAndCollect[R <: Req](req: R): Source[req.ackType] =
        val next = nextMsgNumber()
        val aux = ReqAux(ownPeer, next)
        val msg = AnyMsg(req, aux)
        val ch: Channel[Ack] = Channel.bufferedDefault
        subscriptions.put(next, ch)
        outgoing.send(msg)
        ch.asInstanceOf[Source[req.ackType]]

end HeadPeerNetworkTransportWS

object HeadPeerNetworkTransportWS:
    def apply(
        ownPeer: TestPeer,
        ownPort: Int,
        others: Map[TestPeer, Uri],
        handler: IncomingDispatcher
    ): HeadPeerNetworkTransportWS =
        new HeadPeerNetworkTransportWS(ownPeer, ownPort, others, handler)

given anyMsg1WSFCodec: Codec[WebSocketFrame, AnyMsg, CodecFormat.Json] =
    new Codec[WebSocketFrame, AnyMsg, CodecFormat.Json] {

        override def decode(wsf: WebSocketFrame): DecodeResult[AnyMsg] =
            wsf match
                case text: WebSocketFrame.Text =>
                    readFromString(text.payload) |> DecodeResult.Value.apply
                case _ =>
                    DecodeResult.Error(
                      "<string representation is not available>",
                      new RuntimeException("Unsupported web socket frame type")
                    )

        def encode(anyMsg: AnyMsg): WebSocketFrame =
            writeToString(anyMsg) |> WebSocketFrame.text

        override def rawDecode(l: WebSocketFrame): DecodeResult[AnyMsg] = ???

        override def schema: Schema[AnyMsg] = anyMsgSchema

        override def format: CodecFormat.Json = CodecFormat.Json()
    }
