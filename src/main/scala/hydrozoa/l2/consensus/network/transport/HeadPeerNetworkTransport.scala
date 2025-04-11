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
import hydrozoa.l2.consensus.network.actor.ConsensusActorFactory
import hydrozoa.node.TestPeer
import hydrozoa.node.server.Node
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
import sttp.tapir.*
import sttp.tapir.DecodeResult.Value
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.server.netty.sync.OxStreams.Pipe
import sttp.tapir.server.netty.sync.{NettySyncServer, OxStreams}
import sttp.ws.WebSocketFrame

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import hydrozoa.l2.consensus.network.{
    ackInitCodec,
    ackInitSchema,
    ackVerKeySchema,
    ackRefundLaterCodec,
    ackRefundLaterSchema,
    reqInitCodec,
    reqInitSchema,
    reqVerKeySchema,
    reqRefundLaterCodec,
    reqRefundLaterSchema,
    walletIdSchema,
    testPeerSchema
}
import hydrozoa.node.server.Node

trait HeadPeerNetworkTransport:

    /** Get the next number for a message, increases the counter.
      * @return
      *   the next number for a message
      */
    def nextSeq: Long

    /** Sends a message to all peers, and don't wait for a response.
      * @param msg
      */
    def broadcastMessage(seq: Option[Long] = None)(req: Req): Long

    def broadcastMessage(replyTo: TestPeer, replyToSeq: Long)(ack: Ack): Long

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
    def setNodeActorRef(nodeRef: ActorRef[Node]): Unit // FIXME
    def setConsensusActorFactory(consensusActorFactory: ConsensusActorFactory): Unit // FIXME
    def dispatchMessage(anyMsg: AnyMsg, reply: Ack => Long)(using Ox): Unit
    def spawnActorProactively(
        from: TestPeer,
        seq: Long,
        req: Req,
        send: Req => Long,
        reply: Ack => Long
    ): req.resultType

sealed trait Aux

case class ReqAux(from: TestPeer, seq: Long) extends Aux

given reqAuxSchema: Schema[ReqAux] =
    Schema.derived[ReqAux]

case class AckAux(from: TestPeer, seq: Long, replyTo: TestPeer, replyToSeq: Long) extends Aux

given ackAuxSchema: Schema[AckAux] =
    Schema.derived[AckAux]

trait HasMsgId:
    def msgId: Long

enum AnyMsg:
    case ReqVerKeyMsg(content: ReqVerKey, aux: ReqAux)
    case AckVerKeyMsg(content: AckVerKey, aux: AckAux)
    case ReqInitMsg(content: ReqInit, aux: ReqAux)
    case AckInitMsg(content: AckInit, aux: AckAux)
    case ReqRefundLaterMsg(content: ReqRefundLater, aux: ReqAux)
    case AckRefundLaterMsg(content: AckRefundLater, aux: AckAux)

    def getFromSeq: (TestPeer, Long) = this match
        case ReqVerKeyMsg(_, aux)      => aux.from -> aux.seq
        case AckVerKeyMsg(_, aux)      => aux.from -> aux.seq
        case ReqInitMsg(_, aux)        => aux.from -> aux.seq
        case AckInitMsg(_, aux)        => aux.from -> aux.seq
        case ReqRefundLaterMsg(_, aux) => aux.from -> aux.seq
        case AckRefundLaterMsg(_, aux) => aux.from -> aux.seq

    def asAck: Option[(TestPeer, Long, Ack)] = this match
        case AckVerKeyMsg(content, aux)      => Some(aux.replyTo, aux.replyToSeq, content)
        case AckInitMsg(content, aux)        => Some(aux.replyTo, aux.replyToSeq, content)
        case AckRefundLaterMsg(content, aux) => Some(aux.replyTo, aux.replyToSeq, content)
        case _                               => None

    def asReqOrAck: Either[(TestPeer, Long, Req), (TestPeer, Long, TestPeer, Long, Ack)] =
        this match
            case ReqVerKeyMsg(content, aux) =>
                Left(aux.from, aux.seq, content)
            case AckVerKeyMsg(content, aux) =>
                Right(aux.from, aux.seq, aux.replyTo, aux.replyToSeq, content)
            case ReqInitMsg(content, aux) =>
                Left(aux.from, aux.seq, content)
            case AckInitMsg(content, aux) =>
                Right(aux.from, aux.seq, aux.replyTo, aux.replyToSeq, content)
            case ReqRefundLaterMsg(content, aux) =>
                Left(aux.from, aux.seq, content)
            case AckRefundLaterMsg(content, aux) =>
                Right(aux.from, aux.seq, aux.replyTo, aux.replyToSeq, content)

    def origin: (TestPeer, Long) = this.asAck match
        case Some(from, seq, _) => (from, seq)
        case None               => this.getFromSeq

    def asMsg: Msg = this match
        case ReqVerKeyMsg(content, _)      => content
        case AckVerKeyMsg(content, _)      => content
        case ReqInitMsg(content, _)        => content
        case AckInitMsg(content, _)        => content
        case ReqRefundLaterMsg(content, _) => content
        case AckRefundLaterMsg(content, _) => content

object AnyMsg:
    def apply[A <: Aux](msg: Req, aux: ReqAux): AnyMsg = msg match
        case content: ReqVerKey      => ReqVerKeyMsg(content, aux)
        case content: ReqInit        => ReqInitMsg(content, aux)
        case content: ReqRefundLater => ReqRefundLaterMsg(content, aux)

    def apply[A <: Aux](msg: Ack, aux: AckAux): AnyMsg = msg match
        case content: AckVerKey      => AckVerKeyMsg(content, aux)
        case content: AckInit        => AckInitMsg(content, aux)
        case content: AckRefundLater => AckRefundLaterMsg(content, aux)

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

    override def nextSeq: Long =
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
                        val (from, seq) = anyMsg.getFromSeq
                        // If an ack -- pass to possible subscription channel
                        anyMsg.asAck match
                            case Some(replyTo, replyToSeq, ack) =>
                                subscriptions.get(replyToSeq).foreach(_.send(ack))
                            case _ => ()
                        // Always pass to the common dispatcher
                        handler.dispatchMessage(anyMsg, broadcastMessage(from, seq))
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

    override def broadcastMessage(seq: Option[Long])(req: Req): Long =
        val next: Long = seq.getOrElse(nextSeq)
        val aux = ReqAux(ownPeer, next)
        val anyMsg = AnyMsg(req, aux)
        log.info(s"Sending req: $anyMsg")
        outgoing.send(anyMsg)
        next

    override def broadcastMessage(replyTo: TestPeer, replyToSeq: Long)(ack: Ack): Long =
        log.info(s"broadcastMessage")
        val next = nextSeq
        val aux = AckAux(ownPeer, next, replyTo, replyToSeq)
        val anyMsg = AnyMsg(ack, aux)
        log.info(s"Sending an ack: $anyMsg")
        outgoing.send(anyMsg)
        next

    override def broadcastAndCollect[R <: Req](req: R): Source[req.ackType] =
        val next = nextSeq
        val aux = ReqAux(ownPeer, next)
        val anyMsg = AnyMsg(req, aux)
        log.info(s"Sending a req for sync acks: $anyMsg")
        val ch: Channel[Ack] = Channel.bufferedDefault
        subscriptions.put(next, ch)
        outgoing.send(anyMsg)
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
