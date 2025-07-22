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
import hydrozoa.l2.consensus.ConsensusDispatcher
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
import sttp.tapir.*
import sttp.tapir.DecodeResult.Value
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.server.netty.sync.OxStreams.Pipe
import sttp.tapir.server.netty.sync.{NettySyncServer, OxStreams}
import sttp.ws.WebSocketFrame

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import hydrozoa.l2.consensus.network.{
    ackFinal2Codec,
    ackFinal2Schema,
    ackFinalCodec,
    ackFinalSchema,
    ackInitCodec,
    ackInitSchema,
    ackDeinitCodec,
    ackDeinitSchema,
    ackMajor2Codec,
    ackMajor2Schema,
    ackMajorCodec,
    ackMajorSchema,
    ackMinorCodec,
    ackMinorSchema,
    ackRefundLaterCodec,
    ackRefundLaterSchema,
    ackVerKeySchema,
    reqEventL2Schema,
    reqFinalCodec,
    reqFinalSchema,
    reqInitCodec,
    reqInitSchema,
    reqDeinitCodec,
    reqDeinitSchema,
    reqMajorCodec,
    reqMajorSchema,
    reqMinorCodec,
    reqMinorSchema,
    reqRefundLaterCodec,
    reqRefundLaterSchema,
    reqVerKeySchema,
    testPeerSchema
}
import hydrozoa.node.server.Node

trait HeadPeerNetworkTransport:

    /** Installs the dispatcher actor.
      */
    def setDispatcher(dispatcher: ActorRef[ConsensusDispatcher]): Unit

    /** Runs the transport.
      */
    def run(): Unit

    /** Get the next number for a message, increases the counter.
      * @return
      *   the next number for a message
      */
    def nextSeq: Long

    /** Sends a request to all peers, returns immediately.
      */
    def broadcastReq(seq: Option[Long] = None)(req: Req): Long

    /** Sends an ack as a reply to a request.
      */
    def broadcastAck(replyTo: TestPeer, replyToSeq: Long)(ack: Ack): Long

end HeadPeerNetworkTransport

sealed trait Aux

case class ReqAux(from: TestPeer, seq: Long) extends Aux

given reqAuxSchema: Schema[ReqAux] =
    Schema.derived[ReqAux]

case class AckAux(from: TestPeer, seq: Long, replyTo: TestPeer, replyToSeq: Long) extends Aux

given ackAuxSchema: Schema[AckAux] =
    Schema.derived[AckAux]

enum AnyMsg:
    case ReqVerKeyMsg(content: ReqVerKey, aux: ReqAux)
    case AckVerKeyMsg(content: AckVerKey, aux: AckAux)
    case ReqInitMsg(content: ReqInit, aux: ReqAux)
    case AckInitMsg(content: AckInit, aux: AckAux)
    case ReqRefundLaterMsg(content: ReqRefundLater, aux: ReqAux)
    case AckRefundLaterMsg(content: AckRefundLater, aux: AckAux)
    case ReqEventL2Msg(content: ReqEventL2, aux: ReqAux)
    case AckUnitMsg(aux: AckAux)
    case ReqMinorMsg(content: ReqMinor, aux: ReqAux)
    case AckMinorMsg(content: AckMinor, aux: AckAux)
    case ReqMajorMsg(content: ReqMajor, aux: ReqAux)
    case AckMajorMsg(content: AckMajor, aux: AckAux)
    case AckMajor2Msg(content: AckMajor2, aux: AckAux)
    case ReqFinalMsg(content: ReqFinal, aux: ReqAux)
    case AckFinalMsg(content: AckFinal, aux: AckAux)
    case AckFinal2Msg(content: AckFinal2, aux: AckAux)
    case ReqDeinitMsg(content: ReqDeinit, aux: ReqAux)
    case AckDeinitMsg(content: AckDeinit, aux: AckAux)

    def getFromSeq: (TestPeer, Long) = this match
        case ReqVerKeyMsg(_, aux)      => aux.from -> aux.seq
        case AckVerKeyMsg(_, aux)      => aux.from -> aux.seq
        case ReqInitMsg(_, aux)        => aux.from -> aux.seq
        case AckInitMsg(_, aux)        => aux.from -> aux.seq
        case ReqRefundLaterMsg(_, aux) => aux.from -> aux.seq
        case AckRefundLaterMsg(_, aux) => aux.from -> aux.seq
        case ReqEventL2Msg(_, aux)     => aux.from -> aux.seq
        case AckUnitMsg(aux)           => aux.from -> aux.seq
        case ReqMinorMsg(_, aux)       => aux.from -> aux.seq
        case AckMinorMsg(_, aux)       => aux.from -> aux.seq
        case ReqMajorMsg(_, aux)       => aux.from -> aux.seq
        case AckMajorMsg(_, aux)       => aux.from -> aux.seq
        case AckMajor2Msg(_, aux)      => aux.from -> aux.seq
        case ReqFinalMsg(_, aux)       => aux.from -> aux.seq
        case AckFinalMsg(_, aux)       => aux.from -> aux.seq
        case AckFinal2Msg(_, aux)      => aux.from -> aux.seq
        case ReqDeinitMsg(_, aux)      => aux.from -> aux.seq
        case AckDeinitMsg(_, aux)      => aux.from -> aux.seq

    def asAck: Option[(TestPeer, Long, Ack)] = this match
        case AckVerKeyMsg(content, aux)      => Some(aux.replyTo, aux.replyToSeq, content)
        case AckInitMsg(content, aux)        => Some(aux.replyTo, aux.replyToSeq, content)
        case AckRefundLaterMsg(content, aux) => Some(aux.replyTo, aux.replyToSeq, content)
        case AckUnitMsg(aux)                 => Some(aux.replyTo, aux.replyToSeq, AckUnit())
        case AckMinorMsg(content, aux)       => Some(aux.replyTo, aux.replyToSeq, content)
        case AckMajorMsg(content, aux)       => Some(aux.replyTo, aux.replyToSeq, content)
        case AckMajor2Msg(content, aux)      => Some(aux.replyTo, aux.replyToSeq, content)
        case AckFinalMsg(content, aux)       => Some(aux.replyTo, aux.replyToSeq, content)
        case AckFinal2Msg(content, aux)      => Some(aux.replyTo, aux.replyToSeq, content)
        case AckDeinitMsg(content, aux)      => Some(aux.replyTo, aux.replyToSeq, content)
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
            case ReqEventL2Msg(content, aux) =>
                Left(aux.from, aux.seq, content)
            case AckUnitMsg(aux) =>
                Right(aux.from, aux.seq, aux.replyTo, aux.replyToSeq, AckUnit())
            case ReqMinorMsg(content, aux) =>
                Left(aux.from, aux.seq, content)
            case AckMinorMsg(content, aux) =>
                Right(aux.from, aux.seq, aux.replyTo, aux.replyToSeq, content)
            case ReqMajorMsg(content, aux) =>
                Left(aux.from, aux.seq, content)
            case AckMajorMsg(content, aux) =>
                Right(aux.from, aux.seq, aux.replyTo, aux.replyToSeq, content)
            case AckMajor2Msg(content, aux) =>
                Right(aux.from, aux.seq, aux.replyTo, aux.replyToSeq, content)
            case ReqFinalMsg(content, aux) =>
                Left(aux.from, aux.seq, content)
            case AckFinalMsg(content, aux) =>
                Right(aux.from, aux.seq, aux.replyTo, aux.replyToSeq, content)
            case AckFinal2Msg(content, aux) =>
                Right(aux.from, aux.seq, aux.replyTo, aux.replyToSeq, content)
            case ReqDeinitMsg(content, aux) =>
                Left(aux.from, aux.seq, content)
            case AckDeinitMsg(content, aux) =>
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
        case ReqEventL2Msg(content, _)     => content
        case AckUnitMsg(aux)               => AckUnit()
        case ReqMinorMsg(content, _)       => content
        case AckMinorMsg(content, _)       => content
        case ReqMajorMsg(content, _)       => content
        case AckMajorMsg(content, _)       => content
        case AckMajor2Msg(content, _)      => content
        case ReqFinalMsg(content, _)       => content
        case AckFinalMsg(content, _)       => content
        case AckFinal2Msg(content, _)      => content
        case ReqDeinitMsg(content, _)      => content
        case AckDeinitMsg(content, _)      => content

object AnyMsg:
    def apply[A <: Aux](msg: Req, aux: ReqAux): AnyMsg = msg match
        case content: ReqVerKey      => ReqVerKeyMsg(content, aux)
        case content: ReqInit        => ReqInitMsg(content, aux)
        case content: ReqRefundLater => ReqRefundLaterMsg(content, aux)
        case content: ReqEventL2     => ReqEventL2Msg(content, aux)
        case content: ReqMinor       => ReqMinorMsg(content, aux)
        case content: ReqMajor       => ReqMajorMsg(content, aux)
        case content: ReqFinal       => ReqFinalMsg(content, aux)
        case content: ReqDeinit      => ReqDeinitMsg(content, aux)

    def apply[A <: Aux](msg: Ack, aux: AckAux): AnyMsg = msg match
        case content: AckVerKey      => AckVerKeyMsg(content, aux)
        case content: AckInit        => AckInitMsg(content, aux)
        case content: AckRefundLater => AckRefundLaterMsg(content, aux)
        case _: AckUnit              => AckUnitMsg(aux)
        case content: AckMinor       => AckMinorMsg(content, aux)
        case content: AckMajor       => AckMajorMsg(content, aux)
        case content: AckMajor2      => AckMajor2Msg(content, aux)
        case content: AckFinal       => AckFinalMsg(content, aux)
        case content: AckFinal2      => AckFinal2Msg(content, aux)
        case content: AckDeinit      => AckDeinitMsg(content, aux)

given anyMsgCodec: JsonValueCodec[AnyMsg] =
    JsonCodecMaker.make

given anyMsgSchema: Schema[AnyMsg] =
    Schema.derived[AnyMsg]

class HeadPeerNetworkTransportWS(
    ownPeer: TestPeer,
    ownPort: Int,
    peers: Map[TestPeer, Uri]
) extends HeadPeerNetworkTransport:

    private val log = Logger(getClass)

    private var dispatcher: ActorRef[ConsensusDispatcher] = _
    override def setDispatcher(dispatcher: ActorRef[ConsensusDispatcher]): Unit =
        this.dispatcher = dispatcher

    // Global channel for incoming messages
    private val incoming: Channel[AnyMsg] = Channel.unlimited

    // Global channel for outgoing messages
    private val outgoing: Channel[AnyMsg] = Channel.unlimited

    // A list of private channels for every peer we are talking to
    private val outgoingChannels: mutable.Buffer[Channel[AnyMsg]] = mutable.Buffer.empty

    private var counter: Long = 0

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

    override def run(): Unit =
        supervised {

            // Outgoing fanout thread
            forkDiscard {
                log.info("running outgoing fanout thread")
                Flow.fromSource(outgoing)
                    .runForeach(msg =>
                        log.info(s"fanning out msg: $msg")
                        outgoingChannels.foreach(ch => ch.send(msg))
                    )
            }

            // Incoming channel reader
            forkDiscard {
                log.info("Running incoming reader...")
                Flow.fromSource(incoming)
                    .runForeach(anyMsg =>
                        log.info(s"Handling incoming msg: $anyMsg")
                        val (from, seq) = anyMsg.origin
                        dispatcher.tell(_.dispatchMessage(anyMsg))
                    )
            }

            // Server
            val _ = useInScope(
              NettySyncServer()
                  .host("0.0.0.0")
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

    override def broadcastReq(seq: Option[Long])(req: Req): Long =
        log.info(s"broadcastReq")
        val next: Long = seq.getOrElse(nextSeq)
        val aux = ReqAux(ownPeer, next)
        val anyMsg = AnyMsg(req, aux)
        log.info(s"Sending req: $anyMsg")
        outgoing.send(anyMsg)
        log.info(s"Done!")
        next

    override def broadcastAck(replyTo: TestPeer, replyToSeq: Long)(ack: Ack): Long =
        log.info(s"broadcastAck")
        val next = nextSeq
        val aux = AckAux(ownPeer, next, replyTo, replyToSeq)
        val anyMsg = AnyMsg(ack, aux)
        log.info(s"Sending ack: $anyMsg")
        outgoing.send(anyMsg)
        log.info(s"Done!")
        next

end HeadPeerNetworkTransportWS

object HeadPeerNetworkTransportWS:
    def apply(
        ownPeer: TestPeer,
        ownPort: Int,
        others: Map[TestPeer, Uri]
    ): HeadPeerNetworkTransport =
        new HeadPeerNetworkTransportWS(ownPeer, ownPort, others)

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
