package hydrozoa.l2.consensus.network

import com.typesafe.scalalogging.Logger
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
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.server.netty.sync.OxStreams.Pipe
import sttp.tapir.server.netty.sync.{NettySyncServer, OxStreams}
import sttp.ws.WebSocketFrame

import scala.collection.mutable

trait HeadPeerNetworkTransport:
    type MsgId
    type Msg
    type PeerIdentity
    type PeerConnInfo

    /** Sends a message to all peers, don't wait response
      * @param msg
      */
    def broadcastMessage(msg: Msg): Unit

/** An interface to handle incoming msgs that the node should provide.
  */
trait IncomingDispatcher[M]:
    def dispatchMessage(msg: M): Unit

class HeadPeerNetworkTransportWS(
    ownPeer: TestPeer,
    ownPort: Int,
    peers: Map[TestPeer, Uri],
    handler: IncomingDispatcher[String]
) extends HeadPeerNetworkTransport:

    override type MsgId = Int
    override type Msg = String
    override type PeerIdentity = TestPeer
    override type PeerConnInfo = Uri

    private val log = Logger(getClass)

    // Global channel for incoming messages
    private val incoming: Channel[Msg] = Channel.unlimited

    // Global channel for outgoing messages
    private val outgoing: Channel[Msg] = Channel.unlimited

    // A list of private channels for every peer we are talking to
    private val outgoingChannels: mutable.Buffer[Channel[Msg]] = mutable.Buffer.empty

    // Creates a new channel and returns a flow from it
    private def mkOutgoingFlowCopy(): Flow[Msg] =
        val newChannel: Channel[Msg] = Channel.unlimited
        outgoingChannels.append(newChannel)
        Flow.fromSource(newChannel)

    private val serverPeers = peers.filter((k, _) => ownPeer.compareTo(k) > 0)
    log.info(s"peers to connect: $serverPeers")

    // "Server" component

    // Requests and responses can be treated separately despite the Pipe representation which is Flow[A] => Flow[B]
    // This is called for every new incoming connection
    def mkPipe(outgoingFlow: Flow[Msg]): Pipe[Msg, Msg] =
        in =>
            in.tap(incoming.send)
                .drain()
                .merge(outgoingFlow)

    type FullWs = Full[
      Unit,
      Unit,
      Unit,
      Unit,
      Flow[Msg] => Flow[Msg],
      OxStreams & WebSockets,
      Identity
    ]

    // The WebSocket endpoint
    def wsConsensusEndpoint(): FullWs =
        endpoint.get
            .in("ws")
            .out(
              webSocketBody[Msg, CodecFormat.TextPlain, Msg, CodecFormat.TextPlain](OxStreams)
            )
            .handleSuccess(_ => mkPipe(mkOutgoingFlowCopy()))

    // "Client" component

    def wsConsensusClient()(ws: SyncWebSocket): Unit =
        supervised:
            val (wsSource, wsSink) = asSourceAndSink(ws)

            forkDiscard:
                mkOutgoingFlowCopy()
                    .runToChannel()
                    .map(WebSocketFrame.text)
                    .pipeTo(wsSink, propagateDone = true)

            wsSource.foreach: frame =>
                incoming.send(frame.toString)

    def run(): Unit =
        supervised {

            // Outgoing multiplexer
            forkDiscard {
                log.info("running outgoing multiplexer")
                Flow.fromSource(outgoing)
                    .runForeach(msg => outgoingChannels.foreach(ch => ch.send(msg)))
            }

            // Incoming reader
            forkDiscard {
                log.info("running incoming reader")
                Flow.fromSource(incoming)
                    .runForeach(msg =>
                        log.info(msg)
                        handler.dispatchMessage(msg)
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

    def broadcastMessage(msg: Msg): Unit =
        outgoing.send(msg)

end HeadPeerNetworkTransportWS

object HeadPeerNetworkTransportWS:
    def apply(
        ownPeer: TestPeer,
        ownPort: Int,
        others: Map[TestPeer, Uri],
        handler: IncomingDispatcher[String]
    ): HeadPeerNetworkTransportWS =
        new HeadPeerNetworkTransportWS(ownPeer, ownPort, others, handler)
