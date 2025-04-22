package hydrozoa.ws

import com.typesafe.scalalogging.Logger
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.{Alice, Bob, Carol}
import ox.*
import ox.channels.*
import ox.flow.Flow
import sttp.capabilities.WebSockets
import sttp.client4.*
import sttp.client4.impl.ox.ws.*
import sttp.client4.ws.SyncWebSocket
import sttp.client4.ws.sync.*
import sttp.shared.Identity
import sttp.tapir.*
import sttp.tapir.server.ServerEndpoint.Full
import sttp.tapir.server.netty.sync.OxStreams.Pipe
import sttp.tapir.server.netty.sync.{NettySyncServer, OxStreams}
import sttp.ws.WebSocketFrame

import scala.collection.mutable
import scala.concurrent.duration.*

/** Three nodes will set up all WS connections in a deterministic order:
  *   - every node exposes a WS server endpoint for duplex communication
  *   - every node connects to all other nodes which precede it being sorted lexicographically
  *   - retries are missing, so the order of running currently matters
  */

val peers = Map.from(
  List(
    Alice -> uri"ws://localhost:4937/ws",
    Bob -> uri"ws://localhost:4938/ws",
    Carol -> uri"ws://localhost:4939/ws"
  )
)

object HydrozoaNetworkPeer:

    private val log = Logger(getClass)

    // Global channel for incoming messages
    private val incoming: Channel[String] = Channel.unlimited

    // Global channel for outgoing messages
    private val outgoing: Channel[String] = Channel.unlimited

    // A list of private channels for every peer we are talking to
    private val outgoingChannels: mutable.Buffer[Channel[String]] = mutable.Buffer.empty

    // Creates a new channel and returns a flow from it
    private def mkOutgoingFlowCopy(): Flow[String] =
        val newChannel: Channel[String] = Channel.unlimited
        outgoingChannels.append(newChannel)
        Flow.fromSource(newChannel)

    // "Server" component

    // Requests and responses can be treated separately despite the Pipe representation which is Flow[A] => Flow[B]
    // This is called for every new incoming connection
    def mkPipe(outgoingFlow: Flow[String]): Pipe[String, String] =
        in =>
            in.tap(incoming.send)
                .drain()
                .merge(outgoingFlow)

    type FullWs = Full[
      Unit,
      Unit,
      Unit,
      Unit,
      Flow[String] => Flow[String],
      OxStreams & WebSockets,
      Identity
    ]

    // The WebSocket endpoint
    def wsConsensusEndpoint(): FullWs =
        endpoint.get
            .in("ws")
            .out(
              webSocketBody[String, CodecFormat.TextPlain, String, CodecFormat.TextPlain](OxStreams)
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

    extension (peer: TestPeer)
        def compareTo(another: TestPeer): Int = peer.toString.compareTo(another.toString)

    def main(args: Array[String]): Unit = {

        val ownPeer = TestPeer.valueOf(args.apply(0))
        val ownPort = peers(ownPeer).port.get

        log.info(s"own peer: $ownPeer, own port: $ownPort")

        val serverPeers = peers.filter((k, _) => ownPeer.compareTo(k) > 0)

        log.info(s"peers to connect: $serverPeers")

        supervised {

//            // Produces messages
//            forkDiscard {
//                var cnt = 0
//                Flow
//                    .tick(1.second, ())
//                    .map(_ =>
//                        val time = System.currentTimeMillis()
//                        cnt = cnt + 1
//                        s"Msg #$cnt $ownPeer time is $time"
//                    )
//                    .runForeach(outgoing.send)
//            }

            // Outgoing multiplexer
            forkDiscard {
                Flow.fromSource(outgoing)
                    .runForeach(msg => outgoingChannels.foreach(ch => ch.send(msg)))
            }

            // Incoming reader
            forkDiscard {
                Flow.fromSource(incoming)
                    .runForeach(log.info)
            }

            // Server
            val _ = useInScope(
              NettySyncServer()
                  .port(ownPort)
                  .addEndpoint(wsConsensusEndpoint())
                  .start()
            )(_.stop())

            // Connections to peers
            serverPeers.toList.foreachPar(serverPeers.size) { (peer, uri) =>
                log.info(s"connecting to $peer")
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
    }
