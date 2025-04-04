package hydrozoa

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
import sttp.tapir.*
import sttp.tapir.server.netty.sync.OxStreams.Pipe
import sttp.tapir.server.netty.sync.{NettySyncServer, OxStreams}
import sttp.ws.WebSocketFrame

import scala.concurrent.duration.*

/** Three nodes will set up all WS connections in a deterministic order:
  *   - every node exposes a WS server endpoint for duplex communication
  *   - every node connects to all other nodes which precede it being sorted lexicographically
  *   - reties are missing, so the order currently matters
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

    // "server" component

    // requests and responses can be treated separately
    // despite the Pipe representation
    // not nice, but works
    def wsPipe(peer: TestPeer): Pipe[String, String] = in =>
        // emit periodic messages with peer's timestamp
        val responseFlow: Flow[String] = Flow
            .tick(1.second)
            .map(_ =>
                val time = System.currentTimeMillis()
                s"$peer time is $time (S)"
            )

        // tap to log whatever is sent by the client,
        // but complete the stream once the client closes
        in.tap(log.info).drain().merge(responseFlow, propagateDoneLeft = true)

    // Web socket endpoint
    val wsEndpoint =
        endpoint.get
            .in("ws")
            .out(
              webSocketBody[String, CodecFormat.TextPlain, String, CodecFormat.TextPlain](OxStreams)
            )

    // The WebSocket endpoint, builds the pipeline in serverLogicSuccess
    def wsConsensusEndpoint(peer: TestPeer) = wsEndpoint.handleSuccess(i => wsPipe(peer))

    // "client" component
    def useWebSocket(peer: TestPeer)(ws: SyncWebSocket): Unit =
        supervised:
            val (wsSource, wsSink) = asSourceAndSink(ws)

            val inputs = Flow
                .tick(1.second, ())
                .runToChannel()
                .map(_ =>
                    val time = System.currentTimeMillis()
                    WebSocketFrame.text(s"$peer time is $time (C)")
                )

            forkDiscard:
                inputs.pipeTo(wsSink, propagateDone = true)

            wsSource.foreach: frame =>
                log.info(s"RECEIVED: $frame")

    extension (peer: TestPeer)
        def compareTo(another: TestPeer): Int = peer.toString.compareTo(another.toString)

    def main(args: Array[String]): Unit = {

        val ownPeer = TestPeer.valueOf(args.apply(0))
        val ownPort = peers.get(ownPeer).get.port.get

        log.info(s"own peer: $ownPeer, own port: $ownPort")

        val serverPeers = peers.filter((k, _) => ownPeer.compareTo(k) > 0)

        log.info(s"peers to connect: $serverPeers")

        supervised {
            val _ = useInScope(
              NettySyncServer()
                  .host("0.0.0.0")
                  .port(ownPort)
                  .addEndpoints(List(wsConsensusEndpoint(ownPeer)))
                  .start()
            )(_.stop)

            serverPeers.toList.foreachPar(serverPeers.size) { (peer, uri) =>
                log.info(s"connecting to $peer")
                val backend = DefaultSyncBackend()
                try
                    basicRequest
                        .get(uri)
                        .response(asWebSocket(useWebSocket(ownPeer)))
                        .send(backend)
                        .discard
                finally
                    backend.close()
            }

            never
        }
    }
