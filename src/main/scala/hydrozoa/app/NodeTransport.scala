package hydrozoa.app

import cats.effect.{IO, Resource}
import cats.implicits.*
import com.comcast.ip4s.{Host, Port}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.NodeConfig
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import hydrozoa.multisig.consensus.transport.{CoilPeerWsTransport, HubWsTransport, NodeWsServer, PeerWsTransport}
import hydrozoa.multisig.{CoilMultisigRegimeManager, HeadMultisigRegimeManager}
import org.http4s.Uri
import org.http4s.client.websocket.WSClient
import org.http4s.server.websocket.WebSocketBuilder2

/** Builds a node's per-process WebSocket transports from the shared head config, binds its server,
  * dials its peers, and yields the manager wiring carrier ([[HeadMultisigRegimeManager.WsWiring]] /
  * [[CoilMultisigRegimeManager.WsWiring]]). The bound server and dialer fibers live for the
  * returned resource's lifetime.
  *
  * Each head peer's `webSocketAddress` (e.g. "ws://host:port") is both where that peer binds its
  * own server and where the other peers dial it: a head mounts the `/head` route, a hub head
  * additionally `/hub`, and a coil peer binds no server (it only dials its hub's `/hub`).
  */
object NodeTransport {

    /** Wire a head (or hub) node: the head-mesh transport toward the other head peers, the hub
      * transport for any hubbed coils, a bound server, and the mesh dialers.
      */
    def headResource(
        config: NodeConfig,
        client: WSClient[IO],
        ownHeadNum: HeadPeerNumber
    )(using CardanoNetwork.Section): Resource[IO, HeadMultisigRegimeManager.WsWiring] =
        for {
            ownHeadPeerId <- Resource.eval(
              IO.fromOption(config.headPeerIds.find(_.peerNum == ownHeadNum))(
                new RuntimeException(s"own head peer $ownHeadNum is not in the head config")
              )
            )
            remotes <- Resource.eval(
              config.headPeerIds.toList
                  .filterNot(_.peerNum == ownHeadNum)
                  .traverse(rid => dialUri(config, rid.peerNum, "head").map(rid -> _))
                  .map(_.toMap)
            )
            meshTransport <- Resource.eval(PeerWsTransport.create(ownHeadPeerId, remotes))

            hubbedCoils = config.hubbedCoilPeerNums(ownHeadNum)
            hubTransport <- Resource.eval(
              if hubbedCoils.isEmpty then IO.none[HubWsTransport]
              else HubWsTransport.create(hubbedCoils).map(Some(_))
            )

            bind <- Resource.eval(bindAddress(config, ownHeadNum))
            meshRoute = (wsb: WebSocketBuilder2[IO]) => meshTransport.routes(wsb)
            hubRoute = hubTransport.toList.map(t => (wsb: WebSocketBuilder2[IO]) => t.routes(wsb))
            _ <- NodeWsServer.resource(bind._1, bind._2, meshRoute :: hubRoute)
            _ <- meshTransport.startDialers(client)
        } yield HeadMultisigRegimeManager.WsWiring(meshTransport, hubTransport)

    /** Wire a coil node: the uplink transport toward its hub plus the hub dialer. No server — a
      * coil peer is never dialed.
      */
    def coilResource(
        config: NodeConfig,
        client: WSClient[IO],
        ownCoilNum: CoilPeerNumber
    )(using CardanoNetwork.Section): Resource[IO, CoilMultisigRegimeManager.WsWiring] =
        for {
            hubNum <- Resource.eval(
              IO.fromOption(config.coilPeerHub(ownCoilNum))(
                new RuntimeException(s"no hub configured for coil peer $ownCoilNum")
              )
            )
            hubUri <- Resource.eval(dialUri(config, hubNum, "hub"))
            coilTransport <- Resource.eval(CoilPeerWsTransport.create(ownCoilNum, hubUri))
            _ <- coilTransport.startDialer(client)
        } yield CoilMultisigRegimeManager.WsWiring(coilTransport)

    /** A head peer's advertised `webSocketAddress`, or an error if absent. */
    private def headPeerAddress(config: NodeConfig, num: HeadPeerNumber): IO[String] =
        IO.fromOption(config.headPeers.headPeerData.lookup(num).map(_.webSocketAddress))(
          new RuntimeException(s"no webSocketAddress configured for head peer $num")
        )

    /** Parse a head peer's `webSocketAddress` ("ws://host:port") into a bind (host, port). */
    private def bindAddress(config: NodeConfig, num: HeadPeerNumber): IO[(Host, Port)] =
        headPeerAddress(config, num).flatMap(addr =>
            IO.fromOption(
              Uri.fromString(addr).toOption.flatMap { u =>
                  (
                    u.host.flatMap(h => Host.fromString(h.value)),
                    u.port.flatMap(Port.fromInt)
                  ).tupled
              }
            )(
              new RuntimeException(s"head peer $num webSocketAddress must be ws://host:port: $addr")
            )
        )

    /** The dial [[Uri]] for a head peer's route, e.g. "ws://host:port/head" or ".../hub". */
    private def dialUri(config: NodeConfig, num: HeadPeerNumber, path: String): IO[Uri] =
        headPeerAddress(config, num).flatMap(addr => IO.fromEither(Uri.fromString(s"$addr/$path")))
}
