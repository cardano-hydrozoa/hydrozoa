package hydrozoa

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.typesafe.scalalogging.Logger
import hydrozoa.l1.*
import hydrozoa.l1.event.MultisigL1EventSource
import hydrozoa.l1.multisig.tx.deposit.{BloxBeanDepositTxBuilder, DepositTxBuilder}
import hydrozoa.l1.multisig.tx.finalization.{BloxBeanFinalizationTxBuilder, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{BloxBeanInitializationTxBuilder, InitTxBuilder}
import hydrozoa.l1.multisig.tx.refund.{BloxBeanRefundTxBuilder, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{BloxBeanSettlementTxBuilder, SettlementTxBuilder}
import hydrozoa.l2.block.BlockProducer
import hydrozoa.l2.consensus.network.*
import hydrozoa.l2.consensus.network.actor.ConsensusActorFactory
import hydrozoa.l2.consensus.network.transport.HeadPeerNetworkTransportWS
import hydrozoa.l2.consensus.{ConsensusDispatcher, DefaultConsensusDispatcher}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.monitoring.PrometheusMetrics
import hydrozoa.node.rest.NodeRestApi
import hydrozoa.node.server.Node
import hydrozoa.node.state.HeadPhase.{Finalizing, Initializing, Open}
import hydrozoa.node.state.{HeadStateReader, NodeState, WalletId}
import io.prometheus.metrics.exporter.httpserver.HTTPServer
import ox.*
import ox.channels.Actor
import ox.logback.InheritableMDC
import ox.scheduling.{RepeatConfig, repeat}
import sttp.client4.UriContext
import sttp.model.Uri

import scala.concurrent.duration.DurationInt

// TODO: use external network topology config?
// Docker peers and Yaci endpoint
val peers = Map.from(
  List(
    Alice -> uri"ws://alice:4937/ws",
    Bob -> uri"ws://bob:4938/ws",
    Carol -> uri"ws://carol:4939/ws"
  )
)
val yaciBFApiUri = "http://yaci-cli:8080/api/v1/"

//// Local peers and Yaci endpoint
//val peers = Map.from(
//  List(
//    Alice -> uri"ws://localhost:4937/ws",
//    Bob -> uri"ws://localhost:4938/ws",
//    Carol -> uri"ws://localhost:4939/ws"
//  )
//)
//val yaciBFApiUri = "http://localhost:8080/api/v1/"

object HydrozoaNode extends OxApp:

    private val log = Logger("main")

    // TASK: regular CLI args parsing
    override def run(args: Vector[String])(using Ox): ExitCode =
        InheritableMDC.init

        forkUser {

            val ownPeer = TestPeer.valueOf(args.apply(0))
            val ownPort = peers(ownPeer).port.get

            log.info(s"Node own peer: $ownPeer, node own port: $ownPort")

            // InheritableMDC.supervisedWhere("a" -> "1", "b" -> "2") {
            supervised {

                // Peers that come lexicographically before the own peer are treated as "servers"
                // when establishing web sockets connections.
                val serverPeers = peers.filter((k, _) => ownPeer.compareTo(k) > 0)

                // Network peers minus own peer
                val knownPeers = peers.keySet.-(ownPeer).map(mkWalletId)

                val (
                  cardano,
                  nodeState,
                  initTxBuilder,
                  depositTxBuilder,
                  refundTxBuilder,
                  settlementTxBuilder,
                  finalizationTxBuilder
                ) = mkTxBuilders(
                  knownPeers,
                  yaciBFApiUri = yaciBFApiUri
                )
                val cardanoActor = Actor.create(cardano)
                val nodeStateActor = Actor.create(nodeState)

                val walletActor = Actor.create(mkWallet(ownPeer))

                val actorFactory =
                    new ConsensusActorFactory(
                      nodeStateActor,
                      walletActor,
                      cardanoActor,
                      initTxBuilder,
                      refundTxBuilder,
                      settlementTxBuilder,
                      finalizationTxBuilder
                    )

                val dispatcher: ConsensusDispatcher = DefaultConsensusDispatcher.apply()
                dispatcher.setConsensusActorFactory(actorFactory)
                val dispatcherActor = Actor.create(dispatcher)
                dispatcher.setOwnActor(dispatcherActor)

                val transport = HeadPeerNetworkTransportWS.apply(ownPeer, ownPort, serverPeers)
                val transportActor = Actor.create(transport)

                // TODO: do we really need circular dependency?
                dispatcher.setTransport(transportActor)
                transport.setDispatcher(dispatcherActor)

                val network: HeadPeerNetwork = HeadPeerNetworkWS(ownPeer, knownPeers, transport)
                network.setDispatcher(dispatcherActor)
                val networkActor = Actor.create(network)

                // Static actors for node state
                val multisigL1EventSource = new MultisigL1EventSource(nodeStateActor, cardanoActor)
                nodeState.multisigL1EventSource = Actor.create(multisigL1EventSource)

                val blockProducer = new BlockProducer()
                blockProducer.setNetworkRef(networkActor)
                nodeState.blockProductionActor = Actor.create(blockProducer)

                val metricsActor = Actor.create(PrometheusMetrics.apply())
                nodeState.setMetrics(metricsActor)
                cardano.setMetrics(metricsActor)

                val depositTxBuilderActor = Actor.create(depositTxBuilder)
                val node = Node()
                node.depositTxBuilder = depositTxBuilderActor
                node.network = networkActor
                node.nodeState = nodeStateActor
                node.wallet = walletActor
                node.cardano = cardanoActor
                val nodeActor = Actor.create(node)

                // Run fibers

                // Consensus dispatcher
                forkDiscard { dispatcher.run() }

                // Network transport
                forkDiscard { transport.run() }

                // Head uptime metric
                forkDiscard {
                    repeat(RepeatConfig.fixedRateForever(2000.millis)) {
                        val uptime = nodeStateActor.ask(_.mbInitializedOn) match
                            case Some(initializedOn) =>
                                nodeStateActor.ask(_.head.currentPhase) match
                                    case Initializing | Open | Finalizing =>
                                        (System.currentTimeMillis() - initializedOn) / 1000
                                    case _ => 0
                            case None => 0
                        log.info(s"head uptime is $uptime")
                        metricsActor.tell(_.updateHeadUptime(uptime))
                    }
                }

                // Metrics HTTP server
                forkDiscard {
                    val metricsPort = args.apply(2).toInt
                    log.info(s"Starting metricsActor http server on port $metricsPort")
                    HTTPServer
                        .builder()
                        .port(metricsPort)
                        .buildAndStart()
                }

                // Client node API
                val apiPort = args.apply(1).toInt
                val serverBinding =
                    useInScope(NodeRestApi(nodeActor).mkServer(apiPort).start())(_.stop())

                never
            }
        }

        log.info(s"Started Hydrozoa node with args: ${args.mkString(", ")}")
        ExitCode.Success

def mkTxBuilders(
    knownPeers: Set[WalletId],
    useYaci: Boolean = false,
    yaciBFApiUri: String = "http://localhost:8080/api/v1/",
    pp: Option[ProtocolParams] = None
) =

    // Cardano L1
    val (cardano, backendService) =
        if useYaci then
            val backendService = BFBackendService(yaciBFApiUri, "")
            val cardano: CardanoL1 = CardanoL1YaciDevKit(backendService)
            (cardano, backendService)
        else
            val cardano = CardanoL1Mock()
            val backendService = BackendServiceMock(cardano, pp.get)
            (cardano, backendService)

    // Global head manager (for mocked head during Milestone 2)
    val nodeState: NodeState = NodeState(knownPeers)
    val nodeStateReader: HeadStateReader = nodeState.reader

    // Tx Builders
    val initTxBuilder: InitTxBuilder = BloxBeanInitializationTxBuilder(backendService)
    val depositTxBuilder: DepositTxBuilder =
        BloxBeanDepositTxBuilder(backendService, nodeStateReader)
    val refundTxBuilder: RefundTxBuilder =
        BloxBeanRefundTxBuilder(cardano, backendService, nodeStateReader)
    val settlementTxBuilder: SettlementTxBuilder =
        BloxBeanSettlementTxBuilder(backendService, nodeStateReader)
    val finalizationTxBuilder: FinalizationTxBuilder =
        BloxBeanFinalizationTxBuilder(backendService, nodeStateReader)

    // return a bunch of things
    (
      cardano,
      nodeState,
      initTxBuilder,
      depositTxBuilder,
      refundTxBuilder,
      settlementTxBuilder,
      finalizationTxBuilder
    )

end mkTxBuilders

/** TODO: This function needs a review, it's broken now.
  */
def mkSimpleHydrozoaNode(
    ownPeerWallet: Wallet,
    knownPeers: Set[Wallet],
    useL1Mock: Boolean = false,
    pp: Option[ProtocolParams] = None,
    yaciBFApiUri: String = "http://localhost:8080/api/v1/"
) = {

    // Components
    val log = Logger("Hydrozoa")

    // Cardano L1
    val (cardano, backendService) = if useL1Mock then
        val cardano = CardanoL1Mock()
        (cardano, BackendServiceMock(cardano, pp.get))
    else
        val backendService = BFBackendService(yaciBFApiUri, "")
        val cardano: CardanoL1 = CardanoL1YaciDevKit(backendService)
        (cardano, backendService)

    // Global head manager (for mocked head during Milestone 2)
    val nodeStateManager: NodeState = NodeState(knownPeers.map(p => WalletId(p.getName)))
    val nodeStateReader: HeadStateReader = nodeStateManager.reader

    // Tx Builders
    val initTxBuilder: InitTxBuilder = BloxBeanInitializationTxBuilder(backendService)
    val depositTxBuilder: DepositTxBuilder =
        BloxBeanDepositTxBuilder(backendService, nodeStateReader)
    val refundTxBuilder: RefundTxBuilder =
        BloxBeanRefundTxBuilder(cardano, backendService, nodeStateReader)
    val settlementTxBuilder: SettlementTxBuilder =
        BloxBeanSettlementTxBuilder(backendService, nodeStateReader)
    val finalizationTxBuilder: FinalizationTxBuilder =
        BloxBeanFinalizationTxBuilder(backendService, nodeStateReader)

    val network: HeadPeerNetwork =
        HeadPeerNetworkOneNode(
          nodeStateReader,
          initTxBuilder,
          refundTxBuilder,
          settlementTxBuilder,
          finalizationTxBuilder,
          cardano,
          ownPeerWallet,
          knownPeers
        )

    val node = Node()
    (log, node, cardano)
}
