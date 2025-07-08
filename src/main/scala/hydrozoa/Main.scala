package hydrozoa

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.typesafe.scalalogging.Logger
import hydrozoa.l1.*
import hydrozoa.l1.event.MultisigL1EventSource
import hydrozoa.l1.multisig.tx.deposit.{BloxBeanDepositTxBuilder, DepositTxBuilder}
import hydrozoa.l1.multisig.tx.finalization.{BloxBeanFinalizationTxBuilder, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{BloxBeanInitializationTxBuilder, InitTxBuilder}
import hydrozoa.l1.multisig.tx.refund.{BloxBeanRefundTxBuilder, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{BloxBeanSettlementTxBuilder, SettlementTxBuilder}
import hydrozoa.l1.rulebased.tx.fallback.{BloxBeanFallbackTxBuilder, FallbackTxBuilder}
import hydrozoa.l1.rulebased.tx.tally.{BloxBeanTallyTxBuilder, TallyTxBuilder}
import hydrozoa.l1.rulebased.tx.vote.{BloxBeanVoteTxBuilder, VoteTxBuilder}
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
import ox.channels.{Actor, ActorRef}
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

    // TODO: regular CLI args parsing
    override def run(args: Vector[String])(using Ox): ExitCode =
        InheritableMDC.init

        forkUser {

            val ownPeer = TestPeer.valueOf(args.apply(0))
            val ownPort = peers(ownPeer).port.get

            log.info(s"Node own peer: $ownPeer, node own port: $ownPort")

            supervised {

                // Peers that come lexicographically before the own peer are treated as "servers"
                // when establishing web sockets connections.
                val serverPeers = peers.filter((k, _) => ownPeer.compareTo(k) > 0)

                // Network peers minus own peer
                val knownPeers = peers.keySet.-(ownPeer).map(mkWalletId)

                val (cardano, backendService) = mkCardanoL1(true, yaciBFApiUri = yaciBFApiUri)
                val cardanoActor = Actor.create(cardano)

                val nodeState: NodeState = NodeState.apply(knownPeers)
                nodeState.setCardano(cardanoActor)

                val (
                  initTxBuilder,
                  fallbackTxBuilder,
                  depositTxBuilder,
                  refundTxBuilder,
                  settlementTxBuilder,
                  finalizationTxBuilder,
                  voteTxBuilder,
                  tallyTxBuilder
                ) = mkTxBuilders(backendService, nodeState)

                val nodeStateActor = Actor.create(nodeState)

                val walletActor = Actor.create(mkWallet(ownPeer))

                val actorFactory =
                    new ConsensusActorFactory(
                      nodeStateActor,
                      walletActor,
                      cardanoActor,
                      initTxBuilder,
                      fallbackTxBuilder,
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
                nodeState.setMultisigL1EventSource(Actor.create(multisigL1EventSource))
                nodeState.setVoteTxBuilder(voteTxBuilder)
                nodeState.setTallyTxBuilder(tallyTxBuilder)

                val blockProducer = new BlockProducer()
                blockProducer.setNetworkRef(networkActor)
                nodeState.setBlockProductionActor(Actor.create(blockProducer))

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

def mkCardanoL1(
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
    (cardano, backendService)
end mkCardanoL1

def mkTxBuilders(
    backendService: BackendService,
    nodeState: NodeState,
    _mbTreasuryScriptRefUtxoId: Option[UtxoIdL1] = None,
    _mbDisputeScriptRefUtxoId: Option[UtxoIdL1] = None
) =

    val nodeStateReader: HeadStateReader = nodeState.reader

    // Tx Builders
    val initTxBuilder: InitTxBuilder = BloxBeanInitializationTxBuilder(backendService)
    val fallbackTxBuilder: FallbackTxBuilder =
        BloxBeanFallbackTxBuilder(backendService)
    val depositTxBuilder: DepositTxBuilder =
        BloxBeanDepositTxBuilder(backendService, nodeStateReader)
    val refundTxBuilder: RefundTxBuilder =
        BloxBeanRefundTxBuilder(backendService, nodeStateReader)
    val settlementTxBuilder: SettlementTxBuilder =
        BloxBeanSettlementTxBuilder(backendService, nodeStateReader)
    val finalizationTxBuilder: FinalizationTxBuilder =
        BloxBeanFinalizationTxBuilder(backendService, nodeStateReader)
    val voteTxBuilder: VoteTxBuilder =
        BloxBeanVoteTxBuilder(backendService)
    val tallyTxBuilder: TallyTxBuilder =
        BloxBeanTallyTxBuilder(backendService)

    (
      initTxBuilder,
      fallbackTxBuilder,
      depositTxBuilder,
      refundTxBuilder,
      settlementTxBuilder,
      finalizationTxBuilder,
      voteTxBuilder,
      tallyTxBuilder
    )

end mkTxBuilders
