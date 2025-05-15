package hydrozoa.sut

import com.typesafe.scalalogging.Logger
import hydrozoa.l1.event.MultisigL1EventSource
import hydrozoa.l2.block.BlockProducer
import hydrozoa.l2.consensus.network.actor.ConsensusActorFactory
import hydrozoa.l2.consensus.network.transport.{SimNetwork, SimTransport}
import hydrozoa.l2.consensus.network.{HeadPeerNetwork, HeadPeerNetworkWS}
import hydrozoa.l2.consensus.{ConsensusDispatcher, DefaultConsensusDispatcher}
import hydrozoa.mkTxBuilders
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.monitoring.NoopMetrics
import hydrozoa.node.rest.NodeRestApi
import hydrozoa.node.server.Node
import ox.*
import ox.channels.Actor
import ox.logback.InheritableMDC

val peers = Map.from(
  List(
    Alice -> 8093,
    Bob -> 8094,
    Carol -> 8095
  )
)

val yaciBFApiUri = "http://localhost:8080/api/v1/"

def runNode(
    simNetwork: SimNetwork,
    ownPeer: TestPeer,
    log: Logger,
    nodeCallback: ((TestPeer, Node) => Unit)
) = {
    InheritableMDC.supervisedWhere("node" -> ownPeer.toString) {
        forkUser {

            val ownApiPort = peers(ownPeer)

            log.info(s"Node own peer: $ownPeer, node client API port: $ownApiPort")

            supervised {

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

                val transport = SimTransport.apply(simNetwork, ownPeer)
                val transportActor = Actor.create(transport)

                // TODO: do we really need circular dependency?
                dispatcher.setTransport(transportActor)
                transport.setDispatcher(dispatcherActor)

                val network: HeadPeerNetwork = HeadPeerNetworkWS(ownPeer, knownPeers, transport)
                network.setDispatcher(dispatcherActor)
                val networkActor = Actor.create(network)

                // Static actors for node state
                val multisigL1EventSource =
                    new MultisigL1EventSource(nodeStateActor, cardanoActor)
                nodeState.multisigL1EventSource = Actor.create(multisigL1EventSource)

                val blockProducer = new BlockProducer()
                blockProducer.setNetworkRef(networkActor)
                nodeState.blockProductionActor = Actor.create(blockProducer)

                val metricsActor = Actor.create(NoopMetrics.apply())
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

                nodeCallback(ownPeer, node)

                // Run fibers

                // Consensus dispatcher
                forkDiscard {
                    dispatcher.run()
                }

                // Network transport
                forkDiscard {
                    transport.run()
                }

//                // Client node API
//                val serverBinding =
//                    useInScope(NodeRestApi(nodeActor).mkServer(ownApiPort).start())(_.stop())

                never
            }
        }
    }
}

object HydrozoaLocal extends OxApp:

    private val log = Logger("HydrozoaLocal")

    override def run(args: Vector[String])(using Ox): ExitCode =
        InheritableMDC.init

        supervised {
            val simNetwork = SimNetwork.apply(peers.keys.toList)

            peers.keys.foreach(peer =>
                forkDiscard {
                    runNode(simNetwork, peer, log, (_, _) => ())
                }
            )
            never
        }

        log.info(s"Started Hydrozoa local network with args: ${args.mkString(", ")}")
        ExitCode.Success
