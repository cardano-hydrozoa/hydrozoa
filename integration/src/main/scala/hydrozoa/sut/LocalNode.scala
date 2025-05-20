package hydrozoa.sut

import com.bloxbean.cardano.client.api.model.ProtocolParams
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

val peersApiPorts = Map.from(
  List(
    Alice -> 8093,
    Bob -> 8094,
    Carol -> 8095,
//    Daniella -> 8096,
//    Erin -> 8097,
//    Frank -> 8098,
//    Gustavo -> 8099,
//    Hector -> 8100,
//    Isabel -> 8101,
//    Julia -> 8102
  )
)

object LocalNode:

    private val log = Logger("LocalNode")

    /** Runs a local node for `ownPeer` connected to `simNetwork`.
      * @param simNetwork
      * @param ownPeer
      * @param hoistApi
      *   whether run client API
      * @param autonomousBlocks
      *   whether produce block autonomously
      * @param useYaci
      *   whether use Yaci
      * @param yaciBFApiUri
      *   and its BF API
      * @param pp
      *   protocol parameters (only if `useYaci` == false)
      * @param nodeCallback
      *   a callback to signal the node is ready
      */
    def runNode(
        simNetwork: SimNetwork,
        ownPeer: TestPeer,
        hoistApi: Boolean = false,
        autonomousBlocks: Boolean = true,
        useYaci: Boolean = true,
        yaciBFApiUri: String = "http://localhost:8080/api/v1/",
        pp: Option[ProtocolParams] = None,
        nodeCallback: ((TestPeer, Node) => Unit)
    ): Unit =
        InheritableMDC.supervisedWhere("node" -> ownPeer.toString) {
            supervised {
                // Network peers minus own peer
                val knownPeers = simNetwork.knownPeers.-(ownPeer).map(mkWalletId)

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
                  autonomousBlocks,
                  useYaci,
                  yaciBFApiUri,
                  pp
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
                nodeState.setMultisigL1EventSource(Actor.create(multisigL1EventSource))

                val blockProducer = new BlockProducer()
                blockProducer.setNetworkRef(networkActor)
                nodeState.setBlockProductionActor(Actor.create(blockProducer))

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

                if hoistApi then
                    val ownApiPort = peersApiPorts(ownPeer)
                    log.info(s"Node own peer: $ownPeer, node client API port: $ownApiPort")

                    // Client node API
                    val serverBinding =
                        useInScope(NodeRestApi(nodeActor).mkServer(ownApiPort).start())(
                          _.stop()
                        )

                never
            }
        }

object HydrozoaLocalApp extends OxApp:

    private val log = Logger("HydrozoaLocalApp")

    override def run(args: Vector[String])(using Ox): ExitCode =
        InheritableMDC.init

        supervised {
            val simNetwork = SimNetwork.apply(peersApiPorts.keys.toList)

            peersApiPorts.keys.foreach(peer =>
                forkDiscard {
                    LocalNode.runNode(
                      simNetwork = simNetwork,
                      ownPeer = peer,
                      hoistApi = true,
                      autonomousBlocks = false,
                      useYaci = true,
                      pp = Some(Utils.protocolParams),
                      nodeCallback = (_, _) => ()
                    )
                }
            )
            never
        }

        log.info(s"Started Hydrozoa local network with args: ${args.mkString(", ")}")
        ExitCode.Success
