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
import hydrozoa.l2.consensus.network.actor.{ConsensusActor, ConsensusActorFactory}
import hydrozoa.l2.consensus.network.transport.{
    AnyMsg,
    HeadPeerNetworkTransportWS,
    IncomingDispatcher
}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.monitoring.PrometheusMetrics
import hydrozoa.node.rest.NodeRestApi
import hydrozoa.node.server.Node
import hydrozoa.node.state.{HeadStateReader, NodeState, WalletId}
import io.prometheus.metrics.exporter.httpserver.HTTPServer
import ox.*
import ox.channels.{Actor, ActorRef, Channel, Source}
import ox.flow.Flow
import ox.logback.InheritableMDC
import ox.scheduling.{RepeatConfig, repeat}
import sttp.client4.UriContext
import sttp.model.Uri

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

def mkHydrozoaNode(
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

def mkHydrozoaNode2(
    ownPeer: TestPeer,
    ownPort: Int,
    serverPeers: Map[TestPeer, Uri],
    ownPeerWallet: Wallet,
    knownPeers: Set[WalletId],
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

    val incomingMsgDispatcher: IncomingDispatcher = new IncomingDispatcher:

        private val actors: mutable.Map[(TestPeer, Long), ActorRef[ConsensusActor]] =
            mutable.Map.empty

        private var transport: ActorRef[HeadPeerNetworkTransportWS] = _

        override def setTransport(transport: ActorRef[HeadPeerNetworkTransportWS]): Unit =
            this.transport = transport

        private var consensusActorFactory: ConsensusActorFactory = _

        override def setConsensusActorFactory(consensusActorFactory: ConsensusActorFactory): Unit =
            this.consensusActorFactory = consensusActorFactory

        def dispatchMessage(msg: AnyMsg): Unit =
            log.info(s"Dispatching incoming message: $msg")

            val origin = msg.origin
            actors.get(origin) match
                case Some(actor) =>
                    log.info(s"Actor was found for origin: $origin")
                    msg.asReqOrAck match
                        case Left(originPeer, originSeq, req) =>
                            // FIXME: add check whether init has not been called
                            val acks = actor.ask(act => act.init(req.asInstanceOf[act.ReqType]))
                            log.info(s"Replying with acks: $acks")
                            acks.foreach(ack =>
                                transport.tell(_.broadcastAck(originPeer, originSeq)(ack))
                            )
                        case Right(_, _, originPeer, originSeq, ack) =>
                            val mbAck = actor.ask(act => act.deliver(ack.asInstanceOf[act.AckType]))
                            log.info(s"Replying with mbAck: $mbAck")
                            mbAck.foreach(ack =>
                                transport.tell(_.broadcastAck(originPeer, originSeq)(ack))
                            )
                case None =>
                    log.info(s"Actor was NOT found for origin: $origin")
                    // TODO: First reaction: here would be nice to check, that if at least one
                    //  block-related consensus actor is already here we must indicate an
                    //  erroneous condition: under normal operation there should be exactly
                    //  one block in work.
                    //  After some thinking: no, we should not, since if the next leader got all
                    //  confirmations and start working on the next block theoretically we can
                    //  get a message about the next block before we finish with the previous one.
                    //  This situation should be definitely tested in simulation.
                    val mbNewActor = msg.asReqOrAck match
                        case Left(originPeer, originSeq, req) =>
                            val (newActor, acks) = consensusActorFactory.spawnByReq(req)
                            acks.foreach(ack =>
                                transport.tell(_.broadcastAck(originPeer, originSeq)(ack))
                            )
                            Some(newActor)
                        case Right(_, _, originPeer, originSeq, ack) =>
                            val mbActor -> mbAck = consensusActorFactory.spawnByAck(ack)
                            mbAck.foreach(ack =>
                                transport.tell(_.broadcastAck(originPeer, originSeq)(ack))
                            )
                            mbActor
                    mbNewActor match
                        case Some(newActor) =>
                            spawnActorReactivelyIn.send(newActor)
                            val newActorRef = spawnActorReactivelyOut.receive()
                            log.info(s"Adding new actor for $origin")
                            actors.put(origin, newActorRef)
                        case None => ()

        override def spawnActorProactively(
            from: TestPeer,
            seq: Long,
            req: Req
        ): Source[req.resultType] =
            // supervised {
            val origin = (from, seq)
            log.info("Spawning actor proactively...")
            val (newActor, acks) = consensusActorFactory.spawnByReq(req)
            // val newActorRef = Actor.create(newActor)

            spawnActorReactivelyIn.send(newActor)
            val newActorRef = spawnActorReactivelyOut.receive()
            actors.put(origin, newActorRef)
            transport.tell(_.broadcastReq(Some(seq))(req))
            acks.foreach(ack => transport.tell(_.broadcastAck(from, seq)(ack)))
            log.info("Getting result source...")
            val source: Source[req.resultType] = newActorRef.ask(act => act.result(using req))
            // log.info("Receiving from source...")
            // val ret = source.receive()
            // log.info("Leaving proactive supervised context")
            // ret
            source
        // }

        private val spawnActorReactivelyIn: Channel[ConsensusActor] = Channel.rendezvous
        private val spawnActorReactivelyOut: Channel[ActorRef[ConsensusActor]] = Channel.rendezvous

        def run()(using Ox): Unit =
            log.info("running reactive spawner...")
            Flow.fromSource(spawnActorReactivelyIn)
                .runForeach(actor =>
                    log.info(s"reactively spanning: ${actor.getClass}")
                    val actorRef = Actor.create(actor)
                    spawnActorReactivelyOut.send(actorRef)
                )

    val networkTransport =
        HeadPeerNetworkTransportWS.apply(ownPeer, ownPort, serverPeers)

    val network: HeadPeerNetwork = HeadPeerNetworkWS(ownPeer, knownPeers, networkTransport)

    val node = Node()

    // return a bunch of things
    (
      node,
      cardano,
      network,
      networkTransport,
      incomingMsgDispatcher,
      nodeState,
      ownPeerWallet,
      initTxBuilder,
      depositTxBuilder,
      refundTxBuilder,
      settlementTxBuilder,
      finalizationTxBuilder
    )
}

// TODO: use external network topology config?
val peers = Map.from(
  List(
    Alice -> uri"ws://alice:4937/ws",
    Bob -> uri"ws://bob:4938/ws",
    Carol -> uri"ws://carol:4939/ws"
  )
)

//val peers = Map.from(
//  List(
//    Alice -> uri"ws://localhost:4937/ws",
//    Bob -> uri"ws://localhost:4938/ws",
//    Carol -> uri"ws://localhost:4939/ws"
//  )
//)

object HydrozoaNode extends OxApp:

    private val log = Logger("main")

    override def run(args: Vector[String])(using Ox): ExitCode =
        InheritableMDC.init

        forkUser {

            val ownPeer = TestPeer.valueOf(args.apply(0))
            val ownPort = peers(ownPeer).port.get

            log.info(s"Node own peer: $ownPeer, node own port: $ownPort")

            val serverPeers = peers.filter((k, _) => ownPeer.compareTo(k) > 0)

            val (
              node: Node,
              cardano,
              network,
              transport,
              dispatcher,
              nodeState,
              ownPeerWallet,
              initTxBuilder,
              depositTxBuilder,
              refundTxBuilder,
              settlementTxBuilder,
              finalizationTxBuilder
            ) = {
                mkHydrozoaNode2(
                  ownPeer,
                  ownPort,
                  serverPeers,
                  mkWallet(ownPeer),
                  peers.keySet.-(ownPeer).map(mkWalletId),
                  yaciBFApiUri = "http://yaci-cli:8080/api/v1/"
                )
            }

            val apiPort = args.apply(1).toInt
            val metricsPort = args.apply(2).toInt

            // InheritableMDC.supervisedWhere("a" -> "1", "b" -> "2") {
            supervised {

                val nodeActor = Actor.create(node)

                val nodeStateActor = Actor.create(nodeState)
                val walletActor = Actor.create(ownPeerWallet)
                val cardanoActor = Actor.create(cardano)

                // Multisig L1 event source
                val multisigL1EventSource = new MultisigL1EventSource(nodeStateActor, cardanoActor)
                nodeState.multisigL1EventSource = Actor.create(multisigL1EventSource)

                val blockProducer = new BlockProducer()

                nodeState.blockProductionActor = Actor.create(blockProducer)

                val metricsActor = Actor.create(PrometheusMetrics())
                nodeState.metrics = metricsActor

                val networkActor = Actor.create(network)

                val depositTxBuilderActor = Actor.create(depositTxBuilder)

                node.network = networkActor
                node.nodeState = nodeStateActor
                node.wallet = walletActor
                node.cardano = cardanoActor
                node.depositTxBuilder = depositTxBuilderActor

                blockProducer.setNetworkRef(networkActor)

                val factory =
                    new ConsensusActorFactory(
                      nodeStateActor,
                      walletActor,
                      cardanoActor,
                      initTxBuilder,
                      refundTxBuilder,
                      settlementTxBuilder,
                      finalizationTxBuilder
                    )
                dispatcher.setConsensusActorFactory(factory)

                val transportRef = Actor.create(transport)
                dispatcher.setTransport(transportRef)

                val dispatcherActor = Actor.create(dispatcher)

                network.setDispatcher(dispatcherActor)
                transport.setDispatcher(dispatcherActor)
                forkDiscard { dispatcher.run() }
                forkDiscard { transport.run() }

                forkDiscard {
                    repeat(RepeatConfig.fixedRateForever(2000.millis)) {
                        val uptime = nodeStateActor.ask(_.mbInitializedOn) match
                            case Some(initializedOn) =>
                                (System.currentTimeMillis() - initializedOn) / 1000
                            case None => 0
                        log.info(s"head uptime is $uptime")
                        metricsActor.tell(_.updateHeadUptime(uptime))
                    }
                }

                val serverBinding =
                    useInScope(NodeRestApi(nodeActor).mkServer(apiPort).start())(_.stop())

                // Metrics HTTP server
                forkDiscard {
                    log.info(s"Starting metricsActor http server on port $metricsPort")
                    HTTPServer
                        .builder()
                        .port(metricsPort)
                        .buildAndStart()
                }

                never
            }
        }

        log.info(s"Started Hydrozoa node with args: ${args.mkString(", ")}")
        ExitCode.Success
