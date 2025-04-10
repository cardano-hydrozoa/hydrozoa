package hydrozoa

import com.bloxbean.cardano.client.api.model.ProtocolParams
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.typesafe.scalalogging.Logger
import hydrozoa.l1.*
import hydrozoa.l1.multisig.tx.deposit.{BloxBeanDepositTxBuilder, DepositTxBuilder}
import hydrozoa.l1.multisig.tx.finalization.{BloxBeanFinalizationTxBuilder, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{BloxBeanInitializationTxBuilder, InitTxBuilder}
import hydrozoa.l1.multisig.tx.refund.{BloxBeanRefundTxBuilder, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{BloxBeanSettlementTxBuilder, SettlementTxBuilder}
import hydrozoa.l2.consensus.network.*
import hydrozoa.l2.consensus.network.actor.{ConsensusActor, ConsensusActorFactory}
import hydrozoa.l2.consensus.network.transport.{
    AnyMsg,
    HeadPeerNetworkTransportWS,
    IncomingDispatcher
}
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.rest.NodeRestApi
import hydrozoa.node.server.Node
import hydrozoa.node.state.{HeadStateReader, NodeState, WalletId}
import ox.*
import ox.channels.{Actor, ActorRef, Source}
import ox.logback.InheritableMDC
import sttp.client4.UriContext
import sttp.model.Uri

import scala.collection.mutable

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

    val node = Node(
      nodeStateManager,
      ownPeerWallet,
      network,
      cardano,
      initTxBuilder,
      depositTxBuilder,
      refundTxBuilder,
      settlementTxBuilder,
      finalizationTxBuilder,
      log
    )
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
    val nodeStateManager: NodeState = NodeState(knownPeers)
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

    val incomingMsgDispatcher = new IncomingDispatcher:

        private val actors: mutable.Map[(TestPeer, Long), ActorRef[ConsensusActor]] =
            mutable.Map.empty

        private var nodeRef: ActorRef[Node] = _

        override def setNodeActorRef(nodeRef: ActorRef[Node]): Unit = this.nodeRef = nodeRef

        private var consensusActorFactory: ConsensusActorFactory = _

        override def setConsensusActorFactory(consensusActorFactory: ConsensusActorFactory): Unit =
            this.consensusActorFactory = consensusActorFactory

        def dispatchMessage(msg: AnyMsg, reply: Ack => Long)(using Ox): Unit =
            log.info(s"Dispatching incoming message: $msg")

            val origin = msg.origin
            actors.get(origin) match
                case Some(actor) =>
                    log.info(s"Actor was found for origin: $origin")
                    msg.asReqOrAck match
                        case Left(_, _, req) =>
                            // FIXME: add check whether init has not been called
                            val ack = actor.ask(act => act.init(req.asInstanceOf[act.ReqType]))
                            reply(ack)
                        case Right(_, _, _, _, ack) =>
                            actor.tell(act => act.deliver(ack.asInstanceOf[act.AckType]))
                case None =>
                    log.info(s"Actor was NOT found for origin: $origin")
                    val newActor = msg.asReqOrAck match
                        case Left(_, _, req) =>
                            val (newActor, ack) = consensusActorFactory.spawnByReq(req)
                            reply(ack)
                            newActor
                        case Right(_, _, _, _, ack) =>
                            consensusActorFactory.spawnByAck(ack)
                    val newActorRef = Actor.create(newActor)
                    actors.put(origin, newActorRef)

        override def spawnActorProactively(
            from: TestPeer,
            seq: Long,
            req: Req,
            send: Req => Long,
            reply: Ack => Long
        ): req.resultType =
            supervised {
                val origin = (from, seq)
                val (newActor, ack) = consensusActorFactory.spawnByReq(req)
                val newActorRef = Actor.create(newActor)
                actors.put(origin, newActorRef)
                send(req)
                reply(ack)
                val source: Source[req.resultType] = newActorRef.ask(act =>
                    act.result(using req))
                source.receive()
            }

    val networkTransport =
        HeadPeerNetworkTransportWS.apply(ownPeer, ownPort, serverPeers, incomingMsgDispatcher)

    val network: HeadPeerNetwork = HeadPeerNetworkWS(ownPeer, knownPeers, networkTransport)

    val node = Node(
      nodeStateManager,
      ownPeerWallet,
      network,
      cardano,
      initTxBuilder,
      depositTxBuilder,
      refundTxBuilder,
      settlementTxBuilder,
      finalizationTxBuilder,
      log
    )
    (
      node,
      cardano,
      network,
      networkTransport,
      incomingMsgDispatcher,
      nodeStateManager,
      ownPeerWallet, 
      initTxBuilder
    )
}

val peers = Map.from(
  List(
    Alice -> uri"ws://localhost:4937/ws",
    Bob -> uri"ws://localhost:4938/ws",
    Carol -> uri"ws://localhost:4939/ws"
  )
)

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
              nodeStateManager,
              ownPeerWallet,
              initTxBuilder
            ) = {
                mkHydrozoaNode2(
                  ownPeer,
                  ownPort,
                  serverPeers,
                  mkWallet(ownPeer),
                  peers.keySet.-(ownPeer).map(mkWalletId)
                )
            }

            val apiPort = args.apply(1).toInt

            // InheritableMDC.supervisedWhere("a" -> "1", "b" -> "2") {
            supervised {

                val nodeActorRef = Actor.create(node)

                dispatcher.setNodeActorRef(nodeActorRef)

                val stateActor = Actor.create(nodeStateManager)
                val walletActor = Actor.create(ownPeerWallet)
                val cardanoActor = Actor.create(cardano)

                val factory = new ConsensusActorFactory(stateActor, walletActor, cardanoActor, initTxBuilder)
                dispatcher.setConsensusActorFactory(factory)

                val dispatcherActor = Actor.create(dispatcher)

                network.setDispatcherActorRef(dispatcherActor)

                fork {
                    transport.run()
                }

                val serverBinding =
                    useInScope(NodeRestApi(nodeActorRef).mkServer(apiPort).start())(_.stop())

                never
            }
        }

        log.info(s"Started Hydrozoa node with args: ${args.mkString(", ")}!")
        ExitCode.Success
