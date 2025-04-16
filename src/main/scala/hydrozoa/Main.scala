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
import hydrozoa.l2.consensus.network.transport.{AnyMsg, HeadPeerNetworkTransportWS, IncomingDispatcher}
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
      // network,
      cardano,
      initTxBuilder,
      depositTxBuilder,
      refundTxBuilder,
      settlementTxBuilder,
      finalizationTxBuilder
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
                    // TODO: First reaction: here would be nice to check, that if at least one 
                    //  block-related consensus actor is already here we must indicate an 
                    //  erroneous condition: under normal operation there should be exactly 
                    //  one block in work.
                    //  After some thinking: no, we should not, since if the next leader got all
                    //  confirmations and start working on the next block theoretically we can 
                    //  get a message about the next block before we finish with the previous one.
                    //  This situation should be definitely tested in simulation. 
                    val mbNewActor = msg.asReqOrAck match
                        case Left(_, _, req) =>
                            val (newActor, ack) = consensusActorFactory.spawnByReq(req)
                            reply(ack)
                            Some(newActor)
                        case Right(_, _, _, _, ack) =>
                            consensusActorFactory.spawnByAck(ack)
                    mbNewActor match
                        case Some(newActor) =>
                            val newActorRef = Actor.create(newActor)
                            actors.put(origin, newActorRef)
                        case None => ()

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
                val source: Source[req.resultType] = newActorRef.ask(act => act.result(using req))
                source.receive()
            }

    val networkTransport =
        HeadPeerNetworkTransportWS.apply(ownPeer, ownPort, serverPeers, incomingMsgDispatcher)

    val network: HeadPeerNetwork = HeadPeerNetworkWS(ownPeer, knownPeers, networkTransport)

    val node = Node(
      nodeState,
      ownPeerWallet,
      cardano,
      initTxBuilder,
      depositTxBuilder,
      refundTxBuilder,
      settlementTxBuilder,
      finalizationTxBuilder
    )

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
      refundTxBuilder, 
      settlementTxBuilder,
      finalizationTxBuilder
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
              nodeState,
              ownPeerWallet,
              initTxBuilder,
              refundTxBuilder,
              settlementTxBuilder,
              finalizationTxBuilder
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

                val nodeStateActor = Actor.create(nodeState)
                val walletActor = Actor.create(ownPeerWallet)
                val cardanoActor = Actor.create(cardano)

                // Multisig L1 event source
                val multisigL1EventSource = new MultisigL1EventSource(nodeStateActor, cardanoActor)
                nodeState.multisigL1EventSource = Actor.create(multisigL1EventSource)

                val blockProducer = new BlockProducer()

                nodeState.blockProductionActor = Actor.create(blockProducer)

                val networkRef = Actor.create(network)
                node.networkRef = networkRef
                blockProducer.setNetworkRef(networkRef)

                val factory =
                    new ConsensusActorFactory(
                      nodeStateActor,
                      walletActor,
                      cardanoActor,
                      initTxBuilder,
                      refundTxBuilder,
                      settlementTxBuilder,
                      finalizationTxBuilder,
                    )
                dispatcher.setConsensusActorFactory(factory)

                val dispatcherActor = Actor.create(dispatcher)

                network.setDispatcherActorRef(dispatcherActor)

                forkDiscard { transport.run() }

                val serverBinding =
                    useInScope(NodeRestApi(nodeActorRef).mkServer(apiPort).start())(_.stop())

                never
            }
        }

        log.info(s"Started Hydrozoa node with args: ${args.mkString(", ")}!")
        ExitCode.Success
