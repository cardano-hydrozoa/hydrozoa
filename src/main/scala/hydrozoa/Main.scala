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
import hydrozoa.node.TestPeer
import hydrozoa.node.TestPeer.*
import hydrozoa.node.rest.NodeRestApi
import hydrozoa.node.server.Node
import hydrozoa.node.state.{HeadStateReader, NodeState, WalletId}
import hydrozoa.infra.{Piper, encodeHex}
import hydrozoa.l2.consensus.network.transport.{HeadPeerNetworkTransportWS, IncomingDispatcher}
import ox.*
import ox.channels.Actor
import ox.logback.InheritableMDC
import sttp.client4.UriContext
import sttp.model.Uri

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

    val logDispatcher = new IncomingDispatcher:
        def dispatchMessage(payload: Msg, reply: Ack => Long): Unit =
            payload match
                case _: ReqVerKey =>
                    val verKey = ownPeerWallet.exportVerificationKeyBytes
                    val ack = AckVerKey(ownPeer, verKey)
                    reply(ack)
                case _ => log.info(s"unknown/unsupported message: $payload")

    val networkTransport =
        HeadPeerNetworkTransportWS.apply(ownPeer, ownPort, serverPeers, logDispatcher)

    val network: HeadPeerNetwork = HeadPeerNetworkWS(ownPeer, networkTransport)

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
    (log, node, cardano, networkTransport)
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

            val (_, node: Node, _, transport) = {
                mkHydrozoaNode2(
                  ownPeer,
                  ownPort,
                  serverPeers,
                  mkWallet(ownPeer),
                  peers.keySet.-(ownPeer).map(mkWallet)
                )
            }

            val apiPort = args.apply(1).toInt

            // InheritableMDC.supervisedWhere("a" -> "1", "b" -> "2") {
            supervised {

                fork {
                    transport.run()
                }

                val nodeActorRef = Actor.create(node)
                val serverBinding =
                    useInScope(NodeRestApi(nodeActorRef).mkServer(apiPort).start())(_.stop())
                never
            }
        }

        log.info(s"Started Hydrozoa node with args: ${args.mkString(", ")}!")
        ExitCode.Success
