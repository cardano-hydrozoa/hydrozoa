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
import hydrozoa.l2.consensus.network.{HeadPeerNetwork, HeadPeerNetworkOneNode}
import hydrozoa.node.TestPeer.{Alice, Bob, Carol, mkWallet}
import hydrozoa.node.rest.NodeRestApi
import hydrozoa.node.server.Node
import hydrozoa.node.state.{HeadStateReader, NodeState, WalletId}

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
          knownPeers.toSet
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

object HydrozoaNodeServer:

    @main def main(args: String*): Unit = {

        val (log: Logger, node: Node, _) = {
            mkHydrozoaNode(
                mkWallet(Alice),
                Set(Bob, Carol).map(mkWallet)
            )
        }

        log.warn("Starting Hydrozoa Node API Server...")
        NodeRestApi(node).start()
    }
