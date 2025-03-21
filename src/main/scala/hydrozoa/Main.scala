package hydrozoa

import com.bloxbean.cardano.client.backend.api.BackendService
import com.typesafe.scalalogging.Logger
import hydrozoa.AppCtx.yaciDevKit
import hydrozoa.infra.genNodeKey
import hydrozoa.l1.multisig.tx.deposit.{BloxBeanDepositTxBuilder, DepositTxBuilder}
import hydrozoa.l1.multisig.tx.finalization.{BloxBeanFinalizationTxBuilder, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{BloxBeanInitializationTxBuilder, InitTxBuilder}
import hydrozoa.l1.multisig.tx.refund.{BloxBeanRefundTxBuilder, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{BloxBeanSettlementTxBuilder, SettlementTxBuilder}
import hydrozoa.l1.wallet.{MockWallet, Wallet}
import hydrozoa.l1.{BackendServiceMock, CardanoL1, CardanoL1Mock, CardanoL1YaciDevKit}
import hydrozoa.l2.consensus.network.{HeadPeerNetwork, HeadPeerNetworkMock}
import hydrozoa.node.rest.NodeRestApi
import hydrozoa.node.server.Node
import hydrozoa.node.state.{HeadStateReader, NodeState}

def mkDefaultHydrozoaNode = {
    val ownKeys = genNodeKey()
    val ctx: AppCtx = yaciDevKit()

    // Components
    val log = Logger("Hydrozoa")
    val wallet = MockWallet(ctx, 0)

    // Cardano L1
//     val cardano: CardanoL1 = CardanoL1YaciDevKit(ctx)
//     val backendService: BackendService = ctx.backendService

    val cardano = CardanoL1Mock()
    val backendService = BackendServiceMock(cardano)

    // Global head manager (for mocked head during Milestone 2)
    val nodeStateManager: NodeState = NodeState()
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
        HeadPeerNetworkMock(
          nodeStateReader,
          initTxBuilder,
          refundTxBuilder,
          settlementTxBuilder,
          finalizationTxBuilder,
          cardano,
          ownKeys._2
        )

    val node = Node(
      nodeStateManager,
      ownKeys,
      network,
      cardano,
      wallet,
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

        val (log: Logger, node: Node, _) = mkDefaultHydrozoaNode

        log.warn("Starting Hydrozoa Node API Server...")
        NodeRestApi(node).start()
    }
