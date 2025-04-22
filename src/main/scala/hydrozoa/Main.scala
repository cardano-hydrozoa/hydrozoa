package hydrozoa

import com.typesafe.scalalogging.Logger
import hydrozoa.AppCtx.yaciDevKit
import hydrozoa.infra.genNodeKey
import hydrozoa.l1.multisig.tx.deposit.{BloxBeanDepositTxBuilder, DepositTxBuilder}
import hydrozoa.l1.multisig.tx.finalization.{BloxBeanFinalizationTxBuilder, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{BloxBeanInitializationTxBuilder, InitTxBuilder}
import hydrozoa.l1.multisig.tx.refund.{BloxBeanRefundTxBuilder, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{BloxBeanSettlementTxBuilder, SettlementTxBuilder}
import hydrozoa.l1.wallet.{MockWallet, Wallet}
import hydrozoa.l1.{Cardano, YaciDevKitCardano}
import hydrozoa.l2.consensus.network.{HeadPeerNetwork, HeadPeerNetworkMock}
import hydrozoa.node.rest.NodeRestApi
import hydrozoa.node.server.Node
import hydrozoa.node.state.{HeadStateReader, NodeState}

def mkDefaultHydrozoaNode = {
    val ownKeys = genNodeKey()
    val ctx: AppCtx = yaciDevKit()

    // Components
    val log: Logger = Logger("Hydrozoa")
    val wallet: Wallet = MockWallet(ctx, 0)
    val cardano: Cardano = YaciDevKitCardano(ctx)

    // Global head manager (for mocked head during Milestone 2)
    val nodeStateManager: NodeState = NodeState()
    val nodeStateReader: HeadStateReader = nodeStateManager.reader

    // Tx Builders
    val initTxBuilder: InitTxBuilder = BloxBeanInitializationTxBuilder(ctx)
    val depositTxBuilder: DepositTxBuilder = BloxBeanDepositTxBuilder(ctx, nodeStateReader)
    val refundTxBuilder: RefundTxBuilder = BloxBeanRefundTxBuilder(ctx, nodeStateReader)
    val settlementTxBuilder: SettlementTxBuilder =
        BloxBeanSettlementTxBuilder(ctx, nodeStateReader)
    val finalizationTxBuilder: FinalizationTxBuilder =
        BloxBeanFinalizationTxBuilder(ctx, nodeStateReader)

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
    (log, node)
}

object HydrozoaNodeServer:

    @main def main(args: String*): Unit = {

        val (log: Logger, node: Node) = mkDefaultHydrozoaNode

        log.warn("Starting Hydrozoa Node API Server...")
        NodeRestApi(node).start()
    }
