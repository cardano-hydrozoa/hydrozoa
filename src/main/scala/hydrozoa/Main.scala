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
import hydrozoa.l2.consensus.network.{HydrozoaNetwork, MockHydrozoaNetwork}
import hydrozoa.node.api.NodeApi
import hydrozoa.node.server.{NodeStateManager, HeadStateReader, Node}

object Cli:

    @main def main(args: String*): Unit = {

        val ownKeys = genNodeKey()
        val ctx: AppCtx = yaciDevKit()

        // Components
        val log: Logger = Logger("Hydrozoa")
        val wallet: Wallet = MockWallet(ctx, 0)
        val cardano: Cardano = YaciDevKitCardano(ctx)

        // Global head manager (for mocked head during Milestone 2)
        val nodeStateManager: NodeStateManager = NodeStateManager(log)
        val nodeStateReader: HeadStateReader = HeadStateReader(nodeStateManager)

        // Tx Builders
        val initTxBuilder: InitTxBuilder = BloxBeanInitializationTxBuilder(ctx)
        val depositTxBuilder: DepositTxBuilder = BloxBeanDepositTxBuilder(ctx, nodeStateReader)
        val refundTxBuilder: RefundTxBuilder = BloxBeanRefundTxBuilder(ctx, nodeStateReader)
        val settlementTxBuilder: SettlementTxBuilder =
            BloxBeanSettlementTxBuilder(ctx, nodeStateReader)
        val finalizationTxBuilder: FinalizationTxBuilder =
            BloxBeanFinalizationTxBuilder(ctx, nodeStateReader)

        val network: HydrozoaNetwork =
            MockHydrozoaNetwork(
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

        log.warn("Starting Hydrozoa Node API Server...")
        NodeApi(node).start()
    }
