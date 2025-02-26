package hydrozoa

import com.typesafe.scalalogging.Logger
import hydrozoa.AppCtx.yaciDevKit
import hydrozoa.infra.genNodeKey
import hydrozoa.l1.multisig.tx.deposit.{BloxBeanDepositTxBuilder, DepositTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{BloxBeanInitTxBuilder, InitTxBuilder}
import hydrozoa.l1.wallet.{MockWallet, Wallet}
import hydrozoa.l1.{Cardano, YaciDevKitCardano}
import hydrozoa.l2.consensus.network.{HydrozoaNetwork, MockHydrozoaNetwork}
import hydrozoa.node.api.NodeApi
import hydrozoa.node.server.{HeadStateManager, HeadStateReader, Node}

object Cli:

    @main def main(args: String*): Unit = {

        val ownKeys = genNodeKey()
        val ctx: AppCtx = yaciDevKit()

        // Global head manager (for mocked head during Milestone 2)
        val headStateManager: HeadStateManager = HeadStateManager()
        val headStateReader: HeadStateReader = HeadStateReader(headStateManager)

        // Components
        val log: Logger = Logger("Hydrozoa")
        val wallet: Wallet = MockWallet(ctx)
        val cardano: Cardano = YaciDevKitCardano(ctx)

        val initTxBuilder: InitTxBuilder = BloxBeanInitTxBuilder(ctx)
        val depositTxBuilder: DepositTxBuilder = BloxBeanDepositTxBuilder(ctx, headStateReader)

        val network: HydrozoaNetwork =
            MockHydrozoaNetwork(headStateReader, initTxBuilder, cardano, ownKeys._2)

        val node = Node(
          headStateManager,
          ownKeys,
          network,
          cardano,
          wallet,
          initTxBuilder,
          depositTxBuilder,
          log
        )

        log.warn("Starting Hydrozoa Node API Server...")
        NodeApi(node).start()
    }
