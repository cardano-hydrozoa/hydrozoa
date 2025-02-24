package hydrozoa

import hydrozoa.head.l1.AppCtx.yaciDevKit
import hydrozoa.head.l1.txbuilder.{BloxBeanTxBuilder, TxBuilder}
import hydrozoa.head.l1.{AppCtx, Cardano, YaciDevKitCardano}
import hydrozoa.head.network.{HydrozoaNetwork, MockHydrozoaNetwork}
import hydrozoa.head.wallet.{MockWallet, Wallet}
import hydrozoa.head.{Node, NodeApi, genNodeKey}
import hydrozoa.logging.{ConsoleLoggingService, LoggingService}

object Cli:

    @main def main(args: String*): Unit = {

        val ownKeys = genNodeKey()
        val ctx: AppCtx = yaciDevKit()

        // Components
        val logging: LoggingService = ConsoleLoggingService()
        val wallet: Wallet = MockWallet(ctx)
        val cardano: Cardano = YaciDevKitCardano(ctx)
        val txBuilder: TxBuilder = BloxBeanTxBuilder(ctx)
        val network: HydrozoaNetwork = MockHydrozoaNetwork(txBuilder, cardano, ownKeys._2)
        val node = Node(ownKeys, network, cardano, wallet, txBuilder, logging)

        logging.logInfo("Starting Hydrozoa Node API Server...")
        NodeApi(node).start()
    }
