package hydrozoa

import com.typesafe.scalalogging.Logger
import hydrozoa.head.l1.AppCtx.yaciDevKit
import hydrozoa.head.l1.txbuilder.{BloxBeanTxBuilder, TxBuilder}
import hydrozoa.head.l1.{AppCtx, Cardano, YaciDevKitCardano}
import hydrozoa.head.network.{HydrozoaNetwork, MockHydrozoaNetwork}
import hydrozoa.head.wallet.{MockWallet, Wallet}
import hydrozoa.head.{Node, NodeApi, genNodeKey}

object Cli:

    @main def main(args: String*): Unit = {

        val ownKeys = genNodeKey()
        val ctx: AppCtx = yaciDevKit()

        // Components
        val log: Logger = Logger("Hydrozoa")
        val wallet: Wallet = MockWallet(ctx)
        val cardano: Cardano = YaciDevKitCardano(ctx)
        val txBuilder: TxBuilder = BloxBeanTxBuilder(ctx)
        val network: HydrozoaNetwork = MockHydrozoaNetwork(txBuilder, cardano, ownKeys._2)
        val node = Node(ownKeys, network, cardano, wallet, txBuilder, log)

        log.warn("Starting Hydrozoa Node API Server...")
        NodeApi(node).start()
    }
