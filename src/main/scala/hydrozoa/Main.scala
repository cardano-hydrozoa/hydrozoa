package hydrozoa

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.genNodeKey
import hydrozoa.l1.multisig.tx.initialization.{BloxBeanInitTxBuilder, InitTxBuilder}
import hydrozoa.l1.wallet.{MockWallet, Wallet}
import hydrozoa.l1.{Cardano, YaciDevKitCardano}
import hydrozoa.l2.consensus.network.{HydrozoaNetwork, MockHydrozoaNetwork}
import AppCtx.yaciDevKit
import hydrozoa.node.api.NodeApi
import hydrozoa.node.server.Node

object Cli:

    @main def main(args: String*): Unit = {

        val ownKeys = genNodeKey()
        val ctx: AppCtx = yaciDevKit()

        // Components
        val log: Logger = Logger("Hydrozoa")
        val wallet: Wallet = MockWallet(ctx)
        val cardano: Cardano = YaciDevKitCardano(ctx)
        val txBuilder: InitTxBuilder = BloxBeanInitTxBuilder(ctx)
        val network: HydrozoaNetwork = MockHydrozoaNetwork(txBuilder, cardano, ownKeys._2)
        val node = Node(ownKeys, network, cardano, wallet, txBuilder, log)

        log.warn("Starting Hydrozoa Node API Server...")
        NodeApi(node).start()
    }
