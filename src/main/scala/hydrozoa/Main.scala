package hydrozoa

import hydrozoa.head.l1.AppCtx.yaciDevKit
import hydrozoa.head.l1.txbuilder.{BloxBeanTxBuilder, TxBuilder}
import hydrozoa.head.l1.{Cardano, YaciDevKitCardano}
import hydrozoa.head.network.{HydrozoaNetwork, MockHydrozoaNetwork}
import hydrozoa.head.wallet.{MockWallet, Wallet}
import hydrozoa.head.{Node, NodeApi}
import hydrozoa.logging.{ConsoleLoggingService, LoggingService}

object Cli:

  @main def main(args: String*): Unit = {
    val network: HydrozoaNetwork = MockHydrozoaNetwork()
    val wallet: Wallet = MockWallet()
    val cardano: Cardano = YaciDevKitCardano(yaciDevKit())
    val txBuilder: TxBuilder = BloxBeanTxBuilder(yaciDevKit())
    val logging: LoggingService = ConsoleLoggingService()

    val node = Node(network, cardano, wallet, txBuilder, logging)
    logging.logInfo("Starting Hydrozoa Node API Server...")
    NodeApi(node).start()
  }
