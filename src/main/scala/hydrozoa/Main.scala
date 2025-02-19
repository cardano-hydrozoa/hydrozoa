package hydrozoa

import hydrozoa.head.AppCtx.yaciDevKit
import hydrozoa.head.{Node, NodeApi}
import hydrozoa.head.multisig.mkHeadNativeScript
import hydrozoa.head.network.{HydrozoaNetwork, MockHydrozoaNetwork}
import hydrozoa.logging.{ConsoleLoggingService, LoggingService}

object Cli:

  @main def main(args: String*): Unit = {
    // subsystems
    val logging: LoggingService = ConsoleLoggingService()
    val network: HydrozoaNetwork = MockHydrozoaNetwork()
    // 
    val node = Node(yaciDevKit(), network, logging)
    logging.logInfo("Starting Hydrozoa Node API Server...")
    NodeApi(node).start()
  }
