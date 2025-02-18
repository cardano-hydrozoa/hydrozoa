package hydrozoa

import hydrozoa.head.multisig.mkHeadNativeScript
import hydrozoa.head.network.{HydrozoaNetwork, MockHydrozoaNetwork}
import hydrozoa.logging.{ConsoleLoggingService, LoggingService}

object Main:

  @main def main(args: String*): Unit = {
    val loggingService: LoggingService = new ConsoleLoggingService()
    val network: HydrozoaNetwork = new MockHydrozoaNetwork()

    //
    loggingService.logInfo("Starting Hydrozoa Node")
    // getting participants' keys
    val headNativeScript = mkHeadNativeScript(network.participantsKeys())
    val policyId = headNativeScript.getPolicyId
    println(policyId)
  }
