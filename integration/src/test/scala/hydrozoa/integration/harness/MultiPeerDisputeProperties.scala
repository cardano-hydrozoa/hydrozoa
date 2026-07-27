package hydrozoa.integration.harness

import hydrozoa.config.head.network.CardanoNetwork
import org.scalacheck.Properties

/** Base for the multi-peer dispute-flow property suites driving [[MultiPeerHeadHarness]]. Each
  * scenario is a single slow end-to-end run, so cap ScalaCheck at one successful evaluation rather
  * than the default 100.
  */
abstract class MultiPeerDisputeProperties(name: String) extends Properties(name):
    override def overrideParameters(
        p: org.scalacheck.Test.Parameters
    ): org.scalacheck.Test.Parameters = p.withMinSuccessfulTests(1)

    protected val cardanoNetwork: CardanoNetwork = CardanoNetwork.Preprod
