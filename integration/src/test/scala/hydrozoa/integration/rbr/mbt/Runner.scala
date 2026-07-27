package hydrozoa.integration.rbr.mbt

import org.scalacheck.{Test, YetAnotherProperties}

/** Registers the RBR fallback→evacuation MBT. One slow real-clock WS run per property, so cap at a
  * single successful evaluation (like the other multi-peer dispute suites).
  */
object RbrMbtProperties extends YetAnotherProperties("RBR MBT"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withWorkers(1).withMinSuccessfulTests(1)

    val _ = property("ws: autonomous evacuation matches the RBRHlNet terminal") =
        RbrMbtSuite(nHeadPeers = 3, nCoilPeers = 2, maxVersionMinor = 2).property()
