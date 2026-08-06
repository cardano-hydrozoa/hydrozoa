package hydrozoa.integration.rbr.mbt

import org.scalacheck.{Test, YetAnotherProperties}

/** Registers the RBR fallback→evacuation MBT. One slow real-clock WS run per property, so cap at a
  * single successful evaluation (like the other multi-peer dispute suites).
  */
object RbrMbtProperties extends YetAnotherProperties("RBR MBT"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withWorkers(1).withMinSuccessfulTests(1)

    val _ = property("ws: autonomous evacuation matches the RBRHlNet terminal") =
        RbrMbtSuite(nHeadPeers = 3, nCoilPeers = 3, maxVersionMinor = 2).property()

/** Yaci-backed variant of [[RbrMbtProperties]]. Same shape, same shrink cap, but runs the SUT
  * against a real Testcontainers-managed Yaci devnet — one JVM-wide container reset + redeployed
  * per iteration. Requires Docker; excluded from the default test run (see build.sbt); run via
  * `just integration-yaci-docker` or explicitly with
  * `integration/testOnly hydrozoa.integration.rbr.mbt.RbrMbtPropertiesYaci`.
  */
object RbrMbtPropertiesYaci extends YetAnotherProperties("RBR MBT (Yaci)"):

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withWorkers(1).withMinSuccessfulTests(1)

    val _ = property("ws: autonomous evacuation matches the RBRHlNet terminal (Yaci)") =
        RbrMbtSuite(
          nHeadPeers = 3,
          nCoilPeers = 3,
          maxVersionMinor = 2,
          backendSpec = RbrMbtSuite.BackendSpec.Yaci(),
        ).property()
