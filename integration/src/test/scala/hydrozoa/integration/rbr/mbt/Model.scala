package hydrozoa.integration.rbr.mbt

import hydrozoa.config.node.MultiNodeConfig
import java.time.Instant
import test.TestPeers

/** Pre-fallback model state for the RBR fallback→evacuation MBT.
  *
  * For now it only carries the generated config and the seed parameters; the L2/deposit accumulator
  * (which will seed the net's committed obligations) arrives with the deposit/L2-tx commands.
  */
final case class ModelState(
    multiNodeConfig: MultiNodeConfig,
    takeoffTime: Option[Instant],
    testPeers: TestPeers,
    nHeadPeers: Int,
    nCoilPeers: Int,
    maxVersionMinor: Int,
)
