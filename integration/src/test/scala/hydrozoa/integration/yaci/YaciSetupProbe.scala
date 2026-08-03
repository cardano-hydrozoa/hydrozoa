package hydrozoa.integration.yaci

import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite

/** Validates the full [[YaciSetup.prepare]] orchestration against a real Yaci devnet: on-chain
  * deploy + resolve of the treasury/dispute/G2 script references, and funded genesis UTxOs for
  * every head and coil peer — i.e. every input the multipeer harness config generator needs.
  *
  * Requires Docker; excluded from the default test run (see build.sbt).
  */
class YaciSetupProbe extends AnyFunSuite {

    test(
      "prepares resolved script refs + funded genesis for a 3+3 head/coil head on a Yaci devnet"
    ) {
        YaciDevnet
            .resource()
            .use(devKit => YaciSetup.prepare(devKit, nHeadPeers = 3, nCoilPeers = 3))
            .map { ready =>
                assert(
                  ready.genesisByPeer.sizeIs == 6 &&
                      ready.genesisByPeer.values.forall(_.nonEmpty) &&
                      ready.scriptReferenceUtxos.setupLadderUtxos.utxos.sizeIs == 7
                )
            }
            .unsafeRunSync()
    }
}
