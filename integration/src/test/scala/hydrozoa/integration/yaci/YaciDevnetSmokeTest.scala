package hydrozoa.integration.yaci

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.integration.harness.MultiPeerHeadHarness
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Coin
import test.TestPeerName.Alice
import test.{SeedPhrase, TestPeers}

/** Spins up a Yaci DevKit devnet container and exercises the [[DevKit]] admin + Blockfrost APIs:
  * fetch devnet info, build the `Custom` network from it, and top up a derived peer address.
  *
  * Requires Docker on the host; excluded from the default test run (see build.sbt), run explicitly
  * with `integration/testOnly hydrozoa.integration.yaci.YaciDevnetSmokeTest`.
  */
class YaciDevnetSmokeTest extends AnyFunSuite {

    test("spins up a Yaci devnet, reports devnet info, and accepts a topup") {
        YaciDevnet
            .resource()
            .use { devKit =>
                for {
                    info <- IO.blocking(devKit.devnetInfo())
                    network <- MultiPeerHeadHarness.CardanoBackend.yaciNetwork(devKit)
                    peers = TestPeers(SeedPhrase.Yaci, network, 1)
                    _ <- IO.blocking(
                      devKit.topup(peers.shelleyAddressFor(Alice), Coin(10_000_000_000L))
                    )
                } yield assert(info.protocolMagic == 42 && info.slotLength > 0)
            }
            .unsafeRunSync()
    }
}
