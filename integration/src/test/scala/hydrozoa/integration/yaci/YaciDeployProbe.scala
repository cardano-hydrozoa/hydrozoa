package hydrozoa.integration.yaci

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.app.DeployScriptsAndG2Setup
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.Coin
import test.TestPeerName.Alice
import test.{SeedPhrase, TestPeers}

/** Validates the reusable [[DeployScriptsAndG2Setup.deploy]] against a real Yaci devnet: fund a
  * deployer wallet, deploy the treasury + dispute reference scripts and the G2 setup ladder, and
  * confirm the reference UTxOs land on chain.
  *
  * Requires Docker; excluded from the default test run (see build.sbt).
  */
class YaciDeployProbe extends AnyFunSuite {

    test("deploys treasury + dispute + G2 ladder reference scripts on a Yaci devnet") {
        YaciDevnet
            .resource()
            .use { devKit =>
                MultiPeerHeadHarness.CardanoBackend.yaciNetwork(devKit).flatMap { network =>
                    given CardanoNetwork.Section = network
                    val peers = TestPeers(SeedPhrase.Yaci, network, 1)
                    val deployerAddr = peers.shelleyAddressFor(Alice)
                    val backend = CardanoBackendBlockfrost.apply_(
                      Right((network, devKit.blockfrostApiBaseUri)),
                      tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
                    )
                    for {
                        _ <- IO.blocking(devKit.topup(deployerAddr, Coin(10_000_000_000L)))
                        _ <- awaitFunded(backend, deployerAddr)
                        refs <- DeployScriptsAndG2Setup.deploy(backend, peers.walletFor(Alice))
                        treasury <- backend
                            .resolve(refs.rulebasedTreasuryScriptInput)
                            .flatMap(IO.fromEither)
                    } yield assert(treasury.isDefined && refs.setupLadderInputs.sizeIs == 7)
                }
            }
            .unsafeRunSync()
    }

    /** Poll until the topup UTxO is visible, so the deploy's funding query sees it. */
    private def awaitFunded(
        backend: CardanoBackend[IO],
        address: ShelleyAddress,
        attemptsLeft: Int = 24
    ): IO[Unit] =
        backend.utxosAt(address).flatMap {
            case Right(u) if u.nonEmpty => IO.unit
            case _ if attemptsLeft > 0 =>
                IO.sleep(2.seconds) *> awaitFunded(backend, address, attemptsLeft - 1)
            case _ => IO.raiseError(new RuntimeException(s"topup to $address never confirmed"))
        }
}
