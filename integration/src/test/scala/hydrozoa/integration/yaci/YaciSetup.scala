package hydrozoa.integration.yaci

import cats.effect.IO
import cats.syntax.all.*
import hydrozoa.app.DeployScriptsAndG2Setup
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.lib.logging.{Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import scala.concurrent.duration.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, Utxos}
import test.{SeedPhrase, TestPeerName, TestPeers}

/** Orchestrates a Yaci devnet into the inputs the multipeer harness config generator needs: a
  * `Custom` network, funded per-head-peer genesis UTxOs, and on-chain-deployed + resolved script
  * references. Feed [[Ready]] into [[MultiPeerHeadHarness.genDisputeMnc]] (`scriptReferenceUtxos =
  * Some(...)`, genesis via `testPeerToUtxos`) and run the harness with `CardanoBackend.Mode.Yaci`.
  */
object YaciSetup {

    /** Everything needed to generate a `MultiNodeConfig` against a live Yaci devnet. */
    final case class Ready(
        network: CardanoNetwork.Custom,
        testPeers: TestPeers,
        backend: CardanoBackend[IO],
        scriptReferenceUtxos: ScriptReferenceUtxos,
        genesisByPeer: Map[TestPeerName, Utxos],
    )

    private val genesisFunding: Coin = Coin(100_000_000_000L) // 100k ADA per peer

    private val log = Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("YaciSetup"))

    /** Reset the devnet, fund every head + coil peer, deploy the treasury/dispute/G2 reference
      * scripts (from head peer 0's wallet — its post-deploy change stays as genesis funding),
      * resolve them, and query each funded peer's genesis UTxOs. Coil peers are funded too because
      * coil-side RBAs need ADA-only wallet UTxOs for collateral (see `yaciTestSauceGenesis`).
      */
    def prepare(devKit: DevKit, nHeadPeers: Int, nCoilPeers: Int = 0): IO[Ready] =
        MultiPeerHeadHarness.CardanoBackend.yaciNetwork(devKit).flatMap { network =>
            given CardanoNetwork.Section = network
            val testPeers = TestPeers(SeedPhrase.Yaci, network, nHeadPeers, nCoilPeers)
            val backend: CardanoBackend[IO] = CardanoBackendBlockfrost.apply_(
              Right((network, devKit.blockfrostApiBaseUri)),
              tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat)
            )
            val headNames = List.tabulate(nHeadPeers)(TestPeerName.fromOrdinal)
            val coilNames = List.tabulate(nCoilPeers)(i => TestPeerName.fromOrdinal(nHeadPeers + i))
            val allNames = headNames ++ coilNames
            def addrOf(n: TestPeerName): ShelleyAddress = testPeers.shelleyAddressFor(n)
            for {
                _ <- log.info(s"Submitting topups for ${allNames.size} peer address(es)")
                _ <- allNames.traverse_(n => IO.blocking(devKit.topup(addrOf(n), genesisFunding)))
                _ <- log.info("Topups submitted; awaiting on-chain confirmation")
                _ <- allNames.parTraverse_(n => awaitFunded(backend, addrOf(n)))
                _ <- log.info("All peer topups confirmed; deploying reference scripts")
                unresolved <- DeployScriptsAndG2Setup.deploy(
                  backend,
                  testPeers.walletFor(headNames.head)
                )
                resolved <- unresolved.resolve(backend).flatMap(IO.fromEither)
                genesis <- allNames
                    .traverse(n => backend.utxosAt(addrOf(n)).flatMap(IO.fromEither).map(n -> _))
            } yield Ready(network, testPeers, backend, resolved, genesis.toMap)
        }

    /** Poll until UTxOs appear at `address`, so downstream funding queries see the topup. */
    def awaitFunded(
        backend: CardanoBackend[IO],
        address: ShelleyAddress,
        attemptsLeft: Int = 150 // 150 * 2s = 5min
    ): IO[Unit] =
        backend.utxosAt(address).flatMap {
            case Right(u) if u.nonEmpty => log.info(s"topup confirmed for $address")
            case _ if attemptsLeft > 0  =>
                // Log every ~10s (5 * 2s) so a stalled devnet is visible rather than silent.
                IO.whenA(attemptsLeft % 5 == 0)(
                  log.info(s"awaiting topup for $address ($attemptsLeft attempts left)")
                ) *> IO.sleep(2.seconds) *> awaitFunded(backend, address, attemptsLeft - 1)
            case _ => IO.raiseError(new RuntimeException(s"topup to $address never confirmed"))
        }
}
