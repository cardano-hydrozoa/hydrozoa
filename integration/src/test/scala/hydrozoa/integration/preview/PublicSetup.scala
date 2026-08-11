package hydrozoa.integration.preview

import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import hydrozoa.app.DeployScriptsAndG2Setup
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.yaci.YaciSetup
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.logging.{Slf4jMsgFormat, Slf4jTracer, info}
import hydrozoa.multisig.backend.cardano.{CardanoBackend, CardanoBackendBlockfrost, CardanoBackendEventFormat}
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.ledger.l1.tx.RawTx
import org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, EvaluatorMode, PlutusScriptEvaluator, Transaction, TransactionHash, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{Change, TransactionBuilder}
import scalus.crypto.ed25519.{SigningKey, VerificationKey}
import scalus.uplc.builtin.ByteString
import scala.concurrent.duration.*
import test.{SeedPhrase, TestPeerName, TestPeers}

/** Orchestrates a public testnet (Preview) into the inputs the multipeer harness config generator
  * needs — the public-Blockfrost analog of [[hydrozoa.integration.yaci.YaciSetup]]. Where the Yaci
  * setup funds peers from the devnet faucet (`devKit.topup`) and can `reset()` between iterations,
  * a public chain has neither: peers are funded from ONE pre-funded master wallet via a single
  * distribution tx, and each run inits a fresh head off the fresh UTxOs that tx produces.
  *
  * Feed the returned [[YaciSetup.Ready]] into [[MultiPeerHeadHarness.genDisputeMnc]]
  * (`scriptReferenceUtxos = Some(...)`, genesis via `testPeerToUtxos`) and run the harness with
  * [[MultiPeerHeadHarness.CardanoBackend.Mode.Public]].
  */
object PublicSetup {

    /** 250 tADA per peer — comfortably above a head peer's genesis floor (`contingency ≈ 12 +
      * equity share + ~20 ADA`, with equity capped low via the suite's `equityRange`) plus its
      * self-scaling deposits and collateral. Kept small so the whole fleet (`nPeers * this +
      * deployer extra + fees`) fits a modestly-funded master wallet (~2_500 tADA), not a fresh
      * ~10_000 tADA faucet drop.
      */
    val defaultPeerFunding: Coin = Coin(250_000_000L)

    /** Extra funding for head peer 0, which additionally deploys the treasury/dispute/G2 reference
      * scripts (three chained deployment txs whose reference outputs each lock min-ADA-for-script;
      * its post-deploy change becomes its genesis funding). Generous margin — the reference
      * outputs' min-ADA plus fees run ~1_270+ tADA under live Preview params (bigger than the ~700
      * the bundled params implied) and it varies with wallet-utxo fragmentation, so 1_000 left the
      * dispute deploy tx ~19 tADA short.
      */
    val defaultDeployerExtraFunding: Coin = Coin(2_500_000_000L)

    private val log = Slf4jTracer.sink.contramap(Slf4jMsgFormat.humanFormat("PublicSetup"))

    /** Build the Preview `Custom` network (live params), fund every head + coil peer from the
      * master wallet, deploy + resolve the reference scripts from head peer 0's wallet, and query
      * each funded peer's genesis UTxOs. Coil peers are funded too because coil-side RBAs need
      * ADA-only wallet UTxOs for collateral (see `yaciTestSauceGenesis`). The master wallet
      * (`masterSigningKeyHex`, from the `keygen` CLI) must already hold enough tPreview-ADA (≈
      * `nPeers * peerFunding + deployerExtraFunding + fees`).
      */
    def prepare(
        apiKey: String,
        masterSigningKeyHex: String,
        nHeadPeers: Int,
        nCoilPeers: Int,
        url: String = BlockfrostProvider.previewUrl,
        peerFunding: Coin = defaultPeerFunding,
        deployerExtraFunding: Coin = defaultDeployerExtraFunding,
    ): IO[YaciSetup.Ready] =
        MultiPeerHeadHarness.CardanoBackend.previewNetwork(apiKey).flatMap { network =>
            given CardanoNetwork.Section = network
            val testPeers = TestPeers(SeedPhrase.Public, network, nHeadPeers, nCoilPeers)
            val masterWallet = mkMasterWallet(masterSigningKeyHex)
            val masterAddress = masterWallet.exportVerificationKey.shelleyAddress()
            val backend: CardanoBackend[IO] = CardanoBackendBlockfrost.apply_(
              Right((network, url)),
              apiKey,
              tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat),
            )
            val headNames = List.tabulate(nHeadPeers)(TestPeerName.fromOrdinal)
            val coilNames = List.tabulate(nCoilPeers)(i => TestPeerName.fromOrdinal(nHeadPeers + i))
            val allNames = headNames ++ coilNames
            def addrOf(n: TestPeerName): ShelleyAddress = testPeers.shelleyAddressFor(n)
            val fundingPlan: List[(ShelleyAddress, Coin)] = allNames.zipWithIndex.map {
                case (n, 0) => addrOf(n) -> Coin(peerFunding.value + deployerExtraFunding.value)
                case (n, _) => addrOf(n) -> peerFunding
            }
            for {
                _ <- log.info(
                  s"Funding ${allNames.size} peer address(es) from master " +
                      s"${masterAddress.toBech32.get}"
                )
                fundingTx <- mkFundingTx(network, backend, masterAddress, fundingPlan)
                signed = masterWallet.signTx(fundingTx)
                _ <- backend.submitTx(RawTx(signed)).flatMap(IO.fromEither)
                _ <- log.info(s"Funding tx ${signed.id} submitted; awaiting on-chain confirmation")
                _ <- allNames.traverse_(n => YaciSetup.awaitFunded(backend, addrOf(n)))
                _ <- log.info("All peer funding confirmed; deploying reference scripts")
                unresolved <- DeployScriptsAndG2Setup.deploy(
                  backend,
                  testPeers.walletFor(headNames.head),
                )
                resolved <- unresolved.resolve(backend).flatMap(IO.fromEither)
                genesis <- awaitLiveGenesis(
                  backend,
                  allNames.map(n => n -> addrOf(n)),
                  deployerName = headNames.head,
                  fundingTxId = signed.id,
                )
            } yield YaciSetup.Ready(network, testPeers, backend, resolved, genesis)
        }

    /** Query each peer's genesis UTxOs, retrying until the snapshot is safe to build the init tx
      * on. Two eventual-consistency hazards on the deployer (head peer 0), which spends its funding
      * UTxO in the script-deploy ladder:
      *   - Its captured genesis must not still be the pre-deploy funding output. Blockfrost's
      *     address-utxo index (and even a direct `resolve`) can lag the deploy's spend, so a stale
      *     funding UTxO looks live; the init tx built on it then fails `BadInputsUTxO` and the head
      *     never initializes. We therefore require the deployer's genesis to no longer contain any
      *     output of `fundingTxId` — i.e. the deploy has landed and its spend is indexed, so the
      *     genesis is the deploy's change output.
      *   - Every captured UTxO must still `resolve` live (a direct utxo lookup, catching a spend
      *     the address index hasn't yet reflected).
      * Non-deployer peers never spend their funding, so their funding output is their genesis.
      */
    private def awaitLiveGenesis(
        backend: CardanoBackend[IO],
        peers: List[(TestPeerName, ShelleyAddress)],
        deployerName: TestPeerName,
        fundingTxId: TransactionHash,
        attemptsLeft: Int = 100, // 100 * (query + 3s) ≈ 5+ min
    ): IO[Map[TestPeerName, Utxos]] =
        for {
            genesis <- peers
                .traverse((n, a) => backend.utxosAt(a).flatMap(IO.fromEither).map(n -> _))
                .map(_.toMap)
            // The deploy consumes the deployer's funding output; until its spend is indexed the
            // deployer's genesis is still that (about-to-be-spent) funding UTxO.
            deployerDeployed = genesis
                .get(deployerName)
                .forall(_.keys.forall(_.transactionId != fundingTxId))
            allLive <- genesis.values
                .flatMap(_.keys)
                .toList
                .traverse(i =>
                    backend.resolve(i).map { case Right(Some(_)) => true; case _ => false }
                )
                .map(_.forall(identity))
            result <-
                if deployerDeployed && allLive then
                    log.info("All genesis UTxOs are live").as(genesis)
                else if attemptsLeft > 0 then
                    log.info(
                      "Genesis UTxOs not yet consistent (deploy spends still propagating); " +
                          s"retrying ($attemptsLeft attempts left)"
                    ) *> IO.sleep(3.seconds) *> awaitLiveGenesis(
                      backend,
                      peers,
                      deployerName,
                      fundingTxId,
                      attemptsLeft - 1,
                    )
                else
                    log.info("Genesis UTxO liveness not confirmed after wait; proceeding")
                        .as(genesis)
        } yield result

    /** Rebuild the master signing wallet from a raw Ed25519 signing-key hex (the "Signing key" the
      * `keygen` CLI prints), deriving its verification key with the same bouncycastle primitive
      * `Bootstrap.generateKeyPair` uses. The scalus-wallet reconstruction mirrors
      * `DemoConfig.readWallet`; its address (`exportVerificationKey.shelleyAddress()`) is exactly
      * what `keygen` reports as the "Testnet address".
      */
    private def mkMasterWallet(signingKeyHex: String): PeerWallet =
        val skey = ByteString.fromHex(signingKeyHex)
        val vkeyBytes = Ed25519PrivateKeyParameters(skey.bytes, 0).generatePublicKey().getEncoded
        PeerWallet.scalusWallet(
          VerificationKey.unsafeFromByteString(ByteString.fromArray(vkeyBytes)),
          SigningKey.unsafeFromByteString(skey),
        )

    /** One payment tx spending all master UTxOs into a fixed output per peer, with change back to
      * the master at output 0. Mirrors stage1's `mkMixSplitTx` builder chain, minus its random
      * `CappedValueGen` split (the peer amounts here are fixed).
      */
    private def mkFundingTx(
        network: CardanoNetwork,
        backend: CardanoBackend[IO],
        masterAddress: ShelleyAddress,
        fundingPlan: List[(ShelleyAddress, Coin)],
    ): IO[Transaction] =
        for {
            masterUtxos <- backend.utxosAt(masterAddress).flatMap(IO.fromEither)
            _ <- IO.raiseWhen(masterUtxos.isEmpty)(
              RuntimeException(
                s"master wallet ${masterAddress.toBech32.get} has no UTxOs; fund it on Preview first"
              )
            )
            tx <- IO.fromEither(
              (for {
                  unbalanced <- TransactionBuilder.build(
                    network.cardanoInfo.network,
                    masterUtxos.map { case (id, o) => Spend(Utxo(id, o)) }.toList
                    // Output 0 is the master change placeholder (`changeOutputIdx = 0` below);
                    // outputs 1.. are the fixed per-peer payments.
                        ++ (Send(TransactionOutput.Babbage(masterAddress, Value.zero))
                            :: fundingPlan.map { case (addr, coin) =>
                                Send(TransactionOutput.Babbage(addr, Value(coin)))
                            }),
                  )
                  balanced <- unbalanced.balanceContext(
                    diffHandler = Change.changeOutputDiffHandler(
                      _,
                      _,
                      protocolParams = network.cardanoProtocolParams,
                      changeOutputIdx = 0,
                    ),
                    protocolParams = network.cardanoProtocolParams,
                    evaluator = PlutusScriptEvaluator(
                      network.cardanoInfo,
                      EvaluatorMode.EvaluateAndComputeCost,
                    ),
                  )
              } yield balanced.transaction).left.map(err => RuntimeException(err.toString))
            )
        } yield tx

    /** Sweep every peer's remaining wallet UTxOs back to the master wallet — the inverse of the
      * funding step, for reclaiming a run's leftover tADA (e.g. a run whose init tx never confirmed
      * leaves the peer genesis UTxOs unspent) without going back to the faucet. One single-output
      * sweep tx per peer, signed by that peer's own wallet. Use the same `nHeadPeers`/`nCoilPeers`
      * the run funded, so the derived peer addresses match.
      */
    def reclaim(
        apiKey: String,
        masterSigningKeyHex: String,
        nHeadPeers: Int,
        nCoilPeers: Int,
        url: String = BlockfrostProvider.previewUrl,
    ): IO[Unit] =
        MultiPeerHeadHarness.CardanoBackend.previewNetwork(apiKey).flatMap { network =>
            given CardanoNetwork.Section = network
            val testPeers = TestPeers(SeedPhrase.Public, network, nHeadPeers, nCoilPeers)
            val masterAddress =
                mkMasterWallet(masterSigningKeyHex).exportVerificationKey.shelleyAddress()
            val backend: CardanoBackend[IO] = CardanoBackendBlockfrost.apply_(
              Right((network, url)),
              apiKey,
              tracer = Slf4jTracer.sink.contramap(CardanoBackendEventFormat.humanFormat),
            )
            val headNames = List.tabulate(nHeadPeers)(TestPeerName.fromOrdinal)
            val coilNames = List.tabulate(nCoilPeers)(i => TestPeerName.fromOrdinal(nHeadPeers + i))
            val pairs: List[(TestPeerName, PeerWallet)] =
                headNames
                    .map(n => n -> testPeers.walletFor(n)) ++ coilNames.zip(testPeers.coilWallets)

            def sweepOne(name: TestPeerName, wallet: PeerWallet): IO[Unit] =
                val addr = testPeers.shelleyAddressFor(name)
                backend.utxosAt(addr).flatMap(IO.fromEither).flatMap { utxos =>
                    if utxos.isEmpty then log.info(s"$name has no UTxOs to reclaim")
                    else
                        for {
                            tx <- IO.fromEither(mkSweepTx(network, utxos, masterAddress))
                            signed = wallet.signTx(tx)
                            _ <- backend.submitTx(RawTx(signed)).flatMap(IO.fromEither)
                            _ <- log.info(s"reclaim from $name: tx ${signed.id}")
                        } yield ()
                }

            for {
                _ <- log.info(
                  s"Reclaiming ${pairs.size} peer wallet(s) to master ${masterAddress.toBech32.get}"
                )
                _ <- pairs.traverse_((n, w) => sweepOne(n, w))
                _ <- log.info("All reclaim txs submitted; awaiting on-chain confirmation")
                _ <- pairs.traverse_((n, _) => awaitEmpty(backend, testPeers.shelleyAddressFor(n)))
                master <- backend.utxosAt(masterAddress).flatMap(IO.fromEither)
                _ <- log.info(s"Reclaim complete; master now holds ${master.size} UTxO(s)")
            } yield ()
        }

    /** One sweep tx: spend every input UTxO into a single output at `dest`, all value minus fee
      * flowing there via the change handler at output 0.
      */
    private def mkSweepTx(
        network: CardanoNetwork,
        inputUtxos: Utxos,
        dest: ShelleyAddress,
    ): Either[RuntimeException, Transaction] =
        (for {
            unbalanced <- TransactionBuilder.build(
              network.cardanoInfo.network,
              inputUtxos.map { case (id, o) => Spend(Utxo(id, o)) }.toList
                  :+ Send(TransactionOutput.Babbage(dest, Value.zero)),
            )
            balanced <- unbalanced.balanceContext(
              diffHandler = Change.changeOutputDiffHandler(
                _,
                _,
                protocolParams = network.cardanoProtocolParams,
                changeOutputIdx = 0,
              ),
              protocolParams = network.cardanoProtocolParams,
              evaluator =
                  PlutusScriptEvaluator(network.cardanoInfo, EvaluatorMode.EvaluateAndComputeCost),
            )
        } yield balanced.transaction).left.map(err => RuntimeException(err.toString))

    /** Poll until `address` holds no UTxOs (its sweep tx confirmed). Best-effort: logs and returns
      * on expiry rather than failing the reclaim.
      */
    private def awaitEmpty(
        backend: CardanoBackend[IO],
        address: ShelleyAddress,
        attemptsLeft: Int = 100, // 100 * 3s = 5min
    ): IO[Unit] =
        backend.utxosAt(address).flatMap {
            case Right(u) if u.isEmpty => log.info(s"reclaim confirmed for $address")
            case _ if attemptsLeft > 0 =>
                IO.sleep(3.seconds) *> awaitEmpty(backend, address, attemptsLeft - 1)
            case _ => log.info(s"reclaim for $address not confirmed after wait (continuing)")
        }
}

/** One-off entry point to reclaim the RBR MBT Preview peers' leftover tADA back to the master
  * wallet (see [[PublicSetup.reclaim]]). Reads `BLOCKFROST_API_KEY` +
  * `RBR_MBT_PREVIEW_MASTER_SIGNING_KEY` from the environment. Run with
  * `integration/Test/runMain hydrozoa.integration.preview.ReclaimPeerFunds`.
  */
object ReclaimPeerFunds extends IOApp.Simple:
    def run: IO[Unit] =
        PublicSetup.reclaim(
          apiKey = sys.env.getOrElse("BLOCKFROST_API_KEY", ""),
          masterSigningKeyHex = sys.env.getOrElse("RBR_MBT_PREVIEW_MASTER_SIGNING_KEY", ""),
          nHeadPeers = 3,
          nCoilPeers = 3,
        )
