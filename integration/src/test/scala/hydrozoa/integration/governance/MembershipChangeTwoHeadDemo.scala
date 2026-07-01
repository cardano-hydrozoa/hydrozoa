package hydrozoa.integration.governance

import cats.data.ReaderT
import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.bootstrap.InitializationFunding
import hydrozoa.config.head.coil.{CoilPeerData, CoilPeers}
import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.*
import hydrozoa.config.head.multisig.timing.generateDefaultTxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{HeadConfig, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.operation.multisig.{NodeOperationMultisigConfig, RateLimits, generateNodeOperationMultisigConfig}
import hydrozoa.config.node.{MultiNodeConfig, NodeConfig}
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.Event as HEvent
import hydrozoa.integration.harness.MultiPeerHeadHarness.StorageBackend.Mode as BackendMode
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.harness.Signal
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.multisig.CommonChildEvent
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.SlowConsensusActorEvent
import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerWallet}
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockEffects, BlockHeader}
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, InitializationTx}
import hydrozoa.multisig.ledger.stack.StackEffects
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Prop, Properties}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.rules.{Context as LedgerContext, UtxoEnv}
import scalus.cardano.ledger.{AssetName, CertState, PolicyId, Transaction, TransactionHash, TransactionInput, Utxos}
import test.{SeedPhrase, TestPeers, genMonad}

/** End-to-end two-head membership-change demo (Phase 3).
  *
  * A full live head A (multi-peer, real fast + slow consensus over an in-process mesh) lands its
  * initialization tx on a shared mock L1. That treasury is then handed to a DISJOINT head B via a
  * membership-change transfer tx: the tx is pre-signed with head A's old keys and installed as head
  * B's own initialization tx. Head B — brought up live against the same L1 — must genuinely
  * hard-confirm stack 0 (adding its own round-2 witnesses to the pre-signed body) and submit the
  * transfer tx, which the real Conway ledger accepts. The demo asserts the transfer landed, head B's
  * new treasury exists under its own head token, and head A's old treasury was spent.
  *
  * Bespoke real-clock test (NOT ModelBasedSuite / TestControl): `Transport.Mode.Direct` with
  * `startEpochMs = 0` + `takeoffTime = None` neutralises the harness presleep so the head boots
  * immediately on the real clock. The mock derives its slot from the wall clock, so both heads'
  * block-creation-end-times are anchored at "now" (via the default `currentTimeBlockCreationEndTime`
  * generator) and the default tx timing gives a ~36h init/TTL window — wide enough that the transfer
  * tx is still valid when head B submits it minutes after head A landed.
  */
object MembershipChangeTwoHeadDemo extends Properties("MembershipChange two-head demo") {

    // One live two-head run is enough — it is a real, slow integration scenario.
    override def overrideParameters(p: Parameters): Parameters =
        p.withMinSuccessfulTests(1)

    private val log: (String => IO[Unit]) = msg => IO(println(s"[two-head-demo] $msg"))

    // ===================================
    // Scenario fixture
    // ===================================

    /** Everything a single live two-head run needs. Head B's config (incl. the pre-signed transfer
      * tx) is derived deterministically from head A's config — head A's init tx id is fixed by its
      * config, so head B's head id / transfer tx are known before head A even boots. Running head A
      * live only serves to LAND that init tx on L1 so the transfer's inputs exist.
      */
    private case class Scenario(
        mncA: MultiNodeConfig,
        coilNodeConfigsA: List[NodeConfig],
        headAGenesis: Utxos,
        headAInitTxId: TransactionHash,
        oldTreasuryInput: TransactionInput,
        transferTx: Transaction,
        transferTxId: TransactionHash,
        mncB: MultiNodeConfig,
        headBMultisigAddress: ShelleyAddress,
        newTreasuryRef: TransactionInput,
        newPolicyId: PolicyId,
        newTreasuryTokenName: AssetName,
    )

    private case class HeadBArtifacts(
        headConfig: HeadConfig,
        transferTx: Transaction,
        headBMultisigAddress: ShelleyAddress,
        newTreasuryRef: TransactionInput,
        newPolicyId: PolicyId,
        newTreasuryTokenName: AssetName,
    )

    /** Deterministically build head B's full [[HeadConfig]] (transfer tx installed as its init tx)
      * from head A's already-built [[MultiNodeConfig]]. Mirrors `HeadConfig.headConfigDecoder`:
      * parse the transfer tx as the new head's init tx, derive its fallback, wrap into an
      * [[InitialBlock]], and assemble the config. The transfer tx is pre-signed with head A's OLD
      * keys only — head B's consensus adds its own witnesses.
      */
    private def deriveHeadB(
        mncA: MultiNodeConfig,
        coilWalletsA: List[PeerWallet],
        testPeersB: TestPeers
    ): Either[String, HeadBArtifacts] = {
        val oldConfig = mncA.headConfig
        val oldInitTx = mncA.initializationTx
        val oldTreasury = oldInitTx.treasuryProduced
        val oldRegime = oldInitTx.multisigRegimeProduced

        val oldTreasuryInput = oldTreasury.asUtxo.input
        val newHeadId = InitializationFunding.mkHeadId(oldTreasuryInput)

        val newInitParams = InitializationParameters(
          initialEvacuationMap = oldConfig.initialEvacuationMap,
          initialEquityContributions = oldConfig.initialEquityContributions,
          headId = newHeadId
        )

        val newBootstrapV = HeadConfig.Bootstrap(
          cardanoNetwork = oldConfig.cardanoNetwork,
          // Head B is a disjoint head with no coil peers, so its coil quorum must be 0. Head A's
          // `coilQuorum` must NOT leak through — the direct `Bootstrap.apply` (unlike the JSON
          // decoder) does not reject `coilQuorum > coilPeers.size`, and a non-zero quorum would make
          // head B's slow consensus wait forever for coil acks that never arrive (and make the new
          // multisig script's coil branch unsatisfiable).
          headParams = oldConfig.headParameters.copy(coilQuorum = 0),
          headPeers = testPeersB.headPeers,
          coilPeers = CoilPeers.empty,
          initializationParams = newInitParams,
          scriptReferenceUtxos = oldConfig.scriptReferenceUtxos
        )

        val blockEndTime = oldConfig.initialBlock.blockBrief.endTime

        newBootstrapV match {
            case Invalid(e) => Left(s"new bootstrap invalid: $e")
            case Valid(newBootstrap) =>
                for {
                    finalized <- MembershipChange
                        .buildTransferTx(
                          oldConfig = oldConfig,
                          newConfig = newBootstrap,
                          oldHeadId = oldConfig.headId,
                          newHeadId = newHeadId,
                          oldTreasury = oldTreasury,
                          oldMultisigRegime = oldRegime,
                          blockCreationEndTime = blockEndTime
                        )
                        .left
                        .map(e => s"transfer tx build failed: $e")

                    unsignedTransfer = finalized.transaction
                    // Only head A's OLD witnesses (they satisfy the old-script spends/burns). The old
                    // threshold script is `AllOf(head) ∧ AtLeast(coilQuorum, coil)`, so the old
                    // treasury/regime spend needs head A's head witnesses AND its coil witnesses —
                    // both pre-attached here (a script utxo cannot be witnessed by head B). Head B's
                    // consensus adds only the new-script mint witnesses at its own stack-0 round 2.
                    oldHeadWitnesses = mncA.mkVKeyWitnesses(unsignedTransfer).toList
                    oldCoilWitnesses = coilWalletsA.map(_.mkVKeyWitness(unsignedTransfer))
                    transferTx =
                        unsignedTransfer.attachVKeyWitnesses(oldHeadWitnesses ++ oldCoilWitnesses)

                    parsed <- InitializationTx
                        .Parse(newBootstrap)(
                          blockCreationEndTime = blockEndTime,
                          tx = transferTx,
                          resolvedUtxos = finalized.resolvedUtxos
                        )
                        .result
                        .left
                        .map(e => s"new head failed to parse transfer tx: $e")

                    fallback <- FallbackTx
                        .Build(
                          newBootstrap.txTiming.newFallbackStartTime(blockEndTime),
                          parsed.treasuryProduced,
                          parsed.multisigRegimeProduced
                        )(using newBootstrap)
                        .result
                        .left
                        .map(e => s"fallback tx build failed: $e")

                    brief = BlockBrief.Initial(
                      BlockHeader.Initial(
                        startTime = BlockCreationStartTime(blockEndTime - 10.seconds),
                        endTime = blockEndTime,
                        fallbackTxStartTime = fallback.fallbackTxStartTime,
                        forcedMajorBlockWakeupTime =
                            newBootstrap.txTiming.forcedMajorBlockWakeupTime(
                              fallback.fallbackTxStartTime
                            ),
                        mDepositDecisionWakeupTime = None
                      )
                    )

                    ib = InitialBlock(
                      Block.Unsigned.Initial(
                        brief,
                        BlockEffects.Unsigned.Initial(parsed, fallback)
                      )
                    )

                    hc <- HeadConfig(newBootstrap, ib).toEither.left
                        .map(e => s"head B HeadConfig assembly failed: $e")
                } yield HeadBArtifacts(
                  headConfig = hc,
                  transferTx = transferTx,
                  headBMultisigAddress = newBootstrap.headMultisigAddress,
                  newTreasuryRef = TransactionInput(transferTx.id, 0),
                  newPolicyId = newBootstrap.headMultisigScript.policyId,
                  newTreasuryTokenName = newBootstrap.headTokenNames.treasuryTokenName
                )
        }
    }

    /** Generate head A (2 head peers, disjoint from head B) and derive head B (2 head peers, disjoint
      * seed). Default tx timing gives a wide init/TTL window; the block-creation-end-time is anchored
      * at "now" by the default `currentTimeBlockCreationEndTime` inside `generateInitialBlock`.
      */
    private val nHeadPeers = 2
    private val nCoilPeers = 2

    // Narrow the slow/fast rate limits (as stage4 does): the production defaults (3-min
    // `hardStackMinPeriod`) pace stack propagation against each stack's `creationEndTime` — anchored
    // at "now" on the real clock — so stack 0 would otherwise be held for minutes.
    private def genNodeOpMultisig(hc: HeadConfig): Gen[NodeOperationMultisigConfig] =
        generateNodeOperationMultisigConfig(
          maxPollingPeriod = hc.maxCardanoLiaisonPollingPeriod,
          rateLimits = RateLimits(softBlockMinPeriod = 5.seconds, hardStackMinPeriod = 2.seconds)
        )

    private val genScenario: Gen[Scenario] = {
        val testPeersA = TestPeers(SeedPhrase.Yaci, CardanoNetwork.Preprod, nHeadPeers)
        val testPeersB = TestPeers(SeedPhrase.Public, CardanoNetwork.Preprod, nHeadPeers)

        // Coil wallets are extra keys from head A's seed, beyond its head set; each coil peer is
        // hubbed by head 0. Their vkeys go into head A's bootstrap so the threshold script requires
        // `coilQuorum` of them, and each coil peer runs a live follower node (`mkCoilConfig` below).
        val testPeersACoils = TestPeers(SeedPhrase.Yaci, CardanoNetwork.Preprod, nHeadPeers + nCoilPeers)
        val coilWalletsA: List[PeerWallet] =
            (0 until nCoilPeers).toList.map(i => testPeersACoils.walletFor(HeadPeerNumber(nHeadPeers + i)))
        val coilPeersA: CoilPeers = CoilPeers.indexed(
          coilWalletsA.map(w => CoilPeerData(w.exportVerificationKey, HeadPeerNumber(0)))
        )

        for {
            bootstrapAndFundingA <- generateHeadConfigBootstrap(
              generateHeadParams = generateHeadParameters(generateTxTiming = generateDefaultTxTiming)
                  .map(_.copy(coilQuorum = nCoilPeers)),
              coilPeers = coilPeersA
            ).run(testPeersA)

            mncA <- MultiNodeConfig.generateWith(testPeersA)(
              generateHeadConfig = generateHeadConfig(
                genHeadConfigBootstrap = ReaderT.pure(bootstrapAndFundingA)(using genMonad)
              ),
              generateNodeOperationMultisigConfig = genNodeOpMultisig
            )

            // Each coil peer's own node config: head A's shared head config plus the coil identity
            // seam, reusing head 0's operational sub-configs (the coil is a read-only follower).
            head0Private = mncA.nodePrivateConfigs(HeadPeerNumber(0))
            coilNodeConfigsA = coilWalletsA.map { w =>
                NodeConfig
                    .mkCoilConfig(
                      headConfig = mncA.headConfig,
                      ownCoilWallet = w,
                      nodeOperationEvacuationConfig = head0Private.nodeOperationEvacuationConfig,
                      nodeOperationMultisigConfig = head0Private.nodeOperationMultisigConfig,
                      blockfrostApiKey = "not-a-real-key",
                      sugarRushUri = "ws://localhost:3001/ws",
                      adminUsername = "admin",
                      adminPassword = "welcome",
                      httpHost = "0.0.0.0",
                      httpPort = "8080"
                    )
                    .get
            }

            headB = deriveHeadB(mncA, coilWalletsA, testPeersB) match {
                case Right(a)  => a
                case Left(err) => throw new RuntimeException(err)
            }

            mncB <- MultiNodeConfig.generateWith(testPeersB)(
              generateHeadConfig = ReaderT.pure(headB.headConfig)(using genMonad),
              generateNodeOperationMultisigConfig = genNodeOpMultisig
            )
        } yield Scenario(
          mncA = mncA,
          coilNodeConfigsA = coilNodeConfigsA,
          headAGenesis = mncA.initializationTx.resolvedUtxos.utxos,
          headAInitTxId = mncA.initializationTx.tx.id,
          oldTreasuryInput = mncA.initializationTx.treasuryProduced.asUtxo.input,
          transferTx = headB.transferTx,
          transferTxId = headB.transferTx.id,
          mncB = mncB,
          headBMultisigAddress = headB.headBMultisigAddress,
          newTreasuryRef = headB.newTreasuryRef,
          newPolicyId = headB.newPolicyId,
          newTreasuryTokenName = headB.newTreasuryTokenName
        )
    }

    // ===================================
    // Live run
    // ===================================

    /** A stack-0 hard-confirm signal: fires the first time any head peer emits a hard-confirmed
      * INITIAL stack (i.e. the head ratified its init tx and is about to submit it).
      */
    private def stackZeroSignal: IO[Signal[Unit]] =
        Signal.make[Unit] {
            case HEvent.Head(
                  _,
                  CommonChildEvent.SlowConsensusActor(
                    SlowConsensusActorEvent.StackHardConfirmed(st)
                  )
                ) =>
                st.effects match {
                    case _: StackEffects.HardConfirmed.Initial => IO.pure(Some(()))
                    case _                                     => IO.pure(None)
                }
            case _ => IO.pure(None)
        }

    private def pollTxKnown(
        backend: L1Backend[IO],
        txId: TransactionHash,
        attempts: Int,
        sleep: FiniteDuration
    ): IO[Boolean] =
        backend.isTxKnown(txId).flatMap {
            case Right(true)             => IO.pure(true)
            case _ if attempts <= 1      => IO.pure(false)
            case _ => IO.sleep(sleep) >> pollTxKnown(backend, txId, attempts - 1, sleep)
        }

    private def inputsFor(
        label: String,
        mnc: MultiNodeConfig,
        coilNodeConfigs: List[NodeConfig],
        backend: L1Backend[IO]
    ): MultiPeerHeadHarness.Inputs =
        MultiPeerHeadHarness.Inputs(
          config = MultiPeerHeadHarness.Config(label, BackendMode.InMemory, TransportMode.Direct),
          multiNodeConfig = mnc,
          coilNodeConfigs = coilNodeConfigs,
          preinitPeerUtxosL1 = Map.empty,
          takeoffTime = None,
          startEpochMs = 0L,
          sharedCardanoBackend = Some(backend)
        )

    private def hooksFor(sig: Signal[Unit]): MultiPeerHeadHarness.Hooks[Unit, Unit] =
        MultiPeerHeadHarness.Hooks[Unit, Unit](
          tracer = sig.tracer,
          peerHandle = (_, _) => IO.unit,
          coilHandle = (_, _) => IO.unit
        )

    private def runDemo(scenario: Scenario): IO[Prop] = {
        val cfg = scenario.mncA.headConfig
        val mkContext: Long => LedgerContext = slot =>
            LedgerContext(
              env = UtxoEnv(
                slot = slot,
                params = cfg.cardanoProtocolParams,
                certState = CertState.empty,
                network = cfg.network
              ),
              slotConfig = cfg.slotConfig
            )

        for {
            backend <- CardanoBackendMock.mockIO(
              initialState = MockState(scenario.headAGenesis),
              mkContext = mkContext
            )

            // ---- Head A: live, lands its init tx on the shared L1 ----
            sigA <- stackZeroSignal
            _    <- log("bringing up head A")
            headAResult <- MultiPeerHeadHarness
                .resource(
                  inputsFor("headA", scenario.mncA, scenario.coilNodeConfigsA, backend),
                  hooksFor(sigA)
                )
                .use { harnessA =>
                    for {
                        _        <- sigA.await.timeout(120.seconds)
                        _        <- log("head A hard-confirmed stack 0; awaiting init tx on L1")
                        landed   <- pollTxKnown(backend, scenario.headAInitTxId, 120, 500.millis)
                        errors   <- harnessA.sutErrors.get
                        _        <- log(s"head A: initLanded=$landed errors=${errors.size}")
                    } yield (landed, errors)
                }
            (headAInitLanded, headAErrors) = headAResult

            // ---- Head B: live, must sign + submit the transfer tx ----
            sigB <- stackZeroSignal
            _    <- log("bringing up head B (transfer tx as its init tx)")
            headBResult <- MultiPeerHeadHarness
                .resource(inputsFor("headB", scenario.mncB, Nil, backend), hooksFor(sigB))
                .use { harnessB =>
                    for {
                        _             <- sigB.await.timeout(120.seconds)
                        _             <- log("head B hard-confirmed stack 0; awaiting transfer tx on L1")
                        landed        <- pollTxKnown(backend, scenario.transferTxId, 120, 500.millis)
                        transferKnown <- backend.isTxKnown(scenario.transferTxId)
                        newTreasury   <- backend.resolve(scenario.newTreasuryRef)
                        oldTreasury   <- backend.resolve(scenario.oldTreasuryInput)
                        errors        <- harnessB.sutErrors.get
                        _             <- log(s"head B: transferLanded=$landed errors=${errors.size}")
                    } yield (landed, transferKnown, newTreasury, oldTreasury, errors)
                }
            (transferLanded, transferKnown, newTreasuryRes, oldTreasuryRes, headBErrors) =
                headBResult
        } yield {
            val newTreasuryOk = newTreasuryRes match {
                case Right(Some(utxo)) =>
                    val atHeadB = utxo.output.address == scenario.headBMultisigAddress
                    val hasToken = utxo.output.value.assets.assets
                        .get(scenario.newPolicyId)
                        .flatMap(_.get(scenario.newTreasuryTokenName))
                        .contains(1L)
                    atHeadB && hasToken
                case _ => false
            }
            val oldTreasurySpent = oldTreasuryRes == Right(None)

            List(
              s"head A init tx did not land on L1" |: headAInitLanded,
              s"head A produced actor errors: ${headAErrors.mkString("; ")}" |: headAErrors.isEmpty,
              s"transfer tx did not land on L1 (isTxKnown=$transferKnown)" |: transferLanded,
              s"transfer tx not known: $transferKnown" |: (transferKnown == Right(true)),
              s"new treasury missing/mismatched at head B: $newTreasuryRes" |: newTreasuryOk,
              s"old treasury not spent: $oldTreasuryRes" |: oldTreasurySpent,
              s"head B produced actor errors: ${headBErrors.mkString("; ")}" |: headBErrors.isEmpty
            ).foldLeft(Prop(true))(_ && _)
        }
    }

    val _ = property("transfer tx flows through a live head B onto a shared L1") =
        Prop.forAll(genScenario) { scenario =>
            runDemo(scenario).unsafeRunSync()
        }
}
