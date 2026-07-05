package hydrozoa.examples.transient

import cats.data.{NonEmptyMap, ReaderT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.config.head.InitParamsType.Constant
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{HeadConfig, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.config.node.operation.multisig.{NodeOperationMultisigConfig, RateLimits, generateNodeOperationMultisigConfig}
import hydrozoa.integration.harness.MultiPeerHeadHarness.StorageBackend.Mode as BackendMode
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.harness.{MultiPeerHeadHarness, Signal}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.ledger.withZeroFees
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.multisig.CommonChildEvent
import hydrozoa.multisig.backend.cardano.CardanoBackend as L1Backend
import hydrozoa.multisig.consensus.UserRequestBody.TransactionRequestBody
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{RequestSequencer, SlowConsensusActorEvent, UserRequest, UserRequestBody, UserRequestHeader}
import hydrozoa.multisig.ledger.eutxol2.toEvacuationKey
import hydrozoa.multisig.ledger.eutxol2.tx.{L2Genesis, TransientOutputs}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.stack.StackEffects
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.evacuationKeyToData
import java.time.Instant
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AssetName, AuxiliaryData, Coin, KeepRaw, Metadatum, MultiAsset, PolicyId, Script, SlotConfig, Timelock, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxo, Utxos, Value, Word64}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, Mint, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{NativeScriptWitness, PubKeyWitness, TransactionBuilder, TransactionBuilderStep}
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString
import test.{SeedPhrase, TestPeers, genMonad}

/** Transient-token demo: minting and burning tokens inside the L2 ledger, scoped to the head's
  * lifetime.
  *
  * An issuer boots a live multi-peer head over a plain ADA pot and then, entirely on L2, mints
  * [[supply]] DEMO tokens under a native single-key policy. The minted tokens are **transient**:
  * they live in the transient-token compartment (declared per output via the `transientOutputs`
  * metadata field), circulate between L2 addresses like any other asset, but can never be withdrawn
  * to L1 — the head rejects a withdrawal that carries them. To exit, the issuer burns the supply
  * and withdraws the freed backing ADA.
  *
  * Bespoke real-clock run (the airdrop-demo pattern, NOT ModelBasedSuite): the head boots
  * immediately on the wall clock and the mock L1 derives its slot from wall time. Rate limits are
  * narrowed so blocks/stacks flow quickly.
  *
  * The final assertions read the mock L1 directly: the freed backing ADA lands at the recipient
  * address, and the transient tokens never appear anywhere on L1.
  */
object TransientTokenDemo extends Properties("Transient token demo") {

    // One live run is enough — it is a real, slow integration scenario.
    override def overrideParameters(p: Parameters): Parameters =
        p.withMinSuccessfulTests(1)

    private val log: String => IO[Unit] = msg => IO(println(s"[transient-demo] $msg"))

    // ===================================
    // Demo parameters
    // ===================================

    private val nHeadPeers = 2

    /** The whole transient supply the issuer mints on L2. */
    private val supply: Long = 1_000L

    /** ADA backing the pot utxo the tokens ride on (withdrawn at the end). */
    private val potAdaLovelace: Long = 100_000_000L

    /** Equity contributed per peer (covers the init tx fee). */
    private val equityPerPeerLovelace: Long = 20_000_000L

    private val demoAssetName: AssetName = AssetName(ByteString.fromString("DEMO"))

    // ===================================
    // Scenario fixture
    // ===================================

    /** The offline-built, signed L2 transaction chain the demo submits in order. */
    private case class DemoTxs(
        mint: Transaction,
        transferToHolder: Transaction,
        transferBack: Transaction,
        rejectedWithdrawal: Transaction,
        burn: Transaction,
        withdrawal: Transaction
    ) {
        def accepted: List[(String, Transaction)] = List(
          "mint" -> mint,
          "transfer to holder" -> transferToHolder,
          "transfer back" -> transferBack,
          "burn" -> burn,
          "withdrawal" -> withdrawal
        )
    }

    private case class Scenario(
        mnc: MultiNodeConfig,
        preinitUtxos: Utxos,
        headInitTxId: TransactionHash,
        policyId: PolicyId,
        recipientAddress: ShelleyAddress,
        txs: DemoTxs
    )

    // Narrow the rate limits (as stage4 does) so stack/block propagation isn't paced against the
    // production defaults while the demo runs on the real clock.
    private def genNodeOperationMultisig(
        headConfig: HeadConfig
    ): Gen[NodeOperationMultisigConfig] =
        generateNodeOperationMultisigConfig(
          maxPollingPeriod = headConfig.maxCardanoLiaisonPollingPeriod,
          rateLimits = RateLimits(softBlockMinPeriod = 5.seconds, hardStackMinPeriod = 2.seconds)
        )

    /** The head-label metadata for an L2 transaction: per-output L1(1)/L2(2) markers plus the
      * transient declarations (Map shape when declarations exist, bare list otherwise).
      */
    private def mkHeadMetadata(
        markers: List[Int],
        transientOutputs: Map[Int, MultiAsset]
    ): AuxiliaryData = {
        val markerList =
            Metadatum.List(markers.map(marker => Metadatum.Int(marker.toLong)).toIndexedSeq)
        val headMetadatum =
            if transientOutputs.isEmpty then markerList
            else
                Metadatum.Map(
                  Map(
                    Metadatum.Text("outputs") -> markerList,
                    Metadatum.Text("transientOutputs") ->
                        TransientOutputs.encodeMetadatum(transientOutputs)
                  )
                )
        Metadata(Map(Word64(CIP67.Tags.head) -> headMetadatum))
    }

    /** Build the deterministic chain of signed demo txs off the initial pot utxo. Tx ids are stable
      * under signing, so the whole chain is known offline.
      */
    private def buildDemoTxs(
        mnc: MultiNodeConfig,
        issuerAddress: ShelleyAddress,
        holderAddress: ShelleyAddress,
        recipientAddress: ShelleyAddress,
        potInput0: TransactionInput,
        potValue: Value
    ): (PolicyId, DemoTxs) = {
        val protocolParams = mnc.headConfig.cardanoProtocolParams.withZeroFees
        val evaluator = mnc.headConfig.plutusScriptEvaluatorForTxBuild
        val network = mnc.headConfig.cardanoNetwork.cardanoInfo.network
        val signAsIssuer = mnc.signTxAs(HeadPeerNumber(0))

        val mintScript: Script.Native =
            Script.Native(Timelock.Signature(mnc.addrKeyHashOf(HeadPeerNumber(0))))
        val policyId: PolicyId = mintScript.scriptHash
        val bundle: MultiAsset = MultiAsset.asset(policyId, demoAssetName, supply)
        val tokenValue: Value = potValue + Value(Coin.zero, bundle)

        def build(
            spentInput: TransactionInput,
            spentValue: Value,
            spentAddress: ShelleyAddress,
            output: TransactionOutput,
            marker: Int,
            mint: Option[Long],
            transientOutputs: Map[Int, MultiAsset]
        ): Transaction = {
            val steps: List[TransactionBuilderStep] = List(
              Spend(Utxo(spentInput, Babbage(spentAddress, spentValue)), PubKeyWitness),
              Send(output),
              Fee(Coin.zero),
              ModifyAuxiliaryData(_ => Some(mkHeadMetadata(List(marker), transientOutputs)))
            ) ++ mint.map(amount =>
                Mint(policyId, demoAssetName, amount, NativeScriptWitness.attached(mintScript))
            )
            val unsigned = TransactionBuilder
                .build(network, steps)
                .flatMap(
                  _.finalizeContext(
                    protocolParams = protocolParams,
                    diffHandler = prebalancedLovelaceDiffHandler,
                    evaluator = evaluator,
                    validators = Seq.empty
                  )
                )
                .fold(
                  error => throw RuntimeException(s"demo tx build failed: $error"),
                  context => context.transaction
                )
            signAsIssuer(unsigned)
        }

        // Every tx in the chain has a single output at index 0; the next tx spends it. The issuer
        // signs everything; the holder's leg works because L2 signature checks are per-input and
        // the holder-owned utxo is spent by... the HOLDER — so that leg is signed by peer 1 too.
        val mint = build(
          potInput0,
          potValue,
          issuerAddress,
          Babbage(issuerAddress, tokenValue),
          marker = 2,
          mint = Some(supply),
          transientOutputs = Map(0 -> bundle)
        )
        val transferToHolder = build(
          TransactionInput(mint.id, 0),
          tokenValue,
          issuerAddress,
          Babbage(holderAddress, tokenValue),
          marker = 2,
          mint = None,
          transientOutputs = Map(0 -> bundle)
        )
        val transferBack0 = build(
          TransactionInput(transferToHolder.id, 0),
          tokenValue,
          holderAddress,
          Babbage(issuerAddress, tokenValue),
          marker = 2,
          mint = None,
          transientOutputs = Map(0 -> bundle)
        )
        // The holder owns the input of the transfer-back leg — add their signature as well.
        val transferBack = mnc.signTxAs(HeadPeerNumber(1))(transferBack0)
        val rejectedWithdrawal = build(
          TransactionInput(transferBack.id, 0),
          tokenValue,
          issuerAddress,
          Babbage(recipientAddress, tokenValue),
          marker = 1,
          mint = None,
          transientOutputs = Map(0 -> bundle)
        )
        val burn = build(
          TransactionInput(transferBack.id, 0),
          tokenValue,
          issuerAddress,
          Babbage(issuerAddress, potValue),
          marker = 2,
          mint = Some(-supply),
          transientOutputs = Map.empty
        )
        val withdrawal = build(
          TransactionInput(burn.id, 0),
          potValue,
          issuerAddress,
          Babbage(recipientAddress, potValue),
          marker = 1,
          mint = None,
          transientOutputs = Map.empty
        )
        (
          policyId,
          DemoTxs(mint, transferToHolder, transferBack, rejectedWithdrawal, burn, withdrawal)
        )
    }

    private val genScenario: Gen[Scenario] = {
        val testPeers = TestPeers(SeedPhrase.Yaci, CardanoNetwork.Preprod, nHeadPeers)
        val cardanoNetwork = testPeers.cardanoNetwork
        val issuerAddress = testPeers.shelleyAddressFor(HeadPeerNumber(0))
        val holderAddress = testPeers.shelleyAddressFor(HeadPeerNumber(1))
        // A bare recipient address for the final withdrawal (no signing key needed).
        val recipientAddress = testPeers.shelleyAddressFor(HeadPeerNumber(1))

        val potValue = Value(Coin(potAdaLovelace))
        val equity: NonEmptyMap[HeadPeerNumber, Coin] =
            NonEmptyMap.fromMapUnsafe(
              SortedMap.from(
                (0 until nHeadPeers).map(i => HeadPeerNumber(i) -> Coin(equityPerPeerLovelace))
              )
            )
        val totalEquity = Coin(equityPerPeerLovelace * nHeadPeers)

        // A fixed seed funding utxo on L1 that the init tx spends; its input fixes the head id.
        val seedInput = TransactionInput(
          TransactionHash.fromByteString(
            blake2b_256(ByteString.fromArray("hydrozoa-transient-demo-seed".getBytes))
          ),
          0
        )

        for {
            headParams <- generateHeadParameters().run(testPeers)

            fc = headParams.fallbackContingency
            totalContingency = Coin(
              fc.collectiveContingency.total.value + nHeadPeers * fc.individualContingency.total.value
            )

            fundingValueTarget = potValue + Value(totalEquity) + Value(totalContingency)
            seedUtxo = Utxo(seedInput, Babbage(issuerAddress, fundingValueTarget))

            genesisId = L2Genesis.mkGenesisId(seedInput)
            potInput0 = TransactionInput(genesisId, 0)
            evacuationMap = EvacuationMap(
              TreeMap(
                potInput0.toEvacuationKey ->
                    Payout
                        .Obligation(KeepRaw(Babbage(issuerAddress, potValue)), cardanoNetwork)
                        .toOption
                        .get
              )
            )
            params = InitializationParameters(
              initialEvacuationMap = evacuationMap,
              initialEquityContributions = equity,
              seedUtxo = seedUtxo,
              additionalFundingUtxos = Map.empty,
              initialChangeOutputs = Nil
            )

            bootstrap <- generateHeadConfigBootstrap(
              generateHeadParams = ReaderT.pure(headParams)(using genMonad),
              generateInitializationParameters = Constant(params)
            ).run(testPeers)

            mnc <- MultiNodeConfig.generateWith(testPeers)(
              generateHeadConfig = generateHeadConfig(
                genHeadConfigBootstrap = ReaderT.pure(bootstrap)(using genMonad)
              ),
              generateNodeOperationMultisigConfig = genNodeOperationMultisig
            )

            policyAndTxs = buildDemoTxs(
              mnc,
              issuerAddress,
              holderAddress,
              recipientAddress,
              potInput0,
              potValue
            )
        } yield Scenario(
          mnc = mnc,
          preinitUtxos = Map(seedUtxo.input -> seedUtxo.output),
          headInitTxId = mnc.initialBlock.effects.initializationTx.tx.id,
          policyId = policyAndTxs._1,
          recipientAddress = recipientAddress,
          txs = policyAndTxs._2
        )
    }

    // ===================================
    // Live run
    // ===================================

    /** Fires the first time any head peer hard-confirms the INITIAL stack (init tx ratified). */
    private def stackZeroSignal: IO[Signal[Unit]] =
        Signal.make[Unit] {
            case MultiPeerHeadHarness.Event.Head(
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

    private def inputsFor(scenario: Scenario): MultiPeerHeadHarness.Inputs =
        MultiPeerHeadHarness.Inputs(
          config = MultiPeerHeadHarness
              .Config("transient-demo", BackendMode.InMemory, TransportMode.Direct),
          multiNodeConfig = scenario.mnc,
          coilNodeConfigs = Nil,
          preinitPeerUtxosL1 = Map(HeadPeerNumber(0) -> scenario.preinitUtxos),
          takeoffTime = None,
          startEpochMs = 0L
        )

    private def hooksFor(
        sig: Signal[Unit]
    ): MultiPeerHeadHarness.Hooks[RequestSequencer.Handle, Unit] =
        MultiPeerHeadHarness.Hooks[RequestSequencer.Handle, Unit](
          tracer = sig.tracer,
          peerHandle = (_, conns) =>
              conns.requestSequencer.fold(
                IO.raiseError[RequestSequencer.Handle](
                  new RuntimeException("head peer has no RequestSequencer")
                )
              )(IO.pure),
          coilHandle = (_, _) => IO.unit
        )

    private def pollTxKnown(
        backend: L1Backend[IO],
        txId: TransactionHash,
        attempts: Int,
        sleep: FiniteDuration
    ): IO[Boolean] =
        backend.isTxKnown(txId).flatMap {
            case Right(true)        => IO.pure(true)
            case _ if attempts <= 1 => IO.pure(false)
            case _ => IO.sleep(sleep) >> pollTxKnown(backend, txId, attempts - 1, sleep)
        }

    /** Poll the recipient address on L1 until the withdrawn pot lands (or attempts run out). */
    private def pollWithdrawnPot(
        backend: L1Backend[IO],
        recipientAddress: ShelleyAddress,
        attempts: Int,
        sleep: FiniteDuration
    ): IO[Utxos] =
        backend.utxosAt(recipientAddress).flatMap {
            case Right(utxos) if utxos.nonEmpty => IO.pure(utxos)
            case _ if attempts <= 1             => IO.pure(Map.empty)
            case _ =>
                IO.sleep(sleep) >> pollWithdrawnPot(backend, recipientAddress, attempts - 1, sleep)
        }

    private def submitRequest(
        requestSequencer: RequestSequencer.Handle,
        headConfig: HeadConfig,
        userVk: VerificationKey,
        tx: Transaction,
        now: Instant
    ): IO[Unit] = {
        val body = TransactionRequestBody(ByteString.fromArray(tx.toCbor))
        val slotConfig: SlotConfig = headConfig.slotConfig
        val header = UserRequestHeader(
          headId = headConfig.headId,
          validityStart = RequestValidityStartTime(
            QuantizedInstant.ofEpochSeconds(slotConfig, now.minusSeconds(60).getEpochSecond)
          ),
          validityEnd = RequestValidityEndTime(
            QuantizedInstant.ofEpochSeconds(slotConfig, now.plusSeconds(3600).getEpochSecond)
          ),
          bodyHash = body.hash
        )
        val request = UserRequest.TransactionRequest(
          header,
          body.asInstanceOf[UserRequestBody.TransactionRequestBody],
          userVk
        )
        (requestSequencer ?: request).void
    }

    private def runDemo(scenario: Scenario): IO[Prop] = {
        val headConfig = scenario.mnc.headConfig
        for {
            sig <- stackZeroSignal
            _ <- log(
              s"booting a $nHeadPeers-peer head over a ${potAdaLovelace / 1_000_000} ADA pot; " +
                  s"$supply transient DEMO will be minted under policy ${scenario.policyId.toHex.take(16)}…"
            )

            result <- MultiPeerHeadHarness
                .resource(inputsFor(scenario), hooksFor(sig))
                .use { harness =>
                    val backend = harness.cardanoBackend
                    for {
                        _ <- sig.await.timeout(120.seconds)
                        _ <- log("head hard-confirmed stack 0; the ADA pot is live in L2")
                        initLanded <- pollTxKnown(backend, scenario.headInitTxId, 120, 500.millis)
                        _ <- log(s"init tx landed on L1: $initLanded")

                        requestSequencer = harness.peers(HeadPeerNumber(0)).handle
                        userVk = scenario.mnc
                            .nodeConfigs(HeadPeerNumber(0))
                            .ownWallet
                            .exportVerificationKey
                        now <- IO.realTimeInstant

                        // The doomed withdrawal first: it must be REJECTED (transient tokens
                        // cannot leave the head), leaving its input unspent for the burn.
                        _ <- log(
                          "submitting a withdrawal that carries the transient tokens — " +
                              "the head must reject it"
                        )
                        _ <- submitRequest(
                          requestSequencer,
                          headConfig,
                          userVk,
                          scenario.txs.rejectedWithdrawal,
                          now
                        )

                        _ <- scenario.txs.accepted.traverse_ { case (label, tx) =>
                            log(s"submitting L2 tx: $label") >>
                                submitRequest(requestSequencer, headConfig, userVk, tx, now)
                        }
                        _ <- log("all L2 txs submitted; settling the withdrawal on L1")

                        withdrawn <- pollWithdrawnPot(
                          backend,
                          scenario.recipientAddress,
                          900,
                          500.millis
                        )
                        errors <- harness.sutErrors.get
                        _ <- log(
                          s"utxos at the recipient address on L1: ${withdrawn.size}; " +
                              s"actor errors=${errors.size}"
                        )
                    } yield (initLanded, withdrawn, errors)
                }
            (initLanded, withdrawn, errors) = result
        } yield {
            val withdrawnValue = Value.combine(withdrawn.values.map(_.value))
            val potLanded = withdrawnValue.coin.value >= potAdaLovelace
            val noTokensOnL1 = withdrawn.values.forall(_.value.assets.isEmpty)
            List(
              "init tx did not land on L1" |: initLanded,
              s"the freed pot did not land at the recipient: $withdrawnValue" |: potLanded,
              // The burn preceding the withdrawal is what makes this possible at all: had the
              // rejected withdrawal been applied, the pot would have carried tokens to L1 (and
              // the burn's input would already be spent).
              "transient tokens leaked to L1" |: noTokensOnL1,
              s"head produced actor errors: ${errors.mkString("; ")}" |: errors.isEmpty
            ).foldLeft(Prop(true))(_ && _)
        }
    }

    val _ = property("transient tokens mint, circulate, and burn on L2; only ADA exits") =
        Prop.forAll(genScenario) { scenario =>
            runDemo(scenario).unsafeRunSync()
        }
}
