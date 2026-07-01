package hydrozoa.examples.oracle

import cats.data.{NonEmptyList, NonEmptyMap, ReaderT}
import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.bootstrap.InitializationFunding
import hydrozoa.config.head.InitParamsType.Constant
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.Durations.{DepositMaturityDuration, DepositSubmissionDuration}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.config.node.operation.multisig.{NodeOperationMultisigConfig, RateLimits, generateNodeOperationMultisigConfig}
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, quantize}
import hydrozoa.lib.cardano.scalus.ledger.withZeroFees
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.multisig.CommonChildEvent
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.UserRequestBody.DepositRequestBody
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.peer.HeadPeerNumber.given
import hydrozoa.multisig.consensus.{RequestSequencer, SlowConsensusActorEvent, UserRequest, UserRequestBody, UserRequestHeader}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.eutxol2.toEvacuationKey
import hydrozoa.multisig.ledger.eutxol2.tx.{GenesisObligation, L2Genesis}
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering, given}
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import hydrozoa.multisig.ledger.stack.StackEffects
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.integration.harness.MultiPeerHeadHarness.StorageBackend.Mode as BackendMode
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.harness.Signal
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.evacuationKeyToData
import io.bullet.borer.Cbor
import java.time.Instant
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.immutable.{SortedMap, TreeMap}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context as LedgerContext, UtxoEnv}
import scalus.cardano.ledger.{AssetName, AuxiliaryData, CertState, Coin, Hash32, Metadatum, PolicyId, ScriptHash, SlotConfig, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxo, Utxos, Value, Word64}
import scalus.cardano.onchain.plutus.prelude.Option as ScalusOption
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilder}
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.Builtins.{blake2b_224, blake2b_256}
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.FromData.given_FromData_BigInt
import scalus.uplc.builtin.{ByteString, Data}
import test.{GenWithTestPeers, SeedPhrase, TestPeers, genMonad}

/** Oracle demo: a live, refreshing on-chain data feed through a Hydrozoa head.
  *
  * A data provider runs an oracle whose feed is the [Hofstadter
  * Q-sequence](https://oeis.org/A005185) — `Q(1)=Q(2)=1, Q(n)=Q(n-Q(n-1))+Q(n-Q(n-2))` — a
  * self-contained, deterministic stand-in for any real-world feed. Each term is published as an
  * **oracle utxo** at the head's native script address, carrying an authentication token + the term
  * in the deposit datum's `oraclePayload`. Any Cardano contract could reference it (CIP-31) to read
  * the current value.
  *
  * This demo is faithful to the whitepaper's refresh cycle, exercising the full deposit lifecycle:
  *   - Term 1 is published as an **external deposit** — the provider brings the auth token into the
  *     head. The deposit sits on L1 during its (short, ~10s) maturity window — a guaranteed
  *     **stability window** during which it is referenceable — then the head absorbs it.
  *   - Terms 2..N are published as **internal deposits**: the provider submits an L2 payout to the
  *     native script address, then re-registers that settled utxo as a deposit (via the internal
  *     deposit backdoor). One auth token cycles L1 -> L2 -> L1 forever; only ~one oracle utxo is
  *     live at a time.
  *
  * The maturity duration is shortened to ~10s (default is 1 hour) so the stability windows are
  * observable in a couple of minutes. For each term the demo asserts it was seen **present** on L1
  * (in its window) and then **absorbed**, in Q-sequence order, with no consensus errors.
  */
object OracleDemo extends Properties("Oracle demo") {

    // One live run is enough — it is a real, slow integration scenario.
    override def overrideParameters(p: Parameters): Parameters =
        p.withMinSuccessfulTests(1)

    private val log: String => IO[Unit] = msg => IO(println(s"[oracle-demo] $msg"))

    // ===================================
    // Oracle parameters
    // ===================================

    private val nHeadPeers = 2

    /** How many terms of the feed to publish (Q(1)..Q(nTerms)). Kept small: each term costs a ~10s
      * maturity window plus settlement rounds.
      */
    private val nTerms: Int = 6

    /** ADA carried by the single cycling oracle utxo. */
    private val oracleAdaLovelace: Long = 2_000_000L

    /** ADA in the provider's L1 funding utxo for the term-1 external deposit (covers the deposit
      * output + change + tx fee).
      */
    private val fundingAdaLovelace: Long = 10_000_000L

    /** Equity contributed per peer (covers the init tx fee). */
    private val equityPerPeerLovelace: Long = 20_000_000L

    /** The provider's single authentication token, seeded on L1 and cycled through the head. */
    private val authPolicyId: PolicyId =
        ScriptHash.fromByteString(
          blake2b_224(ByteString.fromArray("hydrozoa-oracle-auth-policy".getBytes))
        )
    private val authAssetName: AssetName =
        AssetName(ByteString.fromString("ORACLE"))

    /** The [Hofstadter Q-sequence](https://oeis.org/A005185). */
    private val qSequence: Vector[Long] = {
        val q = Array.fill(nTerms + 1)(0L)
        if nTerms >= 1 then q(1) = 1L
        if nTerms >= 2 then q(2) = 1L
        for n <- 3 to nTerms do q(n) = q(n - q(n - 1).toInt) + q(n - q(n - 2).toInt)
        (1 to nTerms).map(q(_)).toVector
    }

    // Poll budgets (real clock): ~2 minutes each at 500ms.
    private val pollAttempts = 240
    private val pollSleep: FiniteDuration = 500.millis

    // ===================================
    // Scenario fixture
    // ===================================

    private case class Scenario(
        mnc: MultiNodeConfig,
        headGenesisUtxos: Utxos,
        headInitTxId: TransactionHash,
        nativeScriptAddress: ShelleyAddress,
        providerAddress: ShelleyAddress,
        providerFundingUtxo: Utxo
    )

    // Narrow the rate limits so blocks/stacks flow quickly on the real clock.
    private def genNodeOpMultisig(hc: HeadConfig): Gen[NodeOperationMultisigConfig] =
        generateNodeOperationMultisigConfig(
          maxPollingPeriod = hc.maxCardanoLiaisonPollingPeriod,
          rateLimits = RateLimits(softBlockMinPeriod = 5.seconds, hardStackMinPeriod = 2.seconds)
        )

    /** Tx timing with a ~10s deposit maturity (default is 1 hour) so stability windows are short
      * enough to watch. Polling is auto-clamped to `maturity / safetyFactor` via
      * `hc.maxCardanoLiaisonPollingPeriod`, so no other timing needs to change.
      */
    private def genShortMaturityTxTiming: GenWithTestPeers[TxTiming] =
        ReaderT(network =>
            Gen.const {
                val sc = network.slotConfig
                TxTiming
                    .default(sc)
                    .copy(
                      depositSubmissionDuration = DepositSubmissionDuration(5.seconds.quantize(sc)),
                      depositMaturityDuration = DepositMaturityDuration(10.seconds.quantize(sc))
                    )
            }
        )

    private def authTokensOf(value: Value): Long =
        value.assets.assets.get(authPolicyId).flatMap(_.get(authAssetName)).getOrElse(0L)

    /** The value of one oracle utxo / one cycle of the token: the auth token + its carried ADA. */
    private val cycleValue: Value =
        Value(Coin(oracleAdaLovelace)) + Value.asset(authPolicyId, authAssetName, 1L)

    /** The genesis obligation minting the token + ADA back to the provider's L2 account on
      * absorption — this is what keeps the token cycling.
      */
    private def providerL2Payload(providerAddress: ShelleyAddress): ByteString = {
        val obligation = GenesisObligation(
          l2OutputPaymentAddress = providerAddress.payment,
          l2OutputNetwork = providerAddress.network,
          l2OutputDatum = ScalusOption.None,
          l2OutputValue = cycleValue,
          l2OutputRefScript = None
        )
        GenesisObligation.serialize(NonEmptyList.of(obligation))
    }

    /** The oracle datum for term `q`: a deposit datum whose `oraclePayload` carries the value. */
    private def oracleDatum(
        providerAddress: ShelleyAddress,
        slotConfig: SlotConfig,
        q: Long
    ): DepositUtxo.Datum = {
        val refundInstructions = DepositUtxo.Refund.Instructions.Onchain(
          DepositUtxo.Refund.Instructions(
            address = providerAddress,
            datum = None,
            validityStart = QuantizedInstant(
              slotConfig = slotConfig,
              instant = Instant.ofEpochMilli(slotConfig.zeroTime)
            )
          )
        )
        DepositUtxo.Datum(refundInstructions, ScalusOption.Some(Data.I(BigInt(q))))
    }

    private val genScenario: Gen[Scenario] = {
        val testPeers = TestPeers(SeedPhrase.Yaci, CardanoNetwork.Preprod, nHeadPeers)
        val providerAddress = testPeers.shelleyAddressFor(HeadPeerNumber(0))

        // The head boots with an EMPTY L2 ledger; the token enters via the term-1 external deposit.
        val equity: NonEmptyMap[HeadPeerNumber, Coin] =
            NonEmptyMap.fromMapUnsafe(
              SortedMap(
                HeadPeerNumber(0) -> Coin(equityPerPeerLovelace),
                HeadPeerNumber(1) -> Coin(equityPerPeerLovelace)
              )
            )
        val totalEquity = Coin(equityPerPeerLovelace * nHeadPeers)

        val seedInput = TransactionInput(
          TransactionHash.fromByteString(
            blake2b_256(ByteString.fromArray("hydrozoa-oracle-seed".getBytes))
          ),
          0
        )

        // The provider's own L1 funding utxo (auth token + ADA) for the term-1 deposit — separate
        // from the head's funding, seeded straight into the mock L1.
        val providerFundingUtxo: Utxo = Utxo(
          TransactionInput(
            TransactionHash.fromByteString(
              blake2b_256(ByteString.fromArray("hydrozoa-oracle-provider-funding".getBytes))
            ),
            0
          ),
          Babbage(
            providerAddress,
            Value(Coin(fundingAdaLovelace)) + Value.asset(authPolicyId, authAssetName, 1L)
          )
        )

        for {
            headParams <- generateHeadParameters(generateTxTiming = genShortMaturityTxTiming)
                .run(testPeers)

            fc = headParams.fallbackContingency
            totalContingency = Coin(
              fc.collectiveContingency.total.value + nHeadPeers * fc.individualContingency.total.value
            )

            // initialL2Value is zero (empty evacuation map), so funding = equity + contingency.
            funding = InitializationFunding(
              seedUtxo = Utxo(
                seedInput,
                Babbage(providerAddress, Value(totalEquity) + Value(totalContingency))
              ),
              additionalFundingUtxos = Map.empty,
              changeOutputs = Nil
            )

            params = InitializationParameters(
              initialEvacuationMap = EvacuationMap(TreeMap.empty),
              initialEquityContributions = equity,
              headId = funding.headId
            )

            bootstrapAndFunding <- generateHeadConfigBootstrap(
              generateHeadParams = ReaderT.pure(headParams)(using genMonad),
              generateInitializationParameters = Constant(params, funding)
            ).run(testPeers)

            mnc <- MultiNodeConfig.generateWith(testPeers)(
              generateHeadConfig = generateHeadConfig(
                genHeadConfigBootstrap = ReaderT.pure(bootstrapAndFunding)(using genMonad)
              ),
              generateNodeOperationMultisigConfig = genNodeOpMultisig
            )
        } yield Scenario(
          mnc = mnc,
          headGenesisUtxos = mnc.initializationTx.resolvedUtxos.utxos + providerFundingUtxo.toTuple,
          headInitTxId = mnc.initializationTx.tx.id,
          nativeScriptAddress = mnc.headConfig.headMultisigAddress,
          providerAddress = providerAddress,
          providerFundingUtxo = providerFundingUtxo
        )
    }

    // ===================================
    // L1 observation helpers
    // ===================================

    /** Decode the oracle term from a utxo's datum, if it is an oracle utxo. */
    private def readOracleTerm(output: TransactionOutput): Option[Long] =
        output match {
            case b: Babbage =>
                b.datumOption match {
                    case Some(Inline(data)) =>
                        Try(fromData[DepositUtxo.Datum](data)).toOption.flatMap { datum =>
                            datum.oraclePayload match {
                                case ScalusOption.Some(d) =>
                                    Try(fromData[BigInt](d).toLong).toOption
                                case _ => None
                            }
                        }
                    case _ => None
                }
            case _ => None
        }

    /** The live oracle utxos on L1: those at the native script address carrying the auth token
      * whose datum decodes to an oracle payload. (Excludes the treasury, which holds the token
      * between cycles but has a treasury datum.)
      */
    private def oracleUtxos(
        backend: L1Backend[IO],
        nativeScriptAddress: ShelleyAddress
    ): IO[List[(TransactionInput, TransactionOutput, Long)]] =
        backend.utxosAt(nativeScriptAddress, (authPolicyId, authAssetName)).map {
            case Right(utxos) =>
                utxos.toList.flatMap { case (input, output) =>
                    readOracleTerm(output)
                        .filter(_ => authTokensOf(output.value) >= 1)
                        .map(t => (input, output, t))
                }
            case Left(_) => Nil
        }

    /** Poll until an oracle utxo carrying `term` is present on L1; return its (input, output). */
    private def waitForTermPresent(
        backend: L1Backend[IO],
        nativeScriptAddress: ShelleyAddress,
        term: Long,
        attempts: Int
    ): IO[Option[(TransactionInput, TransactionOutput)]] =
        oracleUtxos(backend, nativeScriptAddress).flatMap { utxos =>
            utxos.find(_._3 == term).map(u => (u._1, u._2)) match {
                case some @ Some(_)        => IO.pure(some)
                case None if attempts <= 1 => IO.pure(None)
                case None =>
                    IO.sleep(pollSleep) >> waitForTermPresent(
                      backend,
                      nativeScriptAddress,
                      term,
                      attempts - 1
                    )
            }
        }

    /** Poll until `input` is no longer on L1 (its deposit was absorbed by a settlement). */
    private def waitForAbsorbed(
        backend: L1Backend[IO],
        input: TransactionInput,
        attempts: Int
    ): IO[Boolean] =
        backend.resolve(input).flatMap {
            case Right(None)        => IO.pure(true)
            case _ if attempts <= 1 => IO.pure(false)
            case _ => IO.sleep(pollSleep) >> waitForAbsorbed(backend, input, attempts - 1)
        }

    private def pollTxKnown(
        backend: L1Backend[IO],
        txId: TransactionHash,
        attempts: Int
    ): IO[Boolean] =
        backend.isTxKnown(txId).flatMap {
            case Right(true)        => IO.pure(true)
            case _ if attempts <= 1 => IO.pure(false)
            case _ => IO.sleep(pollSleep) >> pollTxKnown(backend, txId, attempts - 1)
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

    private def inputsFor(
        mnc: MultiNodeConfig,
        backend: L1Backend[IO]
    ): MultiPeerHeadHarness.Inputs =
        MultiPeerHeadHarness.Inputs(
          config =
              MultiPeerHeadHarness.Config("oracle", BackendMode.InMemory, TransportMode.Direct),
          multiNodeConfig = mnc,
          coilNodeConfigs = Nil,
          preinitPeerUtxosL1 = Map.empty,
          takeoffTime = None,
          startEpochMs = 0L,
          sharedCardanoBackend = Some(backend)
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

    private def mkHeader(
        headId: InitializationParameters.HeadId,
        slotConfig: SlotConfig,
        now: Instant,
        validityEnd: RequestValidityEndTime,
        bodyHash: Hash32
    ): UserRequestHeader =
        UserRequestHeader(
          headId = headId,
          validityStart = RequestValidityStartTime(
            QuantizedInstant.ofEpochSeconds(slotConfig, now.minusSeconds(60).getEpochSecond)
          ),
          validityEnd = validityEnd,
          bodyHash = bodyHash
        )

    // Deposit absorption starts at `requestValidityEnd + depositSubmissionDuration (5s) +
    // depositMaturityDuration (10s)`, so keep the validity end close to `now` (just enough margin to
    // weave the request into a block) — otherwise the ~15s maturity is pushed far into the future.
    private def validityEndFromNow(slotConfig: SlotConfig, now: Instant): RequestValidityEndTime =
        RequestValidityEndTime(
          QuantizedInstant.ofEpochSeconds(slotConfig, now.plusSeconds(25).getEpochSecond)
        )

    /** Publish term 1 as an EXTERNAL deposit: build + sign the deposit tx, register it, submit it
      * to L1, watch its stability window, and watch it get absorbed. Returns the L2 utxo id the
      * token lands at after absorption (the input for the next payout).
      */
    private def publishExternal(
        backend: L1Backend[IO],
        scenario: Scenario,
        requestSequencer: RequestSequencer.Handle,
        userVk: VerificationKey,
        observed: Ref[IO, Vector[Long]],
        position: Int,
        term: Long
    ): IO[TransactionInput] = {
        val config = scenario.mnc.headConfig
        val slotConfig = config.slotConfig
        val tag = s"Q($position)=$term (external)"
        for {
            now <- IO.realTimeInstant
            validityEnd = validityEndFromNow(slotConfig, now)
            l2Payload = providerL2Payload(scenario.providerAddress)
            depositRefundSeq = DepositRefundTxSeq
                .Build(
                  l2Payload = l2Payload,
                  l2Value = cycleValue,
                  depositFee = Coin.zero,
                  utxosFunding = NonEmptyList.of(scenario.providerFundingUtxo),
                  changeAddress = scenario.providerAddress,
                  requestValidityEndTime = validityEnd,
                  refundAddress = scenario.providerAddress,
                  refundDatum = None,
                  requestId = RequestId(0, 0L),
                  oraclePayload = ScalusOption.Some(Data.I(BigInt(term)))
                )(using config)
                .result
                .fold(err => throw new RuntimeException(s"deposit build failed: $err"), identity)
            signedDepositTx = scenario.mnc.signTxAs(HeadPeerNumber(0))(
              depositRefundSeq.depositTx.tx
            )
            depositInput = depositRefundSeq.depositTx.depositProduced.utxoId
            body = DepositRequestBody(
              l1Payload = ByteString.fromArray(signedDepositTx.toCbor),
              l2Payload = l2Payload
            )
            header = mkHeader(config.headId, slotConfig, now, validityEnd, body.hash)
            request = UserRequest.DepositRequest(
              header,
              body.asInstanceOf[UserRequestBody.DepositRequestBody],
              userVk
            )
            _ <- (requestSequencer ?: request).void
            _ <- backend.submitTx(signedDepositTx)
            _ <- log(s"$tag: external deposit submitted; awaiting its stability window on L1")
            present <- waitForTermPresent(backend, scenario.nativeScriptAddress, term, pollAttempts)
            _ <- IO.raiseWhen(present.isEmpty)(
              new RuntimeException(s"$tag never became present on L1")
            )
            _ <- observed.update(_ :+ term)
            _ <- log(s"$tag: present on L1 (stability window open, referenceable)")
            absorbed <- waitForAbsorbed(backend, depositInput, pollAttempts)
            _ <- IO.raiseWhen(!absorbed)(
              new RuntimeException(s"$tag was never absorbed")
            )
            _ <- log(s"$tag: absorbed by settlement (window closed)")
        } yield TransactionInput(L2Genesis.mkGenesisId(depositInput), 0)
    }

    /** Publish an INTERNAL deposit (terms 2..N): submit an L2 payout spending the recovered token
      * utxo to the native script address, watch it settle onto L1, register it as an internal
      * deposit (the backdoor), watch its window, and watch it get absorbed. Returns the next
      * recovered L2 utxo id.
      */
    private def publishInternal(
        backend: L1Backend[IO],
        scenario: Scenario,
        requestSequencer: RequestSequencer.Handle,
        userVk: VerificationKey,
        observed: Ref[IO, Vector[Long]],
        recoveredL2: TransactionInput,
        position: Int,
        term: Long
    ): IO[TransactionInput] = {
        val config = scenario.mnc.headConfig
        val slotConfig = config.slotConfig
        val network = config.cardanoNetwork.cardanoInfo.network
        val tag = s"Q($position)=$term (internal)"

        val oracleOutput: TransactionOutput = Babbage(
          address = scenario.nativeScriptAddress,
          value = cycleValue,
          datumOption =
              Some(Inline(toData(oracleDatum(scenario.providerAddress, slotConfig, term)))),
          scriptRef = None
        )
        val withdrawMetadata: AuxiliaryData = Metadata(
          Map(Word64(CIP67.Tags.head) -> Metadatum.List(IndexedSeq(Metadatum.Int(1))))
        )
        val unsigned = TransactionBuilder
            .build(
              network,
              List(
                Spend(
                  Utxo(recoveredL2, Babbage(scenario.providerAddress, cycleValue)),
                  PubKeyWitness
                ),
                Send(oracleOutput),
                Fee(Coin.zero),
                ModifyAuxiliaryData(_ => Some(withdrawMetadata))
              )
            )
            .flatMap(
              _.finalizeContext(
                protocolParams = config.cardanoProtocolParams.withZeroFees,
                diffHandler = prebalancedLovelaceDiffHandler,
                evaluator = config.plutusScriptEvaluatorForTxBuild,
                validators = Seq.empty
              )
            )
            .fold(
              err => throw new RuntimeException(s"payout $term build failed: $err"),
              _.transaction
            )
        val signedPayout = scenario.mnc.signTxAs(HeadPeerNumber(0))(unsigned)

        for {
            now <- IO.realTimeInstant
            validityEnd = validityEndFromNow(slotConfig, now)
            txBody = UserRequestBody.TransactionRequestBody(
              ByteString.fromArray(signedPayout.toCbor)
            )
            txRequest = UserRequest.TransactionRequest(
              mkHeader(config.headId, slotConfig, now, validityEnd, txBody.hash),
              txBody.asInstanceOf[UserRequestBody.TransactionRequestBody],
              userVk
            )
            _ <- (requestSequencer ?: txRequest).void
            _ <- log(s"$tag: refresh payout submitted; awaiting settlement onto L1")
            present <- waitForTermPresent(backend, scenario.nativeScriptAddress, term, pollAttempts)
            inputOutput <- present.fold(
              IO.raiseError[(TransactionInput, TransactionOutput)](
                new RuntimeException(s"$tag payout never landed on L1")
              )
            )(IO.pure)
            (onL1Input, output) = inputOutput
            _ <- observed.update(_ :+ term)
            _ <- log(s"$tag: present on L1 (stability window open, referenceable)")
            idBody = UserRequestBody.InternalDepositRequestBody(
              depositInput = ByteString.fromArray(Cbor.encode(onL1Input).toByteArray),
              depositOutput = ByteString.fromArray(Cbor.encode(output).toByteArray),
              l2Payload = providerL2Payload(scenario.providerAddress)
            )
            now2 <- IO.realTimeInstant
            validityEnd2 = validityEndFromNow(slotConfig, now2)
            idRequest = UserRequest.InternalDepositRequest(
              mkHeader(config.headId, slotConfig, now2, validityEnd2, idBody.hash),
              idBody.asInstanceOf[UserRequestBody.InternalDepositRequestBody],
              userVk
            )
            _ <- (requestSequencer ?: idRequest).void
            _ <- log(s"$tag: registered as internal deposit; awaiting absorption")
            absorbed <- waitForAbsorbed(backend, onL1Input, pollAttempts)
            _ <- IO.raiseWhen(!absorbed)(
              new RuntimeException(s"$tag was never absorbed")
            )
            _ <- log(s"$tag: absorbed by settlement (window closed)")
        } yield TransactionInput(L2Genesis.mkGenesisId(onL1Input), 0)
    }

    private def runDemo(scenario: Scenario): IO[Prop] = {
        val cfg = scenario.mnc.headConfig
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
              initialState = MockState(scenario.headGenesisUtxos),
              mkContext = mkContext
            )
            sig <- stackZeroSignal
            _ <- log(s"oracle feed = Hofstadter Q-sequence: ${qSequence.mkString(", ")}")

            result <- MultiPeerHeadHarness
                .resource(inputsFor(scenario.mnc, backend), hooksFor(sig))
                .use { harness =>
                    for {
                        _ <- sig.await.timeout(120.seconds)
                        _ <- log("head hard-confirmed stack 0; L2 empty, provider funded on L1")
                        initLanded <- pollTxKnown(backend, scenario.headInitTxId, pollAttempts)
                        _ <- log(s"init tx landed on L1: $initLanded")

                        requestSequencer = harness.peers(HeadPeerNumber(0)).handle
                        userVk = scenario.mnc
                            .nodeConfigs(HeadPeerNumber(0))
                            .ownWallet
                            .exportVerificationKey
                        observed <- Ref[IO].of(Vector.empty[Long])

                        rec1 <- publishExternal(
                          backend,
                          scenario,
                          requestSequencer,
                          userVk,
                          observed,
                          1,
                          qSequence(0)
                        )
                        _ <- (1 until nTerms).toList.foldLeft(IO.pure(rec1)) { (accIO, i) =>
                            accIO.flatMap(rec =>
                                publishInternal(
                                  backend,
                                  scenario,
                                  requestSequencer,
                                  userVk,
                                  observed,
                                  rec,
                                  i + 1,
                                  qSequence(i)
                                )
                            )
                        }
                        observedTerms <- observed.get
                        errors <- harness.sutErrors.get
                        _ <- log(
                          s"published all ${nTerms} terms through external+internal deposits; " +
                              s"observed feed=${observedTerms.mkString(",")}; actor errors=${errors.size}"
                        )
                    } yield (initLanded, observedTerms, errors)
                }
            (initLanded, observedTerms, errors) = result
        } yield {
            List(
              "init tx did not land on L1" |: initLanded,
              s"observed feed ${observedTerms.mkString(",")} != Q-sequence ${qSequence.mkString(",")}" |:
                  (observedTerms == qSequence),
              s"head produced actor errors: ${errors.mkString("; ")}" |: errors.isEmpty
            ).foldLeft(Prop(true))(_ && _)
        }
    }

    val _ = property("oracle feed (Q-sequence) cycles through the head as internal deposits") =
        Prop.forAll(genScenario) { scenario =>
            runDemo(scenario).unsafeRunSync()
        }
}
