package hydrozoa.examples.oracle

import cats.data.{NonEmptyMap, ReaderT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import hydrozoa.bootstrap.InitializationFunding
import hydrozoa.config.head.InitParamsType.Constant
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.config.head.multisig.timing.generateDefaultTxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.config.node.operation.multisig.{NodeOperationMultisigConfig, RateLimits, generateNodeOperationMultisigConfig}
import hydrozoa.integration.harness.MultiPeerHeadHarness.StorageBackend.Mode as BackendMode
import hydrozoa.integration.harness.MultiPeerHeadHarness.Transport.Mode as TransportMode
import hydrozoa.integration.harness.{
  MultiPeerHeadHarness,
  Signal
}
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.cardano.scalus.ledger.withZeroFees
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.multisig.CommonChildEvent
import hydrozoa.multisig.backend.cardano.{CardanoBackend as L1Backend, CardanoBackendMock, MockState}
import hydrozoa.multisig.consensus.UserRequestBody.TransactionRequestBody
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.peer.HeadPeerNumber.given
import hydrozoa.multisig.consensus.{RequestSequencer, SlowConsensusActorEvent, UserRequest, UserRequestBody, UserRequestHeader}
import hydrozoa.multisig.ledger.eutxol2.toEvacuationKey
import hydrozoa.multisig.ledger.eutxol2.tx.L2Genesis
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import hydrozoa.multisig.ledger.stack.StackEffects
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.evacuationKeyToData
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
import scalus.cardano.ledger.{AssetName, AuxiliaryData, CertState, Coin, KeepRaw, Metadatum, PolicyId, ScriptHash, SlotConfig, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxo, Utxos, Value, Word64}
import scalus.cardano.onchain.plutus.prelude.Option as ScalusOption
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilder}
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.Builtins.{blake2b_224, blake2b_256}
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.FromData.given_FromData_BigInt
import scalus.uplc.builtin.{ByteString, Data}
import test.{SeedPhrase, TestPeers, genMonad}

/** Oracle demo: publishing an on-chain data feed through a Hydrozoa head.
  *
  * A data provider runs an oracle whose feed is the [Hofstadter
  * Q-sequence](https://oeis.org/A005185) — a self-contained, deterministic integer sequence
  * standing in for any real-world feed. Each successive term Q(n) is published to Cardano L1 as an
  * **oracle utxo**: a utxo at the head's native script address carrying (a) an authentication token
  * proving provenance and (b) the term itself in the `oraclePayload` field of the deposit datum.
  * Any Cardano contract can read the current value as a CIP-31 reference input.
  *
  * The provider's authentication tokens live in the head's L2. Each refresh is an L2 transaction
  * request that produces an L1-bound payout to the native script address, carrying one auth token
  * and the new term — the head's settlement machinery lands it on L1. So after the initial funding,
  * the whole feed is driven from L2, at head cost, without repeated external L1 transactions.
  *
  * (The whitepaper's single-token-recycling refresh relies on internal-deposit re-absorption, which
  * is future work; this demo spends one auth token of the provider's policy per publication.
  * Consumers authenticate on the token's *policy*, so this is equivalent from their side.)
  *
  * Bespoke real-clock run (like the airdrop / membership-change demos, NOT ModelBasedSuite). The
  * final assertion reads the mock L1 directly: every published term appears in an oracle utxo, each
  * bearing the provider's auth token, with the payloads decoding to exactly Q(1)..Q(nTerms).
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

    /** How many terms of the feed to publish (Q(1)..Q(nTerms)). */
    private val nTerms: Int = 25

    /** ADA carried by each oracle utxo (leaves L2 with the auth token); above min-ada for a
      * token+datum utxo.
      */
    private val oracleAdaLovelace: Long = 2_000_000L

    /** ADA seeded into the provider's L2 pot — covers [[oracleAdaLovelace]] for every publication
      * plus a change remainder above min-ada.
      */
    private val potAdaLovelace: Long = 100_000_000L

    /** Equity contributed per peer (covers the init tx fee). */
    private val equityPerPeerLovelace: Long = 20_000_000L

    /** The provider's authentication token: one token per publication is spent, all of one policy.
      * A fabricated policy id — the tokens are seeded straight into L2, never minted on the mock
      * L1.
      */
    private val authPolicyId: PolicyId =
        ScriptHash.fromByteString(
          blake2b_224(ByteString.fromArray("hydrozoa-oracle-auth-policy".getBytes))
        )
    private val authAssetName: AssetName =
        AssetName(ByteString.fromString("ORACLE"))

    /** The [Hofstadter Q-sequence](https://oeis.org/A005185): Q(1)=Q(2)=1,
      * Q(n)=Q(n-Q(n-1))+Q(n-Q(n-2)). The feed's data.
      */
    private val qSequence: Vector[Long] = {
        val q = Array.fill(nTerms + 1)(0L)
        if nTerms >= 1 then q(1) = 1L
        if nTerms >= 2 then q(2) = 1L
        for n <- 3 to nTerms do q(n) = q(n - q(n - 1).toInt) + q(n - q(n - 2).toInt)
        (1 to nTerms).map(q(_)).toVector
    }

    // ===================================
    // Scenario fixture
    // ===================================

    /** One built, signed publication: an L2 tx spending the provider's current pot, producing the
      * oracle utxo (auth token + payload Q(n), withdrawn to L1) and change (kept in L2).
      */
    private case class Publication(signedTx: Transaction, term: Long)

    private case class Scenario(
        mnc: MultiNodeConfig,
        headGenesisUtxos: Utxos,
        headInitTxId: TransactionHash,
        nativeScriptAddress: ShelleyAddress,
        publications: List[Publication]
    )

    // Narrow the rate limits (as stage4 does) so stack/block propagation isn't paced against the
    // production defaults while the demo runs on the real clock.
    private def genNodeOpMultisig(
        hc: hydrozoa.config.head.HeadConfig
    ): Gen[NodeOperationMultisigConfig] =
        generateNodeOperationMultisigConfig(
          maxPollingPeriod = hc.maxCardanoLiaisonPollingPeriod,
          rateLimits = RateLimits(softBlockMinPeriod = 5.seconds, hardStackMinPeriod = 2.seconds)
        )

    private def authTokensOf(value: Value): Long =
        value.assets.assets.get(authPolicyId).flatMap(_.get(authAssetName)).getOrElse(0L)

    /** Build the deterministic chain of signed publication txs off the initial pot utxo.
      * Publication `i+1` spends publication `i`'s change output (index 1); tx ids are stable under
      * signing, so the whole chain is known offline.
      */
    private def buildPublications(
        mnc: MultiNodeConfig,
        providerAddress: ShelleyAddress,
        nativeScriptAddress: ShelleyAddress,
        onchainRefund: DepositUtxo.Refund.Instructions.Onchain,
        potInput0: TransactionInput,
        potValue0: Value
    ): List[Publication] = {
        val network = mnc.headConfig.cardanoNetwork.cardanoInfo.network
        val protocolParams = mnc.headConfig.cardanoProtocolParams.withZeroFees
        val evaluator = mnc.headConfig.plutusScriptEvaluatorForTxBuild
        val signAsProvider = mnc.signTxAs(HeadPeerNumber(0))

        // metadata: output 0 (oracle utxo) = 1 (withdraw to L1); output 1 (change) = 2 (stay in L2)
        val withdrawMetadata: AuxiliaryData = Metadata(
          Map(
            Word64(CIP67.Tags.head) ->
                Metadatum.List(IndexedSeq(Metadatum.Int(1), Metadatum.Int(2)))
          )
        )

        def oracleDatum(term: Long): DepositUtxo.Datum =
            DepositUtxo.Datum(onchainRefund, ScalusOption.Some(Data.I(BigInt(term))))

        qSequence.zipWithIndex
            .foldLeft((List.empty[Publication], potInput0, potValue0)) {
                case ((acc, potInput, potValue), (term, _)) =>
                    val oracleValue =
                        Value(Coin(oracleAdaLovelace)) + Value.asset(
                          authPolicyId,
                          authAssetName,
                          1L
                        )
                    val changeValue = potValue - oracleValue

                    val oracleOutput: TransactionOutput = Babbage(
                      address = nativeScriptAddress,
                      value = oracleValue,
                      datumOption = Some(Inline(toData(oracleDatum(term)))),
                      scriptRef = None
                    )
                    val changeOutput: TransactionOutput = Babbage(providerAddress, changeValue)
                    val potOutput: TransactionOutput = Babbage(providerAddress, potValue)

                    val steps = List(
                      Spend(Utxo(potInput, potOutput), PubKeyWitness),
                      Send(oracleOutput),
                      Send(changeOutput),
                      Fee(Coin.zero),
                      ModifyAuxiliaryData(_ => Some(withdrawMetadata))
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
                          err =>
                              throw new RuntimeException(
                                s"publication (Q=$term) tx build failed: $err"
                              ),
                          ctx => ctx.transaction
                        )

                    val signed = signAsProvider(unsigned)
                    (Publication(signed, term) :: acc, TransactionInput(signed.id, 1), changeValue)
            }
            ._1
            .reverse
    }

    private val genScenario: Gen[Scenario] = {
        val testPeers = TestPeers(SeedPhrase.Yaci, CardanoNetwork.Preprod, nHeadPeers)
        val cardanoNetwork = testPeers.cardanoNetwork
        val providerAddress = testPeers.shelleyAddressFor(HeadPeerNumber(0))

        // The provider's whole stock of auth tokens (one per publication) plus ADA to fund each
        // oracle utxo, seeded straight into their L2 account.
        val potValue =
            Value(Coin(potAdaLovelace)) + Value.asset(authPolicyId, authAssetName, nTerms.toLong)
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

        for {
            headParams <- generateHeadParameters(generateTxTiming = generateDefaultTxTiming)
                .run(testPeers)

            fc = headParams.fallbackContingency
            totalContingency = Coin(
              fc.collectiveContingency.total.value + nHeadPeers * fc.individualContingency.total.value
            )

            fundingValueTarget = potValue + Value(totalEquity) + Value(totalContingency)
            funding = InitializationFunding(
              seedUtxo = Utxo(seedInput, Babbage(providerAddress, fundingValueTarget)),
              additionalFundingUtxos = Map.empty,
              changeOutputs = Nil
            )

            genesisId = L2Genesis.mkGenesisId(seedInput)
            potInput0 = TransactionInput(genesisId, 0)
            evacuationMap = EvacuationMap(
              TreeMap(
                potInput0.toEvacuationKey ->
                    Payout
                        .Obligation(KeepRaw(Babbage(providerAddress, potValue)), cardanoNetwork)
                        .toOption
                        .get
              )
            )
            params = InitializationParameters(
              initialEvacuationMap = evacuationMap,
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
        } yield {
            val slotConfig = mnc.headConfig.slotConfig
            // Vestigial refund instructions on the oracle datum (the utxo is a feed, not a
            // refundable deposit); the payload is what consumers read.
            val onchainRefund = DepositUtxo.Refund.Instructions.Onchain(
              DepositUtxo.Refund.Instructions(
                address = providerAddress,
                datum = None,
                validityStart = QuantizedInstant(
                  slotConfig = slotConfig,
                  instant = Instant.ofEpochMilli(slotConfig.zeroTime)
                )
              )
            )
            Scenario(
              mnc = mnc,
              headGenesisUtxos = mnc.initializationTx.resolvedUtxos.utxos,
              headInitTxId = mnc.initializationTx.tx.id,
              nativeScriptAddress = mnc.headConfig.headMultisigAddress,
              publications = buildPublications(
                mnc,
                providerAddress,
                mnc.headConfig.headMultisigAddress,
                onchainRefund,
                potInput0,
                potValue
              )
            )
        }
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

    /** Decode the oracle term from a utxo's datum, if it is an oracle utxo (a `DepositUtxo.Datum`
      * with an integer `oraclePayload`).
      */
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

    /** Read the current feed off L1: every oracle utxo (auth-token-bearing, at the native script
      * address) with its decoded term.
      */
    private def readFeed(
        backend: L1Backend[IO],
        nativeScriptAddress: ShelleyAddress
    ): IO[List[Long]] =
        backend.utxosAt(nativeScriptAddress, (authPolicyId, authAssetName)).map {
            case Right(utxos) =>
                utxos.values.toList
                    .flatMap(o => readOracleTerm(o).filter(_ => authTokensOf(o.value) >= 1))
            case Left(_) => Nil
        }

    /** Poll L1 until all `nTerms` terms have been published as oracle utxos. */
    private def pollFeed(
        backend: L1Backend[IO],
        nativeScriptAddress: ShelleyAddress,
        attempts: Int,
        sleep: FiniteDuration
    ): IO[List[Long]] =
        readFeed(backend, nativeScriptAddress).flatMap { terms =>
            if terms.sizeIs >= nTerms || attempts <= 1 then IO.pure(terms)
            else IO.sleep(sleep) >> pollFeed(backend, nativeScriptAddress, attempts - 1, sleep)
        }

    private def submitPublication(
        requestSequencer: RequestSequencer.Handle,
        headId: InitializationParameters.HeadId,
        slotConfig: SlotConfig,
        userVk: VerificationKey,
        publication: Publication,
        now: Instant
    ): IO[Unit] = {
        val body = TransactionRequestBody(ByteString.fromArray(publication.signedTx.toCbor))
        val header = UserRequestHeader(
          headId = headId,
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
            _ <- log(
              s"publishing the Hofstadter Q-sequence: ${nTerms} terms ${qSequence.take(12).mkString(", ")}, ..."
            )

            result <- MultiPeerHeadHarness
                .resource(inputsFor(scenario.mnc, backend), hooksFor(sig))
                .use { harness =>
                    for {
                        _ <- sig.await.timeout(120.seconds)
                        _ <- log("head hard-confirmed stack 0; oracle provider funded in L2")
                        initLanded <- pollTxKnown(backend, scenario.headInitTxId, 120, 500.millis)
                        _ <- log(s"init tx landed on L1: $initLanded")

                        requestSequencer = harness.peers(HeadPeerNumber(0)).handle
                        userVk = scenario.mnc
                            .nodeConfigs(HeadPeerNumber(0))
                            .ownWallet
                            .exportVerificationKey
                        now <- IO.realTimeInstant

                        _ <- scenario.publications.zipWithIndex.traverse_ { case (publication, i) =>
                            submitPublication(
                              requestSequencer,
                              cfg.headId,
                              cfg.slotConfig,
                              userVk,
                              publication,
                              now
                            ) >>
                                (if (i + 1) % 5 == 0 then
                                     log(s"submitted ${i + 1}/${nTerms} oracle refreshes")
                                 else IO.unit)
                        }
                        _ <- log(s"all ${nTerms} refreshes submitted; settling oracle utxos on L1")

                        feed <- pollFeed(backend, scenario.nativeScriptAddress, 900, 500.millis)
                        errors <- harness.sutErrors.get
                        _ <- log(
                          s"oracle utxos on L1: ${feed.size} / ${nTerms}; " +
                              s"terms=${feed.sorted.mkString(",")}; actor errors=${errors.size}"
                        )
                    } yield (initLanded, feed, errors)
                }
            (initLanded, feed, errors) = result
        } yield {
            val published = feed.sorted
            val expected = qSequence.toList.sorted
            List(
              "init tx did not land on L1" |: initLanded,
              s"not all terms published as oracle utxos: ${feed.size}/$nTerms" |: (feed.sizeIs == nTerms),
              s"published feed does not match Q(1)..Q($nTerms): got $published, want $expected" |:
                  (published == expected),
              s"head produced actor errors: ${errors.mkString("; ")}" |: errors.isEmpty
            ).foldLeft(Prop(true))(_ && _)
        }
    }

    val _ = property("oracle feed (Q-sequence) is published to L1 as referenceable utxos") =
        Prop.forAll(genScenario) { scenario =>
            runDemo(scenario).unsafeRunSync()
        }
}
