package hydrozoa.examples.airdrop

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
import hydrozoa.multisig.consensus.{
  RequestSequencer,
  SlowConsensusActorEvent,
  UserRequest,
  UserRequestBody,
  UserRequestHeader
}
import hydrozoa.multisig.ledger.eutxol2.toEvacuationKey
import hydrozoa.multisig.ledger.eutxol2.tx.L2Genesis
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
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context as LedgerContext, UtxoEnv}
import scalus.cardano.ledger.{
  AddrKeyHash,
  AssetName,
  AuxiliaryData,
  CertState,
  Coin,
  KeepRaw,
  Metadatum,
  PolicyId,
  ScriptHash,
  SlotConfig,
  Transaction,
  TransactionHash,
  TransactionInput,
  TransactionOutput,
  Utxo,
  Utxos,
  Value,
  Word64
}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilder}
import scalus.crypto.ed25519.VerificationKey
import scalus.uplc.builtin.Builtins.{blake2b_224, blake2b_256}
import scalus.uplc.builtin.ByteString
import test.{SeedPhrase, TestPeers, genMonad}

/** Air-drop demo: streamlining L1 token distribution through a Hydrozoa head.
  *
  * A publisher mints the entire token volume up front (here: seeded straight into the head's
  * initial L2 state as one pot utxo of [[totalSupply]] tokens) and boots a live multi-peer head
  * over it. The "claiming" then happens entirely on L2: a chain of cheap L2 transactions each carve
  * a small amount (≤ [[maxClaim]]) off the pot to a fresh recipient, marking that output to be
  * withdrawn to L1. Many claims produce many withdrawals — but the publisher pays for a single init
  * tx on L1 instead of one L1 transfer per recipient. If real claim logic lived in a Plutus
  * validator, all of that script execution would run on L2 too, at head cost rather than L1 cost.
  *
  * Bespoke real-clock run (like the membership-change demo, NOT ModelBasedSuite): the head boots
  * immediately on the wall clock and the mock L1 derives its slot from wall time, so the default tx
  * timing gives a wide validity window. Rate limits are narrowed so blocks/stacks flow quickly.
  *
  * The final assertion reads the mock L1 directly: every airdrop token that started in the treasury
  * must end up withdrawn to a recipient address (full drain, conservation of supply).
  */
object AirdropDemo extends Properties("Airdrop demo") {

    // One live run is enough — it is a real, slow integration scenario.
    override def overrideParameters(p: Parameters): Parameters =
        p.withMinSuccessfulTests(1)

    private val log: String => IO[Unit] = msg => IO(println(s"[airdrop-demo] $msg"))

    // ===================================
    // Airdrop parameters
    // ===================================

    private val nHeadPeers = 2

    /** The whole token volume the publisher mints up front. */
    private val totalSupply: Long = 10_000L

    /** Max tokens a single claim can take (each claim draws [minClaim, maxClaim]). */
    private val minClaim: Long = 40L
    private val maxClaim: Long = 50L

    /** ADA carried by each claim output (leaves L2 on withdrawal); above min-ada for a 1-asset
      * utxo.
      */
    private val claimAdaLovelace: Long = 2_000_000L

    /** ADA seeded into the pot utxo — must cover [[claimAdaLovelace]] for every claim plus a change
      * remainder above min-ada. Sized generously for the worst-case claim count.
      */
    private val potAdaLovelace: Long = 700_000_000L

    /** Equity contributed per peer (covers the init tx fee). */
    private val equityPerPeerLovelace: Long = 20_000_000L

    /** The airdrop token's policy + name. A fabricated policy id — the tokens are seeded straight
      * into L2, never minted on the mock L1, so no real minting script is needed.
      */
    private val airdropPolicyId: PolicyId =
        ScriptHash.fromByteString(
          blake2b_224(ByteString.fromArray("hydrozoa-airdrop-policy".getBytes))
        )
    private val airdropAssetName: AssetName =
        AssetName(ByteString.fromString("AIRDROP"))

    // ===================================
    // Scenario fixture
    // ===================================

    /** One built, signed claim: an L2 tx spending the current pot, paying `amount` tokens to
      * `recipient` (marked for withdrawal) and the remainder back to the publisher (kept in L2).
      */
    private case class BuiltClaim(
        signedTx: Transaction,
        recipient: ShelleyAddress,
        amount: Long
    )

    private case class Scenario(
        mnc: MultiNodeConfig,
        headGenesisUtxos: Utxos,
        headInitTxId: TransactionHash,
        treasuryAddress: ShelleyAddress,
        claims: List[BuiltClaim]
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

    /** Draw random claim amounts (each in [minClaim, maxClaim], the last capped) until the whole
      * supply is allocated. Sum is exactly [[totalSupply]].
      */
    private val genClaimAmounts: Gen[List[Long]] =
        Gen.tailRecM((List.empty[Long], totalSupply)) { case (acc, remaining) =>
            if remaining <= 0 then Gen.const(Right(acc.reverse))
            else
                Gen.choose(minClaim, maxClaim).map { drawn =>
                    val amount = math.min(drawn, remaining)
                    Left((amount :: acc, remaining - amount))
                }
        }

    /** A fresh, distinct L1 recipient address per claim (a bare pubkey address — withdrawal outputs
      * are just paid to it, so no signing key is needed).
      */
    private def recipientAddress(network: Network, i: Int): ShelleyAddress =
        ShelleyAddress(
          network = network,
          payment = ShelleyPaymentPart.Key(
            AddrKeyHash.fromByteString(
              blake2b_224(ByteString.fromArray(s"airdrop-recipient-$i".getBytes))
            )
          ),
          delegation = ShelleyDelegationPart.Null
        )

    private def tokensOf(value: Value): Long =
        value.assets.assets.get(airdropPolicyId).flatMap(_.get(airdropAssetName)).getOrElse(0L)

    /** Build the deterministic chain of signed claim txs off the initial pot utxo. Claim `i+1`
      * spends claim `i`'s change output (index 1); tx ids are stable under signing, so the whole
      * chain is known offline.
      */
    private def buildClaims(
        mnc: MultiNodeConfig,
        network: Network,
        publisherAddress: ShelleyAddress,
        potInput0: TransactionInput,
        potValue0: Value,
        amounts: List[Long]
    ): List[BuiltClaim] = {
        val protocolParams = mnc.headConfig.cardanoProtocolParams.withZeroFees
        val evaluator = mnc.headConfig.plutusScriptEvaluatorForTxBuild
        val signAsPublisher = mnc.signTxAs(HeadPeerNumber(0))

        // metadata: output 0 (claim) = 1 (withdraw to L1); output 1 (change) = 2 (stay in L2)
        val withdrawMetadata: AuxiliaryData = Metadata(
          Map(
            Word64(CIP67.Tags.head) ->
                Metadatum.List(IndexedSeq(Metadatum.Int(1), Metadatum.Int(2)))
          )
        )

        amounts.zipWithIndex
            .foldLeft((List.empty[BuiltClaim], potInput0, potValue0)) {
                case ((acc, potInput, potValue), (amount, i)) =>
                    val claimValue =
                        Value(Coin(claimAdaLovelace)) + Value.asset(
                          airdropPolicyId,
                          airdropAssetName,
                          amount
                        )
                    val changeValue = potValue - claimValue

                    val claimOutput: TransactionOutput =
                        Babbage(recipientAddress(network, i), claimValue)
                    val changeOutput: TransactionOutput = Babbage(publisherAddress, changeValue)

                    val potOutput: TransactionOutput = Babbage(publisherAddress, potValue)

                    val steps = List(
                      Spend(Utxo(potInput, potOutput), PubKeyWitness),
                      Send(claimOutput),
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
                          err => throw new RuntimeException(s"claim $i tx build failed: $err"),
                          ctx => ctx.transaction
                        )

                    val signed = signAsPublisher(unsigned)
                    val claim =
                        BuiltClaim(signed, claimOutput.address.asInstanceOf[ShelleyAddress], amount)
                    (claim :: acc, TransactionInput(signed.id, 1), changeValue)
            }
            ._1
            .reverse
    }

    private val genScenario: Gen[Scenario] = {
        val testPeers = TestPeers(SeedPhrase.Yaci, CardanoNetwork.Preprod, nHeadPeers)
        val cardanoNetwork = testPeers.cardanoNetwork
        val network = cardanoNetwork.cardanoInfo.network
        val publisherAddress = testPeers.shelleyAddressFor(HeadPeerNumber(0))

        val potValue =
            Value(Coin(potAdaLovelace)) + Value.asset(
              airdropPolicyId,
              airdropAssetName,
              totalSupply
            )
        val equity: NonEmptyMap[HeadPeerNumber, Coin] =
            NonEmptyMap.fromMapUnsafe(
              SortedMap(
                HeadPeerNumber(0) -> Coin(equityPerPeerLovelace),
                HeadPeerNumber(1) -> Coin(equityPerPeerLovelace)
              )
            )
        val totalEquity = Coin(equityPerPeerLovelace * nHeadPeers)

        // A fixed seed funding utxo on L1 that the init tx spends; its input fixes the head id.
        val seedInput = TransactionInput(
          TransactionHash.fromByteString(
            blake2b_256(ByteString.fromArray("hydrozoa-airdrop-seed".getBytes))
          ),
          0
        )

        for {
            headParams <- generateHeadParameters(generateTxTiming = generateDefaultTxTiming)
                .run(testPeers)

            // Reuse the SAME generated fallback contingency both to size the funding and (below) in
            // the bootstrap, so `funding.isBalanced` holds exactly.
            fc = headParams.fallbackContingency
            totalContingency = Coin(
              fc.collectiveContingency.total.value + nHeadPeers * fc.individualContingency.total.value
            )

            fundingValueTarget = potValue + Value(totalEquity) + Value(totalContingency)
            funding = InitializationFunding(
              seedUtxo = Utxo(seedInput, Babbage(publisherAddress, fundingValueTarget)),
              additionalFundingUtxos = Map.empty,
              changeOutputs = Nil
            )

            genesisId = L2Genesis.mkGenesisId(seedInput)
            potInput0 = TransactionInput(genesisId, 0)
            evacuationMap = EvacuationMap(
              TreeMap(
                potInput0.toEvacuationKey ->
                    Payout
                        .Obligation(KeepRaw(Babbage(publisherAddress, potValue)), cardanoNetwork)
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

            amounts <- genClaimAmounts
        } yield Scenario(
          mnc = mnc,
          headGenesisUtxos = mnc.initializationTx.resolvedUtxos.utxos,
          headInitTxId = mnc.initializationTx.tx.id,
          treasuryAddress = mnc.headConfig.headMultisigAddress,
          claims = buildClaims(mnc, network, publisherAddress, potInput0, potValue, amounts)
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

    private def inputsFor(
        mnc: MultiNodeConfig,
        backend: L1Backend[IO]
    ): MultiPeerHeadHarness.Inputs =
        MultiPeerHeadHarness.Inputs(
          config =
              MultiPeerHeadHarness.Config("airdrop", BackendMode.InMemory, TransportMode.Direct),
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

    private def withdrawnTokens(utxos: Utxos, treasuryAddress: ShelleyAddress): Long =
        utxos.iterator.collect {
            case (_, o) if o.address != treasuryAddress => tokensOf(o.value)
        }.sum

    /** Poll the L1 snapshot until every airdrop token has left the treasury for a recipient. */
    private def pollWithdrawn(
        snapshot: IO[Utxos],
        treasuryAddress: ShelleyAddress,
        attempts: Int,
        sleep: FiniteDuration
    ): IO[Long] =
        snapshot.flatMap { utxos =>
            val withdrawn = withdrawnTokens(utxos, treasuryAddress)
            if withdrawn >= totalSupply || attempts <= 1 then IO.pure(withdrawn)
            else IO.sleep(sleep) >> pollWithdrawn(snapshot, treasuryAddress, attempts - 1, sleep)
        }

    private def submitClaim(
        requestSequencer: RequestSequencer.Handle,
        headId: InitializationParameters.HeadId,
        slotConfig: SlotConfig,
        userVk: VerificationKey,
        claim: BuiltClaim,
        now: Instant
    ): IO[Unit] = {
        val body = TransactionRequestBody(ByteString.fromArray(claim.signedTx.toCbor))
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
            backendAndSnapshot <- CardanoBackendMock.mockIOWithSnapshot(
              initialState = MockState(scenario.headGenesisUtxos),
              mkContext = mkContext
            )
            (backend, snapshot) = backendAndSnapshot

            sig <- stackZeroSignal
            _ <- log(
              s"minting $totalSupply tokens into the head's initial L2 state; " +
                  s"${scenario.claims.size} claims queued"
            )

            result <- MultiPeerHeadHarness
                .resource(inputsFor(scenario.mnc, backend), hooksFor(sig))
                .use { harness =>
                    for {
                        _ <- sig.await.timeout(120.seconds)
                        _ <- log("head hard-confirmed stack 0; airdrop pot is live in L2")
                        initLanded <- pollTxKnown(backend, scenario.headInitTxId, 120, 500.millis)
                        _ <- log(s"init tx landed on L1: $initLanded")

                        requestSequencer = harness.peers(HeadPeerNumber(0)).handle
                        userVk = scenario.mnc
                            .nodeConfigs(HeadPeerNumber(0))
                            .ownWallet
                            .exportVerificationKey
                        now <- IO.realTimeInstant

                        _ <- scenario.claims.zipWithIndex.traverse_ { case (claim, i) =>
                            submitClaim(
                              requestSequencer,
                              cfg.headId,
                              cfg.slotConfig,
                              userVk,
                              claim,
                              now
                            ) >>
                                (if (i + 1) % 50 == 0 then
                                     log(s"submitted ${i + 1}/${scenario.claims.size} claims")
                                 else IO.unit)
                        }
                        _ <- log(
                          s"all ${scenario.claims.size} claims submitted; settling withdrawals on L1"
                        )

                        withdrawn <- pollWithdrawn(
                          snapshot,
                          scenario.treasuryAddress,
                          900,
                          500.millis
                        )
                        errors <- harness.sutErrors.get
                        _ <- log(
                          s"withdrawn to L1: $withdrawn / $totalSupply tokens; actor errors=${errors.size}"
                        )
                    } yield (initLanded, withdrawn, errors)
                }
            (initLanded, withdrawn, errors) = result
        } yield {
            List(
              "init tx did not land on L1" |: initLanded,
              s"not all tokens withdrawn to L1: $withdrawn/$totalSupply" |: (withdrawn == totalSupply),
              s"head produced actor errors: ${errors.mkString("; ")}" |: errors.isEmpty
            ).foldLeft(Prop(true))(_ && _)
        }
    }

    val _ = property("airdrop tokens flow from L2 claims onto L1 as withdrawals") =
        Prop.forAll(genScenario) { scenario =>
            runDemo(scenario).unsafeRunSync()
        }
}
