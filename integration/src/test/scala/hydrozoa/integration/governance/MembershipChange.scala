package hydrozoa.integration.governance

import cats.data.Validated.{Invalid, Valid}
import cats.data.ReaderT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hydrozoa.bootstrap.InitializationFunding
import hydrozoa.config.head.coil.CoilPeers
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.BlockCreationEndTime
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers.headMultisigAddress
import hydrozoa.config.head.{HeadConfig, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{addExpectedSigners, finalizeContext}
import hydrozoa.lib.cardano.scalus.contextualscalus.{Change as CtxChange, TransactionBuilder as CtxTxBuilder}
import hydrozoa.lib.cardano.scalus.txbuilder.Transaction.attachVKeyWitnesses
import hydrozoa.multisig.backend.cardano.{CardanoBackendMock, MockState}
import hydrozoa.multisig.ledger.l1.tx.{EnrichedTx, InitializationTx, Metadata}
import hydrozoa.multisig.ledger.l1.utxo.{MultisigRegimeOutput, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Test.Parameters
import org.scalacheck.{Gen, Prop, Properties}
import scalus.cardano.ledger.CertState
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.rules.{Context as LedgerContext, UtxoEnv}
import scalus.cardano.ledger.Value
import scalus.cardano.txbuilder.SomeBuildError
import scalus.cardano.txbuilder.TransactionBuilder.Context as BuildContext
import scalus.cardano.txbuilder.TransactionBuilderStep
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, Send, Spend, ValidityEndSlot}
import scalus.uplc.builtin.Data.toData
import test.{PeersNumberSpec, SeedPhrase, TestPeers, TestPeersSpec, genMonad}

/** Membership-change "transfer tx" fixture and its isolated round-trip validation.
  *
  * A transfer tx moves a head's treasury to a fresh peer set under a new head id. It is
  * simultaneously the OLD head's teardown (spend old treasury + regime, burn old tokens) and the NEW
  * head's initialization (mint new tokens, produce new treasury + regime, carry an
  * [[Metadata.Initialization]] tag), so the new head parses it as its own init tx. The old treasury
  * input is the seed, hence the new head id is `mkHeadId(oldTreasuryInput)`.
  *
  * This suite only exercises the builder in isolation: build → multisign with both peer sets →
  * submit to a mock L1 running the real Conway ledger → parse as the new head's init tx. The
  * two-head harness demo is a separate later phase.
  */
object MembershipChange extends Properties("MembershipChange transfer tx") {

    override def overrideParameters(p: Parameters): Parameters =
        p.withMinSuccessfulTests(3)

    /** Builds a membership-change transfer tx from the old head (teardown) to the new head
      * (initialization).
      *
      * Spends the old treasury (the seed) and the old multisig-regime utxo under the old native
      * script, burns the old head + regime tokens, mints the new head + regime tokens under the new
      * native script, and produces the new treasury (output 0, initial evacuation datum) and new
      * regime (output 1). The new treasury absorbs the fee via the change diff handler, so no extra
      * funding input is needed. Carries a dual metadata blob: an [[Metadata.Initialization]] tag
      * under the new head id and a [[Metadata.Transfer]] tag under the old head id.
      *
      * The old native script is provided as the reference script on the spent old regime utxo (hence
      * `witnessAttached` for the old spends/burns); the new native script is provided inline by the
      * first new mint (`witnessValue`).
      *
      * @return
      *   the finalized (unsigned) builder context — the caller attaches both peer sets' witnesses.
      */
    def buildTransferTx(
        oldConfig: HeadConfig.Bootstrap.Section,
        newConfig: HeadConfig.Bootstrap.Section,
        oldHeadId: InitializationParameters.HeadId,
        newHeadId: InitializationParameters.HeadId,
        oldTreasury: MultisigTreasuryUtxo,
        oldMultisigRegime: MultisigRegimeUtxo,
        blockCreationEndTime: BlockCreationEndTime
    ): Either[SomeBuildError, BuildContext] = {
        val oldScript = oldConfig.headMultisigScript
        val newScript = newConfig.headMultisigScript
        val oldTokenNames = oldConfig.headTokenNames
        val newTokenNames = newConfig.headTokenNames

        val newInitEndTime = newConfig.txTiming.initializationEndTime(blockCreationEndTime)

        // Seed index among the tx's (sorted) inputs. The only inputs are the old treasury and old
        // regime — no collateral (native scripts only) and no funding input — so the tx body's input
        // order equals this sort.
        val seedIx = List(oldTreasury.asUtxo.input, oldMultisigRegime.input).sorted
            .indexOf(oldTreasury.asUtxo.input)

        val metadata = Metadata.asAuxData(
          (
            Metadata.Initialization(
              multisigTreasuryIx = 0,
              multisigRegimeIx = 1,
              seedIx = seedIx,
              totalEquity = newConfig.initialEquityContributed
            ),
            newHeadId
          ),
          (Metadata.Transfer(newHeadId), oldHeadId)
        )

        val newTreasuryDatum =
            MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum(newConfig.initialEvacuationMap)

        val newTreasuryOutput = Babbage(
          newConfig.headMultisigAddress,
          newConfig.initialL2Value + Value(newConfig.initialEquityContributed) +
              Value.asset(newScript.policyId, newTokenNames.treasuryTokenName, 1L),
          Some(Inline(newTreasuryDatum.toData))
        )

        val steps: List[TransactionBuilderStep] = List(
          ModifyAuxiliaryData(_ => Some(metadata)),
          ValidityEndSlot(newInitEndTime.toSlot.slot),
          oldMultisigRegime.spend(using oldConfig),
          Spend(oldTreasury.asUtxo, oldScript.witnessAttached),
          Mint(oldScript.policyId, oldTokenNames.treasuryTokenName, -1, oldScript.witnessAttached),
          Mint(
            oldScript.policyId,
            oldTokenNames.multisigRegimeTokenName,
            -1,
            oldScript.witnessAttached
          ),
          Mint(newScript.policyId, newTokenNames.treasuryTokenName, 1, newScript.witnessValue),
          Mint(
            newScript.policyId,
            newTokenNames.multisigRegimeTokenName,
            1,
            newScript.witnessAttached
          ),
          Send(newTreasuryOutput),
          MultisigRegimeOutput.send(using newConfig)
        )

        for {
            ctx0 <- CtxTxBuilder.build(steps)(using oldConfig)
            ctx = ctx0.addExpectedSigners(oldScript.numSigners + newScript.numSigners)
            finalized <- ctx.finalizeContext(
              diffHandler = CtxChange.changeOutputDiffHandler(0)(using oldConfig),
              validators = EnrichedTx.Validators.nonSigningNonValidityChecksValidators
            )(using oldConfig)
        } yield finalized
    }

    /** Generates an OLD head (a fully-built [[MultiNodeConfig]] with its initialization tx) plus its
      * disjoint peer set. Peer counts are pinned small and equal so the new head reuses the old
      * head's parameters/equity and the transfer tx's value balance closes trivially.
      */
    private val genOldHead: Gen[(TestPeers, MultiNodeConfig)] =
        for {
            oldTestPeers <- TestPeers.generate(
              TestPeersSpec(
                SeedPhrase.Yaci,
                CardanoNetwork.Preprod,
                PeersNumberSpec.Range(Some(2), Some(3))
              )
            )
            oldBootstrapAndFunding <- generateHeadConfigBootstrap().run(oldTestPeers)
            oldMnc <- MultiNodeConfig.generateWith(oldTestPeers)(
              generateHeadConfig = generateHeadConfig(
                genHeadConfigBootstrap = ReaderT.pure(oldBootstrapAndFunding)(using genMonad)
              )
            )
        } yield (oldTestPeers, oldMnc)

    val _ = property("transfer tx builds, submits, and parses as the new head's init tx") =
        Prop.forAll(genOldHead) { case (oldTestPeers, oldMnc) =>
            val oldConfig = oldMnc.headConfig
            val oldInitTx = oldMnc.initializationTx
            val oldTreasury = oldInitTx.treasuryProduced
            val oldMultisigRegime = oldInitTx.multisigRegimeProduced

            // New head id derives from the old treasury input — the transfer tx's seed.
            val oldTreasuryInput = oldTreasury.asUtxo.input
            val newHeadId = InitializationFunding.mkHeadId(oldTreasuryInput)

            // Disjoint new peer set (different seed phrase), same network and peer count.
            val newTestPeers =
                TestPeers(SeedPhrase.Public, oldTestPeers.cardanoNetwork, oldTestPeers.peersNumber)

            // New bootstrap: old head parameters/equity/evacuation-map, new disjoint peers, and the
            // seed-derived new head id.
            val newInitParams = InitializationParameters(
              initialEvacuationMap = oldConfig.initialEvacuationMap,
              initialEquityContributions = oldConfig.initialEquityContributions,
              headId = newHeadId
            )
            val newBootstrap: HeadConfig.Bootstrap = HeadConfig.Bootstrap(
              cardanoNetwork = oldConfig.cardanoNetwork,
              headParams = oldConfig.headParameters,
              headPeers = newTestPeers.headPeers,
              coilPeers = CoilPeers.empty,
              initializationParams = newInitParams,
              scriptReferenceUtxos = oldConfig.scriptReferenceUtxos
            ) match {
                case Valid(b)   => b
                case Invalid(e) => throw new RuntimeException(s"new bootstrap invalid: $e")
            }

            val blockEndTime = oldMnc.initialBlock.blockBrief.endTime

            buildTransferTx(
              oldConfig = oldConfig,
              newConfig = newBootstrap,
              oldHeadId = oldConfig.headId,
              newHeadId = newHeadId,
              oldTreasury = oldTreasury,
              oldMultisigRegime = oldMultisigRegime,
              blockCreationEndTime = blockEndTime
            ) match {
                case Left(e) => (s"transfer tx build failed: $e" |: Prop(false))
                case Right(finalized) =>
                    val unsignedTransfer = finalized.transaction

                    // Attach BOTH peer sets' witnesses last: witnesses sign tx.id, so the body must be
                    // final. Old satisfies the spends/burns' native script; new satisfies the mint's.
                    val oldWitnesses = oldMnc.mkVKeyWitnesses(unsignedTransfer).toList
                    val newWitnesses = newTestPeers.mkVKeyWitnesses(unsignedTransfer).toList
                    val signedTransfer =
                        unsignedTransfer.attachVKeyWitnesses(oldWitnesses ++ newWitnesses)

                    // Mock L1 slot config aligned to the config's network so the real-clock validity
                    // check agrees with the TTL slot scale.
                    val mkContext: Long => LedgerContext = slot =>
                        LedgerContext(
                          env = UtxoEnv(
                            slot = slot,
                            params = oldConfig.cardanoProtocolParams,
                            certState = CertState.empty,
                            network = oldConfig.network
                          ),
                          slotConfig = oldConfig.slotConfig
                        )

                    val program: IO[List[Prop]] = for {
                        backend <- CardanoBackendMock.mockIO(
                          initialState = MockState(oldInitTx.resolvedUtxos.utxos),
                          mkContext = mkContext
                        )
                        // Land the old head first so its treasury + regime exist on L1.
                        signedOldInit = oldMnc.multisignTx(oldInitTx.tx)
                        oldSubmit <- backend.submitTx(signedOldInit)
                        transferSubmit <- backend.submitTx(signedTransfer)
                        transferKnown <- backend.isTxKnown(signedTransfer.id)
                    } yield {
                        // Parse the transfer tx as the NEW head's initialization tx.
                        val parseRes = InitializationTx
                            .Parse(newBootstrap)(
                              blockCreationEndTime = blockEndTime,
                              tx = signedTransfer,
                              resolvedUtxos = finalized.resolvedUtxos
                            )
                            .result

                        List(
                          s"old init submit failed: $oldSubmit" |: oldSubmit.isRight,
                          s"transfer tx submit failed (invalid per real ledger): $transferSubmit" |:
                              transferSubmit.isRight,
                          s"transfer tx not landed: $transferKnown" |: transferKnown == Right(true),
                          s"new head failed to parse transfer tx as its init tx: $parseRes" |:
                              parseRes.isRight
                        )
                    }

                    program.unsafeRunSync().foldLeft(Prop(true))(_ && _)
            }
        }
}
