package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming.*
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.ensureMinAda
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toQuantizedInstant}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.Fallback
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.{RuleBasedTreasuryDatum, UnresolvedDatum}
import hydrozoa.rulebased.ledger.dapp.state.VoteDatum as VD
import hydrozoa.rulebased.ledger.dapp.state.VoteState.VoteDatum
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import monocle.{Focus, Lens}
import scala.collection.immutable.SortedMap
import scalus.builtin.Data
import scalus.builtin.Data.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.ScriptSource.NativeScriptAttached
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, *}
import scalus.ledger.api.v1.PubKeyHash
import scalus.prelude.List as SList

final case class FallbackTx(
    override val validityStart: QuantizedInstant,
    treasurySpent: MultisigTreasuryUtxo,
    treasuryProduced: RuleBasedTreasuryUtxo,
    consumedHMRWUtxo: MultisigRegimeUtxo,
    producedDefaultVoteUtxo: Utxo,
    producedPeerVoteUtxos: NonEmptyList[Utxo],
    producedCollateralUtxos: NonEmptyList[Utxo],
    override val tx: Transaction,
    override val txLens: Lens[FallbackTx, Transaction] = Focus[FallbackTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos
) extends HasValidityStart,
    // TODO: shall we add separate raw-type traits for that?
    Tx[FallbackTx] {
    def producedVoteUtxos: NonEmptyList[Utxo] =
        NonEmptyList(producedDefaultVoteUtxo, producedPeerVoteUtxos.toList)

    def producedNonTreasuryUtxos: NonEmptyList[Utxo] =
        producedVoteUtxos ++ producedCollateralUtxos.toList
}

/*
Fallback tx spec:

 - [ ] Spend both the treasury and multisig regime utxos.
 - [ ] Burn the multisig regime token.
 - [ ] Mint N+1 vote tokens.
 - [ ] Produce a rule-based treasury utxo, containing all treasury utxo funds (i.e. don't deduct the fee)
 - [ ] Produce default vote utxo with minimum ADA, plus a vote token.
 - [ ] Produce one vote utxo per peer, containing minimum ADA plus a configured allowance for one tally tx fee, plus a vote token.
 - [ ] Produce one collateral utxo per peer, containing minimum ADA plus a configured allowance for one vote tx fee.
 - [ ] Cover the tx fee and ADA for all non-treasury outputs using funds from the multisig regime utxo.
 - [ ] TreasuryOutput at index 0
 - [ ] Default vote at index 1
 - [ ] Per-peer vote utxos at next indices
 - [ ] Collateral utxos at next indicies
 */

// NOTE TO SELF: The multisig witness utxo is a FIXED SIZE utxo. We know the amount in it -- it is
// maxNonPlutusTxFee + the vote allowance + the collateral allowance
object FallbackTx {
    type Config = HeadConfig.Preinit.Section

    def build(
        config: Config,
        validityStartTime: QuantizedInstant,
        treasuryUtxoSpent: MultisigTreasuryUtxo,
        multisigRegimeUtxo: MultisigRegimeUtxo,
    ): Either[SomeBuildError, FallbackTx] = {
        //////////////////////////////////////
        // Pre-processing

        val multisigDatum: MultisigTreasuryUtxo.Datum = treasuryUtxoSpent.datum

        val hns = config.headMultisigScript

        val newTreasuryDatum = UnresolvedDatum(
          headMp = hns.policyId,
          disputeId = config.headTokenNames.voteTokenName.bytes,
          peers = SList.from(hns.requiredSigners.map(_.hash)),
          peersN = hns.numSigners,
          deadlineVoting = config.slotConfig.slotToTime(validityStartTime.toSlot.slot) +
              config.votingDuration.finiteDuration.toMillis,
          versionMajor = multisigDatum.versionMajor.toInt,
          // TODO: pull in N first elements of G2 CRS
          // KZG setup I think?
          setup = SList.empty
        )

        def mkVoteToken(amount: Long): MultiAsset = MultiAsset(
          SortedMap(
            (
              hns.policyId,
              SortedMap((config.headTokenNames.voteTokenName, amount))
            )
          )
        )

        def mkVoteUtxo(datum: Data): TransactionOutput = Babbage(
          address = config.ruleBasedDisputeResolutionAddress,
          value = Value(config.individualContingency.tallyTxFee, mkVoteToken(1)),
          datumOption = Some(Inline(datum)),
          scriptRef = None
        ).ensureMinAda(config.cardanoProtocolParams)

        val defaultVoteUtxo = mkVoteUtxo(VD.default(treasuryUtxoSpent.datum.commit).toData)

        val peerVoteUtxos: NonEmptyList[TransactionOutput] = {
            val datums = VD(
              NonEmptyList.fromListUnsafe(hns.requiredSigners.map(x => PubKeyHash(x.hash)).toList)
            )
            datums.map(datum => mkVoteUtxo(datum.toData))
        }

        def collateralUtxos: NonEmptyList[TransactionOutput] = {
            NonEmptyList.fromListUnsafe(
              hns.requiredSigners
                  .map(es =>
                      Babbage(
                        address = ShelleyAddress(
                          network = config.network,
                          payment = ShelleyPaymentPart.Key(es.hash),
                          delegation = Null
                        ),
                        value = Value(config.individualContingency.tallyTxFee),
                        datumOption = None,
                        scriptRef = None
                      ).ensureMinAda(config.cardanoProtocolParams)
                  )
                  .toList
            )
        }

        ////////////////////////////////////////////////////
        // Define steps
        val spendHMRW: Spend = Spend(multisigRegimeUtxo.asUtxo, hns.witnessAttached)

        val spendMultisigTreasury: Spend =
            Spend(treasuryUtxoSpent.asUtxo, NativeScriptWitness(NativeScriptAttached, Set.empty))

        val burnMultisigRegimeToken: Mint = Mint(
          hns.policyId,
          assetName = config.headTokenNames.multisigRegimeTokenName,
          amount = -1,
          witness = NativeScriptWitness(NativeScriptAttached, Set.empty)
        )

        val mintVoteTokens = Mint(
          hns.policyId,
          assetName = config.headTokenNames.voteTokenName,
          amount = hns.numSigners + 1L,
          witness = NativeScriptWitness(NativeScriptAttached, Set.empty)
        )

        val createDefaultVoteUtxo: Send = Send(defaultVoteUtxo)
        val createPeerVoteUtxos: NonEmptyList[Send] = peerVoteUtxos.map(Send(_))

        val createCollateralUtxos: NonEmptyList[Send] = collateralUtxos.map(Send(_))

        val disputeTreasuryAddress = config.ruleBasedDisputeResolutionAddress
        val createDisputeTreasury = Send(
          Babbage(
            address = disputeTreasuryAddress,
            value = treasuryUtxoSpent.value,
            datumOption = Some(Inline(newTreasuryDatum.toData)),
            scriptRef = None
          )
        )

        val setMetaData = ModifyAuxiliaryData(_ =>
            Some(
              MD.apply(
                Fallback(config.ruleBasedTreasuryAddress)
              )
            )
        )

        val setStartSlot = ValidityStartSlot(validityStartTime.toSlot.slot)

        val steps = {
            Seq(
              spendHMRW,
              spendMultisigTreasury,
              burnMultisigRegimeToken,
              mintVoteTokens,
              createDisputeTreasury,
              createDefaultVoteUtxo,
              setMetaData,
              setStartSlot
            )
                ++ createPeerVoteUtxos.toList
                ++ createCollateralUtxos.toList
        }

        for {
            unbalanced <- TransactionBuilder.build(network = config.network, steps = steps)
            finalized <- unbalanced.finalizeContext(
              protocolParams = config.cardanoProtocolParams,
              // We balance the excess to the treasury. This will _not_ be pre-balanced, because the
              // HMRW Utxo contains extra ada to account for _any_ fallback transaction fee.
              diffHandler = Change.changeOutputDiffHandler(_, _, config.cardanoProtocolParams, 0),
              evaluator = config.plutusScriptEvaluatorForTxBuild,
              validators = Tx.Validators.nonSigningNonValidityChecksValidators
            )
        } yield {
            val txId = finalized.transaction.id
            FallbackTx(
              // This is safe since we always set it
              validityStart =
                  Slot(setStartSlot.slot).toQuantizedInstant(config.cardanoInfo.slotConfig),
              treasurySpent = treasuryUtxoSpent,
              treasuryProduced = RuleBasedTreasuryUtxo(
                treasuryTokenName = config.headTokenNames.treasuryTokenName,
                utxoId = TransactionInput(txId, 0),
                address = disputeTreasuryAddress,
                datum = RuleBasedTreasuryDatum.Unresolved(newTreasuryDatum),
                value = createDisputeTreasury.output.value
              ),
              consumedHMRWUtxo = MultisigRegimeUtxo(
                multisigRegimeTokenName = config.headTokenNames.multisigRegimeTokenName,
                utxoId = TransactionInput(txId, 1),
                address = config.headMultisigAddress,
                value = spendHMRW.utxo.output.value,
                script = config.headMultisigScript
              ),
              producedDefaultVoteUtxo =
                  Utxo(TransactionInput(txId, 1), createDefaultVoteUtxo.output),
              producedPeerVoteUtxos = createPeerVoteUtxos
                  .map(_.output)
                  .zipWithIndex
                  .map(outputsZipped => {
                      val output = outputsZipped._1
                      val actualIndex = outputsZipped._2 + 2
                      Utxo(
                        TransactionInput(txId, actualIndex),
                        output
                      )
                  }),
              producedCollateralUtxos = createCollateralUtxos
                  .map(_.output)
                  .zipWithIndex
                  .map(outputsZipped => {
                      val output = outputsZipped._1
                      val actualIndex = outputsZipped._2 + 2 + config.headMultisigScript.numSigners
                      Utxo(
                        TransactionInput(txId, actualIndex),
                        output
                      )
                  }),
              tx = finalized.transaction,
              resolvedUtxos = finalized.resolvedUtxos
            )
        }
    }
}
