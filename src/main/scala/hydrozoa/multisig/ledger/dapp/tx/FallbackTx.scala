package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.ensureMinAda
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.Fallback
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.UnresolvedDatum
import hydrozoa.rulebased.ledger.dapp.state.VoteDatum as VD
import hydrozoa.rulebased.ledger.dapp.state.VoteState.VoteDatum
import scala.collection.immutable.SortedMap
import scalus.builtin.Data
import scalus.builtin.Data.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.ScriptSource.NativeScriptAttached
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, *}
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.prelude.List as SList

final case class FallbackTx(
    validityStartSlot: Slot,
    treasurySpent: TreasuryUtxo,
    // FIXME: I think this needs to be a different type than just TreasuryUtxo,
    // because its a rules-based treasury utxo.
    // The rest should have domain-specific types as well. See:
    // https://github.com/cardano-hydrozoa/hydrozoa/issues/262
    treasuryProduced: Utxo,
    consumedHMRWUtxo: Utxo,
    producedDefaultVoteUtxo: Utxo,
    producedPeerVoteUtxos: NonEmptyList[Utxo],
    producedCollateralUtxos: NonEmptyList[Utxo],
    override val tx: Transaction
) extends Tx {
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
    def build(recipe: Recipe): Either[SomeBuildError, FallbackTx] = {
        import recipe.*
        import config.*
        import env.*
        import tokenNames.*
        //////////////////////////////////////
        // Pre-processing

        val multisigDatum: TreasuryUtxo.Datum = treasuryUtxo.datum

        val hns = headNativeScript

        val newTreasuryDatum = UnresolvedDatum(
          headMp = hns.policyId,
          disputeId = voteTokenName.bytes,
          peers = SList.from(hns.requiredSigners.map(_.hash)),
          peersN = hns.numSigners,
          deadlineVoting = recipe.votingDuration,
          versionMajor = multisigDatum.versionMajor.toInt,
          params = multisigDatum.paramsHash,
          // KZG setup I think?
          setup = SList.empty
        )

        def mkVoteToken(amount: Long): MultiAsset = MultiAsset(
          SortedMap(
            (
              hns.policyId,
              SortedMap((voteTokenName, amount))
            )
          )
        )

        def mkVoteUtxo(datum: Data): TransactionOutput = Babbage(
          address = DisputeResolutionScript.address(network),
          value = Value(recipe.tallyFeeAllowance, mkVoteToken(1)),
          datumOption = Some(Inline(datum)),
          scriptRef = None
        ).ensureMinAda(env.protocolParams)

        val defaultVoteUtxo = mkVoteUtxo(VD.default(treasuryUtxo.datum.commit).toData)

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
                          network = env.network,
                          payment = ShelleyPaymentPart.Key(es.hash),
                          delegation = Null
                        ),
                        value = Value(recipe.tallyFeeAllowance),
                        datumOption = None,
                        scriptRef = None
                      ).ensureMinAda(env.protocolParams)
                  )
                  .toList
            )
        }

        ////////////////////////////////////////////////////
        // Define steps
        val spendHMRW: Spend = Spend(config.headNativeScriptReferenceInput, hns.witness)

        val spendMultisigTreasury: Spend =
            Spend(treasuryUtxo.asUtxo, NativeScriptWitness(NativeScriptAttached, Set.empty))

        val burnMultisigRegimeToken: Mint = Mint(
          hns.policyId,
          assetName = tokenNames.multisigRegimeTokenName,
          amount = -1,
          witness = NativeScriptWitness(NativeScriptAttached, Set.empty)
        )

        val mintVoteTokens = Mint(
          hns.policyId,
          assetName = voteTokenName,
          amount = hns.numSigners + 1L,
          witness = NativeScriptWitness(NativeScriptAttached, Set.empty)
        )

        val createDefaultVoteUtxo: Send = Send(defaultVoteUtxo)
        val createPeerVoteUtxos: NonEmptyList[Send] = peerVoteUtxos.map(Send(_))

        val createCollateralUtxos: NonEmptyList[Send] = collateralUtxos.map(Send(_))

        val createDisputeTreasury = Send(
          Babbage(
            address = RuleBasedTreasuryScript.address(network),
            value = treasuryUtxo.value,
            datumOption = Some(Inline(newTreasuryDatum.toData)),
            scriptRef = None
          )
        )

        val setMetaData = ModifyAuxiliaryData(_ =>
            Some(
              MD.apply(
                Fallback(RuleBasedTreasuryScript.address(network))
              )
            )
        )

        val steps = {
            Seq(
              spendHMRW,
              spendMultisigTreasury,
              burnMultisigRegimeToken,
              mintVoteTokens,
              createDisputeTreasury,
              createDefaultVoteUtxo,
              setMetaData
            )
                ++ createPeerVoteUtxos.toList
                ++ createCollateralUtxos.toList
        }

        for {
            unbalanced <- TransactionBuilder.build(network = network, steps = steps)
            finalized <- unbalanced.finalizeContext(
              protocolParams = protocolParams,
              // We balance the excess to the treasury. This will _not_ be pre-balanced, because the
              // HMRW Utxo contains extra ada to account for _any_ fallback transaction fee.
              diffHandler = new ChangeOutputDiffHandler(protocolParams, 0).changeOutputDiffHandler,
              evaluator = evaluator,
              validators = validators
            )
        } yield {
            val txId = finalized.transaction.id
            FallbackTx(
              // FIXME:
              validityStartSlot = Slot(100),
              treasurySpent = treasuryUtxo,
              //
              treasuryProduced = Utxo(TransactionInput(txId, 0), createDisputeTreasury.output),
              consumedHMRWUtxo = spendHMRW.utxo,
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
                      val actualIndex = outputsZipped._2 + 2 + headNativeScript.numSigners
                      Utxo(
                        TransactionInput(txId, actualIndex),
                        output
                      )
                  }),
              tx = finalized.transaction
            )
        }
    }

    case class Recipe(
        config: Tx.Builder.Config,
        treasuryUtxo: TreasuryUtxo,
        tallyFeeAllowance: Coin,
        // Voting duration from head parameters.
        // TODO: see https://github.com/cardano-hydrozoa/hydrozoa/issues/129//
        votingDuration: PosixTime
    )
}
