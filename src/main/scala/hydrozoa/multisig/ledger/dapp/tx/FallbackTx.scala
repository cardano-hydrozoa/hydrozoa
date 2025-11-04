package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.UnresolvedDatum
import hydrozoa.rulebased.ledger.dapp.state.VoteState.VoteDatum
import scala.collection.immutable.SortedMap
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.ScriptSource.{NativeScriptAttached, NativeScriptValue}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, *}
import scalus.ledger.api.v1.PosixTime
import scalus.prelude.List as SList

final case class FallbackTx(
    treasurySpent: TreasuryUtxo,
    treasuryProduced : TreasuryUtxo,
    override val tx: Transaction
) extends Tx


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


object FallbackTx {
    case class Recipe(
        config : Tx.Builder.Config,
        treasuryUtxo: TreasuryUtxo,
        disputeTreasuryAddress: ShelleyAddress,
        disputeResolutionAddress: ShelleyAddress,
        tallyFeeAllowance: Coin,
        // Voting duration from head parameters.
        // TODO: see https://github.com/cardano-hydrozoa/hydrozoa/issues/129//
        votingDuration: PosixTime,
    )

    def build(recipe: Recipe): Either[SomeBuildError, FallbackTx] = {
        //////////////////////////////////////
        // Pre-processing
        val multisigDatum: TreasuryUtxo.Datum = recipe.treasuryUtxo.datum
        
        val hns = recipe.config.headNativeScript

        // FIXME: This isn't correct, I'm not totally sure what it should be
        val voteTokenName = CIP67.TokenNames(recipe.treasuryUtxo.asUtxo._1).voteTokenName

        val newTreasuryDatum = UnresolvedDatum(
          headMp = hns.policyId,
          disputeId = voteTokenName.bytes,
          peers = SList.from(hns.requiredSigners.map(_.hash)),
          peersN = hns.numSigners,
          deadlineVoting = recipe.votingDuration,
          versionMajor = multisigDatum.versionMajor,
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
        // TODO: magic number.
        // I think we want this to be minAda + enough to get through voting?
        val voteUtxoAda = Coin(4_000_000L)

        def mkVoteOutput(datum: VoteDatum): Babbage = {
            Babbage(
              address = recipe.disputeResolutionAddress,
              value = Value(coin = voteUtxoAda, assets = mkVoteToken(1)),
              datumOption = Some(Inline(datum.toData)),
              scriptRef = None
            )
        }

        // TODO: bring this over from the old code
        def mkDefVoteDatum(i: Int, unit: Unit): VoteDatum = ???
        def mkVoteDatum(i: Int, i1: Int, hash: AddrKeyHash): VoteDatum = ???

        mkVoteOutput(mkDefVoteDatum(hns.numSigners, ()))

        val voteUtxos: List[TransactionOutput] =
            hns.requiredSigners.toList.zipWithIndex.map((peer, key) =>
                val datum = mkVoteDatum(key + 1, hns.numSigners, peer.hash)
                mkVoteOutput(datum)
            )

        ////////////////////////////////////////////////////
        // Define steps
        val createVoteUtxos: Seq[Send] = voteUtxos.map(Send(_))

        val spendMultisigTreasury: Spend =
            Spend(
              recipe.treasuryUtxo.asUtxo,
              witness = NativeScriptWitness(
                scriptSource = NativeScriptValue(hns.script),
                additionalSigners = hns.requiredSigners
              )
            )

        val createDisputeTreasury = Send(
          Babbage(
            address = recipe.disputeTreasuryAddress,
            value = recipe.treasuryUtxo.value,
            datumOption = Some(Inline(newTreasuryDatum.toData)),
            scriptRef = None
          )
        )

        val mintVoteTokens = Mint(
          hns.policyId,
          assetName = voteTokenName,
          amount = hns.numSigners + 1L,
          witness = NativeScriptWitness(NativeScriptAttached, Set.empty)
        )

        val steps = {
            // FIXME: I think we need to create a default vote output as well?
            Seq(createDisputeTreasury)
                .appendedAll(createVoteUtxos)
                .appended(spendMultisigTreasury)
                .appended(mintVoteTokens)
        }

        /////////////////////////////////////////
        // Build and finalize
        for {
            unbalanced <- TransactionBuilder
                .build(recipe.config.env.network, steps)
            finalized <- unbalanced
                .finalizeContext(
                  recipe.config.env.protocolParams,
                  // FIXME: Check change handling
                  diffHandler = new ChangeOutputDiffHandler(
                    recipe.config.env.protocolParams,
                    0
                  ).changeOutputDiffHandler,
                  evaluator = recipe.config.env.evaluator,
                  validators = recipe.config.validators
                )
        } yield FallbackTx(
            treasurySpent = recipe.treasuryUtxo, 
            treasuryProduced = 
            ???, tx = finalized.transaction
        )
    }
}
