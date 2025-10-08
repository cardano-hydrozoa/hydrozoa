package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.lib.tx.*
import hydrozoa.lib.tx.BuildError.{BalancingError, ValidationError}
import hydrozoa.lib.tx.ScriptSource.{NativeScriptAttached, NativeScriptValue}
import hydrozoa.lib.tx.TransactionBuilderStep.{Mint, *}
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token
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
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.txbuilder.TxBalancingError
import scalus.ledger.api.v1.PosixTime
import scalus.prelude.List as SList

final case class FallbackTx(
    treasurySpent: TreasuryUtxo,
    override val tx: Transaction
) extends Tx

object FallbackTx {
    case class Recipe(
        headScript: HeadMultisigScript,
        treasuryUtxo: TreasuryUtxo,
        disputeTreasuryAddress: ShelleyAddress,
        disputeResolutionAddress: ShelleyAddress,
        // Voting duration from head parameters.
        // TODO: see https://github.com/cardano-hydrozoa/hydrozoa/issues/129//
        votingDuration: PosixTime,
        network: Network,
        protocolParams: ProtocolParams,
        evaluator: PlutusScriptEvaluator,
        validators: Seq[Validator]
    )

    def build(recipe: Recipe): Either[BuildError, FallbackTx] = {
        //////////////////////////////////////
        // Pre-processing
        val multisigDatum: TreasuryUtxo.Datum = recipe.treasuryUtxo.datum

        // FIXME: This isn't correct, I'm not totally sure what it should be
        val voteTokenName = CIP67.TokenNames(recipe.treasuryUtxo.asUtxo._1).voteTokenName

        val newTreasuryDatum = UnresolvedDatum(
          headMp = recipe.headScript.policyId,
          disputeId = voteTokenName.bytes,
          peers = SList.from(recipe.headScript.requiredSigners.map(_.hash)),
          peersN = recipe.headScript.numSigners,
          deadlineVoting = recipe.votingDuration,
          versionMajor = multisigDatum.versionMajor,
          params = multisigDatum.paramsHash,
          // KZG setup I think?
          setup = ???
        )

        def mkVoteToken(amount: Long): MultiAsset = MultiAsset(
          SortedMap(
            (
              recipe.headScript.policyId,
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
              value = Value(coin = voteUtxoAda, multiAsset = mkVoteToken(1)),
              datumOption = Some(Inline(datum.toData)),
              scriptRef = None
            )
        }

        // TODO: bring this over from the old code
        def mkDefVoteDatum(i: Int, unit: Unit): VoteDatum = ???
        def mkVoteDatum(i: Int, i1: Int, hash: AddrKeyHash): VoteDatum = ???

        mkVoteOutput(mkDefVoteDatum(recipe.headScript.numSigners, ()))

        val voteUtxos: List[TransactionOutput] =
            recipe.headScript.requiredSigners.toList.zipWithIndex.map((peer, key) =>
                val datum = mkVoteDatum(key + 1, recipe.headScript.numSigners, peer.hash)
                mkVoteOutput(datum)
            )

        ////////////////////////////////////////////////////
        // Define steps
        val createVoteUtxos: Seq[Send] = voteUtxos.map(Send(_))

        val spendMultisigTreasury: Spend =
            Spend(
              recipe.treasuryUtxo.asUtxo,
              witness = NativeScriptWitness(
                scriptSource = NativeScriptValue(recipe.headScript.script),
                additionalSigners =
                    recipe.headScript.requiredSigners)
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
          recipe.headScript.policyId,
          assetName = voteTokenName,
          amount = recipe.headScript.numSigners + 1L,
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
                .build(recipe.network, steps)
                .left
                .map(BuildError.StepError(_))
            finalized <- unbalanced
                .finalizeContext(
                  recipe.protocolParams,
                  // FIXME: Check change handling
                  diffHandler = new ChangeOutputDiffHandler(
                    recipe.protocolParams,
                    0
                  ).changeOutputDiffHandler,
                  evaluator = recipe.evaluator,
                  validators = recipe.validators
                )
                .left
                .map({
                    case balanceError: TxBalancingError => BalancingError(balanceError)
                    case validationError: TransactionException =>
                        ValidationError(validationError)
                })
        } yield FallbackTx(treasurySpent = recipe.treasuryUtxo, tx = finalized.transaction)

    }
}
