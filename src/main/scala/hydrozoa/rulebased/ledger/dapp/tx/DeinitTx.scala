package hydrozoa.rulebased.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.lib.tx.*
import hydrozoa.lib.tx.Datum.DatumInlined
import hydrozoa.lib.tx.ScriptSource.{NativeScriptValue, PlutusScriptValue}
import hydrozoa.lib.tx.TransactionBuilderStep.{Mint, *}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.Environment
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler

final case class DeinitTx(
    treasuryUtxoSpent: RuleBasedTreasuryUtxo,
    initializerOutput: TransactionOutput,
    tx: Transaction
)

/** TODO: should also spend the equity from dispute utxos as long as they exist. It's an interesting
  * hybrid:
  *   - we need collateral since it's a plutus tx
  *   - treasury can't be a collateral since it's not pkh-owned
  *   - we do need to sign this, but then all should use the same collateral
  *   - it's asymmetric in this sense
  */
object DeinitTx {

    case class Recipe(
        headNativeScript: HeadMultisigScript,
        // Might be _unresolved_ or _resolved_
        treasuryUtxo: RuleBasedTreasuryUtxo,
        // TODO: consume dispute utxos? is it mandatory?
        // TODO: consume multisig regime utxo?
        // TODO: outputs?

        collateralUtxo: Utxo[L1],
        // The reference script for the HNS should live inside the multisig regime UTxO
        headNativeScriptReferenceInput: TransactionUnspentOutput,
        env: Environment,
        validators: Seq[Validator]
    )

    enum DeinitTxError:
        case InvalidTreasuryDatum(msg: String)
        case NoBeaconTokensFound

    def build(recipe: Recipe): Either[SomeBuildError | DeinitTxError, DeinitTx] = {

        val policyId = recipe.headNativeScript.policyId

        for {
            headTokens <- extractHeadTokens(policyId, recipe.treasuryUtxo)
            result <- buildDeinitTx(recipe, headTokens)
        } yield result
    }

    private def extractHeadTokens(
        policyId: PolicyId,
        treasuryUtxo: RuleBasedTreasuryUtxo
    ): Either[DeinitTxError, MultiAsset] = {
        import DeinitTxError.*

        treasuryUtxo.value.assets.assets.get(policyId) match {
            case Some(headTokens) =>
                if headTokens.nonEmpty
                then Right(MultiAsset.fromPolicy(policyId, headTokens))
                else Left(NoBeaconTokensFound)
            case None => Left(NoBeaconTokensFound)
        }
    }

    private def buildDeinitTx(
        recipe: Recipe,
        headTokens: MultiAsset
    ): Either[SomeBuildError | DeinitTxError, DeinitTx] = {
        import recipe.*

        // Create treasury redeemer for deinitialization
        val treasuryRedeemer = TreasuryRedeemer.Deinit

        // Calculate the value to send to initializer (treasury minus beacon tokens)
        val initializerValue = treasuryUtxo.value - Value.apply(Coin.zero, headTokens)

        // Create initializer output
        val initializerOutput = Babbage(
          address = ???,
          value = initializerValue,
          datumOption = None,
          scriptRef = None
        )

        // Get treasury input and output
        val (treasuryInput, treasuryOutput) = recipe.treasuryUtxo.toUtxo

        for {
            context <- TransactionBuilder
                .build(
                  env.network,
                  List(
                    // Spend the treasury utxo
                    Spend(
                      TransactionUnspentOutput(treasuryInput, treasuryOutput),
                      ThreeArgumentPlutusScriptWitness(
                        PlutusScriptValue(RuleBasedTreasuryScript.compiledPlutusV3Script),
                        treasuryRedeemer.toData,
                        DatumInlined,
                        Set.empty // Treasury script doesn't require specific signers for deinit
                      )
                    ),
                    // Burn beacon tokens
                    Mint(
                      scriptHash = headNativeScript.policyId,
                      assetName = treasuryUtxo.beaconTokenName,
                      amount = -1,
                      witness = NativeScriptWitness(
                        NativeScriptValue(headNativeScript.script),
                        headNativeScript.requiredSigners
                      )
                    ),
                    // Send remaining funds to initializer
                    Send(initializerOutput),
                    // Add collateral
                    AddCollateral(TransactionUnspentOutput.fromUtxo(collateralUtxo))
                  )
                )

            finalized <- context
                .finalizeContext(
                  protocolParams = env.protocolParams,
                  diffHandler = new ChangeOutputDiffHandler(
                    env.protocolParams,
                    0
                  ).changeOutputDiffHandler,
                  evaluator = env.evaluator,
                  validators = validators
                )

        } yield DeinitTx(
          treasuryUtxoSpent = treasuryUtxo,
          initializerOutput = initializerOutput,
          tx = finalized.transaction
        )
    }
}
