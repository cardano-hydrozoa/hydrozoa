package hydrozoa.rulebased.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum.{Resolved, Unresolved}
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import scala.collection.immutable.SortedMap
import scalus.builtin.ByteString.hex
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.{NativeScriptValue, PlutusScriptValue}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, *}

final case class DeinitTx(
    treasuryUtxoSpent: RuleBasedTreasuryUtxo,
    tx: Transaction
)

/** The deinit tx spends an empty (i.e. not containing any l2 utxos) treasury utxo, distributing the
  * residual _head equity_ according to peers' shares. If a share happens to be less than min ada,
  * it goes for the fees.
  *
  * Since the treasury is locked at the Plutus script it requires the collateral. This collateral is
  * used for fees as well, which simplifies building - we don't need to subtract fees from the
  * treasury.
  *
  * When it comes to multi-signing, all nodes cannot be built _exactly_ the same transaction since
  * every will use their own collateral. This should be addressed when implementing automatic
  * signing if we decide to have it, for now we expect this operation to be done manually.
  *
  * All head tokens under the head's policy id (and only those) should be burnt.
  */
object DeinitTx {

    case class Recipe(
        headNativeScript: HeadMultisigScript,
        treasuryUtxo: RuleBasedTreasuryUtxo,
        defaultVoteDeposit: Coin,
        voteDeposit: Coin,
        collateralUtxo: Utxo,
        env: CardanoInfo,
        evaluator: PlutusScriptEvaluator,
        validators: Seq[Validator]
    )

    enum DeinitTxError:
        case TreasuryShouldBeResolved
        case TreasuryShouldBeEmpty
        case NoHeadTokensFound

    import DeinitTxError.*

    def build(recipe: Recipe): Either[SomeBuildError | DeinitTxError, DeinitTx] = {
        import recipe.*

        val policyId = headNativeScript.policyId

        for {
            _ <- checkTreasury(treasuryUtxo)

            headTokens <- extractHeadTokens(policyId, treasuryUtxo)

            result <- buildDeinitTx(recipe, headTokens)
        } yield result
    }

    private def checkTreasury(
        treasury: RuleBasedTreasuryUtxo
    ): Either[DeinitTxError, Unit] =

        // TODO use G1.generatorCompressed once it's here
        val g1bs =
            hex"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"

        for {
            resolved <- treasury.datum match {
                case Unresolved(_) => Left(TreasuryShouldBeResolved)
                case Resolved(d)   => Right(d)
            }
            _ <- Either.cond(resolved.utxosActive == g1bs, (), TreasuryShouldBeEmpty)

        } yield ()

    private def extractHeadTokens(
        policyId: PolicyId,
        treasuryUtxo: RuleBasedTreasuryUtxo
    ): Either[DeinitTxError, SortedMap[AssetName, Long]] = {
        treasuryUtxo.value.assets.assets.get(policyId) match {
            case Some(headTokens) =>
                if headTokens.nonEmpty
                then Right(headTokens)
                else Left(NoHeadTokensFound)
            case None => Left(NoHeadTokensFound)
        }
    }

    private def buildDeinitTx(
        recipe: Recipe,
        headTokens: SortedMap[AssetName, Long]
    ): Either[SomeBuildError | DeinitTxError, DeinitTx] = {
        import recipe.*

        val policyId = recipe.headNativeScript.policyId

        for {
            context <- TransactionBuilder
                .build(
                  env.network,
                  List(
                    // Spend the treasury utxo
                    Spend(
                      Utxo(treasuryUtxo.asTuple._1, treasuryUtxo.asTuple._2),
                      ThreeArgumentPlutusScriptWitness(
                        PlutusScriptValue(RuleBasedTreasuryScript.compiledPlutusV3Script),
                        TreasuryRedeemer.Deinit.toData,
                        DatumInlined,
                        Set.empty
                      )
                    ),
                    // Fees are covered by the collateral to simplify the balancing
                    Spend(collateralUtxo, PubKeyWitness),
                    AddCollateral(collateralUtxo),
                    // Send collateral back as the first output
                    Send(collateralUtxo.output)
                  )
                  // Burn head tokens
                      ++ headTokens.map((assetName, amount) =>
                          Mint(
                            scriptHash = policyId,
                            assetName = assetName,
                            amount = -amount,
                            witness = NativeScriptWitness(
                              NativeScriptValue(headNativeScript.script),
                              headNativeScript.requiredSigners
                            )
                          )
                      )
                )

            finalized <- context
                .finalizeContext(
                  protocolParams = env.protocolParams,
                  diffHandler = Change.changeOutputDiffHandler(
                    _,
                    _,
                    env.protocolParams,
                    0
                  ), // the collateral sent back
                  evaluator = evaluator,
                  validators = validators
                )

        } yield DeinitTx(
          treasuryUtxoSpent = treasuryUtxo,
          tx = finalized.transaction
        )
    }
}
