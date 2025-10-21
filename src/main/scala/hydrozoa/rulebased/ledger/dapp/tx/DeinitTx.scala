package hydrozoa.rulebased.ledger.dapp.tx

import cats.implicits.*
import hydrozoa.*
import hydrozoa.config.EquityShares
import hydrozoa.lib.tx.*
import hydrozoa.lib.tx.Datum.DatumInlined
import hydrozoa.lib.tx.ScriptSource.{NativeScriptValue, PlutusScriptValue}
import hydrozoa.lib.tx.TransactionBuilderStep.{Mint, *}
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.TreasuryRedeemer
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum.{
    Resolved,
    Unresolved
}
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import scala.collection.immutable.SortedMap
import scalus.builtin.ByteString.hex
import scalus.builtin.Data.toData
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.Environment
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.ledger.{Utxo as _, *}

final case class DeinitTx(
    treasuryUtxoSpent: RuleBasedTreasuryUtxo,
    equityOutputs: List[TransactionOutput],
    tx: Transaction
)

/** The deinit tx spends an empty (i.e. not containing any l2 utxos) treasury utxo, distributing the
  * residual _head equity_ according to peers' shares. If a share happens to be less than min ada,
  * it goes for the fees.
  *
  * Since the treasury is locked at the Plutus script it requires the collateral. It's used for fees
  * as well, which simplifies the building - we don't need to subtract fees from the treasury.
  *
  * When it comes to multi-signing, all nodes cannot be built _exactly_ the same transaction since
  * every will use their own collateral. This should be addressed when implementing automatic
  * signing if we decide to have it.
  *
  * All head tokens under the head's policy id (and only those) should be burnt.
  */
object DeinitTx {

    case class Recipe(
        headNativeScript: HeadMultisigScript,
        treasuryUtxo: RuleBasedTreasuryUtxo,
        shares: EquityShares,
        collateralUtxo: Utxo[L1],
        env: Environment,
        validators: Seq[Validator]
    )

    enum DeinitTxError:
        case TreasuryShouldBeResolved
        case TreasuryShouldBeEmpty
        case NoHeadTokensFound

    import DeinitTxError.*

    def build(recipe: Recipe): Either[SomeBuildError | DeinitTxError, DeinitTx] = {
        val treasuryUtxo = recipe.treasuryUtxo
        val policyId = recipe.headNativeScript.policyId

        for {
            _ <- checkTreasury(treasuryUtxo)

            headTokens <- extractHeadTokens(policyId, treasuryUtxo)

            equityOutputs = mkEquityProduced(
              treasuryUtxo.value.coin,
              recipe.shares,
              recipe.env.protocolParams
            )

            result <- buildDeinitTx(recipe, equityOutputs.outputs, equityOutputs.dust, headTokens)
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

    case class EquityProduced(
        outputs: List[Babbage],
        dust: Option[Coin]
    )

    object EquityProduced:
        def apply(dust: Coin): EquityProduced =
            EquityProduced(List.empty, Option.when(dust < Coin.zero)(dust))

    private def mkEquityProduced(
        treasuryEquity: Coin,
        shares: EquityShares,
        params: ProtocolParams
    ): EquityProduced =

        val minAda0 = minAda(params)

        val distribution = shares.distribute(treasuryEquity)
        val initial = EquityProduced(distribution.dust)

        distribution.shares.foldLeft(initial)((acc, share) => {
            val output =
                Babbage(
                  address = share._1,
                  value = Value.apply(share._2),
                  datumOption = None,
                  scriptRef = None
                )
            if share._2 >= minAda0(output)
            then EquityProduced(acc.outputs :+ output, acc.dust)
            else EquityProduced(acc.outputs, Some(share._2 + acc.dust.getOrElse(Coin.zero)))
        })

    private def minAda(params: ProtocolParams)(output: TransactionOutput) =
        MinCoinSizedTransactionOutput(Sized(output), params)

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
        equityOutputs: List[Babbage],
        mbEquityFees: Option[Coin],
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
                      TransactionUnspentOutput(treasuryUtxo.toUtxo),
                      ThreeArgumentPlutusScriptWitness(
                        PlutusScriptValue(RuleBasedTreasuryScript.compiledPlutusV3Script),
                        TreasuryRedeemer.Deinit.toData,
                        DatumInlined,
                        Set.empty
                      )
                    ),
                    // Fees are covered by the collateral to simplify the balancing
                    Spend(TransactionUnspentOutput.fromUtxo(collateralUtxo), PubKeyWitness),
                    AddCollateral(TransactionUnspentOutput.fromUtxo(collateralUtxo)),
                    // Send collateral back as the first output
                    Send(collateralUtxo.output)
                  )
                  // Possible fees from equity sares that are < minAda
                      ++ mbEquityFees.toList.map(Fee(_))
                      // Equity shares outputs
                      ++ equityOutputs.map(o => Send(o))
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
                  diffHandler = new ChangeOutputDiffHandler(
                    env.protocolParams,
                    0 // the collateral sent back
                  ).changeOutputDiffHandler,
                  evaluator = env.evaluator,
                  validators = validators
                )

        } yield DeinitTx(
          treasuryUtxoSpent = treasuryUtxo,
          equityOutputs = equityOutputs,
          tx = finalized.transaction
        )
    }
}
