package hydrozoa.rulebased.ledger.l1.tx

import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.multisig.ledger.commitment.Membership
import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.ledger.l1.tx.Tx
import hydrozoa.multisig.ledger.l1.tx.Tx.Builder.explainConst
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer, given}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.{ResolvedDatum, RuleBasedTreasuryDatum}
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTxOps.Build.Error.BuilderError
import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedTreasuryUtxo
import monocle.*
import scala.annotation.tailrec
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.TransactionException.{ExUnitsExceedMaxException, InvalidTransactionSizeException}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.ScriptSource.PlutusScriptValue
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{AddCollateral, Send, Spend}
import scalus.uplc.builtin.Data.toData

final case class EvacuationTx(
    treasuryUtxoSpent: RuleBasedTreasuryUtxo,
    treasuryUtxoProduced: RuleBasedTreasuryUtxo,
    evacuatedOutputs: List[TransactionOutput],
    override val tx: Transaction,
    override val txLens: Lens[EvacuationTx, Transaction] = Focus[EvacuationTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
) extends Tx[EvacuationTx]

object EvacuationTx {
    export EvacuationTxOps.{Build, Config}

}

private object EvacuationTxOps {
    type Config = CardanoNetwork.Section & NodeOperationEvacuationConfig.Section

    object Build {
        enum Error:
            case InvalidTreasuryDatum(msg: String)
            case TreasuryNotResolved
            case InsufficientTreasuryFunds(negativeDiff: Value)
            case NoEvacuatees
            case NotASubset(evacuatees: EvacuationMap, evaucationMap: EvacuationMap)
            case MembershipError(wrapped: Membership.MembershipCheckError)
            case BuilderError(wrapped: (SomeBuildError, String))

            // TODO
            // override def toString: String = ???
    }

    /** @param evacuatees
      *   The sub-map of evacuations to try in this transaction
      * @param notEvacuatedYet
      *   The map of all evacuations that still need to be processed
      * @param feeUtxos
      *   Utxos to use to pay the fees
      */
    final case class Build(config: Config)(
        _treasuryUtxo: RuleBasedTreasuryUtxo,
        _evacuatees: EvacuationMap,
        _notEvacuatedYet: EvacuationMap,
        _collateralUtxo: Utxo,
        _feeUtxos: Utxos,
    ) {

        // If I don't do these here, I can't access them directly from a `build : Build`. Why?
        val treasuryUtxo: RuleBasedTreasuryUtxo = _treasuryUtxo
        val evacuatees: EvacuationMap = _evacuatees
        val notEvacuatedYet: EvacuationMap = _notEvacuatedYet
        val feeUtxos: Utxos = _feeUtxos
        val collateralUtxo: Utxo = _collateralUtxo

        private def halveEvacuation(evacuatees: EvacuationMap): EvacuationMap = {
            EvacuationMap(evacuatees.evacuationMap.drop(evacuatees.size / 2))
        }

        def result: Either[Build.Error, EvacuationTx] = loop(evacuatees)

        @tailrec
        private def loop(evacuatees: EvacuationMap): Either[Build.Error, EvacuationTx] =
            (for {
                _ <-
                    if evacuatees.nonEmpty then Right(())
                    else Left(EvacuationTx.Build.Error.NoEvacuatees)
                _ <-
                    if evacuatees.evacuationMap.keySet
                            .subsetOf(notEvacuatedYet.evacuationMap.keySet)
                    then Right(())
                    else
                        Left(
                          EvacuationTx.Build.Error.NotASubset(
                            evacuatees = evacuatees,
                            evaucationMap = notEvacuatedYet
                          )
                        )

                // TODO: This can be hoisted outside the loop
                treasuryDatum <- extractTreasuryDatum(treasuryUtxo)

                membershipProof <- Membership
                    .mkMembershipProofValidated(
                      set = notEvacuatedYet,
                      subset = evacuatees
                    )
                    .left
                    .map(Build.Error.MembershipError(_))

                // From this point we should choose and stick to a particular order of withdrawals, so
                // to order of outputs in the tx (starting from index 1) and the order of utxo ids in
                // the redeemer should be the same.
                evacuationList = evacuatees.evacuationMap.toList

                evacuationRedeemer = TreasuryRedeemer.Evacuate(
                  EvacuateRedeemer(
                    evacuationKeys = SList.from(
                      evacuationList.map(_._1)
                    ),
                    proof = membershipProof
                  )
                )

                newTreasuryDatum =
                    RuleBasedTreasuryDatum.Resolved(
                      treasuryDatum.copy(evacuationActive = membershipProof)
                    )

                // evacuation outputs
                evacuationOutputs = evacuationList.map(_._2.utxo.value)

                residualValue <- calculateResidualTreasury(
                  treasuryUtxo,
                  evacuatees
                )

                /////////////
                // Steps

                // TODO: I guess this can be lifted out of the loop
                // Spend the treasury utxo with withdrawal proof
                spendTreasury = Spend(
                  treasuryUtxo.asUtxo,
                  ThreeArgumentPlutusScriptWitness(
                    PlutusScriptValue(RuleBasedTreasuryScript.compiledPlutusV3Script),
                    evacuationRedeemer.toData,
                    DatumInlined,
                    Set.empty
                  )
                )

                // TODO: also can be lifted out
                addCollateral = AddCollateral(collateralUtxo)

                // Create the empty change utxo
                sendChangeUtxo = Send(
                  Babbage(
                    address = config.evacuationWallet.address(config.network),
                    value = Value.zero,
                    datumOption = None,
                    scriptRef = None
                  )
                )

                sendResidualTreasury = Send(
                  Babbage(
                    address = treasuryUtxo.address,
                    value = residualValue,
                    datumOption = Some(Inline(newTreasuryDatum.toData)),
                    scriptRef = None
                  )
                )

                context <- TransactionBuilder
                    .build(
                      config.cardanoInfo.network,
                      List(spendTreasury, addCollateral, sendChangeUtxo, sendResidualTreasury)
                          ++
                              // Outputs for withdrawals
                              evacuationOutputs.map(Send(_))
                              // Spend the fee utxos
                              ++ feeUtxos.toList.map((ti, to) =>
                                  Spend(scalus.cardano.ledger.Utxo(ti, to), PubKeyWitness)
                              )
                    )
                    .explainConst("Building evacuation tx failed")
                    .left
                    .map(BuilderError(_))

                finalized <- context
                    .finalizeContext(
                      protocolParams = config.cardanoInfo.protocolParams,
                      diffHandler = Change
                          .changeOutputDiffHandler(_, _, config.cardanoInfo.protocolParams, 0),
                      evaluator = PlutusScriptEvaluator(config.cardanoInfo, EvaluateAndComputeCost),
                      validators = Tx.Validators.nonSigningValidators
                    )
                    .explainConst("Finalizing evacuation tx failed")
                    .left
                    .map(BuilderError(_))

                newTreasuryUtxo = RuleBasedTreasuryUtxo(
                  utxoId = TransactionInput(
                    finalized.transaction.id,
                    0
                  ),
                  address = treasuryUtxo.address,
                  datum = treasuryUtxo.datum,
                  value = residualValue
                )

                withdrawTx: EvacuationTx = EvacuationTx(
                  treasuryUtxo,
                  newTreasuryUtxo,
                  evacuationOutputs,
                  finalized.transaction
                )
            } yield withdrawTx) match {
                case Right(w) => Right(w)
                case Left(
                      BuilderError(
                        SomeBuildError.ValidationError(e: InvalidTransactionSizeException, ctx),
                        _
                      )
                    ) =>
                    loop(halveEvacuation(evacuatees))
                case Left(
                      BuilderError(
                        SomeBuildError.ValidationError(e: ExUnitsExceedMaxException, ctx),
                        _
                      )
                    ) =>
                    loop(halveEvacuation(evacuatees))
                case Left(e) => Left(e)
            }

        def extractTreasuryDatum(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): Either[Build.Error.TreasuryNotResolved.type, ResolvedDatum] = {

            treasuryUtxo.datum match {
                case RuleBasedTreasuryDatum.Resolved(resolved) => Right(resolved)
                case RuleBasedTreasuryDatum.Unresolved(_) => Left(Build.Error.TreasuryNotResolved)
            }
        }

        private def calculateResidualTreasury(
            treasuryUtxo: RuleBasedTreasuryUtxo,
            evacuatees: EvacuationMap
        ): Either[Build.Error.InsufficientTreasuryFunds, Value] = {

            val treasuryValue = treasuryUtxo.value
            val residueValue = treasuryValue - evacuatees.totalValue

            if residueValue.isPositive
            then Right(residueValue)
            else Left(Build.Error.InsufficientTreasuryFunds(residueValue))
        }
    }
}
