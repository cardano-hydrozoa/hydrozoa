package hydrozoa.rulebased.ledger.l1.tx

import hydrozoa.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{build, finalizeContext}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.ledger.commitment.Membership
import hydrozoa.multisig.ledger.joint.{EvacuationMap, evacuationKeyOrdering}
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx.Builder.explainConst
import hydrozoa.multisig.ledger.l1.tx.{EnrichedTx, TxFamily}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer, given}
import hydrozoa.rulebased.ledger.l1.script.plutus.SetupLadder
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.{RuleBasedTreasuryDatum, evacuate}
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx.Assumptions
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTxOps.Build.Error.BuilderError
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedRegimeUtxo, RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import monocle.*
import scala.annotation.tailrec
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionException.{ExUnitsExceedMaxException, InvalidTransactionSizeException}
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{ReferenceOutput, Send}

final case class EvacuationTx(
    treasuryUtxoSpent: RuleBasedTreasuryUtxo,
    treasuryUtxoProduced: RuleBasedTreasuryUtxo,
    evacuatedOutputs: List[TransactionOutput],
    override val tx: Transaction,
    override val txLens: Lens[EvacuationTx, Transaction] = Focus[EvacuationTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos
) extends EnrichedTx[EvacuationTx] {}

object EvacuationTx {
    given TxFamily[EvacuationTx] = TxFamily.of("EvacuationTx")
    export EvacuationTxOps.{Build, Config}

    object Assumptions {
        /* Hardcoding this until we genuinely want to test that the whole setup works with other values.
         * Also, the whole KZG setup should be wrapped into something higher-level soon.
         */
        val maxEvacuationsPerTx = 64
    }

}

private object EvacuationTxOps {
    type Config = HeadConfig.Bootstrap.Section & NodeOperationEvacuationConfig.Section

    object Build {
        enum Error extends Throwable:
            case InvalidTreasuryDatum(msg: String)
            case TreasuryNotResolved
            case InsufficientTreasuryFunds(negativeDiff: Value)
            case NoEvacuatees
            case NotASubset(evacuatees: EvacuationMap, evaucationMap: EvacuationMap)
            case MembershipError(wrapped: Membership.MembershipCheckError)
            case SetupLadderError(wrapped: SetupLadder.UncoveredEvacuationCount)
            case BuilderError(wrapped: (SomeBuildError, String))

            override def toString: String = getMessage

            override def getMessage: String = this match
                case InvalidTreasuryDatum(msg) =>
                    s"Invalid treasury datum: $msg"
                case TreasuryNotResolved =>
                    "Treasury datum is not resolved"
                case InsufficientTreasuryFunds(negativeDiff) =>
                    s"Insufficient treasury funds, negative difference: $negativeDiff"
                case NoEvacuatees =>
                    "No evacuatees provided"
                case NotASubset(evacuatees, evacuationMap) =>
                    "Evacuatees are not a subset of the evacuation map." +
                        s"Evacuatees - EvacuationMap = ${evacuatees.evacuationMap.keySet
                                -- evacuationMap.evacuationMap.keySet}"
                case MembershipError(wrapped) =>
                    s"Membership error: ${wrapped}"
                case SetupLadderError(wrapped) =>
                    s"Setup ladder error: ${wrapped.getMessage}"
                case BuilderError((buildError, explanation)) =>
                    s"Builder error: $explanation - $buildError"
    }

    /** @param evacuateesToTryNext
      *   The sub-map of evacuations to try in this transaction
      * @param allRemainingEvacuatees
      *   The map of all evacuations that still need to be processed
      * @param collateralUtxo
      *   Wallet UTxO used both as Plutus collateral and as the fee-paying regular input; the
      *   returned collateral output absorbs the fee via the diff handler, matching the pattern used
      *   by [[VoteTx]], [[ResolutionTx]], [[DeinitTx]] et al.
      */
    final case class Build(
        inputTreasuryUtxo: RuleBasedTreasuryUtxo,
        regimeUtxo: RuleBasedRegimeUtxo,
        evacuateesToTryNext: EvacuationMap,
        allRemainingEvacuatees: EvacuationMap,
        collateralUtxo: CollateralUtxo,
    ) {

        private def halveEvacuation(evacuatees: EvacuationMap): EvacuationMap = {
            EvacuationMap(evacuatees.evacuationMap.drop(evacuatees.size / 2))
        }

        /** Current strategy (implementation detail, can be swapped out):
          *   - Try to evacuate all evacuatees
          *   - If we fail due to exunits or tx size, try with half.
          */
        def result(using config: Config): Either[Build.Error, EvacuationTx] = for {

            // "loop" requires that the subset actually be a subset. We establish this once before looping, and
            // rely on `halveEvacuation` to maintain this property
            _ <- Either.cond(
              test = evacuateesToTryNext.evacuationMap.keySet
                  .subsetOf(allRemainingEvacuatees.evacuationMap.keySet),
              right = (),
              left = EvacuationTx.Build.Error.NotASubset(
                evacuatees = evacuateesToTryNext,
                evaucationMap = allRemainingEvacuatees
              )
            )

            inputTreasuryDatum <- extractTreasuryDatum(inputTreasuryUtxo)

            res <- {
                given Resolved = inputTreasuryDatum
                loop(evacuateesToTryNext)
            }
        } yield res

        @tailrec
        private def loop(
            evacuatees: EvacuationMap
        )(using config: Config, treasuryDatum: Resolved): Either[Build.Error, EvacuationTx] =
            (for {
                // Recursion termination condition
                _ <- Either.cond(
                  test = evacuatees.nonEmpty,
                  right = (),
                  left = EvacuationTx.Build.Error.NoEvacuatees
                )

                // The subset we will actually evacuate in this transaction
                // (capped by [[EvacutionTx.Assumptions.maxEvacuationsPerTx]]).
                // All subsequent computations (membership proof, outputs, residual) must use this
                // same subset so that the on-chain value invariant holds:
                //   treasuryInput = treasuryOutput + Σ evacuationOutput
                evacuatingNow = EvacuationMap(
                  evacuatees.evacuationMap.take(Assumptions.maxEvacuationsPerTx)
                )

                membershipProof <- Membership
                    .mkMembershipProofValidated(
                      set = allRemainingEvacuatees,
                      subset = evacuatingNow
                    )
                    .left
                    .map(Build.Error.MembershipError(_))

                // The smallest setup-ladder rung covering this batch (recomputed per halving
                // retry, since the batch size changes).
                setupRungUtxo <- config
                    .setupRungUtxo(evacuatingNow.size)
                    .left
                    .map(Build.Error.SetupLadderError(_))

                // Reference inputs are canonically ordered in the tx body, so the redeemer index
                // of the rung is its position within the sorted set of all reference inputs.
                setupRefInputIdx = BigInt(
                  List(
                    config.rulebasedTreasuryScriptInput,
                    regimeUtxo.input,
                    setupRungUtxo.input
                  ).sorted.indexOf(setupRungUtxo.input)
                )

                // From this point we should choose and stick to a particular order of withdrawals, so
                // the order of outputs in the tx (starting from index 1) and the order of utxo ids in
                // the redeemer should be the same.
                evacuationList = evacuatingNow.evacuationMap.toList

                evacuationRedeemer = TreasuryRedeemer.Evacuate(
                  EvacuateRedeemer(
                    evacuationKeys = SList.from(
                      evacuationList.map(_._1)
                    ),
                    proof = membershipProof,
                    setupRefInputIdx = setupRefInputIdx
                  )
                )

                newTreasuryDatum = treasuryDatum.evacuate(membershipProof)

                // evacuation outputs
                evacuationOutputs = evacuationList.map(_._2.utxo.value)

                residualValue <- calculateResidualTreasury(
                  inputTreasuryUtxo,
                  evacuatingNow
                )

                /////////////
                // Steps

                residualTreasury = RuleBasedTreasuryOutput(
                  newTreasuryDatum,
                  residualValue
                )

                context <- build(
                  List(
                    config.referenceTreasury,
                    // The treasury validator authenticates the rung against the ladder anchor
                    // in the regime datum
                    regimeUtxo.referenceOutput,
                    ReferenceOutput(setupRungUtxo),
                    inputTreasuryUtxo.spendAttached(evacuationRedeemer),
                    // Spend the collateral utxo as a regular input; the returned
                    // collateral output absorbs the tx fee via the index-0 diff handler.
                    collateralUtxo.add,
                    collateralUtxo.spend,
                    collateralUtxo.collateralOutput.send,
                    residualTreasury.send
                  )
                      ++
                          // Outputs for withdrawals
                          evacuationOutputs.map(Send(_))
                )
                    .explainConst("Building evacuation tx failed")
                    .left
                    .map(BuilderError(_))

                finalize = context
                    .finalizeContext(
                      diffHandler = Change
                          .changeOutputDiffHandler(_, _, config.cardanoInfo.protocolParams, 0),
                      validators = EnrichedTx.Validators.nonSigningValidators
                    )
                    .explainConst(
                      "Finalizing evacuation tx failed. Failed context txid :" +
                          s"\n\n${context.transaction.id}"
                    )
                    .left
                    .map(BuilderError(_))

                finalized <- finalize

                newTreasuryUtxo = RuleBasedTreasuryUtxo(
                  utxoId = TransactionInput(
                    finalized.transaction.id,
                    0
                  ),
                  residualTreasury
                )

                evacuationTx = EvacuationTx(
                  treasuryUtxoSpent = inputTreasuryUtxo,
                  treasuryUtxoProduced = newTreasuryUtxo,
                  evacuatedOutputs = evacuationOutputs,
                  tx = finalized.transaction,
                  resolvedUtxos = finalized.resolvedUtxos
                )
            } yield evacuationTx) match {
                case Right(w) => Right(w)
                // When the tx is too large or exceeds ex-unit limits, retry with a smaller batch.
                // TODO: non-size errors (e.g. value conservation failures from the balancer adding
                //   ADA when minAda in the residual treasury is too small) also end up here and
                //   cause infinite halving down to a batch of 1. Root cause: the resolution tx does
                //   not reserve enough ADA in the treasury output. Fix there and restore Left(e).
                case Left(
                      BuilderError(
                        SomeBuildError.ValidationError(_: InvalidTransactionSizeException, _),
                        _
                      )
                    ) =>
                    loop(halveEvacuation(evacuatees))
                case Left(
                      BuilderError(
                        SomeBuildError.ValidationError(_: ExUnitsExceedMaxException, _),
                        _
                      )
                    ) =>
                    loop(halveEvacuation(evacuatees))
                case Left(e) => Left(e)
            }

        // TODO: Make method on RuleBasedTreasuryUtxo
        def extractTreasuryDatum(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): Either[Build.Error.TreasuryNotResolved.type, Resolved] = {

            treasuryUtxo.treasuryOutput.datum match {
                case resolved: RuleBasedTreasuryDatum.Resolved => Right(resolved)
                case _: RuleBasedTreasuryDatum.Unresolved => Left(Build.Error.TreasuryNotResolved)
            }
        }

        // TODO: Make method on RuleBasedTreasuryUtxo
        private def calculateResidualTreasury(
            treasuryUtxo: RuleBasedTreasuryUtxo,
            evacuatees: EvacuationMap
        ): Either[Build.Error.InsufficientTreasuryFunds, Value] = {

            val treasuryValue = treasuryUtxo.treasuryOutput.value
            val residueValue = treasuryValue - evacuatees.totalValue

            if residueValue.isPositive
            then Right(residueValue)
            else Left(Build.Error.InsufficientTreasuryFunds(residueValue))
        }
    }
}
