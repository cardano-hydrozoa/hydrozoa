package hydrozoa.bootstrap

import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, InitializationTxEndTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.addExpectedSigners
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx.Builder.{BuilderResultSimple, explainConst}
import hydrozoa.multisig.ledger.l1.tx.Metadata.Initialization
import hydrozoa.multisig.ledger.l1.tx.{EnrichedTx, InitializationTx}
import hydrozoa.multisig.ledger.l1.utxo.{Equity, MultisigRegimeOutput, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, ModifyAuxiliaryData, Send, Spend, ValidityEndSlot}
import scalus.cardano.txbuilder.TxBalancingError.InsufficientFunds
import scalus.uplc.builtin.Data.toData

/** Builds the head's initialization transaction. This lives in the `hydrozoa.bootstrap` package
  * because building the init tx is a bootstrapping concern — the running head only ever *parses* it
  * (see [[InitializationTx.Parse]]).
  *
  * Output ordering:
  *   - Treasury Utxo
  *   - Multisig Regime Utxo
  *   - Change outputs
  */
object InitializationTxBuilder {

    type Config = CardanoNetwork.Section & HeadPeers.Section & FallbackContingency.Section &
        TxTiming.Section & InitializationParameters.Section

    final case class Build(config: Config)(blockCreationEndTime: BlockCreationEndTime) {

        lazy val result: BuilderResultSimple[InitializationTx] = for {
            _ <- Either
                .cond(
                  config.isBalancedInitializationFunding,
                  (),
                  SomeBuildError.BalancingError(
                    TxBalancingError.Failed(new IllegalArgumentException),
                    TransactionBuilder.Context.empty(networkId = config.network)
                  )
                )
                .explainConst(
                  "Initialization tx funding is unbalanced. We must have" +
                      "\n\tconfig.initialFundingValue == config.initialL2Value " +
                      "+ Value(config.initialEquityContributed " +
                      "+ config.totalFallbackContingency)" +
                      ""
                )

            unbalanced <- {
                TransactionBuilder
                    .build(config.network, Steps())
                    .explainConst("Initialization tx build steps failed.")
            }

            finalized <- {
                TxBuilder
                    .finalizeContext(
                      unbalanced.addExpectedSigners(config.headMultisigScript.numSigners)
                    )
                    .explainConst("Initialization tx failed to finalize")
            }

            completed <- Complete(finalized)
        } yield completed

        object Steps {
            def apply(): List[TransactionBuilderStep] =
                Base() ++ Spends() ++ Mints() ++ Sends()

            object Base {
                def apply(): List[TransactionBuilderStep] =
                    List(modifyAuxiliaryData, validityEndSlot)

                private val modifyAuxiliaryData =
                    ModifyAuxiliaryData(_ =>
                        Some(
                          Initialization(
                            multisigTreasuryIx = 0,
                            multisigRegimeIx = 1,
                            seedIx = config.initialSeedIx,
                            totalEquity = config.initialEquityContributed
                          ).asAuxData(config.headId)
                        )
                    )

                private[bootstrap] val initializationTxEndTime: InitializationTxEndTime =
                    config.txTiming.initializationEndTime(blockCreationEndTime)

                private val validityEndSlot = ValidityEndSlot(
                  initializationTxEndTime.toSlot.slot
                )
            }

            object Mints {
                def apply(): List[Mint] = List(mintTreasuryToken, mintMultisigRegimeToken)

                private val mintTreasuryToken = Mint(
                  config.headMultisigScript.script.scriptHash,
                  config.headTokenNames.treasuryTokenName,
                  1,
                  config.headMultisigScript.witnessValue
                )

                private val mintMultisigRegimeToken = Mint(
                  config.headMultisigScript.script.scriptHash,
                  config.headTokenNames.multisigRegimeTokenName,
                  1,
                  config.headMultisigScript.witnessAttached
                )
            }

            object Spends {
                def apply(): List[Spend] = config.initialFundingUtxos.iterator
                    .map(kv => Spend(Utxo(kv), PubKeyWitness))
                    .toList
            }

            object Sends {
                def apply(): List[Send] =
                    List(Treasury(), MultisigRegimeOutput.send(using config)) ++ ChangeOutputs()

                object Treasury {
                    def apply(): Send = Send(treasuryOutput)

                    private val treasuryValueUnbalanced: Value =
                        config.initialL2Value + Value(config.initialEquityContributed) +
                            Value.asset(
                              config.headMultisigScript.policyId,
                              config.headTokenNames.treasuryTokenName,
                              1L
                            )

                    private[bootstrap] val treasuryDatum =
                        MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum(
                          config.initialEvacuationMap
                        )

                    private val treasuryOutput = Babbage(
                      config.headMultisigAddress,
                      treasuryValueUnbalanced,
                      Some(Inline(treasuryDatum.toData))
                    )
                }

                object ChangeOutputs {
                    def apply(): List[Send] = config.initialChangeOutputs.map(Send.apply)
                }
            }

        }

        private object Complete {
            def apply(
                finalized: TransactionBuilder.Context
            ): BuilderResultSimple[InitializationTx] =
                val treasuryIndex = 0
                val multisigRegimeIndex = 1

                val treasuryValue =
                    finalized.transaction.body.value.outputs(treasuryIndex).value.value

                val multisigRegimeProduced = MultisigRegimeUtxo(
                  input = TransactionInput(finalized.transaction.id, multisigRegimeIndex),
                )

                val equityCoin =
                    config.initialEquityContributed - finalized.transaction.body.value.fee

                for {
                    equity <- Equity(equityCoin)
                        .toRight(
                          SomeBuildError.BalancingError(
                            InsufficientFunds(
                              Value(equityCoin),
                              finalized.transaction.body.value.fee.value
                            ),
                            finalized
                          )
                        )
                        .explainConst(
                          s"There is not enough equity (${config.initialEquityContributed}) to pay for the" +
                              s" initialization tx fee of ${finalized.transaction.body.value.fee}"
                        )
                    treasuryProduced = MultisigTreasuryUtxo(
                      treasuryTokenName = config.headTokenNames.treasuryTokenName,
                      utxoId = TransactionInput(finalized.transaction.id, treasuryIndex),
                      address = config.headMultisigAddress,
                      datum = Steps.Sends.Treasury.treasuryDatum,
                      value = treasuryValue,
                      equity = equity
                    )

                } yield new InitializationTx(
                  initializationTxEndTime = Steps.Base.initializationTxEndTime,
                  treasuryProduced = treasuryProduced,
                  tx = finalized.transaction,
                  multisigRegimeProduced = multisigRegimeProduced,
                  headTokenNames = config.headTokenNames,
                  resolvedUtxos = finalized.resolvedUtxos,
                  seedUtxo = config.initializationParameters.seedUtxo
                )
        }

        private object TxBuilder {
            private val diffHandler = Change.changeOutputDiffHandler(
              _,
              _,
              protocolParams = config.cardanoProtocolParams,
              changeOutputIdx = 0
            )

            def finalizeContext(
                ctx: TransactionBuilder.Context
            ): Either[SomeBuildError, TransactionBuilder.Context] =
                ctx.finalizeContext(
                  config.cardanoProtocolParams,
                  diffHandler = diffHandler,
                  evaluator = config.plutusScriptEvaluatorForTxBuild,
                  validators = EnrichedTx.Validators.nonSigningNonValidityChecksValidators
                )
        }
    }
}
