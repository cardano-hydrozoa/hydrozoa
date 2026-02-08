package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.HeadConfig
import hydrozoa.ensureMinAda
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Metadata.Fallback
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.{RuleBasedTreasuryDatum, UnresolvedDatum}
import hydrozoa.rulebased.ledger.dapp.state.VoteDatum as VD
import hydrozoa.rulebased.ledger.dapp.state.VoteState.VoteDatum
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import monocle.{Focus, Lens}
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Mint as _, *}
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.ScriptSource.NativeScriptAttached
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import scalus.ledger.api.v1.PubKeyHash
import scalus.prelude.List as SList

final case class FallbackTx(
    override val validityStart: QuantizedInstant,
    override val treasurySpent: MultisigTreasuryUtxo,
    override val treasuryProduced: RuleBasedTreasuryUtxo,
    override val multisigRegimeUtxoSpent: MultisigRegimeUtxo,
    override val tx: Transaction,
    override val txLens: Lens[FallbackTx, Transaction] = Focus[FallbackTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos
) extends HasValidityStart,
      MultisigTreasuryUtxo.Spent,
      MultisigRegimeUtxo.Spent,
      RuleBasedTreasuryUtxo.Produced,
      Tx[FallbackTx] {}

object FallbackTx {
    export FallbackTxOps.Build
}

private object FallbackTxOps {
    type Config = HeadConfig.Preinit.Section

    final case class Build(config: Config)(
        validityStartTime: QuantizedInstant,
        treasuryUtxoSpent: MultisigTreasuryUtxo,
        multisigRegimeUtxo: MultisigRegimeUtxo,
    ) {
        private val hns = config.headMultisigScript

        lazy val result: Either[SomeBuildError, FallbackTx] = for {
            unbalanced <- TransactionBuilder.build(config.network, Steps())

            finalized <- TxBuilder.finalizeContext(unbalanced)

            completed = Complete(finalized)
        } yield completed

        object Steps {
            def apply(): List[TransactionBuilderStep] = Base() ++ Spends() ++ Mints() ++ Sends()

            object Base {
                def apply(): List[TransactionBuilderStep] =
                    List(modifyAuxiliaryData, validityStartSlot)

                val modifyAuxiliaryData = ModifyAuxiliaryData(_ =>
                    Some(MD.apply(Fallback(config.ruleBasedTreasuryAddress)))
                )

                val validityStartSlot = ValidityStartSlot(validityStartTime.toSlot.slot)
            }

            object Spends {
                def apply(): List[Spend] = List(Treasury(), MultisigRegime())

                object Treasury {
                    def apply() = Spend(
                      treasuryUtxoSpent.asUtxo,
                      NativeScriptWitness(NativeScriptAttached, Set.empty)
                    )

                    val datum: MultisigTreasuryUtxo.Datum = treasuryUtxoSpent.datum
                }

                object MultisigRegime {
                    def apply() = Spend(multisigRegimeUtxo.asUtxo, hns.witnessAttached)
                }
            }

            object Mints {
                def apply(): List[Mint] = List(BurnMultisigRegime(), MintVotes())

                object BurnMultisigRegime {
                    def apply() = Mint(
                      hns.policyId,
                      assetName = config.headTokenNames.multisigRegimeTokenName,
                      amount = -1,
                      witness = NativeScriptWitness(NativeScriptAttached, Set.empty)
                    )
                }

                object MintVotes {
                    def apply() = Mint(
                      hns.policyId,
                      assetName = config.headTokenNames.voteTokenName,
                      amount = hns.numSigners + 1L,
                      witness = NativeScriptWitness(NativeScriptAttached, Set.empty)
                    )
                }
            }
            object Sends {
                def apply(): List[Send] = List(Treasury()) ++ Collaterals().toList ++ Votes().toList

                object Treasury {
                    def apply() = Send(
                      Babbage(
                        address = config.ruleBasedDisputeResolutionAddress,
                        value = treasuryUtxoSpent.value,
                        datumOption = Some(Inline(datum.toData)),
                        scriptRef = None
                      )
                    )

                    val datum = UnresolvedDatum(
                      headMp = hns.policyId,
                      disputeId = config.headTokenNames.voteTokenName.bytes,
                      peers = SList.from(hns.requiredSigners.map(_.hash)),
                      peersN = hns.numSigners,
                      deadlineVoting = config.slotConfig.slotToTime(validityStartTime.toSlot.slot) +
                          config.votingDuration.finiteDuration.toMillis,
                      versionMajor = Steps.Spends.Treasury.datum.versionMajor.toInt,
                      // TODO: pull in N first elements of G2 CRS
                      // KZG setup I think?
                      setup = SList.empty
                    )
                }

                object Votes {
                    def apply(): NonEmptyList[Send] = Peers() :+ Default()

                    private def mkVoteUtxo(datum: Data, voteDeposit: Coin): TransactionOutput =
                        Babbage(
                          address = config.ruleBasedDisputeResolutionAddress,
                          value = Value(
                            voteDeposit,
                            MultiAsset.asset(hns.policyId, config.headTokenNames.voteTokenName, 1L)
                          ),
                          datumOption = Some(Inline(datum)),
                          scriptRef = None
                        ).ensureMinAda(config.cardanoProtocolParams)

                    object Default {
                        def apply() = Send(utxo)

                        private val utxo = mkVoteUtxo(
                          VD.default(treasuryUtxoSpent.datum.commit).toData,
                          config.collectiveContingency.defaultVoteDeposit
                        )
                    }

                    object Peers {
                        def apply(): NonEmptyList[Send] = utxos.map(Send(_))

                        private val utxos = {
                            val datums = VD(
                              NonEmptyList.fromListUnsafe(
                                hns.requiredSigners.map(x => PubKeyHash(x.hash)).toList
                              )
                            )
                            datums.map(datum =>
                                mkVoteUtxo(datum.toData, config.individualContingency.voteDeposit)
                            )
                        }
                    }
                }

                object Collaterals {
                    def apply(): NonEmptyList[Send] = utxos.map(Send(_))

                    val utxos: NonEmptyList[TransactionOutput] = {
                        NonEmptyList.fromListUnsafe(
                          hns.requiredSigners
                              .map(es =>
                                  Babbage(
                                    address = ShelleyAddress(
                                      network = config.network,
                                      payment = ShelleyPaymentPart.Key(es.hash),
                                      delegation = ShelleyDelegationPart.Null
                                    ),
                                    value = Value(config.individualContingency.collateralDeposit),
                                    datumOption = None,
                                    scriptRef = None
                                  ).ensureMinAda(config.cardanoProtocolParams)
                              )
                              .toList
                        )
                    }
                }

            }
        }

        object Complete {
            def apply(finalized: TransactionBuilder.Context): FallbackTx = {
                val txId = finalized.transaction.id

                val treasuryProduced = RuleBasedTreasuryUtxo(
                  treasuryTokenName = config.headTokenNames.treasuryTokenName,
                  utxoId = TransactionInput(txId, 0),
                  address = config.ruleBasedDisputeResolutionAddress,
                  datum = RuleBasedTreasuryDatum.Unresolved(Steps.Sends.Treasury.datum),
                  value = treasuryUtxoSpent.value
                )

                FallbackTx(
                  validityStart = validityStartTime,
                  treasurySpent = treasuryUtxoSpent,
                  treasuryProduced = treasuryProduced,
                  multisigRegimeUtxoSpent = multisigRegimeUtxo,
                  tx = finalized.transaction,
                  resolvedUtxos = finalized.resolvedUtxos
                )
            }
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
                  validators = Tx.Validators.nonSigningNonValidityChecksValidators
                )
        }

    }

}
