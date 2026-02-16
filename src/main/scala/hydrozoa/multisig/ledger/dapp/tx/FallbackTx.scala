package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
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
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import scalus.ledger.api.v1.PubKeyHash
import scalus.prelude.List as SList

/** Output order:
  *   - Treasury Utxo (1)
  *   - Collateral Utxos (n)
  *   - Peer Vote Utxos (n)
  *   - Default Vote Utxo
  * @param validityStart
  * @param treasurySpent
  * @param treasuryProduced
  * @param multisigRegimeUtxoSpent
  * @param tx
  * @param txLens
  * @param resolvedUtxos
  * @param peerVoteUtxosProduced
  */
final case class FallbackTx(
    override val validityStart: QuantizedInstant,
    override val treasurySpent: MultisigTreasuryUtxo,
    override val treasuryProduced: RuleBasedTreasuryUtxo,
    override val multisigRegimeUtxoSpent: MultisigRegimeUtxo,
    override val tx: Transaction,
    override val txLens: Lens[FallbackTx, Transaction] = Focus[FallbackTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos,
    // TODO type better
    val peerVoteUtxosProduced: NonEmptyList[Utxo]
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

    private val logger = org.slf4j.LoggerFactory.getLogger("FallbackTx")

    private def time[A](label: String)(block: => A): A = {
        val start = System.nanoTime()
        val result = block
        val elapsed = (System.nanoTime() - start) / 1_000_000.0
        logger.info(f"\t\t⏱️ $label: ${elapsed}%.2f ms")
        result
    }

    // TODO: Distribute equity
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
                      // TODO: switch back to witnessAttached after resolving https://github.com/scalus3/scalus/issues/207
                      hns.witnessValue
                    )

                    val datum: MultisigTreasuryUtxo.Datum = treasuryUtxoSpent.datum
                }

                object MultisigRegime {
                    // TODO: switch back to witnessAttached after resolving https://github.com/scalus3/scalus/issues/207
                    def apply() = Spend(multisigRegimeUtxo.asUtxo, hns.witnessValue)
                }
            }

            object Mints {
                def apply(): List[Mint] = List(BurnMultisigRegime(), MintVotes())

                object BurnMultisigRegime {
                    def apply() = Mint(
                      hns.policyId,
                      assetName = config.headTokenNames.multisigRegimeTokenName,
                      amount = -1,
                      witness = hns.witnessAttached
                    )
                }

                object MintVotes {
                    def apply() = Mint(
                      hns.policyId,
                      assetName = config.headTokenNames.voteTokenName,
                      amount = hns.numSigners + 1L,
                      witness = hns.witnessAttached
                    )
                }
            }
            object Sends {
                def apply(): List[Send] = List(Treasury()) ++ Collaterals().toList ++ Votes().toList

                object Treasury {
                    def apply() = Send(
                      Babbage(
                        address = config.ruleBasedTreasuryAddress,
                        value = treasuryUtxoSpent.value,
                        datumOption = Some(Inline(datum.toData)),
                        scriptRef = None
                      )
                    )

                    val datum: UnresolvedDatum = time("newTreasuryDatum") {
                        UnresolvedDatum(
                          headMp = hns.policyId,
                          disputeId = config.headTokenNames.voteTokenName.bytes,
                          peers = SList.from(hns.requiredSigners.map(_.hash)),
                          peersN = hns.numSigners,
                          deadlineVoting =
                              config.slotConfig.slotToTime(validityStartTime.toSlot.slot) +
                                  config.votingDuration.finiteDuration.toMillis,
                          versionMajor = Steps.Spends.Treasury.datum.versionMajor.toInt,
                          // TODO: pull in N first elements of G2 CRS
                          // KZG setup I think?
                          setup = SList.empty
                        )
                    }
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
                        ).ensureMinAda(config)

                    object Default {
                        def apply() = Send(utxo)

                        private val utxo = time("defaultVoteUtxo") {
                            mkVoteUtxo(
                              VD.default(treasuryUtxoSpent.datum.commit).toData,
                              config.collectiveContingency.defaultVoteDeposit
                            )
                        }
                    }

                    object Peers {
                        def apply(): NonEmptyList[Send] = utxos.map(Send(_))

                        private val utxos = time("peerVoteUtxos") {
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

                    val utxos: NonEmptyList[TransactionOutput] = time("collateralUtxos") {
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
                                  ).ensureMinAda(config)
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
                  address = config.ruleBasedTreasuryAddress,
                  datum = RuleBasedTreasuryDatum.Unresolved(Steps.Sends.Treasury.datum),
                  value = treasuryUtxoSpent.value
                )

                FallbackTx(
                  validityStart = validityStartTime,
                  treasurySpent = treasuryUtxoSpent,
                  treasuryProduced = treasuryProduced,
                  multisigRegimeUtxoSpent = multisigRegimeUtxo,
                  tx = finalized.transaction,
                  resolvedUtxos = finalized.resolvedUtxos,
                  // Ordering:
                  // - Treasury (1)
                  // - Collateral  (n)
                  // - Peer votes (n)
                  // - Default vote (1)
                  peerVoteUtxosProduced = {
                      val inputs = List
                          .range(1 + config.headPeerIds.length, 1 + config.headPeerIds.length * 2)
                          .map(idx => TransactionInput(txId, idx))
                      val outputs = Steps.Sends.Votes.Peers.apply().map(_.output).toList
                      val list = inputs.zip(outputs).map(Utxo(_, _))
                      NonEmptyList.fromListUnsafe(list)
                  }
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
