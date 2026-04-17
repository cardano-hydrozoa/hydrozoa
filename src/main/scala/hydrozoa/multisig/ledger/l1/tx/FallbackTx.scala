package hydrozoa.multisig.ledger.l1.tx

import cats.data.NonEmptyList
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.initialization.InitializationParameters
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.FallbackTxStartTime
import hydrozoa.lib.cardano.scalus.contextualscalus.Change
import hydrozoa.lib.cardano.scalus.contextualscalus.TransactionBuilder.{build, finalizeContext}
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.l1.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.l1.tx.Metadata.Fallback
import hydrozoa.multisig.ledger.l1.utxo.{MultisigRegimeOutput, MultisigRegimeUtxo, MultisigTreasuryUtxo}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Unresolved
import hydrozoa.rulebased.ledger.l1.state.VoteDatum as VD
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteDatum
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import io.circe.*
import io.circe.generic.semiauto.*
import monocle.{Focus, Lens}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Mint as _, TransactionOutput, *}
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.*
import scalus.uplc.builtin.Builtins.blake2b_224
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.|>

/** Output order:
  *   - Treasury Utxo (1)
  *   - Collateral Utxos (n)
  *   - Peer Vote Utxos (n)
  *   - Default Vote Utxo
  */
final case class FallbackTx(
    fallbackTxStartTime: FallbackTxStartTime,
    override val treasurySpent: MultisigTreasuryUtxo,
    override val treasuryProduced: RuleBasedTreasuryUtxo,
    override val multisigRegimeUtxoSpent: MultisigRegimeUtxo,
    override val tx: Transaction,
    override val txLens: Lens[FallbackTx, Transaction] = Focus[FallbackTx](_.tx),
    override val resolvedUtxos: ResolvedUtxos,
    // TODO type better
    peerVoteUtxosProduced: NonEmptyList[Utxo]
    // TODO
    // val collateralUtxos : Map[HeadPeerNumber, CollateralUtxo]
) extends MultisigTreasuryUtxo.Spent,
      MultisigRegimeUtxo.Spent,
      RuleBasedTreasuryUtxo.Produced,
      Tx[FallbackTx] {
    override def transactionFamily: String = "FallbackTx"
}

object FallbackTx {
    export FallbackTxOps.Build

    given fallbackTxEncoder: Encoder[FallbackTx] =
        Encoder.encodeString.contramap(fallbackTx =>
            fallbackTx.tx.toCbor |> ByteString.fromArray |> (_.toHex)
        )
}

private object FallbackTxOps {
    type Config = HeadConfig.Bootstrap.Section & InitializationParameters.Section

    private val logger = org.slf4j.LoggerFactory.getLogger("FallbackTx")

    private def time[A](label: String)(block: => A): A = {
        val start = System.nanoTime()
        val result = block
        val elapsed = (System.nanoTime() - start) / 1_000_000.0
        logger.trace(f"\t\t⏱️ $label: $elapsed%.2f ms")
        result
    }

    // TODO: Distribute equity
    final case class Build(
        validityStartTime: FallbackTxStartTime,
        treasuryUtxoSpent: MultisigTreasuryUtxo,
        multisigRegimeUtxo: MultisigRegimeUtxo,
    )(using config: Config) {

        lazy val result: Either[SomeBuildError, FallbackTx] = for {
            unbalanced <- build(Steps())
            finalized <- unbalanced.finalizeContext(
              diffHandler = Change.changeOutputDiffHandler(1),
              validators = Tx.Validators.nonSigningNonValidityChecksValidators
            )
            completed = Complete(finalized)
        } yield completed

        object Steps {
            def apply(): List[TransactionBuilderStep] = Base() ++ Spends() ++ Mints() ++ Sends()

            val hns: HeadMultisigScript = config.headMultisigScript

            object Base {
                def apply(): List[TransactionBuilderStep] =
                    List(modifyAuxiliaryData, validityStartSlot)

                val modifyAuxiliaryData =
                    ModifyAuxiliaryData(_ => Some(Fallback().asAuxData(config.headId)))

                val validityStartSlot = ValidityStartSlot(validityStartTime.toSlot.slot)
            }

            object Spends {
                def apply(): List[Spend] = List(MultisigRegime(), Treasury())

                object Treasury {
                    def apply() = Spend(
                      treasuryUtxoSpent.asUtxo,
                      config.headMultisigScript.witnessAttached
                    )

                    val datum: MultisigTreasuryUtxo.Datum = treasuryUtxoSpent.datum
                }

                object MultisigRegime {
                    def apply() =
                        Spend(multisigRegimeUtxo.toUtxo, config.headMultisigScript.witnessAttached)
                }
            }

            object Mints {
                def apply(): List[Mint] =
                    List(MultisigRegimeOutput.burnMultisigRegimeTokens, MintVotes())

                private object MintVotes {
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
                    def apply(): Send = output.send

                    val datum: Unresolved = time("newTreasuryDatum") {
                        Unresolved(
                          deadlineVoting =
                              config.slotConfig.slotToTime(validityStartTime.toSlot.slot) +
                                  config.votingDuration.finiteDuration.toMillis,
                          versionMajor = Steps.Spends.Treasury.datum.versionMajor.toInt,
                          // TODO: pull in N first elements of G2 CRS
                          // KZG setup I think?
                          setup = SList.empty
                        )
                    }

                    // TODO: Partial, hacked in during a refactor
                    private val output = {
                        val v = treasuryUtxoSpent.value - Value(treasuryUtxoSpent.equity.coin)
                        RuleBasedTreasuryOutput(
                          value = v,
                          datum = datum,
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
                        )

                    private object Default {
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
                              config.headPeerVKeys
                                  .map(x => PubKeyHash(blake2b_224(x)))
                            )
                            datums.map(datum =>
                                mkVoteUtxo(
                                  datum.toData,
                                  config.individualContingency.forVoteUtxo
                                )
                            )
                        }
                    }
                }

                private object Collaterals {
                    def apply(): NonEmptyList[Send] = NonEmptyList.fromListUnsafe(
                      config.headPeerAddresses.toSortedMap
                          .transform((pNum, addr) =>
                              mkPeerPayout(
                                addr,
                                equityPayouts(pNum)
                                    + config.individualContingency.forCollateralUtxo
                                    + (if pNum.convert == 0
                                       then config.collectiveContingency.fallbackTxFee
                                       else Coin.zero)
                              )
                          )
                          .values
                          .toList
                    )

                    private def mkPeerPayout(addr: ShelleyAddress, lovelace: Coin): Send = Send(
                      TransactionOutput.Babbage(
                        address = addr,
                        value = Value(lovelace),
                        datumOption = None,
                        scriptRef = None,
                      )
                    )

                    private val equityPayouts: Map[HeadPeerNumber, Coin] =
                        config.distributeEquity(treasuryUtxoSpent.equity.coin).toSortedMap
                }

            }
        }

        object Complete {
            def apply(finalized: TransactionBuilder.Context): FallbackTx = {
                val txId = finalized.transaction.id

                // FIXME: Partial, introduced during a refactor where RuleBasedTreasuryOutput began to verify
                // that the constructed output did indeed have a valid treasury token
                val treasuryOutputProduced = {
                    val value = treasuryUtxoSpent.value - Value(treasuryUtxoSpent.equity.coin)
                    RuleBasedTreasuryOutput(
                      datum = Steps.Sends.Treasury.datum,
                      value = value
                    )

                }

                FallbackTx(
                  fallbackTxStartTime = validityStartTime,
                  treasurySpent = treasuryUtxoSpent,
                  treasuryProduced = RuleBasedTreasuryUtxo(
                    utxoId = TransactionInput(txId, 0),
                    treasuryOutput = treasuryOutputProduced
                  ),
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
    }
}
