package hydrozoa.multisig.ledger.l1.txseq

import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.ledger.l1.tx.*
import hydrozoa.multisig.ledger.l1.txseq.SettlementTxSeq.{NoRollouts, WithRollouts}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Prop, Properties}
import scala.collection.mutable
import scalus.cardano.ledger.*
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.cardano.txbuilder.TransactionBuilder
import test.*
import test.TransactionChain.*

object SettlementTxSeqBuilderTest extends Properties("SettlementTxSeq") {

    val _ = property(
      "Observe settlement tx seq"
    ) =

        val props = mutable.Buffer.empty[Prop]

        Prop.forAll(MultiNodeConfig.generate(TestPeersSpec.default)())(multiNodeConfig =>
            Prop.forAll(genSettlementTxSeqBuilder(multiNodeConfig.headConfig)()) { builder =>
                {

                    builder.result match {
                        case Left(e) => throw RuntimeException(s"Build failed: $e")
                        case Right(txSeq) => {
                            val unsignedTxsAndUtxos
                                : (Vector[Transaction], TransactionBuilder.ResolvedUtxos) =
                                txSeq match {
                                    case NoRollouts(settlementTx, fallbackTx) =>
                                        (
                                          Vector(settlementTx.tx, fallbackTx.tx),
                                          settlementTx.resolvedUtxos
                                        )

                                    case WithRollouts(settlementTx, fallbackTx, rolloutTxSeq) =>
                                        (
                                          Vector(settlementTx.tx)
                                              .appendedAll(rolloutTxSeq.notLast.map(_.tx))
                                              .appended(rolloutTxSeq.last.tx)
                                              .appended(fallbackTx.tx),
                                          settlementTx.resolvedUtxos
                                        )
                                }

                            val initialState: State = State(utxos = unsignedTxsAndUtxos._2.utxos)

                            val signedTxs: Vector[Transaction] =
                                unsignedTxsAndUtxos._1.map(multiNodeConfig.multisignTx)

                            val eRes = observeTxChain(signedTxs)(
                              initialState,
                              ObserverMutator,
                              Context(
                                fee = Coin.zero,
                                env = UtxoEnv(
                                  slot = 0L,
                                  params = multiNodeConfig.headConfig.cardanoProtocolParams,
                                  certState = CertState.empty,
                                  network = multiNodeConfig.headConfig.network
                                ),
                                slotConfig = multiNodeConfig.headConfig.slotConfig,
                                evaluatorMode = EvaluateAndComputeCost
                              )
                            )

                            props.append(
                              s"SettlementTxSeq observation should be successful: ${eRes}" |: eRes.isRight
                            )

                            // The value-conservation postcondition: the produced treasury's
                            // liability pool (value minus equity) moves by exactly the L1
                            // boundary crossings — fees are paid from equity (replenished by
                            // deposit fees) and never touch the pool — in the coin and in every
                            // asset. This is the treasury-side half of the head's double-entry
                            // balance identity (treasury.value == map.totalValue + equity +
                            // beacon); the map-side half is StackEffectsBuilder's per-command
                            // conservation gate. Together they form the identity's inductive
                            // step, so a balanced anchor stays balanced — the identity is
                            // established here, not re-checked at runtime.
                            props.append {
                                val treasuryIn = builder.treasuryToSpend
                                val treasuryOut = txSeq.settlementTx.treasuryProduced
                                val absorbed =
                                    Value.combine(builder.depositsToSpend.map(_.l2Value))
                                val paidOut = Value.combine(
                                  builder.payoutObligationsRemaining.map(_.utxo.value.value)
                                )
                                val drift = (treasuryOut.value - Value(treasuryOut.equity.coin)) -
                                    (treasuryIn.value - Value(treasuryIn.equity.coin)) -
                                    absorbed + paidOut
                                "the produced treasury's liability pool must move by exactly" +
                                    s" absorbed minus payouts (drifted by $drift)" |: drift.isZero
                            }
                            // TODO: rewrite this in Either/PropertyM[Either,_]
                            val Right(res) = eRes: @unchecked

                            // Inspecting the final two states of the chain
                            val afterFallback: State = res.last._1
                            val beforeFallback: State = res.init.last._1 // second-to-last state

                            props.append(
                              "numPeers + 1 Utxos should appear at the dispute resolution address after the fallback" |: {
                                  // Gets the number of utxos at the dispute resolution address
                                  val disputeAddress = HydrozoaBlueprint.mkDisputeAddress(
                                    multiNodeConfig.headConfig.network
                                  )
                                  val helper: State => Int =
                                      s => s.utxos.values.count(_.address == disputeAddress)
                                  helper(beforeFallback) == 0 && helper(
                                    afterFallback
                                  ) == multiNodeConfig.headConfig.headPeers.nHeadPeers + 1
                              }
                            )

                            props.append(
                              "One utxo should appear at the rules based treasury script address after the fallback" |: {
                                  val treasuryAddress = HydrozoaBlueprint.mkTreasuryAddress(
                                    multiNodeConfig.headConfig.network
                                  )
                                  val helper: State => Int =
                                      s => s.utxos.values.count(_.address == treasuryAddress)
                                  helper(beforeFallback) == 0 && helper(afterFallback) == 1
                              }
                            )

                            props.fold(Prop(true))(_ && _)
                        }
                    }
                }
            }
        )
}
