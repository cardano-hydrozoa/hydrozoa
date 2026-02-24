package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.config.node.TestNodeConfig.generateTestNodeConfig
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq.{NoRollouts, WithRollouts}
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Prop, Properties}
import scala.collection.mutable
import scalus.cardano.ledger.*
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.rules.{Context, State, UtxoEnv}
import scalus.cardano.txbuilder.TransactionBuilder
import test.*
import test.TestPeer.signTx
import test.TransactionChain.*

object SettlementTxSeqBuilderTest extends Properties("SettlementTxSeq") {

    val _ = property(
      "Observe settlement tx seq"
    ) =

        val props = mutable.Buffer.empty[Prop]

        Prop.forAll(generateTestNodeConfig)(config =>
            Prop.forAll(genSettlementTxSeqBuilder(config)()) { builder =>
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
                                config.testPeers._testPeers.foldLeft(unsignedTxsAndUtxos._1)(
                                  (txsToSign, peer) => txsToSign.map(tx => peer._2.signTx(tx))
                                )

                            val eRes = observeTxChain(signedTxs)(
                              initialState,
                              ObserverMutator,
                              Context(
                                fee = Coin.zero,
                                env = UtxoEnv(
                                  slot = 0L,
                                  params = config.nodeConfig.cardanoProtocolParams,
                                  certState = CertState.empty,
                                  network = config.nodeConfig.network
                                ),
                                slotConfig = config.nodeConfig.slotConfig,
                                evaluatorMode = EvaluateAndComputeCost
                              )
                            )

                            props.append(
                              s"SettlementTxSeq observation should be successful: ${eRes}" |: eRes.isRight
                            )
                            // TODO: rewrite this in Either/PropertyM[Either,_]
                            val Right(res) = eRes

                            // Inspecting the final two states of the chain
                            val afterFallback: State = res.last._1
                            val beforeFallback: State = res.init.last._1 // second-to-last state

                            props.append(
                              "numPeers + 1 Utxos should appear at the dispute resolution address after the fallback" |: {
                                  // Gets the number of utxos at the dispute resolution script hash
                                  val helper: State => Int = s =>
                                      s.utxos.values
                                          .map(
                                            _.address.scriptHashOption
                                                .contains(
                                                  DisputeResolutionScript.compiledScriptHash
                                                )
                                          )
                                          .count(identity)
                                  helper(beforeFallback) == 0 && helper(
                                    afterFallback
                                  ) == config.testPeers._testPeers.length + 1
                              }
                            )

                            props.append(
                              "One utxo should appear at the rules based treasury script address after the fallback" |: {
                                  val helper: State => Int = s =>
                                      s.utxos.values
                                          .map(
                                            _.address.scriptHashOption
                                                .contains(
                                                  RuleBasedTreasuryScript.compiledScriptHash
                                                )
                                          )
                                          .count(identity)
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
