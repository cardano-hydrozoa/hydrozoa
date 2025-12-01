package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq.{NoRollouts, WithRollouts}
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Prop, Properties, Test}
import scalus.cardano.address.Network.Testnet
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State}
import scalus.cardano.txbuilder.TransactionBuilder
import test.*
import test.TransactionChain.*

import scala.collection.mutable

object SettlementTxSeqBuilderTest extends Properties("SettlementTxSeq") {
    import Prop.forAll

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withMinSuccessfulTests(100)

//    val _ = propertyWithSeed("Build settlement tx sequence", None) = {
//        forAll(genSettlementTxSeqBuilder()) { (builder, args, _) =>
//            "SettlementTxSeq builds" |: (builder.build(args) match {
//                case Left(e) => false
//                case Right(r) => true
//            })
//        }
//    }

    val _ = propertyWithSeed(
      "Observe settlement tx seq",
      None
    ) = {
        val gen = genSettlementTxSeqBuilder()
        val props = mutable.Buffer.empty[Prop]

        forAll(gen) { (builder, args, peers) => {

                builder.build(args) match {
                    case Left(e) => throw RuntimeException(s"Build failed: $e")
                    case Right(txSeq) => {
                        val unsignedTxsAndUtxos
                            : (Vector[Transaction], TransactionBuilder.ResolvedUtxos) =
                            txSeq.settlementTxSeq match {
                                case NoRollouts(settlementTx) => {
                                    (Vector(settlementTx.tx, txSeq.fallbackTx.tx), settlementTx.resolvedUtxos)
                                }
                                case WithRollouts(settlementTx, rolloutTxSeq) =>
                                    (
                                      Vector(settlementTx.tx)
                                          .appendedAll(rolloutTxSeq.notLast.map(_.tx))
                                          .appended(rolloutTxSeq.last.tx).appended(txSeq.fallbackTx.tx),
                                      settlementTx.resolvedUtxos
                                    )
                            }

                    val initialState: State = State(utxos = unsignedTxsAndUtxos._2.utxos)

                    val signedTxs: Vector[Transaction] =
                        peers.foldLeft(unsignedTxsAndUtxos._1)((txsToSign, peer) =>
                            txsToSign.map(tx => signTx(peer, tx))
                        )

                    val res = observeTxChain(signedTxs)(initialState, CardanoMutator, Context())

                    props.append(s"SettlementTxSeq observation should be successful: ${res}" |: res.isRight)

                    // Inspecting the final two states of the chain
                    val afterFallback : State = res.get.last._1
                    val beforeFallback : State = res.get.init.last._1 // second-to-last state

                    props.append(s"numPeers + 1 Utxos should appear at the dispute resolution address after the fallback" |: {
                        // Gets the number of utxos at the dispute resolution script hash
                        val helper : State => Int = s =>
                            s
                                .utxos
                                .values
                                .map(_.address.scriptHashOption.contains(DisputeResolutionScript.compiledScriptHash))
                                .count(identity)
                        helper(beforeFallback) == 0 && helper(afterFallback) == peers.length + 1
                    })

                    props.append(s"One utxo should appear at the rules based treasury script address after the fallback" |: {
                        val helper : State => Int = s =>
                            s
                                .utxos
                                .values
                                .map(_.address.scriptHashOption.contains(RuleBasedTreasuryScript.compiledScriptHash))
                                .count(identity)
                        helper(beforeFallback) == 0 && helper(afterFallback) == 1
                    })

                    props.fold(Prop(true))(_ && _)
                }
            }
        }
        }
    }
}
