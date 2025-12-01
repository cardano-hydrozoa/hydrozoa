package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq.{NoRollouts, WithRollouts}
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Prop, Properties, Test}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State}
import scalus.cardano.txbuilder.TransactionBuilder
import test.*
import test.TransactionChain.*

object SettlementTxSeqBuilderTest extends Properties("SettlementTxSeq") {
    import Prop.forAll

    override def overrideParameters(p: Test.Parameters): Test.Parameters =
        p.withMinSuccessfulTests(100)

//    val _ = property("Build settlement tx sequence")  = {
//        forAll(genSettlementTxSeqBuilder()) { (builder, args, _) =>
//            "SettlementTxSeq builds" |: (builder.build(args) match {
//                case Left(e)  => false
//                case Right(r) => true
//            })
//        }
//    }

    val _ = propertyWithSeed(
      "Observe settlement tx seq",
      Some("rxYNJitNF9c6LoezXCCDfRPn-lKWyG7Uz9-GH8ikRFI=")
    ) = {
        val gen = genSettlementTxSeqBuilder()

        forAll(gen) { (builder, args, peers) =>
            {
                builder.build(args) match {
                    case Left(e) => throw RuntimeException(s"Build failed: $e")
                    case Right(txSeq) => {
                        val unsignedTxsAndUtxos
                            : (Vector[Transaction], TransactionBuilder.ResolvedUtxos) =
                            txSeq.settlementTxSeq match {
                                case NoRollouts(settlementTx) => {
                                    (Vector(settlementTx.tx), settlementTx.resolvedUtxos)
                                }
                                case WithRollouts(settlementTx, rolloutTxSeq) =>
                                    (
                                      Vector(settlementTx.tx)
                                          .appendedAll(rolloutTxSeq.notLast.map(_.tx))
                                          .appended(rolloutTxSeq.last.tx),
                                      settlementTx.resolvedUtxos
                                    )
                            }

                        val initialState: State = State(utxos = unsignedTxsAndUtxos._2.utxos)

                        val signedTxs: Vector[Transaction] =
                            peers.foldLeft(unsignedTxsAndUtxos._1)((txsToSign, peer) =>
                                txsToSign.map(tx => signTx(peer, tx))
                            )

                        val res = observeTxChain(signedTxs)(initialState, CardanoMutator, Context())

                        res.isRight :| s"SettlementTxSeq observation unsuccessful: ${res}"
                    }
                }
            }
        }
    }
}
