package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq.{NoRollouts, WithRollouts}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State}
import scalus.cardano.txbuilder.TransactionBuilder
import test.*
import test.TransactionChain.*

class SettlementTxSeqBuilderTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)

    test("Build settlement tx sequence") {
        forAll(genSettlementTxSeqBuilder()) { (builder, args, _) =>
            builder.build(args) match {
                case Left(e)  => fail(s"Build failed $e")
                case Right(r) => ()
            }
        }
    }

    test("Observe settlement tx seq") {
        val gen = genSettlementTxSeqBuilder()

        forAll(gen) { (builder, args, peers) =>
            {
                builder.build(args) match {
                    case Left(e) => throw RuntimeException(s"Build failed: $e")
                    case Right(txSeq) =>
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

                        val initialState: State = State(utxo = unsignedTxsAndUtxos._2.utxos)

                        val signedTxs: Vector[Transaction] =
                            peers.foldLeft(unsignedTxsAndUtxos._1)((txsToSign, peer) =>
                                txsToSign.map(tx => signTx(peer, tx))
                            )

                        observeTxChain(signedTxs)(initialState, CardanoMutator, Context()) match {
                            case Left(e) =>
                                throw new RuntimeException(
                                  s"\nFailed: ${e._1}. " // +
//                                  s"\n SettlementTxId: ${signedTxs.head.id}" +
//                                  s"\n rollout tx Id: ${signedTxs(1).id}"
                                )
                            case Right(v) => ()
                        }
                }
            }
        }
    }
}
