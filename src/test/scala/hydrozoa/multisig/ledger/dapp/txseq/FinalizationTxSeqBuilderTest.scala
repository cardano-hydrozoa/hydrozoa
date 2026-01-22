package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.multisig.ledger.dapp.tx.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State}
import scalus.cardano.txbuilder.TransactionBuilder
import test.*
import test.TransactionChain.*

class FinalizationTxSeqBuilderTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 100)

    test("Build finalization tx sequence") {
        forAll(genStandaloneFinalizationTxSeqBuilder()) { (builder, args, _) =>
            builder.build(args) match {
                case Left(e)  => fail(s"Build failed $e")
                case Right(r) => ()
            }
        }
    }

    ignore("Observe finalization tx seq") {
        val gen = genStandaloneFinalizationTxSeqBuilder()

        forAll(gen) { (builder, args, peers) =>
            {
                builder.build(args) match {
                    case Left(e) => throw RuntimeException(s"Build failed: $e")
                    case Right(txSeq) =>
                        val unsignedTxsAndUtxos
                            : (Vector[Transaction], TransactionBuilder.ResolvedUtxos) = {
                            val utxos = txSeq.finalizationTx.resolvedUtxos
                            txSeq match {
                                case s: FinalizationTxSeq.Monolithic =>
                                    (Vector(s.finalizationTx.tx), utxos)
                                case s: FinalizationTxSeq.WithDeinit =>
                                    (Vector(s.finalizationTx.tx, s.deinitTx.tx), utxos)
                                case s: FinalizationTxSeq.WithRollouts =>
                                    (
                                      Vector(s.finalizationTx.tx)
                                          .appendedAll(s.rolloutTxSeq.notLast.map(_.tx))
                                          .appended(s.rolloutTxSeq.last.tx),
                                      utxos
                                    )
                                case s: FinalizationTxSeq.WithDeinitAndRollouts =>
                                    (
                                      Vector(s.finalizationTx.tx, s.deinitTx.tx)
                                          .appendedAll(s.rolloutTxSeq.notLast.map(_.tx))
                                          .appended(s.rolloutTxSeq.last.tx),
                                      utxos
                                    )
                            }
                        }

                        val initialState: State = State(utxos = unsignedTxsAndUtxos._2.utxos)

                        val signedTxs: Vector[Transaction] =
                            peers.foldLeft(unsignedTxsAndUtxos._1)((txsToSign, peer) =>
                                txsToSign.map(tx => peer.signTx(tx))
                            )

                        observeTxChain(signedTxs)(
                          initialState,
                          CardanoMutator,
                          Context.testMainnet()
                        ) match {
                            case Left(e) =>
                                throw new RuntimeException(
                                  s"\nFailed: ${e._1}"
                                )
                            case Right(v) => ()
                        }
                }
            }
        }
    }
}
