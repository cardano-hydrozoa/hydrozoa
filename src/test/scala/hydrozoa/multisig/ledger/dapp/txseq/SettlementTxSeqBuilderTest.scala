package hydrozoa.multisig.ledger.dapp.txseq

import hydrozoa.lib.tx.TransactionBuilder
import hydrozoa.multisig.ledger.dapp.tx.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import hydrozoa.multisig.ledger.dapp.txseq.SettlementTxSeq.{NoRollouts, WithRollouts}
import munit.FunSuite
import org.scalacheck.{Prop, Test as ScalaCheckTest}
import scalus.cardano.ledger.Transaction
import scalus.cardano.ledger.rules.{CardanoMutator, Context, State}
import test.*
import test.TransactionChain.*

class SettlementTxSeqBuilderTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
        PropertyCheckConfiguration(minSuccessful = 1)

    test("Build settlement tx sequence") {
        // TODO: I guess this generator doesn't work well
        forAll(genSettlementTxSeqBuilder()) { (builder, args, _) =>
            builder.build(args) match {
                case Left(e)  => fail(s"Build failed $e")
                case Right(r) => ()
            }
        }
    }

    override def scalaCheckInitialSeed = "sTq_YFUmSG1l-iE4IHq_srByhYpnUpFgVh-1rRL-gMM="

    property("Observe settlement tx seq")(Prop.forAll(genSettlementTxSeqBuilder())({
        (builder, args, peers) =>
            {
                builder.build(args) match {
                    case Left(e) => throw RuntimeException(s"Build failed: $e")
                    case Right(txSeq) =>
                        val unsignedTxsAndUtxos
                            : (Vector[Transaction], TransactionBuilder.ResolvedUtxos) =
                            txSeq.settlementTxSeq match {
                                case NoRollouts(settlementTx) => {
                                    println(settlementTx.resolvedUtxos.utxos.keys)
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
                                  s"\nFailed: ${e._1}. " +
                                      s"\n SettlementTxId: ${signedTxs.head.id}" +
                                      s"\n rollout tx Id: ${signedTxs(1).id}"
                                )
                            case Right(v) => ()
                        }

                }
            }
    }))
}
