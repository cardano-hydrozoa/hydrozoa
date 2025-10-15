package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyVector
import cats.implicits.*
import hydrozoa.lib.tx.TransactionUnspentOutput
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.txseq.tx.{RolloutTx, SettlementTx}
import hydrozoa.multisig.ledger.dapp.utxo.{DepositUtxo, TreasuryUtxo}
import hydrozoa.multisig.ledger.joint.utxo.Payout
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.Environment

import scala.collection.immutable.Queue

enum SettlementTxSeq:
    def settlementTx: SettlementTx

    case NoRollouts(
        override val settlementTx: SettlementTx.NoRollouts
    )

    case WithRollouts(
        override val settlementTx: SettlementTx.WithRollouts,
        rolloutTxSeq: RolloutTxSeq
    )

object SettlementTxSeq {
    case class Builder(
        majorVersion: Int,
        deposits: Queue[DepositUtxo],
        payouts: Vector[Payout.Obligation.L1],
        treasuryUtxo: TreasuryUtxo,
        headNativeScript: HeadMultisigScript,
        // The reference script for the HNS should live inside the multisig regime witness UTxO
        headNativeScriptReferenceInput: TransactionUnspentOutput,
        env: Environment,
        validators: Seq[Validator]
    ) {
        import Builder.*
        def build(): Either[Error, Result] = {
            NonEmptyVector.fromVector(payouts) match {
                case None =>
                    SettlementTx.Builder
                        .NoPayouts(
                          majorVersion,
                          deposits,
                          treasuryUtxo,
                          headNativeScript,
                          headNativeScriptReferenceInput,
                          env,
                          validators
                        )
                        .build()
                        .left
                        .map(Error.SettlementError(_))
                        .map(_.asSettlementTxSeqResult)
                case Some(nePayouts) =>
                    for {
                        rolloutTxSeqPartial <- RolloutTxSeq
                            .Builder(
                              config = RolloutTx.Builder.Config(
                                headNativeScript,
                                headNativeScriptReferenceInput,
                                env,
                                validators
                              ),
                              payouts = nePayouts
                            )
                            .buildPartial()
                            .left
                            .map(Error.RolloutSeqError(_))

                        settlementTxRes <- SettlementTx.Builder
                            .WithPayouts(
                              majorVersion = majorVersion,
                              deposits = deposits,
                              rolloutTxSeqPartial = rolloutTxSeqPartial,
                              treasuryUtxo = treasuryUtxo,
                              headNativeScript = headNativeScript,
                              headNativeScriptReferenceInput = headNativeScriptReferenceInput,
                              env = env,
                              validators = validators
                            )
                            .build()
                            .left
                            .map(Error.SettlementError(_))

                        settlementTxSeq <-
                            import SettlementTx.Builder.Result
                            settlementTxRes match {
                                case res: Result.WithOnlyDirectPayouts =>
                                    Right(SettlementTxSeq.NoRollouts(res.settlementTx))
                                case res: Result.WithRollouts =>
                                    val tx: SettlementTx.WithRollouts = res.settlementTx
                                    res.rolloutTxSeqPartial
                                        .finishPostProcess(tx.rolloutProduced)
                                        .left
                                        .map(Error.RolloutSeqError(_))
                                        .map(SettlementTxSeq.WithRollouts(tx, _))
                            }
                    } yield Result(
                      settlementTxSeq = settlementTxSeq,
                      absorbedDeposits = settlementTxRes.absorbedDeposits,
                      remainingDeposits = settlementTxRes.remainingDeposits
                    )
            }
        }

        object Builder {
            enum Error:
                case SettlementError(e: SettlementTx.Builder.Error)
                case RolloutSeqError(e: RolloutTxSeq.Builder.Error)

            final case class Result(
                settlementTxSeq: SettlementTxSeq,
                override val absorbedDeposits: Queue[DepositUtxo],
                override val remainingDeposits: Queue[DepositUtxo]
            ) extends SettlementTx.Builder.State.Fields.HasDepositsPartition

            extension (res: SettlementTx.Builder.Result.NoPayouts)
                def asSettlementTxSeqResult: Result = {
                    import res.*
                    Result(
                      SettlementTxSeq.NoRollouts(settlementTx),
                      absorbedDeposits = absorbedDeposits,
                      remainingDeposits = remainingDeposits
                    )
                }
        }
    }
}
