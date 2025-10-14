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
    
    case NoPayouts(
        override val settlementTx: SettlementTx
    )
    
    case WithPayouts(
        override val settlementTx: SettlementTx,
        rolloutTxSeq: Option[RolloutTxSeq]
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
        def build(): Either[Error, SettlementTxSeq] = {
            NonEmptyVector.fromVector(payouts) match {
                case None => SettlementTx.Builder.NoPayouts(
                        majorVersion,
                        deposits,
                        treasuryUtxo,
                        headNativeScript,
                        headNativeScriptReferenceInput,
                        env,
                        validators).build()
                    .map((r: SettlementTx.Builder.Result) => SettlementTxSeq.NoPayouts(r.settlementTx))
                    .left.map(Error.SettlementError(_))
                case Some(nePayouts) =>
                    for {
                        rolloutTxSeqPartial <- RolloutTxSeq.Builder(
                            config = RolloutTx.Builder.Config(
                                headNativeScript,
                                headNativeScriptReferenceInput,
                                env,
                                validators),
                            payouts = nePayouts
                        ).buildPartial().left.map(Error.RolloutSeqError(_))

                        settlementTxRes <- SettlementTx.Builder.WithPayouts(majorVersion = majorVersion,
                            deposits = deposits,
                            firstRolloutTxPartial = rolloutTxSeqPartial.firstOrOnly,
                            treasuryUtxo = treasuryUtxo,
                            headNativeScript = headNativeScript,
                            headNativeScriptReferenceInput = headNativeScriptReferenceInput,
                            env = env, validators = validators).build().left.map(Error.SettlementError(_))

                        rollouts <- settlementTxRes.settlementTx match {
                            case tx: SettlementTx.NoPayouts => Right(None)
                            case tx: SettlementTx.WithPayouts => tx.mbRolloutProduced match {
                                case None => Right(None)
                                case Some(rollout) => rolloutTxSeqPartial.finishPostProcess(rollout)
                                    .map(Some(_))
                                    .left.map(Error.RolloutSeqError(_))
                            }
                        }
                    } yield SettlementTxSeq.WithPayouts(settlementTxRes.settlementTx, rolloutTxSeq = rollouts)
            }
        }

        enum Error:
            case SettlementError(e: SettlementTx.Builder.Error)
            case RolloutSeqError(e: RolloutTxSeq.Builder.Error)
    }
}
