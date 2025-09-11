package hydrozoa.multisig.ledger

import cats.effect.{IO, Ref}
import hydrozoa.multisig.ledger.dapp.DappLedger
import hydrozoa.multisig.ledger.dapp.DappLedger.ErrorAddDeposit
import hydrozoa.multisig.ledger.dapp.tx.DepositTx
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.VirtualLedger
import hydrozoa.multisig.ledger.virtual.VirtualLedger.{ErrorApplyInternalTx, ErrorApplyWithdrawalTx}
import hydrozoa.multisig.ledger.virtual.tx.{InternalTx, WithdrawalTx}
import scalus.cardano.ledger.TransactionOutput

final case class JointLedger()(
    private val dappLedger: DappLedger,
    private val virtualLedger: VirtualLedger,
    private val blockWithdrawnUtxos: Ref[IO, List[TransactionOutput]]
) {
    def registerDepositL1(tx: DepositTx): IO[Either[ErrorAddDeposit, DepositUtxo]] =
        ???

    def applyInternalTxL2(tx: InternalTx): IO[Either[ErrorApplyInternalTx, Unit]] =
        ???

    def applyWithdrawalTxL2(tx: WithdrawalTx): IO[Either[ErrorApplyWithdrawalTx, Unit]] =
        ???

    // TODO: more methods related to block completion
}

/** ==Hydrozoa's joint ledger on Cardano in the multisig regime==
  *
  * This joint ledger connects Hydrozoa's dapp ledger to its virtual ledger. It dispatches some
  * state transitions to them individually, but it also reconciles some state transitions across
  * them (sometimes in aggregate) to keep them aligned.
  */
object JointLedger {

}
