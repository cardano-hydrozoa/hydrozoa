package hydrozoa.multisig.ledger

import cats.effect.{IO, Ref}
import hydrozoa.multisig.ledger.DappLedger.*
import hydrozoa.multisig.ledger.DappLedger.Errors.ParseDepositError
import hydrozoa.multisig.ledger.VirtualLedger.ErrorApplyInternalTx
import hydrozoa.multisig.ledger.dapp.tx.DepositTx
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.tx.InternalTx
import scalus.cardano.ledger.*

final case class JointLedger()(
    private val dappLedger: DappLedger,
    private val virtualLedger: VirtualLedger,
    private val blockWithdrawnUtxos: Ref[IO, List[TransactionOutput]]
) {
    def registerDepositL1(tx: DepositTx): IO[Either[ParseDepositError, DepositUtxo]] =
        ???

    def applyInternalTxL2(tx: InternalTx): IO[Either[ErrorApplyInternalTx, Unit]] =
        ???

    // TODO: more methods related to block completion
}

/** ==Hydrozoa's joint ledger on Cardano in the multisig regime==
  *
  * Hydrozoa's joint ledger connects its dapp ledger to its virtual ledger. It dispatches some state
  * transitions to them individually, but it also periodically reconciles state transitions across
  * them to keep them aligned.
  */
object JointLedger {
    final case class CompleteBlockError() extends Throwable
}
