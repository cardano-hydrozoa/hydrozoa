package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.utxo.{DepositUtxo, RolloutUtxo, TreasuryUtxo}
import scalus.cardano.ledger.Transaction

final case class SettlementTx(
    treasurySpent: TreasuryUtxo,
    treasuryProduced: TreasuryUtxo,
    depositsSpent: List[DepositUtxo],
    rolloutProduced: Option[RolloutUtxo],
    override val tx: Transaction
) extends Tx

object SettlementTx {
//    sealed trait ParseError
//
//    def parse(txSerialized: Tx.Serialized.Settlement): Either[ParseError, Tx.Settlement] = {
//        val deserialized = txCborToScalus(txSerialized.txCbor)
//        Right(
//          Tx.Settlement(
//            treasurySpent = ???,
//            treasuryProduced = ???,
//            depositsSpent = ???,
//            rolloutProduced = ???,
//            headAddress = txSerialized.headAddress,
//            headPolicy = txSerialized.headPolicy,
//            txCbor = txSerialized.headPolicy,
//            tx = deserialized
//          )
//        )
//    }
}
