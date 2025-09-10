package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.utxo.TreasuryUtxo
import scalus.cardano.ledger.Transaction

final case class FinalizationTx(
    treasurySpent: TreasuryUtxo,
    override val tx: Transaction
) extends Tx

object FinalizationTx {
//    sealed trait ParseError
//
//    def parse(txSerialized: Tx.Serialized.Finalization): Either[ParseError, Tx.Finalization] = {
//        val deserialized = txCborToScalus(txSerialized.txCbor)
//        Right(
//          Tx.Finalization(
//            treasurySpent = ???,
//            headAddress = txSerialized.headAddress,
//            headPolicy = txSerialized.headPolicy,
//            txCbor = txSerialized.headPolicy,
//            tx = deserialized
//          )
//        )
//    }
}
