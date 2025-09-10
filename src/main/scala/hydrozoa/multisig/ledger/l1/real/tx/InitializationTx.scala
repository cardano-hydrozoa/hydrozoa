package hydrozoa.multisig.ledger.l1.real.tx

import hydrozoa.multisig.ledger.l1.real.LedgerL1.Tx
import hydrozoa.multisig.ledger.l1.real.utxo.TreasuryUtxo
import scalus.cardano.ledger.Transaction

final case class InitializationTx(
    treasuryProduced: TreasuryUtxo,
    override val tx: Transaction
) extends Tx

object InitializationTx {
//    sealed trait ParseError
//
//    def parse(txSerialized: Tx.Serialized): Either[ParseError, InitializationTx] = {
//        val deserialized = txCborToScalus(txSerialized.txCbor)
//        Right(
//          InitializationTx(
//            treasuryProduced = ???,
//            headAddress = txSerialized.headAddress,
//            headPolicy = txSerialized.headPolicy,
//            txCbor = txSerialized.headPolicy,
//            tx = deserialized
//          )
//        )
//    }
}
