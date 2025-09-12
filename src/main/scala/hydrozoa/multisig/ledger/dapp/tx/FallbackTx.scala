package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import scalus.cardano.ledger.Transaction

final case class FallbackTx(
    treasurySpent: TreasuryUtxo,
    override val tx: Transaction
) extends Tx

object FallbackTx {
//    sealed trait ParseError
//
//    final case class SomeParseError() extends ParseError
//
//    def parse(txSerialized: Tx.Serialized.Fallback): Either[ParseError, Tx.Fallback] = {
//        val deserialized = txCborToScalus(txSerialized.txCbor)
//        Right(
//          Tx.Fallback(
//            treasurySpent = ???,
//            headAddress = txSerialized.headAddress,
//            headPolicy = txSerialized.headPolicy,
//            txCbor = txSerialized.headPolicy,
//            tx = deserialized
//          )
//        )
//    }

}
