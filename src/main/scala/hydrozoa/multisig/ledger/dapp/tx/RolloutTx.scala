package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import scalus.cardano.ledger.Transaction

final case class RolloutTx(
    rolloutSpent: RolloutUtxo,
    rolloutProduced: Option[RolloutUtxo],
    override val tx: Transaction
) extends Tx

object RolloutTx {
//    sealed trait ParseError
//
//    def parse(txSerialized: Tx.Serialized.Rollout): Either[ParseError, Tx.Rollout] = {
//        val deserialized = txCborToScalus(txSerialized.txCbor)
//        Right(
//          Tx.Rollout(
//            rolloutSpent = ???,
//            rolloutProduced = ???,
//            headAddress = txSerialized.headAddress,
//            headPolicy = txSerialized.headPolicy,
//            txCbor = txSerialized.headPolicy,
//            tx = deserialized
//          )
//        )
//    }
}
