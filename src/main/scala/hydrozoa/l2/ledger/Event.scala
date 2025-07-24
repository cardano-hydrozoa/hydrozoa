package hydrozoa.l2.ledger

import hydrozoa.*
import hydrozoa.l1.multisig.state.depositDatum
import io.bullet.borer.Cbor
import scalus.builtin.Builtins.blake2b_256
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import hydrozoa.infra.{Piper, plutusAddressAsL2}

// A sum type for ledger events
sealed trait L2Event:
    def getEventId: Hash[Blake2b_256, HashPurpose.TransactionHash]

final case class L2EventTransaction(transaction: Transaction) extends L2Event {
    override def getEventId: Hash[Blake2b_256, HashPurpose.TransactionHash] = transaction.id
    def volume : Long = transaction.body.value.outputs.map(sto => sto.value.value.coin.value).sum
}
case class L2EventWithdrawal(transaction: Transaction) extends L2Event {
    override def getEventId: Hash[Blake2b_256, HashPurpose.TransactionHash] = transaction.id
}

/** A genesis event absorbs a number of transaction inputs from L1 and produces corresponding L2
  * outputs. The TxId of a Genesis Event comes from sorting the TxIds of the absorbed UTxOs,
  * encoding them to Cbor, concatenating, and taking the blake2b_256 hash.
  */
case class L2EventGenesis(utxos: Seq[(TransactionInput, TransactionOutput)]) extends L2Event {
    override def getEventId: Hash[Blake2b_256, HashPurpose.TransactionHash] = Hash(
      blake2b_256(
        ByteString.fromArray(
          // I know this is an insane way to do it, but transaction input apparently doesn't have an ordering instance
          // yet
          utxos
              .map((ti, to) => ti.transactionId.toHex ++ ti.index.toString)
              .sorted
              .flatMap(ti => Cbor.encode(ti).toByteArray)
              .toArray
        )
      )
    )
    def volume : Long = utxos.map((ti, to) => to.value.coin.value).sum
    
}

// Tags used in blocks
enum L2EventLabel derives CanEqual:
    case L2EventGenesisLabel
    case L2EventTransactionLabel
    case L2EventWithdrawalLabel

def l2EventLabel(e: L2Event): L2EventLabel =
    e match
        case _: L2EventGenesis     => L2EventLabel.L2EventGenesisLabel
        case _: L2EventTransaction => L2EventLabel.L2EventTransactionLabel
        case _: L2EventWithdrawal  => L2EventLabel.L2EventWithdrawalLabel
