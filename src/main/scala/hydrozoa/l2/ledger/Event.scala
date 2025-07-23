package hydrozoa.l2.ledger

import hydrozoa.*
import hydrozoa.l1.multisig.state.depositDatum
import io.bullet.borer.Cbor
import scalus.builtin.Builtins.blake2b_256
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import hydrozoa.infra.Piper

// A sum type for ledger events
sealed trait L2Event:
    def getEventId: Hash[Blake2b_256, HashPurpose.TransactionHash]

final case class L2EventTransaction(transaction: Transaction) extends L2Event {
    override def getEventId: Hash[Blake2b_256, HashPurpose.TransactionHash] = transaction.id
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
          utxos.map((ti, to) => ti).sorted.flatMap(ti => Cbor.encode(ti).toByteArray).toArray
        )
      )
    )
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

//////////////////////////////////////////////////
// For compatibility with Tapir/Bloxbean/Old hydrozoa types

// FIXME: these should be deprecated
case class L2Genesis(
    depositUtxos: List[(UtxoId[L1], Output[L1])],
    outputs: List[OutputL2]
) derives CanEqual:
    def volume(): Long = outputs.map(_.coins).sum.toLong

object L2Genesis:
    def apply(ds: List[(UtxoId[L1], Output[L1])]): L2Genesis =
        L2Genesis(
          ds,
          ds.map((_, o) =>
              val datum = depositDatum(o) match
                  case Some(datum) => datum
                  case None =>
                      throw RuntimeException("deposit UTxO doesn't contain a proper datum")
              Output.apply(datum.address |> plutusAddressAsL2, o.coins, o.tokens)
          ).toList
        )

case class L2Transaction(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[UtxoIdL2],
    outputs: List[OutputNoTokens[L2]]
):
    def volume(): Long = outputs.map(_.coins).sum.toLong

case class L2Withdrawal(
    // FIXME: Should be Set, using List for now since Set is not supported in Tapir's Schema deriving
    inputs: List[UtxoIdL2]
)
