package hydrozoa.multisig.ledger.virtual

import cats.data.NonEmptyList
import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Hash as _, *}
import scalus.prelude.Option as SOption

// A sum type for ledger events
sealed trait L2Event

// TODO: Run L2 conformance during parsing?
final case class L2EventTransaction(transaction: Transaction) extends L2Event {
    def volume: Long = transaction.body.value.outputs.map(sto => sto.value.value.coin.value).sum
}
final case class L2EventWithdrawal(transaction: Transaction) extends L2Event {
    val getEventId: TransactionHash = transaction.id
}

// TODO: Rename to L2Genesis
// TODO: Fix to work with the new way that virtual utxos are created in deposit transactions.
object L2EventGenesis:
    enum L2EventGenesisError:
        case EmptyInputs

    /** Smart constructor for L2EventGenesis, ensuring that the event contains at least one valid
      * Genesis Utxo. A genesis event absorbs a number of transaction inputs from L1 and produces
      * corresponding L2 outputs. The TxId of a Genesis Event comes from a hash of the block number
      * and token name of the head
      */
    def fromDepositUtxo(
        utxosL1: NonEmptyList[DepositUtxo],
        // blockMajorVersion : Block.Version.Major,
        // treasuryTokenName : TokenName
    ): L2EventGenesis = ???

final case class L2EventGenesis(
    genesisObligations: NonEmptyList[GenesisObligation],
    // blake2b_256(treasuryTokenName.bytestring ++ nextBlockVersion)
    // TODO: Type this better? It shouldn't really be a TransactionHash, because it's
    // preimage is not a [[Transaction]]
    genesisId: TransactionHash
) extends L2Event {
    val asUtxos: Utxos = {
        Map.from(
          genesisObligations.toList.zipWithIndex.map(x =>
              (TransactionInput(genesisId, x._2), x._1.toBabbage)
          )
        )
    }
}

/** A genesis obligation is the boundary between the L1 and L2 ledgers. It contains the well-formed
  * fields of L2-conformant UTxOs.
  */
case class GenesisObligation(
    l2OutputPaymentAddress: ShelleyPaymentPart,
    l2OutputNetwork: Network,
    l2OutputDatum: SOption[Data],
    l2OutputValue: Coin,
    l2OutputRefScript: Option[Script.Native | Script.PlutusV3]
) {
    def toBabbage: Babbage =
        Babbage(
          address = ShelleyAddress(
            network = l2OutputNetwork,
            payment = l2OutputPaymentAddress,
            delegation = Null
          ),
          value = Value(l2OutputValue),
          datumOption = Some(Inline(toData(l2OutputDatum))),
          scriptRef = l2OutputRefScript.map(ScriptRef(_))
        )
}
