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
import scalus.cardano.ledger.{Hash as SHash, *}
import scalus.ledger.api.v3
import scalus.prelude.Option as SOption

// A sum type for ledger events
sealed trait L2Event:
    def getEventId: TransactionHash

final case class L2EventTransaction(transaction: Transaction) extends L2Event {
    val getEventId: TransactionHash = transaction.id
    def volume: Long = transaction.body.value.outputs.map(sto => sto.value.value.coin.value).sum
}
final case class L2EventWithdrawal(transaction: Transaction) extends L2Event {
    val getEventId: TransactionHash = transaction.id
}

// TODO: Rename to L2Genesis
object L2EventGenesis:
    enum L2EventGenesisError:
        case EmptyInputs

    /** Smart constructor for L2EventGenesis, ensuring that the event contains at least one valid
      * Genesis Utxo. A genesis event absorbs a number of transaction inputs from L1 and produces
      * corresponding L2 outputs. The TxId of a Genesis Event comes from a hash of the block number
      * and token name of the head
      */
    def fromDepositUtxos(
        utxosL1: NonEmptyList[DepositUtxo],
        // blockMajorVersion : Block.Version.Major,
        // treasuryTokenName : TokenName
    ): L2EventGenesis = {
        val hash: SHash[Blake2b_256, HashPurpose.TransactionHash] = ???
        /*
        hash(treasuryTokenNAme ++ blockMajorVersion.toString)
         */

        // Maybe use validation monad instead? This will only report the first error
        val l2Obligations = utxosL1.zipWithIndex.map(utxoAndIndex =>
            createObligation(utxoAndIndex._1, utxoAndIndex._2, hash)
        )

        val volume = Coin(utxosL1.toList.map(dutxo => dutxo._4.value).sum)
        L2EventGenesis(l2Obligations, hash, volume)
    }

final case class L2EventGenesis private (
    genesisObligations: NonEmptyList[GenesisObligation],
    eventId: SHash[Blake2b_256, HashPurpose.TransactionHash],
    volume: Coin
) extends L2Event {
    // TODO: This is not an "EventId" anymore, this is just the "genesis Id" -- its the transaction hash
    override def getEventId: TransactionHash = this.eventId
}

/** Tags used in blocks */
sealed trait L2EventLabel derives CanEqual

case object L2EventGenesisLabel extends L2EventLabel derives CanEqual
case object L2EventTransactionLabel extends L2EventLabel derives CanEqual

def l2EventLabel(e: L2Event): L2EventLabel =
    e match
        case _: L2EventGenesis     => L2EventGenesisLabel
        case _: L2EventTransaction => L2EventTransactionLabel

/** A genesis obligation is the boundary between the L1 and L2 ledgers. It contains the well-formed
  * fields of L2-conformant UTxOs.
  */
case class GenesisObligation(
    private val input: TransactionInput,
    private val l2OutputPaymentAddress: ShelleyPaymentPart,
    private val l2OutputNetwork: Network,
    private val l2OutputDatum: SOption[Data],
    private val l2OutputValue: Coin,
    private val l2OutputRefScript: Option[Script.Native | Script.PlutusV3]
) {
    def toUtxo: (TransactionInput, TransactionOutput) =
        (
          input,
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
        )
}

// This is a private constructor because we cannot create a genesis obligation in
// isolation -- we must construct batches in order to know the correct index
// and tx hash
private def createObligation(
    utxo: DepositUtxo,
    index: Int,
    txId: TransactionHash
): GenesisObligation = {
    val datum = utxo._3
    GenesisObligation(
      input = TransactionInput(txId, index),
      l2OutputPaymentAddress = v3
          .Address(credential = datum.address, stakingCredential = SOption.None)
          .toScalusLedger(utxo._2.network)
          .payment,
      l2OutputNetwork = utxo._2.network,
      l2OutputDatum = datum.datum,
      l2OutputValue = utxo._4,
      l2OutputRefScript = utxo._5
    )
}
