package hydrozoa.multisig.ledger.virtual

import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import io.bullet.borer.Cbor
import scalus.builtin.Builtins.blake2b_256
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Hash as SHash, *}
import scalus.ledger.api.v3
import scalus.prelude.Option as SOption

// A sum type for ledger events
sealed trait L2Event:
    def getEventId: SHash[Blake2b_256, HashPurpose.TransactionHash]

final case class L2EventTransaction(transaction: Transaction) extends L2Event {
    val getEventId: SHash[Blake2b_256, HashPurpose.TransactionHash] = transaction.id
    def volume: Long = transaction.body.value.outputs.map(sto => sto.value.value.coin.value).sum
}
final case class L2EventWithdrawal(transaction: Transaction) extends L2Event {
    val getEventId: SHash[Blake2b_256, HashPurpose.TransactionHash] = transaction.id
}

object L2EventGenesis:
    enum L2EventGenesisError:
        case EmptyInputs

    /** Smart constructor for L2EventGenesis, ensuring that the event contains at least one valid
      * Genesis Utxo. A genesis event absorbs a number of transaction inputs from L1 and produces
      * corresponding L2 outputs. The TxId of a Genesis Event comes from sorting the TxIds of the
      * absorbed UTxOs, encoding them to Cbor, concatenating, and taking the blake2b_256 hash.
      */
    def apply(
        utxosL1: Seq[DepositUtxo]
    ): Either[L2EventGenesisError, L2EventGenesis] = {

        for {
            _ <- if utxosL1.nonEmpty then Right(()) else Left(L2EventGenesisError.EmptyInputs)

            hash: SHash[Blake2b_256, HashPurpose.TransactionHash] = SHash(
              blake2b_256(
                ByteString.fromArray(
                  // I know this is an insane way to do it, but transaction input apparently doesn't have an ordering instance
                  // yet
                  utxosL1
                      .map(depositUtxo => {
                          val ti = depositUtxo._1
                          ti.transactionId.toHex ++ ti.index.toString
                      })
                      .sorted
                      .flatMap(ti => Cbor.encode(ti).toByteArray)
                      .toArray
                )
              )
            )

            // Maybe use validation monad instead? This will only report the first error
            l2Obligations = utxosL1.zipWithIndex.map(utxoAndIndex =>
                createObligation(utxoAndIndex._1, utxoAndIndex._2, hash)
            )

            volume = Coin(utxosL1.map(dutxo => dutxo._4.value).sum)
        } yield L2EventGenesis(l2Obligations, hash, volume)
    }

final case class L2EventGenesis(
    private val genesisObligations: Seq[GenesisObligation],
    private val eventId: SHash[Blake2b_256, HashPurpose.TransactionHash],
    private val volume: Coin
) extends L2Event {
    override def getEventId: SHash[Blake2b_256, HashPurpose.TransactionHash] = this.eventId
}

/** Tags used in blocks */
sealed trait L2EventLabel derives CanEqual

case object L2EventGenesisLabel extends L2EventLabel derives CanEqual
case object L2EventTransactionLabel extends L2EventLabel derives CanEqual
case object L2EventWithdrawalLabel extends L2EventLabel derives CanEqual

def l2EventLabel(e: L2Event): L2EventLabel =
    e match
        case _: L2EventGenesis     => L2EventGenesisLabel
        case _: L2EventTransaction => L2EventTransactionLabel
        case _: L2EventWithdrawal  => L2EventWithdrawalLabel

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
