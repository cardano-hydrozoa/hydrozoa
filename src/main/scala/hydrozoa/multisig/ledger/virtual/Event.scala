package hydrozoa.multisig.ledger.virtual

import cats.*
import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.GenesisObligation.UtxoToGenesisError
import hydrozoa.multisig.ledger.virtual.GenesisObligation.UtxoToGenesisError.*
import hydrozoa.multisig.ledger.virtual.L2EventGenesis.L2EventGenesisError.*
import io.bullet.borer.Cbor
import scalus.builtin.Builtins.blake2b_256
import scalus.builtin.ByteString
import scalus.builtin.Data.{fromData, toData}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Hash as SHash, *}

import scala.util.{Failure, Success, Try}

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
        case GenesisObligationError(e: GenesisObligation.UtxoToGenesisError)

    /** Smart constructor for L2EventGenesis, ensuring that the event contains at least one valid
      * Genesis Utxo. A genesis event absorbs a number of transaction inputs from L1 and produces
      * corresponding L2 outputs. The TxId of a Genesis Event comes from sorting the TxIds of the
      * absorbed UTxOs, encoding them to Cbor, concatenating, and taking the blake2b_256 hash.
      */
    def apply(
        utxosL1: Seq[(TransactionInput, TransactionOutput)]
    ): Either[L2EventGenesisError, L2EventGenesis] = {
        val hash: SHash[Blake2b_256, HashPurpose.TransactionHash] = SHash(
          blake2b_256(
            ByteString.fromArray(
              // I know this is an insane way to do it, but transaction input apparently doesn't have an ordering instance
              // yet
              utxosL1
                  .map((ti, _) => ti.transactionId.toHex ++ ti.index.toString)
                  .sorted
                  .flatMap(ti => Cbor.encode(ti).toByteArray)
                  .toArray
            )
          )
        )
        for {
            _ <- if utxosL1.nonEmpty then Right(()) else Left(L2EventGenesisError.EmptyInputs)
            // Maybe use validation monad instead? This will only report the first error
            l2Obligations <- utxosL1.zipWithIndex
                .traverse((utxo, index) => createObligation(utxo, index, hash))
                .left
                .map(GenesisObligationError(_))

            volume = Coin(utxosL1.map((_, to) => to.value.coin.value).sum)
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
    private val outputPaymentAddress: ShelleyPaymentPart,
    private val outputNetwork: Network,
    private val outputDatum: DepositUtxo.Datum,
    private val outputValue: Coin,
    private val outputRefScript: Option[Script.Native | Script.PlutusV3]
) {
    def toUtxo: (TransactionInput, TransactionOutput) =
        (
          input,
          Babbage(
            address = ShelleyAddress(
              network = outputNetwork,
              payment = outputPaymentAddress,
              delegation = Null
            ),
            value = Value(outputValue),
            datumOption = Some(Inline(toData(outputDatum))),
            scriptRef = outputRefScript.map(ScriptRef(_))
          )
        )
}

object GenesisObligation:
    enum UtxoToGenesisError {
        case DepositUtxoNotBabbage
        case AddressNotShelley
        case InvalidDelegation
        case InvalidDatumType
        case InvalidDatumContent(e: Throwable)
        case InvalidValue
        case InvalidRefScript
    }

// This is a private constructor because we cannot create a genesis obligation in
// isolation -- we must construct batches in order to know the correct index
// and tx hash
private def createObligation(
    utxo: (TransactionInput, TransactionOutput),
    index: Int,
    txId: TransactionHash
): Either[UtxoToGenesisError, GenesisObligation] =
    for {
        babbage <- utxo._2 match {
            case babbage: Babbage => Right(babbage)
            case _                => Left(DepositUtxoNotBabbage)
        }
        addrAndNetwork <- babbage.address match {
            case sa: ShelleyAddress =>
                sa.delegation match {
                    case Null => Right(sa.payment, sa.network)
                    case _    => Left(InvalidDelegation)
                }
            case _ => Left(UtxoToGenesisError.AddressNotShelley)
        }
        datum <- babbage.datumOption match {
            case Some(Inline(d)) =>
                Try(fromData[DepositUtxo.Datum](d)) match {
                    case Failure(e)  => Left(UtxoToGenesisError.InvalidDatumContent(e))
                    case Success(dd) => Right(dd)
                }
            case _ => Left(UtxoToGenesisError.InvalidDatumType)
        }
        value <-
            if babbage.value.assets == MultiAsset.empty then Right(babbage.value.coin)
            else Left(InvalidValue)
        refScript <- babbage.scriptRef match {
            case None => Right(None)
            case Some(ScriptRef(s)) =>
                s match {
                    case n: Script.Native    => Right(Some(n))
                    case v3: Script.PlutusV3 => Right(Some(v3))
                    case _                   => Left(InvalidRefScript)
                }
        }

    } yield GenesisObligation(
      input = TransactionInput(txId, index),
      outputPaymentAddress = addrAndNetwork._1,
      outputNetwork = addrAndNetwork._2,
      outputDatum = datum,
      outputValue = value,
      outputRefScript = refScript
    )
