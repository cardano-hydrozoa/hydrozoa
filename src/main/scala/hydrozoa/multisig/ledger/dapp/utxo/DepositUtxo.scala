package hydrozoa.multisig.ledger.dapp.utxo

import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo.DepositUtxoConversionError.*

import scala.util.{Failure, Success, Try}
import scalus.*
import scalus.builtin.Data.{FromData, ToData, fromData, toData}
import scalus.builtin.{Data, FromData, ToData}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.ledger.api.v1.PosixTime
import scalus.ledger.api.v3.{Address, Credential}
import scalus.prelude.Option as ScalusOption

import scala.collection.immutable.Queue

final case class DepositUtxo(
    private val l1Input: TransactionInput,
    private val l1OutputAddress: ShelleyAddress,
    private val l1OutputDatum: DepositUtxo.Datum,
    private val l1OutputValue: Coin,
    private val l1RefScript: Option[Script.Native | Script.PlutusV3]
) {
    def toUtxo: (TransactionInput, TransactionOutput) =
        (
          l1Input,
          Babbage(
            address = l1OutputAddress,
            value = Value(l1OutputValue),
            datumOption = Some(Inline(toData(l1OutputDatum))),
            scriptRef = l1RefScript.map(ScriptRef(_))
          )
        )

}

object DepositUtxo {
    trait Spent {
        def depositSpent: DepositUtxo
    }

    trait Produced {
        def depositProduced: DepositUtxo
    }

    object Many {
        trait Spent {
            def depositsSpent: Vector[DepositUtxo]
        }

        trait ToSpend {
            def depositsToSpend: Vector[DepositUtxo]
        }

        object Spent {
            trait Partition extends Spent, ToSpend
        }
        
        trait Produced {
            def depositsProduced: Vector[DepositUtxo]
        }

        trait ToProduce {
            def depositsToProduce: Vector[DepositUtxo]
        }
        
        object Produced {
            trait Partition extends Produced, ToProduce
        }
    }

    enum DepositUtxoConversionError:
        case DepositUtxoNotBabbage
        case AddressNotShelley
        case InvalidDatumContent(e: Throwable)
        case InvalidDatumType
        case InvalidValue
        case InvalidRefScript

    def fromUtxo(
        utxo: (TransactionInput, TransactionOutput)
    ): Either[DepositUtxoConversionError, DepositUtxo] =
        for {
            babbage <- utxo._2 match {
                case babbage: Babbage => Right(babbage)
                case _                => Left(DepositUtxoNotBabbage)
            }
            addr <- babbage.address match {
                case sa: ShelleyAddress => Right(sa)
                case _                  => Left(AddressNotShelley)
            }
            datum <- babbage.datumOption match {
                case Some(Inline(d)) =>
                    Try(fromData[DepositUtxo.Datum](d)) match {
                        case Failure(e)  => Left(InvalidDatumContent(e))
                        case Success(dd) => Right(dd)
                    }
                case _ => Left(InvalidDatumType)
            }
            value <-
                if babbage.value.assets == MultiAsset.empty then Right(babbage.value.coin)
                else Left(InvalidValue)

            refScript: Option[Script.Native | Script.PlutusV3] <- babbage.scriptRef match {
                case None => Right(None)
                case Some(ScriptRef(s)) =>
                    s match {
                        case n: Script.Native    => Right(Some(n))
                        case v3: Script.PlutusV3 => Right(Some(v3))
                        case _                   => Left(InvalidRefScript)
                    }
            }

        } yield DepositUtxo(
          l1Input = utxo._1,
          l1OutputAddress = addr,
          l1OutputDatum = datum,
          l1OutputValue = value,
          l1RefScript = refScript
        )

    /** This is all placeholder stuff: */

    case class Datum(
        address: Credential,
        datum: ScalusOption[Data],
        deadline: PosixTime,
        refundAddress: Address,
        refundDatum: ScalusOption[Data]
    ) derives FromData,
          ToData
}
