package hydrozoa.multisig.ledger.dapp.utxo

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo.DepositUtxoConversionError.*
import scala.util.{Failure, Success, Try}
import scalus.*
import scalus.builtin.Data.{FromData, ToData, fromData, toData}
import scalus.builtin.{Data, FromData, ToData}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.ledger.api.v3.{Address, PosixTime}
import scalus.prelude.Option as ScalusOption

final case class DepositUtxo(
    l1Input: TransactionInput,
    l1OutputAddress: ShelleyAddress,
    l1OutputDatum: DepositUtxo.Datum,
    l1OutputValue: Value,
    virtualOutputs: NonEmptyList[TransactionOutput.Babbage]
) {
    def toUtxo: Utxo =
        Utxo(
          l1Input,
          TransactionOutput.apply(
            address = l1OutputAddress,
            value = l1OutputValue,
            datumOption = Some(Inline(toData(l1OutputDatum))),
            scriptRef = None
          )
        )
    // NOTE: I need this in the dapp ledger, but can't access it because the field is private and
    // this is a separate package.
    // we could make the _constructor_ private with
    //   final case class DepositUtxo private ( ... )
    // but then we need to make sure that the DepositTx build can still access it...
    val datum: DepositUtxo.Datum = l1OutputDatum
}

object DepositUtxo {

    /** A deposit utxo's datum contains:
      * @param refundInstructions
      *   instructions for when and how the deposit should be refunded if it is not absorbed into
      *   the head's treasury.
      */
    final case class Datum(refundInstructions: Refund.Instructions) derives FromData, ToData

    object Refund {

        /** A deposit's refund instructions specify:
          *
          * @param address
          *   the address to which the deposit's funds should be refunded.
          * @param datum
          *   the datum that should be attached to the refund utxo.
          * @param startTime
          *   starting at this time, the head must refund the deposit's funds and must not attempt
          *   to absorb them into the head's treasury. -
          */
        final case class Instructions(
            address: Address,
            datum: ScalusOption[Data],
            startTime: PosixTime,
        ) derives FromData,
              ToData
    }

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
        case NotAtExpectedHeadAddress
        case InvalidDatumContent(e: Throwable)
        case InvalidDatumType
        case RefScriptNotAllowed

    def fromUtxo(
        utxo: Utxo,
        headNativeScriptAddress: ShelleyAddress,
        virtualOutputs: NonEmptyList[TransactionOutput.Babbage]
    ): Either[DepositUtxoConversionError, DepositUtxo] =
        for {
            babbage <- utxo._2 match {
                case babbage: Babbage => Right(babbage)
                case _                => Left(DepositUtxoNotBabbage)
            }
            addr <- babbage.address match {
                case sa: ShelleyAddress if sa == headNativeScriptAddress => Right(sa)
                case _ => Left(NotAtExpectedHeadAddress)
            }
            datum <- babbage.datumOption match {
                case Some(Inline(d)) =>
                    Try(fromData[DepositUtxo.Datum](d)) match {
                        case Failure(e)  => Left(InvalidDatumContent(e))
                        case Success(dd) => Right(dd)
                    }
                case _ => Left(InvalidDatumType)
            }

            refScript: Option[Script.Native | Script.PlutusV3] <- babbage.scriptRef match {
                case None               => Right(None)
                case Some(ScriptRef(s)) => Left(RefScriptNotAllowed)
            }

        } yield DepositUtxo(
          l1Input = utxo._1,
          l1OutputAddress = addr,
          l1OutputDatum = datum,
          l1OutputValue = babbage.value,
          virtualOutputs = virtualOutputs
        )
}
