package hydrozoa.multisig.ledger.dapp.utxo

import cats.data.NonEmptyList
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toEpochQuantizedInstant}
import hydrozoa.lib.cardano.scalus.ledger.plutusAddressToShelley
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo.DepositUtxoConversionError.*
import hydrozoa.multisig.ledger.virtual.tx.GenesisObligation
import scala.util.{Failure, Success, Try}
import scalus.*
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.prelude.{Option as ScalusOption, asScalus}
import scalus.cardano.onchain.plutus.v3.{Address as PlutusAddress, PosixTime}
import scalus.uplc.builtin.Data.{FromData, ToData, fromData, toData}
import scalus.uplc.builtin.{Data, FromData, ToData}

final case class DepositUtxo(
    utxoId: TransactionInput,
    address: ShelleyAddress,
    datum: DepositUtxo.Datum,
    value: Value,
    virtualOutputs: NonEmptyList[GenesisObligation],
    depositFee: Coin,
    // This field comes from DepositTx, but it's convenient to duplicate it here
    submissionDeadline: QuantizedInstant,
) {
    def toUtxo: Utxo =
        Utxo(
          utxoId,
          TransactionOutput.apply(
            address = address,
            value = value,
            datumOption = Some(Inline(toData(datum))),
            scriptRef = None
          )
        )

    // NOTE: I need this in the dapp ledger, but can't access it because the field is private and
    // this is a separate package.
    // we could make the _constructor_ private with
    //   final case class DepositUtxo private ( ... )
    // but then we need to make sure that the DepositTx build can still access it...
    // val datum: DepositUtxo.Datum = datum
}

object DepositUtxo {

    /** A deposit utxo's datum contains:
      * @param refundInstructions
      *   instructions for when and how the deposit should be refunded if it is not absorbed into
      *   the head's treasury.
      */
    final case class Datum(
        refundInstructions: Refund.Instructions.Onchain
    ) derives FromData,
          ToData

    object Refund {

        // TODO: move around?
        /** A deposit's refund instructions specify:
          *
          * @param address
          *   the address to which the deposit's funds should be refunded.
          * @param mbDatum
          *   the datum that should be attached to the refund utxo.
          * @param validityStart
          *   currently just matches the post-dated refund tx validity start time. when observer
          *   scripts are here, this will be used to allow users to recover their deposits if they
          *   are not absorbed without a post-dated presigned transaction.
          */
        final case class Instructions private (
            address: ShelleyAddress,
            mbDatum: Option[Data],
            validityStart: QuantizedInstant
        )

        object Instructions {

            def apply(
                address: ShelleyAddress,
                datum: Option[Data],
                validityStart: QuantizedInstant
            ): Instructions =
                new Instructions(address, datum, validityStart)

            def apply(onchain: Onchain, network: Network, slotConfig: SlotConfig): Instructions = {
                Instructions(
                  address = plutusAddressToShelley(onchain.address, network),
                  datum = onchain.datum.asScala,
                  validityStart = onchain.validityStart.toEpochQuantizedInstant(slotConfig),
                )
            }

            final case class Onchain(
                address: PlutusAddress,
                datum: ScalusOption[Data],
                validityStart: PosixTime
            ) derives FromData,
                  ToData

            object Onchain {
                def apply(offchainInstructions: Instructions): Onchain = {
                    import offchainInstructions.*

                    Onchain(
                      address = LedgerToPlutusTranslation.getAddress(address),
                      datum = mbDatum.asScalus,
                      validityStart = validityStart.instant.toEpochMilli
                    )
                }
            }
        }
    }

    // TODO: review
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
            DepositUtxoConversionError
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
        virtualOutputs: NonEmptyList[GenesisObligation],
        depositFee: Coin,
        submissionDeadline: QuantizedInstant
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
          utxoId = utxo._1,
          address = addr,
          datum = datum,
          value = babbage.value,
          virtualOutputs = virtualOutputs,
          depositFee = depositFee,
          submissionDeadline = submissionDeadline
        )
}
