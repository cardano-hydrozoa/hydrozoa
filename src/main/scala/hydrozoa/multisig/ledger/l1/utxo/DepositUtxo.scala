package hydrozoa.multisig.ledger.l1.utxo

import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{DepositAbsorptionStartTime, DepositSubmissionDeadline, RequestValidityEndTime}
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toEpochQuantizedInstant}
import hydrozoa.lib.cardano.scalus.ledger.plutusAddressToShelley
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo.DepositUtxoConversionError.*
import scala.util.{Failure, Success, Try}
import scalus.*
import scalus.cardano.address.{Network, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.onchain.plutus.prelude.{Option as ScalusOption, asScalus}
import scalus.cardano.onchain.plutus.v3.{Address as PlutusAddress, PosixTime}
import scalus.uplc.builtin.Data.{FromData, ToData, fromData, toData}
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}

/** @param l2Payload
  *   The L2 payload associated with the deposit transaction that created this deposit utxo. A hash
  *   of this must appear in the metadata of the corresponding deposit transaction. This includes
  *   ONLY the part relevant to L2, and not the hydrozoa/cardano/L1-specific stuff.
  */
final case class DepositUtxo(
    utxoId: Input,
    address: ShelleyAddress,
    datum: DepositUtxo.Datum,
    value: Value,
    l2Payload: ByteString,
    depositFee: Coin,
    requestValidityEndTime: RequestValidityEndTime,
    submissionDeadline: DepositSubmissionDeadline,
    absorptionStartTime: DepositAbsorptionStartTime
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

    val l2Value: Value = value - Value(depositFee)
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
                  validityStart = onchain.refundStart.toEpochQuantizedInstant(slotConfig),
                )
            }

            final case class Onchain(
                address: PlutusAddress,
                datum: ScalusOption[Data],
                refundStart: PosixTime
            ) derives FromData,
                  ToData

            object Onchain {
                def apply(offchainInstructions: Instructions): Onchain = {
                    import offchainInstructions.*

                    Onchain(
                      address = LedgerToPlutusTranslation.getAddress(address),
                      datum = mbDatum.asScalus,
                      refundStart = validityStart.instant.toEpochMilli
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

    def parseUtxo(
        utxo: Utxo,
        headNativeScriptAddress: ShelleyAddress,
        l2Payload: ByteString,
        depositFee: Coin,
        requestValidityEndTime: RequestValidityEndTime,
        txTiming: TxTiming
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

            submissionDeadline = txTiming.depositSubmissionDeadline(requestValidityEndTime)
            absorptionStartTime = txTiming.depositAbsorptionStartTime(requestValidityEndTime)

        } yield new DepositUtxo(
          utxoId = utxo._1,
          address = addr,
          datum = datum,
          value = babbage.value,
          l2Payload = l2Payload,
          depositFee = depositFee,
          requestValidityEndTime = requestValidityEndTime,
          submissionDeadline = submissionDeadline,
          absorptionStartTime = absorptionStartTime
        )
}
