package hydrozoa.multisig.ledger.dapp.tx

import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.AuxiliaryData.Metadata as MD
import scalus.cardano.ledger.{
    AuxiliaryData,
    Transaction,
    TransactionMetadatum,
    TransactionMetadatumLabel
}
import Metadata.L1TxTypes.*
import hydrozoa.multisig.ledger.dapp.token.CIP67

object Metadata {
    // NOTE (from Peter to George): I assume we have these types somewhere else? They used to be in DappLedger.
    enum L1TxTypes:
        case Deposit
        case Fallback
        case Finalization
        case Initialization
        case RefundPostDated
        case RefundImmediate
        case Rollout
        case Settlement

    private def typeToString(txType: L1TxTypes): String = txType match
        case Deposit         => "Deposit"
        case Fallback        => "Fallback"
        case Finalization    => "Finalization"
        case Initialization  => "Initialization"
        case RefundPostDated => "RefundPostDated"
        case RefundImmediate => "RefundImmediate"
        case Rollout         => "Rollout"
        case Settlement      => "Settlement"

    private def stringToType(s: String): Option[L1TxTypes] = s match
        case "Deposit"         => Some(Deposit)
        case "Fallback"        => Some(Fallback)
        case "Finalization"    => Some(Finalization)
        case "Initialization"  => Some(Initialization)
        case "RefundPostDated" => Some(RefundPostDated)
        case "RefundImmediate" => Some(RefundImmediate)
        case "Rollout"         => Some(Rollout)
        case "Settlement"      => Some(Settlement)
        case _                 => None

    def apply(txType: L1TxTypes, headAddress: ShelleyAddress): AuxiliaryData = {
        MD(
          Map(
            TransactionMetadatumLabel(CIP67.Tags.head) ->
                TransactionMetadatum.Map(entries =
                    Map(
                      TransactionMetadatum.Text(typeToString(txType))
                          -> TransactionMetadatum.Bytes(headAddress.toBytes)
                    )
                )
          )
        )
    }

    sealed trait ParseError extends Throwable
    case object MissingAuxData extends ParseError
    case object AuxDataIsNotMetadata extends ParseError
    case object MissingCIP67Tag extends ParseError
    case object MetadataValueIsNotMap extends ParseError
    // There should only be one internal map key signifying the transaction type
    case class WrongNumberOfTxTypeKeys(numKeys: Int) extends ParseError
    case object MalformedTxTypeKey extends ParseError
    case object MalformedHeadAddress extends ParseError
    case class UnexpectedTxType(actual: L1TxTypes, expected: L1TxTypes) extends ParseError

    def parse(tx: Transaction): Either[ParseError, (L1TxTypes, ShelleyAddress)] = {
        for {
            ad: AuxiliaryData <- tx.auxiliaryData.toRight(MissingAuxData)
            md: MD <- ad match {
                case md: MD => Right(md)
                case _      => Left(AuxDataIsNotMetadata)
            }
            mv <- md.metadata
                .get(TransactionMetadatumLabel(CIP67.Tags.head))
                .toRight(MissingCIP67Tag)
            mdMap <- mv match {
                case m: TransactionMetadatum.Map => Right(m.entries)
                case _                           => Left(MetadataValueIsNotMap)
            }
            _ <- if mdMap.size == 1 then Right(()) else Left(WrongNumberOfTxTypeKeys(mdMap.size))
            txType <- mdMap.head._1 match {
                case TransactionMetadatum.Text(s) => stringToType(s).toRight(MalformedTxTypeKey)
                case _                            => Left(MalformedTxTypeKey)
            }
            addr <- mdMap.head._2 match {
                case TransactionMetadatum.Bytes(b) =>
                    Address.fromByteString(b) match {
                        case sa: ShelleyAddress => Right(sa)
                        case _                  => Left(MalformedHeadAddress)
                    }
                case _ => Left(MalformedHeadAddress)
            }

        } yield (txType, addr)
    }

    def parseExpected(
        tx: Transaction,
        expectedTxType: L1TxTypes
    ): Either[ParseError, ShelleyAddress] =
        parse(tx) match {
            case Right(t) =>
                if t._1 == expectedTxType then Right(t._2)
                else Left(UnexpectedTxType(t._1, expectedTxType))
            case Left(e) => Left(e)
        }

}
