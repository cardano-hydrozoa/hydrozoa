package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.encodeByteString
import hydrozoa.multisig.ledger.dapp.token.CIP67
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.AuxiliaryData.Metadata as MD
import scalus.cardano.ledger.{
    AuxiliaryData,
    KeepRaw,
    Metadatum,
    Transaction,
    TransactionInput,
    Word64
}
import hydrozoa.multisig.ledger.dapp.tx.Metadata.{Initialization, L1TxTypes}
import io.bullet.borer.Cbor
import scalus.builtin.ByteString
import scalus.cardano.ledger.TransactionOutput.Babbage

import scala.util.Try

object Metadata {
    // NOTE (from Peter to George): I assume we have these types somewhere else? They used to be in DappLedger.
    sealed trait L1TxTypes {
        val headAddress: ShelleyAddress
    }

    case class Deposit(override val headAddress: ShelleyAddress) extends L1TxTypes

    case class Fallback(override val headAddress: ShelleyAddress) extends L1TxTypes

    case class Finalization(override val headAddress: ShelleyAddress) extends L1TxTypes

    case class Initialization(
        override val headAddress: ShelleyAddress,
        seedInput: TransactionInput
    ) extends L1TxTypes

    case class RefundPostDated(override val headAddress: ShelleyAddress) extends L1TxTypes

    case class RefundImmediate(override val headAddress: ShelleyAddress) extends L1TxTypes

    case class Rollout(override val headAddress: ShelleyAddress) extends L1TxTypes

    case class Settlement(override val headAddress: ShelleyAddress) extends L1TxTypes

    // TODO: Also include the head instance ID
    def apply(txType: L1TxTypes): AuxiliaryData = {

        // The common structure is a metadata map, labeled by the CIP67 "HEAD" tag, pointing a map with the
        // transaction name, followed by a list of bytestrings starting with the head address.
        // Just pass in the extra stuff you want encoded as varargs.
        def helper(name: String, list: Metadatum*): AuxiliaryData =
            MD(
              Map(
                Word64(CIP67.Tags.head) -> Metadatum.Map(
                  entries = Map(
                    Metadatum.Text(name) ->
                        Metadatum.List(
                          list.toIndexedSeq.prepended(Metadatum.Bytes(txType.headAddress.toBytes))
                        )
                  )
                )
              )
            )

        txType match {
            case Deposit(_)      => helper("Deposit")
            case Fallback(_)     => helper("Fallback")
            case Finalization(_) => helper("Finalization")
            case Initialization(_, seedInput) =>
                helper("Initialization", Metadatum.Bytes(encodeByteString(seedInput)))
            case RefundPostDated(_) => helper("RefundPostDated")
            case RefundImmediate(_) => helper("RefundImmediate")
            case Rollout(_)         => helper("Rollout")
            case Settlement(_)      => helper("Settlement")
        }
    }

    private def parseMetadataMap(mdMap: Metadatum.Map): Either[ParseError, L1TxTypes] = for {
        _ <-
            if mdMap.entries.size == 1 then Right(())
            else Left(WrongNumberOfTxTypeKeys(mdMap.entries.size))
        _ <- Right(println("6"))

        key <- mdMap.entries.head._1 match {
            case Metadatum.Text(s) => Right(s)
            case _                 => Left(MalformedTxTypeKey)
        }
        _ <- Right(println("7"))

        valueList <- mdMap.entries.head._2 match {
            case Metadatum.List(l) => Right(l)
            case _                 => Left(ValueListError) // Not a list of values
        }
        _ <- Right(println("8"))

        txType <- key match {
            case "Initialization" =>
                valueList match {
                    case IndexedSeq(headAddressBS: Metadatum.Bytes, seedInputBS: Metadatum.Bytes) =>
                        for {
                            ha <- Try(
                              Cbor
                                  .decode(headAddressBS.value.bytes)
                                  .to[Address]
                                  .value
                                  .asInstanceOf[ShelleyAddress]
                            ).toEither.left
                                .map(CborDecodingError(_))
                            si <- Try(
                              Cbor.decode(seedInputBS.value.bytes).to[TransactionInput].value
                            ).toEither.left.map(CborDecodingError(_))
                        } yield Initialization(ha, si)

                }
            case "Dpeosit" =>
                valueList match {
                    case IndexedSeq(headAddressBS: Metadatum.Bytes) =>
                        for {
                            ha <- Try(
                                Cbor
                                    .decode(headAddressBS.value.bytes)
                                    .to[Address]
                                    .value
                                    .asInstanceOf[ShelleyAddress]).toEither.left.map(CborDecodingError(_))
                        } yield Deposit(ha)
                }
            case _ => Left(MalformedTxTypeKey)
        }

    } yield txType

    def parse(mbAuxData: Option[KeepRaw[AuxiliaryData]]): Either[ParseError, L1TxTypes] =
        for {
            ad <- mbAuxData.toRight(MissingAuxData)
            _ <- Right(println("1"))
            md: MD <- ad.value match {
                case md: MD => Right(md)
                case _      => Left(AuxDataIsNotMetadata)
            }
            _ <- Right(println("2"))
            mv <- md.metadata
                .get(Word64(CIP67.Tags.head))
                .toRight(MissingCIP67Tag)
            _ <- Right(println("3"))
            mdMap <- mv match {
                case m: Metadatum.Map => Right(m)
                case _                => Left(MetadataValueIsNotMap)
            }
            _ <- Right(println("4"))
            res <- parseMetadataMap(mdMap)
        } yield res

    def parse(tx: Transaction): Either[ParseError, L1TxTypes] =
        parse(tx.auxiliaryData)

    def parse(txSerialized: Array[Byte]): Either[ParseError, L1TxTypes] =
        for {
            tx <- Try(Transaction.fromCbor(txSerialized)).toEither.left.map(CborDecodingError(_))
            res <- parse(tx)
        } yield res

    sealed trait ParseError extends Throwable
    case object MissingAuxData extends ParseError
    case object AuxDataIsNotMetadata extends ParseError
    case object MissingCIP67Tag extends ParseError
    case object MetadataValueIsNotMap extends ParseError
    // There should only be one internal map key signifying the transaction type
    case class WrongNumberOfTxTypeKeys(numKeys: Int) extends ParseError
    case object MalformedTxTypeKey extends ParseError
    case object MalformedHeadAddress extends ParseError
    case class UnexpectedTxType(actual: Metadata.L1TxTypes, expected: String)
        extends ParseError
    case class CborDecodingError(wrapped: Throwable) extends ParseError
    case object ValueListError extends ParseError
}
