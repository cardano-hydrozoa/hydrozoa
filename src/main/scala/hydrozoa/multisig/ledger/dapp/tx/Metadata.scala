package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.encodeByteString
import hydrozoa.multisig.ledger.dapp.token.CIP67
import io.bullet.borer.Cbor
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.AuxiliaryData.Metadata as MD
import scalus.cardano.ledger.{AuxiliaryData, KeepRaw, Metadatum, Transaction, TransactionInput, Word64}

import scala.util.Try

/**
 * The metadata associated with hydrozoa L1 transactions serves two purposes:
 *
 * 1.) To assist heads in parsing certain types of transaction sequences (initialization->fallback and deposit->refund)
 * that are obtained from external sources such as users and peers.
 * 2.) To assist indexers in parsing transactions.
 *
 * The main idea is that we want to provide some helpful information in a well-known place so that we don't have to
 * waste as many resources searching through the transaction in order to determine whether its malformed or not.
 *
 * The general format of the metadata consists of:
 *
 * - The CIP67 Head Tag (WARNING: this currently isn't compliant with the specification: https://github.com/cardano-hydrozoa/hydrozoa/issues/260)
 * - The name of the transaction type
 * - The head address
 * - A list of transaction-specific datums, cbor-encoded as byte strings
 *
 * It should look something like:
 *
 * {
 * $CIP67HeadTag: {
 * $TransactionTypeName: [ $headAddressInCbor, $CBORDatum1, $CBORDatum2, ...]
 * }
 * }
 */
object Metadata {
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
        def helper(name: String, list: Metadatum.Bytes*): AuxiliaryData =
            MD(
              Map(
                Word64(CIP67.Tags.head) -> Metadatum.Map(
                  entries = Map(
                    Metadatum.Text(name) ->
                        Metadatum.List(
                            list.toIndexedSeq
                                .prepended(
                                    Metadatum.Bytes(encodeByteString(txType.headAddress.asInstanceOf[Address]))
                        )
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

    def parse(txSerialized: Array[Byte]): Either[ParseError, L1TxTypes] =
        for {
            tx <- Try(Transaction.fromCbor(txSerialized))
                .toEither.left.map(CborDecodingError("could not decode serialized tx", _))
            res <- parse(tx)
        } yield res

    def parse(tx: Transaction): Either[ParseError, L1TxTypes] =
        parse(tx.auxiliaryData)

    def parse(mbAuxData: Option[KeepRaw[AuxiliaryData]]): Either[ParseError, L1TxTypes] =
        for {
            ad <- mbAuxData.toRight(MissingAuxData)
            md: MD <- ad.value match {
                case md: MD => Right(md)
                case _ => Left(AuxDataIsNotMetadata)
            }
            mv <- md.metadata
                .get(Word64(CIP67.Tags.head))
                .toRight(MissingCIP67Tag)
            mdMap <- mv match {
                case m: Metadatum.Map => Right(m)
                case _ => Left(MetadataValueIsNotMap)
            }
            res <- parseMetadataMap(mdMap)
        } yield res

    // This is a private helper for parsing once we've destructured to the "inner" map (the value of the CIP67 key)
    private def parseMetadataMap(mdMap: Metadatum.Map): Either[ParseError, L1TxTypes] = for {
        _ <- if mdMap.entries.size == 1 then Right(())
        else Left(WrongNumberOfTxTypeKeys(mdMap.entries.size))

        // "Initialization", "Deposit", etc.
        txTypeKey <- mdMap.entries.head._1 match {
            case Metadatum.Text(s) => Right(s)
            case m => Left(MalformedTxTypeKey(s"Expected a Metadatum.Text, but got a: ${m}"))
        }

        valueList <- mdMap.entries.head._2 match {
            case Metadatum.List(l) => Right(l)
            case _                 => Left(ValueListError) // Not a list of values
        }

        txType <- txTypeKey match {
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
                                .map(CborDecodingError("could not decode head address for Initialization tx", _))
                            si <- Try(
                              Cbor.decode(seedInputBS.value.bytes).to[TransactionInput].value
                            ).toEither.left.map(CborDecodingError("could not decode seed input for initialization tx", _))
                        } yield Initialization(ha, si)

                }
            case "Deposit" =>
                valueList match {
                    case IndexedSeq(headAddressBS: Metadatum.Bytes) =>
                        for {
                            ha <- Try(
                                Cbor
                                    .decode(headAddressBS.value.bytes)
                                    .to[Address]
                                    .value
                                    .asInstanceOf[ShelleyAddress])
                                .toEither
                                .left
                                .map(CborDecodingError("could not decode head address for deposit tx", _))
                        } yield Deposit(ha)
                }
            case s => Left(MalformedTxTypeKey(s"TxTypeKey did not match the expected transaction names. Got: $s"))
        }

    } yield txType

    sealed trait ParseError extends Throwable
    case object MissingAuxData extends ParseError
    case object AuxDataIsNotMetadata extends ParseError
    case object MissingCIP67Tag extends ParseError
    case object MetadataValueIsNotMap extends ParseError
    // There should only be one internal map key signifying the transaction type
    case class WrongNumberOfTxTypeKeys(numKeys: Int) extends ParseError

    case class MalformedTxTypeKey(msg: String) extends ParseError
    case object MalformedHeadAddress extends ParseError
    case class UnexpectedTxType(actual: Metadata.L1TxTypes, expected: String)
        extends ParseError

    case class CborDecodingError(msg: String, wrapped: Throwable) extends ParseError
    case object ValueListError extends ParseError
}
