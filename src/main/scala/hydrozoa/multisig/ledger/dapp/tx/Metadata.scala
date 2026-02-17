package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.multisig.ledger.dapp.token.CIP67
import io.bullet.borer
import io.bullet.borer.derivation.ArrayBasedCodecs.derived
import io.bullet.borer.{Cbor, Decoder, Encoder}
import scala.util.Try
import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.AuxiliaryData.Metadata as MD
import scalus.cardano.ledger.{AuxiliaryData, Hash32, KeepRaw, Metadatum, OriginalCborByteArray, ProtocolVersion, Transaction, TransactionInput, Word64}
import scalus.uplc.builtin.ByteString

/** The metadata associated with hydrozoa L1 transactions serves two purposes:
  *
  * 1.) To assist heads in parsing certain types of transaction sequences (initialization->fallback
  * and deposit->refund) that are obtained from external sources such as users and peers. 2.) To
  * assist indexers in parsing transactions.
  *
  * The main idea is that we want to provide some helpful information in a well-known place so that
  * we don't have to waste as many resources searching through the transaction in order to determine
  * whether it's malformed or not.
  *
  * The general format of the metadata consists of:
  *
  *   - The CIP67 Head Tag (WARNING: this currently isn't compliant with the specification:
  *     https://github.com/cardano-hydrozoa/hydrozoa/issues/260)
  *   - The name of the transaction type
  *   - A cbor-encoded payload, represented as a Metadatum.List of Metadatum.Bytes, where each
  *     element is at most 64 bytes long.
  *
  * It should look something like:
  *
  * { $CIP67HeadTag: { $TransactionTypeName: [ $cborChunk1, $cborChunk2, $cborChunk3, ...] } }
  */
// FIXME: This whole thing should be probably be a trait
object Metadata {

    given Encoder[ShelleyAddress] =
        summon[Encoder[Address]].asInstanceOf[Encoder[ShelleyAddress]]

    given Decoder[ShelleyAddress] =
        summon[Decoder[Address]].asInstanceOf[Decoder[ShelleyAddress]]

    sealed trait L1TxTypes {
        val headAddress: ShelleyAddress
    }

    case class Deposit(
        override val headAddress: ShelleyAddress,
        depositUtxoIx: Int,
        virtualOutputsHash: Hash32
    ) extends L1TxTypes
    given Encoder[Deposit] = Encoder.derived
    given depositDecoder(using OriginalCborByteArray, ProtocolVersion): Decoder[Deposit] =
        Decoder.derived[Deposit]

    case class Fallback(override val headAddress: ShelleyAddress) extends L1TxTypes
    given Encoder[Fallback] = Encoder.derived
    given fallbackDecoder(using OriginalCborByteArray): Decoder[Fallback] =
        Decoder.derived[Fallback]

    case class Finalization(override val headAddress: ShelleyAddress) extends L1TxTypes
    given Encoder[Finalization] = Encoder.derived
    given finalizationDecoder(using OriginalCborByteArray): Decoder[Finalization] =
        Decoder.derived[Finalization]

    case class Initialization(
        override val headAddress: ShelleyAddress,
        treasuryOutputIndex: Int,
        multisigRegimeOutputIndex: Int,
        seedInput: TransactionInput
    ) extends L1TxTypes
    given Encoder[Initialization] = Encoder.derived
    given initializationDecoder(using OriginalCborByteArray): Decoder[Initialization] =
        Decoder.derived[Initialization]

    case class Refund(override val headAddress: ShelleyAddress) extends L1TxTypes
    given Encoder[Refund] = Encoder.derived
    given refundDecoder(using OriginalCborByteArray): Decoder[Refund] =
        Decoder.derived[Refund]

    case class Rollout(override val headAddress: ShelleyAddress) extends L1TxTypes
    given Encoder[Rollout] = Encoder.derived
    given rolloutDecoder(using OriginalCborByteArray): Decoder[Rollout] =
        Decoder.derived[Rollout]

    case class Settlement(override val headAddress: ShelleyAddress) extends L1TxTypes
    given Encoder[Settlement] = Encoder.derived
    given settlementDecoder(using OriginalCborByteArray): Decoder[Settlement] =
        Decoder.derived[Settlement]

    /** Create the auxiliary data for a specific transaction by cbor-encoding the desired metadata
      * and chunking it into a list of bytes, where each element is no more that 64 bytes long.
      * @param txType
      * @return
      */
    def apply(txType: L1TxTypes): AuxiliaryData = {

        // The max size of a metadata bytestring is 64 bytes. Thus, we chunk longer byte arrays into a list of
        // bytestrings
        def chunker(byteArray: Array[Byte]): Metadatum.List = {
            val chunked = byteArray.grouped(64).toIndexedSeq
            val bytes = chunked.map(ba => Metadatum.Bytes(ByteString.fromArray(ba)))
            Metadatum.List(bytes)
        }

        // The common structure is a metadata map, labeled by the CIP67 "HEAD" tag, pointing a map with the
        // transaction name, followed by the cbor-encoded metadata chunked into 64 bytes in a list
        def helper[A](name: String, data: A)(using encoder: Encoder[A]): AuxiliaryData = {
            val byteArray = Cbor.encode(data).toByteArray
            MD(
              Map(
                Word64(CIP67.Tags.head) -> Metadatum.Map(
                  entries = Map(
                    Metadatum.Text(name) ->
                        chunker(byteArray)
                  )
                )
              )
            )
        }

        txType match {
            case x: Deposit        => helper("Deposit", x)
            case x: Fallback       => helper("Fallback", x)
            case x: Finalization   => helper("Finalization", x)
            case x: Initialization => helper("Initialization", x)
            case x: Refund         => helper("Refund", x)
            case x: Rollout        => helper("Rollout", x)
            case x: Settlement     => helper("Settlement", x)
        }
    }

    def parse(
        txSerialized: Array[Byte]
    )(using protocolVersion: ProtocolVersion): Either[ParseError, L1TxTypes] =
        for {
            tx <- Try(Transaction.fromCbor(txSerialized)).toEither.left.map(CborDecodingError(_))
            res <- parse(tx)
        } yield res

    def parse(tx: Transaction)(using
        protocolVersion: ProtocolVersion
    ): Either[ParseError, L1TxTypes] =
        parse(tx.auxiliaryData)

    def parse(
        mbAuxData: Option[KeepRaw[AuxiliaryData]]
    )(using protocolVersion: ProtocolVersion): Either[ParseError, L1TxTypes] =
        for {
            ad <- mbAuxData.toRight(MissingAuxData)
            md: MD <- ad.value match {
                case md: MD => Right(md)
                case _      => Left(AuxDataIsNotMetadata)
            }
            mv <- md.metadata
                .get(Word64(CIP67.Tags.head))
                .toRight(MissingCIP67Tag)
            mdMap <- mv match {
                case m: Metadatum.Map => Right(m)
                case _                => Left(MetadataValueIsNotMap)
            }
            res <- parseMetadataMap(mdMap)
        } yield res

    // This is a private helper for parsing once we've destructured to the "inner" map (the value of the CIP67 key)
    private def parseMetadataMap(mdMap: Metadatum.Map)(using
        protocolVersion: ProtocolVersion
    ): Either[ParseError, L1TxTypes] =
        def helper[A](cborData: Array[Byte])(using Decoder[A]) =
            Try(Cbor.decode(cborData).to[A].value).toEither.left.map(CborDecodingError(_))

        for {
            _ <-
                if mdMap.entries.size == 1 then Right(())
                else Left(WrongNumberOfTxTypeKeys(mdMap.entries.size))

            // "Initialization", "Deposit", etc.
            txTypeKey <- mdMap.entries.head._1 match {
                case Metadatum.Text(s) => Right(s)
                case m => Left(MalformedTxTypeKey(s"Expected a Metadatum.Text, but got a: ${m}"))
            }

            metadataList <- mdMap.entries.head._2 match {
                case Metadatum.List(l) => Right(l)
                case _                 => Left(MetadataNotListError) // Not a list of values
            }

            // Concat the chunked CBOR
            cborData <- metadataList
                .foldLeft(Right(Array.empty[Byte]))(
                  (acc: Either[ParseError, Array[Byte]], elem: Metadatum) =>
                      elem match {
                          case x: Metadatum.Bytes => acc.map(_.appendedAll(x.value.bytes))
                          case _                  => Left(MetadataNotBytesError)
                      }
                )

            given OriginalCborByteArray = OriginalCborByteArray(cborData)

            txType <- txTypeKey match {
                case "Deposit"        => helper[Deposit](cborData)
                case "Fallback"       => helper[Fallback](cborData)
                case "Finalization"   => helper[Finalization](cborData)
                case "Initialization" => helper[Initialization](cborData)
                case "Refund"         => helper[Refund](cborData)
                case "Rollout"        => helper[Rollout](cborData)
                case "Settlement"     => helper[Settlement](cborData)
                case s =>
                    Left(
                      MalformedTxTypeKey(
                        s"TxTypeKey did not match the expected transaction names. Got: $s"
                      )
                    )
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
    case class UnexpectedTxType(actual: Metadata.L1TxTypes, expected: String) extends ParseError

    case class CborDecodingError(wrapped: Throwable) extends ParseError
    case object MetadataNotListError extends ParseError
    case object MetadataNotBytesError extends ParseError
}
