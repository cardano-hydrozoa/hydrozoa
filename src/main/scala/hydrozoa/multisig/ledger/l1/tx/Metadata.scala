package hydrozoa.multisig.ledger.l1.tx

import hydrozoa.config.head.initialization.InitializationParameters.HeadId
import hydrozoa.multisig.ledger.l1.token.CIP67
import hydrozoa.multisig.ledger.l1.tx.Tx.Type
import scala.util.Try
import scalus.cardano.ledger.AuxiliaryData.Metadata as MD
import scalus.cardano.ledger.{AssetName, AuxiliaryData, Coin, Hash32, Metadatum, ProtocolVersion, Transaction, Word64}

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
sealed trait Metadata(val txType: Tx.Type) {
    def asMap: Map[Metadatum, Metadatum] = Map.empty

    /** Create the auxiliary data for a specific transaction by cbor-encoding the desired metadata
      * and chunking it into a list of bytes, where each element is no more than 64 bytes long.
      *
      * @param md
      * @return
      *
      * The common structure is a metadata map, labeled by the CIP67 "HYDR" tag, pointing a map with
      * the transaction type name, pointing at a map with the head ID, pointing at the actual
      * Hydrozoa tx metadata.
      */
    final def asAuxData(headId: HeadId): AuxiliaryData = {
        val txTypeName = txType.toString

        val headMdMap =
            Metadatum.Map(Map(Metadatum.Text(headId.toHex) -> Metadatum.Map(asMap)))

        val txTypeMdMap = Metadatum.Map(Map(Metadatum.Text(txTypeName) -> headMdMap))

        MD(Map(Word64(CIP67.Tags.head) -> txTypeMdMap))
    }
}

object Metadata {

    trait Parser[T](val txType: Tx.Type) {
        def parseInner(innerMap: Metadatum.Map): Either[ParseError, T]

        final def parse(
            protocolVersion: ProtocolVersion,
            txSerialized: Array[Byte]
        ): Either[ParseError, (HeadId, T)] = for {
            md <- parseMdFromTxSerialized(protocolVersion, txSerialized)
            res <- parse(md)
        } yield res

        final def parse(tx: Transaction): Either[ParseError, (HeadId, T)] = for {
            md <- parseMdFromTx(tx)
            res <- parse(md)
        } yield res

        final def parse(md: MD): Either[ParseError, (HeadId, T)] = for {
            hydrMap <- parseHydrMapFromMd(md)
            roleMap <- parseRoleMapFromHydrMap(txType, hydrMap)
            innerMapRes <- parseInnerMapFromRoleMap(roleMap)
            (headId, innerMap) = innerMapRes
            res <- parseInner(innerMap)
        } yield (headId, res)
    }

    /** @param depositIx
      *   The output index of the deposit utxo in this transaction
      * @param depositFee
      *   The deposit fee, which the head will absorb from the deposit into equity.
      * @param l2PayloadHash
      *   The L2 payload is passed out-of-band. This is the blake2b_256 hash of that payload
      */
    case class Deposit(
        depositIx: Int,
        depositFee: Coin,
        l2PayloadHash: Hash32,
    ) extends Metadata(Tx.Type.Deposit) {
        override def asMap: Map[Metadatum, Metadatum] = Map.from(
          List(
            Metadatum.Text("depositIx") -> Metadatum.Int(depositIx),
            Metadatum.Text("depositFee") -> Metadatum.Int(depositFee.value),
            Metadatum.Text("l2PayloadHash") -> Metadatum.Text(l2PayloadHash.toHex)
          )
        )
    }

    object Deposit extends Parser[Deposit](Tx.Type.Deposit) {
        override def parseInner(innerMap: Metadatum.Map): Either[ParseError, Deposit] = {
            val innerMapEntries = innerMap.entries
            for {
                depositIxRaw <- innerMapEntries
                    .get(Metadatum.Text("depositIx"))
                    .toRight(
                      MissingMetadataKeyForTransactionType(
                        "depositIx",
                        innerMapEntries.keys.map(_.toString).toList
                      )
                    )

                depositFeeRaw <- innerMapEntries
                    .get(Metadatum.Text("depositFee"))
                    .toRight(
                      MissingMetadataKeyForTransactionType(
                        "depositFee",
                        innerMapEntries.keys.map(_.toString).toList
                      )
                    )
                l2PayloadHashRaw <- innerMapEntries
                    .get(Metadatum.Text("l2PayloadHash"))
                    .toRight(
                      MissingMetadataKeyForTransactionType(
                        "l2PayloadHash",
                        innerMapEntries.keys.map(_.toString).toList
                      )
                    )

                depositIx <- depositIxRaw match {
                    case i: Metadatum.Int => Right(i.value.intValue)
                    case _                => Left(WrongMetadataValue("depositIx", depositIxRaw))
                }

                depositFee <- depositFeeRaw match {
                    case i: Metadatum.Int => Right(i.value.intValue)
                    case _                => Left(WrongMetadataValue("depositFee", depositFeeRaw))
                }

                l2PayloadHash <- l2PayloadHashRaw match {
                    case i: Metadatum.Text => Right(i)
                    case _ => Left(WrongMetadataValue("l2PayloadHash", l2PayloadHashRaw))
                }
            } yield Deposit(
              depositIx = depositIx,
              depositFee = Coin(depositFee),
              l2PayloadHash = Hash32.fromHex(l2PayloadHash.value)
            )
        }
    }

    case class Fallback() extends Metadata(Tx.Type.Fallback)

    object Fallback extends Parser[Fallback](Tx.Type.Fallback) {
        override def parseInner(innerMap: Metadatum.Map): Either[ParseError, Fallback] =
            Right(Fallback())
    }

    case class Finalization() extends Metadata(Tx.Type.Finalization)

    object Finalization extends Parser[Finalization](Tx.Type.Finalization) {
        override def parseInner(innerMap: Metadatum.Map): Either[ParseError, Finalization] =
            Right(Finalization())
    }

    // FIXME: All of these must be non-negative.
    case class Initialization(
        multisigTreasuryIx: Int,
        multisigRegimeIx: Int,
        seedIx: Int
    ) extends Metadata(Tx.Type.Initialization) {
        override def asMap: Map[Metadatum, Metadatum] = Map.from(
          List(
            Metadatum.Text("multisigTreasuryIx") -> Metadatum.Int(multisigTreasuryIx),
            Metadatum.Text("multisigRegimeIx") -> Metadatum.Int(multisigRegimeIx),
            Metadatum.Text("seedIx") -> Metadatum.Int(seedIx)
          )
        )
    }

    object Initialization extends Parser[Initialization](Tx.Type.Initialization) {
        override def parseInner(innerMap: Metadatum.Map): Either[ParseError, Initialization] = {
            val innerMapEntries = innerMap.entries
            for {
                multisigTreasuryIxRaw <- innerMapEntries
                    .get(Metadatum.Text("multisigTreasuryIx"))
                    .toRight(
                      MissingMetadataKeyForTransactionType(
                        "multisigTreasuryIx",
                        innerMap.entries.keys.map(_.toString).toList
                      )
                    )
                multisigRegimeIxRaw <- innerMapEntries
                    .get(Metadatum.Text("multisigRegimeIx"))
                    .toRight(
                      MissingMetadataKeyForTransactionType(
                        "multisigRegimeIx",
                        innerMap.entries.keys.map(_.toString).toList
                      )
                    )
                seedIxRaw <- innerMapEntries
                    .get(Metadatum.Text("seedIx"))
                    .toRight(
                      MissingMetadataKeyForTransactionType(
                        "seedIx",
                        innerMapEntries.keys.map(_.toString).toList
                      )
                    )

                multisigTreasuryIx <- multisigTreasuryIxRaw match {
                    case i: Metadatum.Int if i.value >= 0L => Right(i.value.intValue)
                    case _ => Left(WrongMetadataValue("multisigTreasuryIx", multisigTreasuryIxRaw))
                }

                multisigRegimeIx <- multisigRegimeIxRaw match {
                    case i: Metadatum.Int
                        if i.value >= 0L && i.value.intValue() != multisigTreasuryIx =>
                        Right(i.value.intValue)
                    case _ => Left(WrongMetadataValue("multisigRegimeIx", multisigRegimeIxRaw))
                }

                seedIx <- seedIxRaw match {
                    case i: Metadatum.Int if i.value >= 0 => Right(i.value.intValue)
                    case _ => Left(WrongMetadataValue("seedIx", seedIxRaw))
                }
            } yield Initialization(
              multisigTreasuryIx = multisigTreasuryIx,
              multisigRegimeIx = multisigRegimeIx,
              seedIx = seedIx
            )
        }
    }

    case class Refund() extends Metadata(Tx.Type.Refund)

    object Refund extends Parser[Refund](Tx.Type.Refund) {
        override def parseInner(innerMap: Metadatum.Map): Either[ParseError, Refund] =
            Right(Refund())
    }

    case class Rollout() extends Metadata(Tx.Type.Rollout)

    object Rollout extends Parser[Rollout](Tx.Type.Rollout) {
        override def parseInner(innerMap: Metadatum.Map): Either[ParseError, Rollout] =
            Right(Rollout())
    }

    case class Settlement() extends Metadata(Tx.Type.Settlement)

    object Settlement extends Parser[Settlement](Tx.Type.Settlement) {
        override def parseInner(innerMap: Metadatum.Map): Either[ParseError, Settlement] =
            Right(Settlement())
    }

    private def parseMdFromTxSerialized(
        protocolVersion: ProtocolVersion,
        txSerialized: Array[Byte]
    ): Either[ParseError, MD] =
        for {
            tx <- Try(Transaction.fromCbor(txSerialized)(using protocolVersion)).toEither.left
                .map(CborDecodingError(_))
            md <- parseMdFromTx(tx)
        } yield md

    private def parseMdFromTx(tx: Transaction): Either[ParseError, MD] = for {
        ad <- tx.auxiliaryData.toRight(MissingAuxData)
        md: MD <- ad.value match {
            case md: MD => Right(md)
            case _      => Left(AuxDataIsNotMetadata)
        }
    } yield md

    private def parseHydrMapFromMd(md: MD): Either[ParseError, Metadatum.Map] = for {
        hydrEntry <- md.metadata
            .get(Word64(CIP67.Tags.head))
            .toRight(MissingCIP67Tag)
        hydrMap <- hydrEntry match {
            case m: Metadatum.Map => Right(m)
            case _                => Left(MetadataValueIsNotMap)
        }
    } yield hydrMap

    private def parseRoleMapFromHydrMap(
        txType: Tx.Type,
        hydrMap: Metadatum.Map
    ): Either[ParseError, Metadatum.Map] = for {
        roleEntry <- hydrMap.entries
            .get(Metadatum.Text(txType.toString))
            .toRight(
              MissingMetadataKeyForTransactionType(
                txType.toString,
                hydrMap.entries.keys.toList.map(_.toString)
              )
            )
        roleMap <- roleEntry match {
            case m: Metadatum.Map => Right(m)
            case _                => Left(MetadataValueIsNotMap)
        }
    } yield roleMap

    private def parseInnerMapFromRoleMap(
        roleMap: Metadatum.Map
    ): Either[ParseError, (HeadId, Metadatum.Map)] = {
        val headEntries = roleMap.entries
        for {
            // FIXME: This currently assumes that the transaction is only meant for one head.
            //  A single transaction could be capable of interacting with multiple heads (e.g. deposit)
            _ <- Either.cond(headEntries.size == 1, (), TooManyHeadIDs(headEntries.size))
            soleHeadEntry = headEntries.head
            (headIdRaw, headMdMapRaw) = soleHeadEntry
            headId <- headIdRaw match {
                case x: Metadatum.Text => Right(HeadId(AssetName.fromHex(x.value)))
                case _                 => Left(MalformedHeadId(headIdRaw))
            }

            innerMap <- headMdMapRaw match {
                case x: Metadatum.Map => Right(x)
                case _                => Left(WrongMetadataValue(headId.toHex, roleMap))
            }
        } yield (headId, innerMap)
    }

    sealed trait ParseError extends Throwable
    case class CborDecodingError(wrapped: Throwable) extends ParseError {
        override def toString: String = s"CBOR Decoding failed during metadata parsing $wrapped"
    }
    case object MissingAuxData extends ParseError {
        override def toString: String = "Auxiliary Data is missing from the transaction"
    }
    case object AuxDataIsNotMetadata extends ParseError {
        override def toString: String = "Auxiliary data is not metadata"
    }
    case object MissingCIP67Tag extends ParseError {
        override def toString: String =
            s"HYDR (${CIP67.Tags.head}) top level CIP67 missing from metadata"
    }
    case object MetadataValueIsNotMap extends ParseError {
        override def toString: String = "Metadata value is not map"
    }

    case class TooManyHeadIDs(numHeads: Int) extends ParseError {
        override def toString: String =
            s"Too many HeadIds in metadata. Got: $numHeads. Currently we only support 1."
    }
    case class MalformedHeadId(actual: Metadatum) extends ParseError {
        override def toString: String = s"Head ID is malformed. Got: $actual"
    }

    case class MissingMetadataKeyForTransactionType(
        expectedKey: String,
        availableKeys: List[String]
    ) extends ParseError {
        override def toString: String = "Missing metadata key for this transaction type." +
            s" Expected $expectedKey." +
            s" Found: $availableKeys"
    }
    case class WrongMetadataValue(key: String, value: Metadatum) extends ParseError {
        override def toString: String = s"Wrong metadata value. Key: $key, Value: $value"
    }
}
