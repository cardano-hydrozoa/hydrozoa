package hydrozoa.multisig.ledger.eutxol2.tx

import cats.syntax.all.*
import scala.collection.immutable.SortedMap
import scalus.cardano.ledger.{AssetName, Metadatum, MultiAsset, PolicyId, ScriptHash}

/** Codec for the optional `transientOutputs` field of the L2 transaction metadata: a map from
  * transaction output index to the transient token content declared for that output. An output
  * index absent from the map carries no transient tokens.
  *
  * Wire shape (all levels are metadatum maps):
  * {{{
  * Map(Int(outputIndex) -> Map(Bytes(policyId /*28B*/) -> Map(Bytes(assetName /*<=32B*/) -> Int(quantity))))
  * }}}
  *
  * Quantities are in `[1, Long.MaxValue]`: metadata integers are i64, matching the `Long`
  * quantities of [[MultiAsset]], so larger amounts are unrepresentable and rejected at decode.
  * Duplicate keys in a metadatum map collapse last-wins at CBOR decode (deterministic upstream
  * behavior), so they cannot be detected here.
  */
object TransientOutputs {

    /** Encode per-output transient declarations as the `transientOutputs` metadatum. */
    def encodeMetadatum(declarations: Map[Int, MultiAsset]): Metadatum =
        Metadatum.Map(
          declarations.map { case (index, bundle) =>
              (Metadatum.Int(index.toLong): Metadatum) -> encodeBundle(bundle)
          }
        )

    /** Decode the `transientOutputs` metadatum, rejecting malformed shapes: non-Int or negative
      * output indices, non-28-byte policy ids, over-32-byte asset names, quantities outside
      * `[1, Long.MaxValue]`, and empty bundles or token maps.
      */
    def decodeMetadatum(metadatum: Metadatum): Either[String, Map[Int, MultiAsset]] =
        metadatum match {
            case Metadatum.Map(entries) =>
                entries.toList
                    .traverse { case (key, value) =>
                        for {
                            index <- key match {
                                case Metadatum.Int(i) if i >= 0 && i <= Int.MaxValue =>
                                    Right(i.toInt)
                                case other =>
                                    Left(
                                      s"transientOutputs: output index must be a non-negative Int, got $other"
                                    )
                            }
                            bundle <- decodeBundle(value)
                        } yield index -> bundle
                    }
                    .map(_.toMap)
            case other => Left(s"transientOutputs must be a metadatum Map, got $other")
        }

    private def encodeBundle(bundle: MultiAsset): Metadatum =
        Metadatum.Map(
          bundle.assets.toSeq.map { case (policyId, tokens) =>
              (Metadatum.Bytes(policyId): Metadatum) ->
                  (Metadatum.Map(
                    tokens.toSeq.map { case (assetName, quantity) =>
                        (Metadatum.Bytes(assetName.bytes): Metadatum) ->
                            (Metadatum.Int(quantity): Metadatum)
                    }.toMap
                  ): Metadatum)
          }.toMap
        )

    private def decodeBundle(metadatum: Metadatum): Either[String, MultiAsset] =
        metadatum match {
            case Metadatum.Map(policies) if policies.nonEmpty =>
                policies.toList
                    .traverse { case (policyKey, tokensMetadatum) =>
                        for {
                            policyId <- policyKey match {
                                case Metadatum.Bytes(bytes) if bytes.size == 28 =>
                                    Right(ScriptHash.fromByteString(bytes): PolicyId)
                                case other =>
                                    Left(
                                      s"transientOutputs: policy id must be 28 Bytes, got $other"
                                    )
                            }
                            tokens <- decodeTokens(tokensMetadatum)
                        } yield policyId -> tokens
                    }
                    .map(policies => MultiAsset(SortedMap.from(policies)))
            case Metadatum.Map(_) => Left("transientOutputs: empty bundle")
            case other => Left(s"transientOutputs: bundle must be a metadatum Map, got $other")
        }

    private def decodeTokens(
        metadatum: Metadatum
    ): Either[String, SortedMap[AssetName, Long]] =
        metadatum match {
            case Metadatum.Map(tokens) if tokens.nonEmpty =>
                tokens.toList
                    .traverse { case (nameKey, quantityMetadatum) =>
                        for {
                            assetName <- nameKey match {
                                case Metadatum.Bytes(bytes) if bytes.size <= 32 =>
                                    Right(AssetName(bytes))
                                case other =>
                                    Left(
                                      s"transientOutputs: asset name must be at most 32 Bytes, got $other"
                                    )
                            }
                            quantity <- quantityMetadatum match {
                                case Metadatum.Int(quantity) if quantity >= 1 => Right(quantity)
                                case other =>
                                    Left(
                                      s"transientOutputs: quantity must be a positive Int, got $other"
                                    )
                            }
                        } yield assetName -> quantity
                    }
                    .map(SortedMap.from)
            case Metadatum.Map(_) => Left("transientOutputs: empty token map")
            case other => Left(s"transientOutputs: token map must be a metadatum Map, got $other")
        }
}
