package hydrozoa.lib.cardano.cip116

import io.circe.syntax.*
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import scala.util.Try
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.crypto.ed25519.{Signature, VerificationKey}
import scalus.uplc.builtin.ByteString
import scodec.bits.ByteVector

object JsonCodecs {
    object CIP0116 {
        object Conway {
            // Scalus VerificationKey codec (32 bytes as hex)
            given Encoder[VerificationKey] =
                Encoder.encodeString.contramap(vk => ByteVector(vk.bytes.toArray).toHex)

            given Decoder[VerificationKey] =
                Decoder.decodeString.emap { hexStr =>
                    ByteVector
                        .fromHex(hexStr)
                        .toRight(s"Invalid hex string for verification key: $hexStr")
                        .flatMap { bv =>
                            if bv.size == 32 then
                                Right(
                                  VerificationKey
                                      .unsafeFromByteString(ByteString.fromArray(bv.toArray))
                                )
                            else Left(s"Verification key must be 32 bytes, got ${bv.size}")
                        }
                }

            // Scalus Signature codec (64 bytes as hex)
            given Encoder[Signature] =
                Encoder.encodeString.contramap(sig => ByteVector(sig.bytes.toArray).toHex)

            given Decoder[Signature] =
                Decoder.decodeString.emap { hexStr =>
                    ByteVector
                        .fromHex(hexStr)
                        .toRight(s"Invalid hex string for signature: $hexStr")
                        .flatMap { bv =>
                            if bv.size == 64 then
                                Right(
                                  Signature.unsafeFromByteString(ByteString.fromArray(bv.toArray))
                                )
                            else Left(s"Signature must be 64 bytes, got ${bv.size}")
                        }
                }

            // Hash32 codec (32 bytes as hex) - Hash32 is Hash[Blake2b_256, Any]
            given Encoder[Hash32] =
                Encoder.encodeString.contramap(hash => ByteVector(hash.bytes.toArray).toHex)

            given Decoder[Hash32] =
                Decoder.decodeString.emap { hexStr =>
                    ByteVector
                        .fromHex(hexStr)
                        .toRight(s"Invalid hex string for hash: $hexStr")
                        .flatMap { bv =>
                            if bv.size == 32 then
                                import scalus.cardano.ledger.{Blake2b_256, Hash}
                                Right(Hash[Blake2b_256, Any](ByteString.fromArray(bv.toArray)))
                            else Left(s"Hash32 must be 32 bytes, got ${bv.size}")
                        }
                }

            // AssetName codec (as plain value, not key)
            given assetNameValueEncoder: Encoder[AssetName] =
                Encoder.encodeString.contramap(assetName => assetName.bytes.toHex)

            given assetNameValueDecoder: Decoder[AssetName] =
                Decoder.decodeString.map(s => AssetName.fromHex(s))

            // TODO: this is not nice
            // ShelleyAddress codec (as bech32 string)
            given shelleyAddressEncoder: Encoder[ShelleyAddress] =
                Encoder.encodeString.contramap(addr => addr.toBech32.get)

            given shelleyAddressDecoder: Decoder[ShelleyAddress] =
                Decoder.decodeString.emap { str =>
                    scala.util
                        .Try {
                            scalus.cardano.address.Address.fromBech32(str) match {
                                case shelley: ShelleyAddress => shelley
                                case _ =>
                                    throw new Exception(s"Address is not a Shelley address: $str")
                            }
                        }
                        .toEither
                        .left
                        .map(_.getMessage)
                }

            // Encode/decode byte arrays as lowercase hex strings
            given byteStringEncoder: Encoder[ByteString] =
                Encoder.encodeString.contramap(_.toHex)

            given byteStringDecoder: Decoder[ByteString] =
                Decoder.decodeString.emapTry { hexStr =>
                    Try(ByteString.fromHex(hexStr))
                }

            // Coin codec
            given coinEncoder: Encoder[Coin] =
                Encoder.encodeString.contramap(coin => java.lang.Long.toUnsignedString(coin.value))

            // N.B.: The scalus `Coin` uses a _signed_ long. This will fail if the long wraps around to negative
            given coinDecoder: Decoder[Coin] = Decoder.decodeString.emap(s =>
                Try(Coin(java.lang.Long.parseUnsignedLong(s))).toEither.left.map(_.getMessage)
            )

            // Policy ID Codec
            given policyIdKeyEncoder: KeyEncoder[PolicyId] =
                KeyEncoder.encodeKeyString.contramap(policyId => policyId.toHex)

            // Script hash must be exactly 56 hex characters long
            given policyIdKeyDecoder: KeyDecoder[PolicyId] =
                KeyDecoder.decodeKeyString.map(ScriptHash.fromHex)

            // Asset Name Codec
            given assetNameKeyEncoder: KeyEncoder[AssetName] =
                KeyEncoder.encodeKeyString.contramap(assetName => assetName.bytes.toHex)

            // Asset name must be exactly 64 hexits
            given assetNameKeyDecoder: KeyDecoder[AssetName] =
                KeyDecoder.decodeKeyString.map(s => AssetName.fromHex(s))

            // MultiAsset Codec
            given multiAssetEncoder: Encoder[MultiAsset] = {
                // FIXME: The CIP0116 allows only for PosInt64 in the map, but scalus allows for nonzero.
                //  If we want maximum safety, we should create a `CIP0116.Value` type that enforces the CIP invariants.
                val longAsPosInt64Encoder: Encoder[Long] =
                    Encoder.encodeString
                        .contramap(l => java.lang.Long.toUnsignedString(l))

                val innerMapEncoder
                    : Encoder[scala.collection.immutable.SortedMap[AssetName, Long]] =
                    Encoder
                        .encodeMap(using assetNameKeyEncoder, longAsPosInt64Encoder)
                        .contramap(_.unsorted)

                Encoder
                    .encodeMap(using policyIdKeyEncoder, innerMapEncoder)
                    .contramap(_.assets.unsorted)

            }

            given multiAssetDecoder: Decoder[MultiAsset] = {
                // FIXME: The CIP0116 allows only for PosInt64 in the map, but scalus allows for nonzero.
                //  If we want maximum safety, we should create a `CIP0116.Value` type that enforces the CIP invariants.
                val posInt64AsLongDecoder: Decoder[Long] =
                    Decoder.decodeString
                        .emap(s =>
                            Try {
                                val fromUnsigned: Long = java.lang.Long.parseUnsignedLong(s)
                                require(
                                  fromUnsigned > 0,
                                  "scalus only supports signed integers in Value"
                                )
                                fromUnsigned
                            }.toEither.left.map(_.getMessage)
                        )

                val innerMapDecoder
                    : Decoder[scala.collection.immutable.SortedMap[AssetName, Long]] =
                    Decoder
                        .decodeMap(using assetNameKeyDecoder, posInt64AsLongDecoder)
                        .map(scala.collection.immutable.SortedMap.from(_))

                Decoder
                    .decodeMap(using policyIdKeyDecoder, innerMapDecoder)
                    .map(m => MultiAsset(scala.collection.immutable.SortedMap.from(m)))
            }

            // Value codec
            // See: https://github.com/cardano-foundation/CIPs/blob/f105b3ed93936a3a073f33263e3b8461b7277031/CIP-0116/cardano-conway.json#L508-L522
            given valueEncoder: Encoder[Value] = (value: Value) =>
                io.circe.Json.obj(
                  "coin" -> value.coin.asJson,
                  "assets" -> value.assets.asJson
                )
            given valueDecoder: Decoder[Value] = c =>
                for {
                    coin <- c.downField("coin").as[Coin]
                    assets <- c.downField("assets").as[MultiAsset]
                } yield Value(coin, assets)

            implicit val transactionInputEncoder: Encoder[TransactionInput] =
                (ti: TransactionInput) =>
                    io.circe.Json.obj(
                      "transaction_id" -> ti.transactionId.toHex.asJson,
                      // N.B.: CIP-0116 defines this a UInt32; scalus defines it as signed.
                      "index" -> ti.index.toInt.asJson
                    )

            implicit val transactionInputDecoder: Decoder[TransactionInput] = c =>
                for {
                    txIdBytes <- c.downField("transaction_id").as[Array[Byte]]
                    index <- c.downField("index").as[Int]
                } yield {
                    import scalus.cardano.ledger.{Blake2b_256, Hash, HashPurpose}
                    val txHash = Hash[Blake2b_256, HashPurpose.TransactionHash](
                      scalus.uplc.builtin.ByteString.fromArray(txIdBytes)
                    )
                    TransactionInput(txHash, index)
                }
        }
    }
}
