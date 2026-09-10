package hydrozoa.multisig.persistence.codec

import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{byteStringDecoder, byteStringEncoder}
import hydrozoa.multisig.ledger.block.{BlockHeader, BlockNumber, BlockVersion}
import hydrozoa.multisig.ledger.l2.L2StateHash
import hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.uplc.builtin.ByteString

/** Persistence-layer JSON codecs for [[StandaloneEvacuationCommitment]] (SEC) and its `MultiSigned`
  * form.
  *
  * The SEC's `header` field is `StandaloneEvacuationCommitment.Onchain.Serialized` (opaque
  * `IArray[Byte]`); we ride the public `Serialized.fromBytes` constructor added for the codec.
  * `BlockHeader.Minor.HeaderSignature` is similarly an opaque `IArray[Byte]` with a public bytes
  * constructor.
  */
object SecCodec:

    given headerSerializedEncoder: Encoder[StandaloneEvacuationCommitment.Onchain.Serialized] =
        Encoder.instance { s =>
            val bytes: Array[Byte] = s // implicit Conversion[Serialized, Array[Byte]]
            ByteString.fromArray(bytes).asJson
        }

    given headerSerializedDecoder: Decoder[StandaloneEvacuationCommitment.Onchain.Serialized] =
        byteStringDecoder.map(bs =>
            StandaloneEvacuationCommitment.Onchain.Serialized.fromBytes(bs.bytes)
        )

    given headerSignatureEncoder: Encoder[BlockHeader.Minor.HeaderSignature] = Encoder.instance {
        sig =>
            val bytes: Array[Byte] = sig // implicit Conversion[HeaderSignature, Array[Byte]]
            ByteString.fromArray(bytes).asJson
    }

    given headerSignatureDecoder: Decoder[BlockHeader.Minor.HeaderSignature] =
        byteStringDecoder.map(bs => BlockHeader.Minor.HeaderSignature(IArray.from(bs.bytes)))

    given standaloneEvacCommitmentEncoder: Encoder[StandaloneEvacuationCommitment] =
        Encoder.instance { sec =>
            Json.obj(
              "blockNum" -> sec.blockNum.asJson,
              "blockVersion" -> sec.blockVersion.asJson,
              "kzgCommitment" -> (sec.kzgCommitment: ByteString).asJson,
              "l2StateHash" -> sec.l2StateHash.asJson,
              "header" -> sec.header.asJson
            )
        }

    given standaloneEvacCommitmentDecoder: Decoder[StandaloneEvacuationCommitment] =
        Decoder.instance { c =>
            for
                blockNum <- c.downField("blockNum").as[BlockNumber]
                blockVersion <- c.downField("blockVersion").as[BlockVersion.Full]
                kzg <- c.downField("kzgCommitment").as[ByteString]
                l2StateHash <- c.downField("l2StateHash").as[L2StateHash]
                header <- c
                    .downField("header")
                    .as[StandaloneEvacuationCommitment.Onchain.Serialized]
            yield StandaloneEvacuationCommitment(
              blockNum = blockNum,
              blockVersion = blockVersion,
              kzgCommitment = kzg,
              l2StateHash = l2StateHash,
              header = header
            )
        }

    given multiSignedEncoder: Encoder[StandaloneEvacuationCommitment.MultiSigned] =
        Encoder.instance { ms =>
            Json.obj(
              "commitment" -> ms.commitment.asJson,
              "headerMultiSigned" -> ms.headerMultiSigned.asJson
            )
        }

    given multiSignedDecoder: Decoder[StandaloneEvacuationCommitment.MultiSigned] =
        Decoder.instance { c =>
            for
                commitment <- c.downField("commitment").as[StandaloneEvacuationCommitment]
                sigs <- c
                    .downField("headerMultiSigned")
                    .as[List[Option[BlockHeader.Minor.HeaderSignature]]]
            yield StandaloneEvacuationCommitment.MultiSigned(
              commitment = commitment,
              headerMultiSigned = sigs
            )
        }
