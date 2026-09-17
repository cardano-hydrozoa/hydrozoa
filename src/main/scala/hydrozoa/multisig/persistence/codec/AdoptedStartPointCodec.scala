package hydrozoa.multisig.persistence.codec

import hydrozoa.multisig.consensus.ack.HardAckNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.l2.L2CommandNumber
import hydrozoa.multisig.ledger.stack.StackNumber
import hydrozoa.multisig.persistence.AdoptedStartPoint
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}

/** Persistence codec for [[AdoptedStartPoint]] — four numbers, written once and read on every boot
  * of a seeded coil peer.
  */
object AdoptedStartPointCodec:

    given encoder: Encoder[AdoptedStartPoint] = Encoder.instance(p =>
        Json.obj(
          "startStack" -> (p.startStack: Int).asJson,
          "lastBlockNum" -> (p.lastBlockNum: Int).asJson,
          "commandNumber" -> (p.commandNumber: Long).asJson,
          "ownHardAckStart" -> (p.ownHardAckStart: Int).asJson
        )
    )

    given decoder: Decoder[AdoptedStartPoint] = Decoder.instance(c =>
        for {
            startStack <- c.downField("startStack").as[Int]
            lastBlockNum <- c.downField("lastBlockNum").as[Int]
            commandNumber <- c.downField("commandNumber").as[Long]
            ownHardAckStart <- c.downField("ownHardAckStart").as[Int]
        } yield AdoptedStartPoint(
          StackNumber(startStack),
          BlockNumber(lastBlockNum),
          L2CommandNumber(commandNumber),
          HardAckNumber(ownHardAckStart)
        )
    )
