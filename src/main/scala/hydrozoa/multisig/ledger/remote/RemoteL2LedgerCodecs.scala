package hydrozoa.multisig.ledger.remote

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{keepRawTransactionOutputDecoder, keepRawTransactionOutputEncoder}
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.EvacuationDiff
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l2.{Destination, L2CommandNumber, L2LedgerCommand, L2LedgerResponse}
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import io.circe.{Codec, Decoder, Encoder}
import scalus.cardano.ledger.{AssetName, Coin, KeepRaw, MultiAsset, PolicyId, ScriptHash, TransactionOutput, Value}

/** JSON codecs for RemoteL2Ledger WebSocket protocol */
object RemoteL2LedgerCodecs {

    // Reuse codecs from the HTTP server, excluding types we override for sugar-rush-ledger compatibility
    // We exclude certain codecs here to provide sugar-rush-ledger compatible format
    import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.{coinEncoder as _, coinDecoder as _, valueEncoder as _, valueDecoder as _}
    import EvacuationDiff.given

    export BlockNumber.given
    export EvacuationDiff.given
    export L2LedgerCommand.given
    export Destination.given

    // Coin as raw number (sugar-rush-ledger expects u64, not string)
    given Encoder[Coin] = Encoder.encodeLong.contramap(_.value)
    given Decoder[Coin] = Decoder.decodeLong.map(Coin.apply)

    // Value codec for sugar-rush-ledger format:
    // {"assets": [{"asset": {"tag": "Ada"}, "value": N}, {"asset": {"tag": "NativeToken", ...}, "value": M}]}
    given Encoder[Value] = (v: Value) => {
        val adaEntry = io.circe.Json.obj(
          "asset" -> io.circe.Json.obj("tag" -> io.circe.Json.fromString("Ada")),
          "value" -> io.circe.Json.fromLong(v.coin.value)
        )

        val nativeTokenEntries = v.assets.assets.flatMap { case (policyId, assetMap) =>
            assetMap.map { case (assetName, quantity) =>
                io.circe.Json.obj(
                  "asset" -> io.circe.Json.obj(
                    "tag" -> io.circe.Json.fromString("NativeToken"),
                    "policyId" -> io.circe.Json.fromString(policyId.toHex),
                    "assetName" -> io.circe.Json.fromString(assetName.bytes.toHex)
                  ),
                  "value" -> io.circe.Json.fromLong(quantity)
                )
            }
        }

        val allEntries = adaEntry +: nativeTokenEntries.toSeq
        io.circe.Json.obj("assets" -> io.circe.Json.arr(allEntries*))
    }

    given Decoder[Value] = Decoder.instance { c =>
        c.downField("assets").as[List[io.circe.Json]].flatMap { assets =>
            var coin = Coin(0)
            val tokenMap = scala.collection.mutable
                .Map[PolicyId, scala.collection.mutable.Map[AssetName, Long]]()

            assets.foreach { assetEntry =>
                val assetCursor = assetEntry.hcursor
                val tag = assetCursor.downField("asset").downField("tag").as[String].getOrElse("")
                val value = assetCursor.downField("value").as[Long].getOrElse(0L)

                tag match {
                    case "Ada" =>
                        coin = Coin(value)
                    case "NativeToken" =>
                        val policyIdHex = assetCursor
                            .downField("asset")
                            .downField("policyId")
                            .as[String]
                            .getOrElse("")
                        val assetNameHex = assetCursor
                            .downField("asset")
                            .downField("assetName")
                            .as[String]
                            .getOrElse("")

                        val policyId = ScriptHash.fromHex(policyIdHex)
                        val assetName = AssetName.fromHex(assetNameHex)

                        val innerMap =
                            tokenMap.getOrElseUpdate(policyId, scala.collection.mutable.Map())
                        innerMap(assetName) = value
                    case unknown =>
                        Left(io.circe.DecodingFailure(s"Unknown asset tag: $unknown", c.history))
                }
            }

            val multiAsset = MultiAsset(
              scala.collection.immutable.SortedMap.from(
                tokenMap.view.mapValues(m => scala.collection.immutable.SortedMap.from(m))
              )
            )

            Right(Value(coin, multiAsset))
        }
    }

    // Applied codecs: one concrete descendant per command. Each is a single-key object tagging the
    // command; RegisterDeposit carries just the commandNumber, ApplyDepositDecisions adds the diffs,
    // ApplyTransaction adds diffs + payouts.
    import L2LedgerResponse.Applied
    given registerDepositAppliedEncoder: Encoder[Applied.RegisterDeposit] = deriveEncoder
    given registerDepositAppliedDecoder: Decoder[Applied.RegisterDeposit] = deriveDecoder
    given applyDepositDecisionsAppliedEncoder: Encoder[Applied.ApplyDepositDecisions] =
        deriveEncoder
    given applyDepositDecisionsAppliedDecoder(using
        CardanoNetwork.Section
    ): Decoder[Applied.ApplyDepositDecisions] = deriveDecoder
    given applyTransactionAppliedEncoder: Encoder[Applied.ApplyTransaction] = deriveEncoder
    given applyTransactionAppliedDecoder(using
        CardanoNetwork.Section
    ): Decoder[Applied.ApplyTransaction] = deriveDecoder

    given appliedEncoder: Encoder[Applied] = {
        case a: Applied.RegisterDeposit => io.circe.Json.obj("RegisterDeposit" -> a.asJson)
        case a: Applied.ApplyDepositDecisions =>
            io.circe.Json.obj("ApplyDepositDecisions" -> a.asJson)
        case a: Applied.ApplyTransaction => io.circe.Json.obj("ApplyTransaction" -> a.asJson)
    }

    given appliedDecoder(using CardanoNetwork.Section): Decoder[Applied] =
        Decoder.instance { c =>
            c.keys
                .flatMap(_.headOption)
                .toRight(io.circe.DecodingFailure("Applied must have exactly one field", c.history))
                .flatMap {
                    case "RegisterDeposit" =>
                        c.downField("RegisterDeposit").as[Applied.RegisterDeposit]
                    case "ApplyDepositDecisions" =>
                        c.downField("ApplyDepositDecisions").as[Applied.ApplyDepositDecisions]
                    case "ApplyTransaction" =>
                        c.downField("ApplyTransaction").as[Applied.ApplyTransaction]
                    case other =>
                        Left(io.circe.DecodingFailure(s"Unknown Applied type: $other", c.history))
                }
        }

    given rejectedEncoder: Encoder[L2LedgerResponse.Rejected] = deriveEncoder
    given rejectedDecoder: Decoder[L2LedgerResponse.Rejected] = deriveDecoder

    given outOfOrderEncoder: Encoder[L2LedgerResponse.OutOfOrder] = deriveEncoder
    given outOfOrderDecoder: Decoder[L2LedgerResponse.OutOfOrder] = deriveDecoder

    given ledgerFreezeEncoder: Encoder[L2LedgerResponse.LedgerFreeze] = deriveEncoder
    given ledgerFreezeDecoder: Decoder[L2LedgerResponse.LedgerFreeze] = deriveDecoder

    // Each response is a single-key object tagging the outcome kind (Applied is itself nested).
    given responseEncoder: Encoder[L2LedgerResponse] = {
        case a: L2LedgerResponse.Applied      => io.circe.Json.obj("Applied" -> a.asJson)
        case r: L2LedgerResponse.Rejected     => io.circe.Json.obj("Rejected" -> r.asJson)
        case o: L2LedgerResponse.OutOfOrder   => io.circe.Json.obj("OutOfOrder" -> o.asJson)
        case f: L2LedgerResponse.LedgerFreeze => io.circe.Json.obj("LedgerFreeze" -> f.asJson)
    }

    given responseDecoder(using CardanoNetwork.Section): Decoder[L2LedgerResponse] =
        Decoder.instance { c =>
            c.keys
                .flatMap(_.headOption)
                .toRight(
                  io.circe.DecodingFailure("Response must have exactly one field", c.history)
                )
                .flatMap {
                    case "Applied"    => c.downField("Applied").as[L2LedgerResponse.Applied]
                    case "Rejected"   => c.downField("Rejected").as[L2LedgerResponse.Rejected]
                    case "OutOfOrder" => c.downField("OutOfOrder").as[L2LedgerResponse.OutOfOrder]
                    case "LedgerFreeze" =>
                        c.downField("LedgerFreeze").as[L2LedgerResponse.LedgerFreeze]
                    case other =>
                        Left(io.circe.DecodingFailure(s"Unknown response type: $other", c.history))
                }
        }

    // Request codecs. Each request is a single-key object tagging the command variant, whose value
    // carries the Hydrozoa-assigned `commandNumber` and the `command` payload.
    import RemoteL2Ledger.Request
    given requestCodec: Codec[Request] = {
        def tagged(
            tag: String,
            commandNumber: L2CommandNumber,
            command: io.circe.Json
        ): io.circe.Json =
            io.circe.Json.obj(
              tag -> io.circe.Json.obj(
                "commandNumber" -> commandNumber.asJson,
                "command" -> command
              )
            )

        Codec.from(
          encodeA = {
              case Request.RegisterDeposit(cn, command) =>
                  tagged("RegisterDeposit", cn, command.asJson)
              case Request.ApplyDepositDecisions(cn, command) =>
                  tagged("ApplyDepositDecisions", cn, command.asJson)
              case Request.ApplyTransaction(cn, command) =>
                  tagged("ApplyTransaction", cn, command.asJson)
          },
          decodeA = c =>
              c.keys
                  .flatMap(_.headOption)
                  .toRight(
                    io.circe.DecodingFailure("Request must have exactly one field", c.history)
                  )
                  .flatMap { tag =>
                      val body = c.downField(tag)
                      val cn = body.downField("commandNumber").as[L2CommandNumber]
                      val command = body.downField("command")
                      tag match {
                          case "RegisterDeposit" =>
                              for {
                                  n <- cn
                                  cmd <- command.as[L2LedgerCommand.RegisterDeposit]
                              } yield Request.RegisterDeposit(n, cmd)
                          case "ApplyDepositDecisions" =>
                              for {
                                  n <- cn
                                  cmd <- command.as[L2LedgerCommand.ApplyDepositDecisions]
                              } yield Request.ApplyDepositDecisions(n, cmd)
                          case "ApplyTransaction" =>
                              for {
                                  n <- cn
                                  cmd <- command.as[L2LedgerCommand.ApplyTransaction]
                              } yield Request.ApplyTransaction(n, cmd)
                          case other =>
                              Left(
                                io.circe.DecodingFailure(s"Unknown request type: $other", c.history)
                              )
                      }
                  }
        )
    }

    // KeepRaw[TransactionOutput] CBOR-hex codec is hoisted into
    // `lib/cardano/scalus/codecs/json/Codecs.scala`; imported above.

    // Payout.Obligation codec
    // Encode directly as TransactionOutput (without "utxo" wrapper) for API compatibility
    given payoutObligationEncoder: Encoder[Payout.Obligation] = Encoder.instance { po =>
        po.utxo.asJson
    }
    given payoutObligationDecoder(using
        config: CardanoNetwork.Section
    ): Decoder[Payout.Obligation] = Decoder.instance { c =>
        for {
            unvalidated <- c.as[KeepRaw[TransactionOutput]]
            value <- Payout
                .Obligation(unvalidated, config)
                .left
                .map(e => io.circe.DecodingFailure(e.toString, c.history))
        } yield value
    }

    // Unit codec
    implicit val unitEncoder: Encoder[Unit] = _ => io.circe.Json.obj()
    implicit val unitDecoder: Decoder[Unit] = _ => Right(())
}
