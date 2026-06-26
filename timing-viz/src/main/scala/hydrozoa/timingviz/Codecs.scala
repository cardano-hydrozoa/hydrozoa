package hydrozoa.timingviz

import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.{BlockTimes, RequestTimes}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import hydrozoa.timingviz.Command.*
import hydrozoa.timingviz.Ids.*
import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}

/** JSON codecs for the visualizer wire format. Server emits `Frame`; client emits `Command`.
  *
  * The discriminator field for all sum types is `"type"`. Times go on the wire as epoch-milli
  * `Long`s; the `CardanoNetwork.Section` in scope decides the slot configuration used to quantize
  * incoming instants/durations.
  */
object Codecs:

    // --- string-based codecs for nullary enums -------------------------------------------------

    private def stringEnumCodec[E](toName: E => String, fromName: String => Option[E]): Codec[E] =
        Codec.from(
          Decoder.decodeString.emap(s => fromName(s).toRight(s"unknown enum value: $s")),
          Encoder.encodeString.contramap(toName)
        )

    given Codec[BlockKind] = stringEnumCodec(
      _.toString,
      s => BlockKind.values.find(_.toString == s)
    )
    given Codec[SpineKind] = stringEnumCodec(
      _.toString,
      s => SpineKind.values.find(_.toString == s)
    )
    given Codec[Track] =
        stringEnumCodec(_.toString, s => Track.values.find(_.toString == s))
    given Codec[ItemKind] =
        stringEnumCodec(_.toString, s => ItemKind.values.find(_.toString == s))
    given Codec[FieldKey] =
        stringEnumCodec(_.toString, s => FieldKey.values.find(_.toString == s))
    given Codec[ParameterKey] = stringEnumCodec(
      _.toString,
      s => ParameterKey.values.find(_.toString == s)
    )
    given Codec[Source] =
        stringEnumCodec(_.toString, s => Source.values.find(_.toString == s))

    // --- opaque ID codecs ---------------------------------------------------------------------

    given Codec[BlockId] = Codec.from(
      Decoder.decodeString.map(BlockId(_)),
      Encoder.encodeString.contramap(_.asString)
    )
    given Codec[RequestId] = Codec.from(
      Decoder.decodeString.map(RequestId(_)),
      Encoder.encodeString.contramap(_.asString)
    )
    given Codec[DepositId] = Codec.from(
      Decoder.decodeString.map(DepositId(_)),
      Encoder.encodeString.contramap(_.asString)
    )
    given Codec[EffectId] = Codec.from(
      Decoder.decodeString.map(EffectId(_)),
      Encoder.encodeString.contramap(_.asString)
    )

    // --- ObjectId (discriminated sum) ---------------------------------------------------------

    given Encoder[ObjectId] = Encoder.instance {
        case ObjectId.OfBlock(id)   => Json.obj("type" -> "OfBlock".asJson, "id" -> id.asJson)
        case ObjectId.OfRequest(id) => Json.obj("type" -> "OfRequest".asJson, "id" -> id.asJson)
        case ObjectId.OfDeposit(id) => Json.obj("type" -> "OfDeposit".asJson, "id" -> id.asJson)
        case ObjectId.OfEffect(id)  => Json.obj("type" -> "OfEffect".asJson, "id" -> id.asJson)
    }
    given Decoder[ObjectId] = Decoder.instance(c =>
        c.downField("type").as[String].flatMap {
            case "OfBlock"   => c.downField("id").as[BlockId].map(ObjectId.OfBlock(_))
            case "OfRequest" => c.downField("id").as[RequestId].map(ObjectId.OfRequest(_))
            case "OfDeposit" => c.downField("id").as[DepositId].map(ObjectId.OfDeposit(_))
            case "OfEffect"  => c.downField("id").as[EffectId].map(ObjectId.OfEffect(_))
            case other       => Left(DecodingFailure(s"unknown ObjectId type: $other", c.history))
        }
    )

    // --- DepositStatus (discriminated sum, mixed nullary/payload) -----------------------------

    given Encoder[DepositStatus] = Encoder.instance {
        case DepositStatus.Pending => Json.obj("type" -> "Pending".asJson)
        case DepositStatus.OnChain => Json.obj("type" -> "OnChain".asJson)
        case DepositStatus.Mature  => Json.obj("type" -> "Mature".asJson)
        case DepositStatus.Absorbed(by) =>
            Json.obj("type" -> "Absorbed".asJson, "by" -> by.asJson)
        case DepositStatus.Rejected(by) =>
            Json.obj("type" -> "Rejected".asJson, "by" -> by.asJson)
        case DepositStatus.Refunded => Json.obj("type" -> "Refunded".asJson)
    }
    given Decoder[DepositStatus] = Decoder.instance(c =>
        c.downField("type").as[String].flatMap {
            case "Pending"  => Right(DepositStatus.Pending)
            case "OnChain"  => Right(DepositStatus.OnChain)
            case "Mature"   => Right(DepositStatus.Mature)
            case "Refunded" => Right(DepositStatus.Refunded)
            case "Absorbed" => c.downField("by").as[BlockId].map(DepositStatus.Absorbed(_))
            case "Rejected" => c.downField("by").as[BlockId].map(DepositStatus.Rejected(_))
            case other => Left(DecodingFailure(s"unknown DepositStatus type: $other", c.history))
        }
    )

    // --- time / duration codecs ---------------------------------------------------------------

    given (using CardanoNetwork.Section): Codec[Interval] = Codec.from(
      Decoder.instance(c =>
          for
              start <- c.downField("startMs").as[Long].map(quantizedFromMs)
              end <- c.downField("endMs").as[Long].map(quantizedFromMs)
          yield Interval(start, end)
      ),
      Encoder.instance(i =>
          Json.obj(
            "startMs" -> i.start.instant.toEpochMilli.asJson,
            "endMs" -> i.end.instant.toEpochMilli.asJson
          )
      )
    )

    private def quantizedFromMs(ms: Long)(using net: CardanoNetwork.Section): QuantizedInstant =
        QuantizedInstant(net.slotConfig, java.time.Instant.ofEpochMilli(ms))

    private def quantizedDurationFromMs(
        ms: Long
    )(using net: CardanoNetwork.Section): QuantizedFiniteDuration =
        import scala.concurrent.duration.DurationLong
        QuantizedFiniteDuration(net.slotConfig, ms.millis)

    // --- Command codec (client → server) -------------------------------------------------------

    given commandEncoder(using CardanoNetwork.Section): Encoder[Command] = Encoder.instance {
        case AdvanceClock(to) =>
            Json.obj("type" -> "AdvanceClock".asJson, "toMs" -> to.instant.toEpochMilli.asJson)
        case SetParameter(key, value) =>
            Json.obj(
              "type" -> "SetParameter".asJson,
              "key" -> key.asJson,
              "valueMs" -> value.finiteDuration.toMillis.asJson
            )
        case ObserveBlock(id, creation, kind) =>
            Json.obj(
              "type" -> "ObserveBlock".asJson,
              "id" -> id.asJson,
              "creation" -> creation.asJson,
              "kind" -> kind.asJson
            )
        case ObserveRequest(id, validity) =>
            Json.obj(
              "type" -> "ObserveRequest".asJson,
              "id" -> id.asJson,
              "validity" -> validity.asJson
            )
        case ObserveDepositRequest(id, end) =>
            Json.obj(
              "type" -> "ObserveDepositRequest".asJson,
              "id" -> id.asJson,
              "endMs" -> end.convert.instant.toEpochMilli.asJson
            )
        case ObserveDepositOnChain(id) =>
            Json.obj("type" -> "ObserveDepositOnChain".asJson, "id" -> id.asJson)
        case ObserveDepositAbsorbed(id, by) =>
            Json.obj(
              "type" -> "ObserveDepositAbsorbed".asJson,
              "id" -> id.asJson,
              "by" -> by.asJson
            )
        case ObserveDepositRejected(id, by) =>
            Json.obj(
              "type" -> "ObserveDepositRejected".asJson,
              "id" -> id.asJson,
              "by" -> by.asJson
            )
        case ObserveSpineEffect(id, kind, blockId, validity) =>
            Json.obj(
              "type" -> "ObserveSpineEffect".asJson,
              "id" -> id.asJson,
              "kind" -> kind.asJson,
              "blockId" -> blockId.asJson,
              "validity" -> validity.asJson
            )
        case ObserveFallback(id, pairedEffectId, start) =>
            Json.obj(
              "type" -> "ObserveFallback".asJson,
              "id" -> id.asJson,
              "pairedEffectId" -> pairedEffectId.asJson,
              "startMs" -> start.convert.instant.toEpochMilli.asJson
            )
        case ObserveRefund(id, depositId, start) =>
            Json.obj(
              "type" -> "ObserveRefund".asJson,
              "id" -> id.asJson,
              "depositId" -> depositId.asJson,
              "startMs" -> start.instant.toEpochMilli.asJson
            )
        case HypothesizeDeposit(id, end) =>
            Json.obj(
              "type" -> "HypothesizeDeposit".asJson,
              "id" -> id.asJson,
              "endMs" -> end.convert.instant.toEpochMilli.asJson
            )
        case HypothesizeBlock(id, creation, kind) =>
            Json.obj(
              "type" -> "HypothesizeBlock".asJson,
              "id" -> id.asJson,
              "creation" -> creation.asJson,
              "kind" -> kind.asJson
            )
        case Retract(id) =>
            Json.obj("type" -> "Retract".asJson, "id" -> id.asJson)
    }

    given commandDecoder(using CardanoNetwork.Section): Decoder[Command] =
        Decoder.instance(c => c.downField("type").as[String].flatMap(decodeCommand(c, _)))

    private def decodeCommand(c: HCursor, tag: String)(using
        net: CardanoNetwork.Section
    ): Decoder.Result[Command] =
        tag match
            case "AdvanceClock" =>
                c.downField("toMs").as[Long].map(ms => AdvanceClock(quantizedFromMs(ms)))
            case "SetParameter" =>
                for
                    key <- c.downField("key").as[ParameterKey]
                    ms <- c.downField("valueMs").as[Long]
                yield SetParameter(key, quantizedDurationFromMs(ms))
            case "ObserveBlock" =>
                for
                    id <- c.downField("id").as[BlockId]
                    creation <- c.downField("creation").as[Interval]
                    kind <- c.downField("kind").as[BlockKind]
                yield ObserveBlock(id, creation, kind)
            case "ObserveRequest" =>
                for
                    id <- c.downField("id").as[RequestId]
                    v <- c.downField("validity").as[Interval]
                yield ObserveRequest(id, v)
            case "ObserveDepositRequest" =>
                for
                    id <- c.downField("id").as[DepositId]
                    ms <- c.downField("endMs").as[Long]
                yield ObserveDepositRequest(
                  id,
                  RequestTimes.RequestValidityEndTime(quantizedFromMs(ms))
                )
            case "ObserveDepositOnChain" =>
                c.downField("id").as[DepositId].map(ObserveDepositOnChain(_))
            case "ObserveDepositAbsorbed" =>
                for
                    id <- c.downField("id").as[DepositId]
                    by <- c.downField("by").as[BlockId]
                yield ObserveDepositAbsorbed(id, by)
            case "ObserveDepositRejected" =>
                for
                    id <- c.downField("id").as[DepositId]
                    by <- c.downField("by").as[BlockId]
                yield ObserveDepositRejected(id, by)
            case "ObserveSpineEffect" =>
                for
                    id <- c.downField("id").as[EffectId]
                    kind <- c.downField("kind").as[SpineKind]
                    blockId <- c.downField("blockId").as[BlockId]
                    v <- c.downField("validity").as[Interval]
                yield ObserveSpineEffect(id, kind, blockId, v)
            case "ObserveFallback" =>
                for
                    id <- c.downField("id").as[EffectId]
                    paired <- c.downField("pairedEffectId").as[EffectId]
                    ms <- c.downField("startMs").as[Long]
                yield ObserveFallback(
                  id,
                  paired,
                  BlockTimes.FallbackTxStartTime(quantizedFromMs(ms))
                )
            case "ObserveRefund" =>
                for
                    id <- c.downField("id").as[EffectId]
                    dep <- c.downField("depositId").as[DepositId]
                    ms <- c.downField("startMs").as[Long]
                yield ObserveRefund(id, dep, quantizedFromMs(ms))
            case "HypothesizeDeposit" =>
                for
                    id <- c.downField("id").as[DepositId]
                    ms <- c.downField("endMs").as[Long]
                yield HypothesizeDeposit(
                  id,
                  RequestTimes.RequestValidityEndTime(quantizedFromMs(ms))
                )
            case "HypothesizeBlock" =>
                for
                    id <- c.downField("id").as[BlockId]
                    creation <- c.downField("creation").as[Interval]
                    kind <- c.downField("kind").as[BlockKind]
                yield HypothesizeBlock(id, creation, kind)
            case "Retract" =>
                c.downField("id").as[ObjectId].map(Retract(_))
            case other =>
                Left(DecodingFailure(s"unknown Command type: $other", c.history))

    // --- Frame codecs (server → client) --------------------------------------------------------

    given Encoder[ConfigFrame] = Encoder.instance(cf =>
        Json.obj(
          "minSettlementMs" -> cf.minSettlementMs.asJson,
          "inactivityMarginMs" -> cf.inactivityMarginMs.asJson,
          "silenceMs" -> cf.silenceMs.asJson,
          "depositSubmissionMs" -> cf.depositSubmissionMs.asJson,
          "depositMaturityMs" -> cf.depositMaturityMs.asJson,
          "depositAbsorptionMs" -> cf.depositAbsorptionMs.asJson
        )
    )

    given Encoder[TrackItem] = Encoder.instance {
        case TrackItem.Bar(id, oid, label, kind, source, startMs, endMs) =>
            Json.obj(
              "type" -> "Bar".asJson,
              "id" -> id.asJson,
              "objectId" -> oid.asJson,
              "label" -> label.asJson,
              "kind" -> kind.asJson,
              "source" -> source.asJson,
              "startMs" -> startMs.asJson,
              "endMs" -> endMs.asJson
            )
        case TrackItem.Marker(id, oid, label, kind, source, atMs) =>
            Json.obj(
              "type" -> "Marker".asJson,
              "id" -> id.asJson,
              "objectId" -> oid.asJson,
              "label" -> label.asJson,
              "kind" -> kind.asJson,
              "source" -> source.asJson,
              "atMs" -> atMs.asJson
            )
    }

    given Encoder[TracksFrame] = Encoder.instance(tf =>
        Json.obj(
          "requests" -> tf.requests.asJson,
          "blocks" -> tf.blocks.asJson,
          "spineEffects" -> tf.spineEffects.asJson,
          "fallbacks" -> tf.fallbacks.asJson,
          "deposits" -> tf.deposits.asJson,
          "refunds" -> tf.refunds.asJson
        )
    )

    given Encoder[DerivationEdge] = Encoder.instance(d =>
        Json.obj(
          "targetObject" -> d.targetObject.asJson,
          "targetField" -> d.targetField.asJson,
          "sourceObject" -> d.sourceObject.asJson,
          "sourceField" -> d.sourceField.asJson
        )
    )

    given Encoder[Frame] = Encoder.instance(f =>
        Json.obj(
          "nowMs" -> f.nowMs.asJson,
          "config" -> f.config.asJson,
          "tracks" -> f.tracks.asJson,
          "derivations" -> f.derivations.asJson
        )
    )

    // --- helper: parse a single command from string ---------------------------------------------

    def parseCommand(json: String)(using
        CardanoNetwork.Section
    ): Either[io.circe.Error, Command] =
        io.circe.parser.parse(json).flatMap(_.as[Command])
