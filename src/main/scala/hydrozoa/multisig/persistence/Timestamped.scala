package hydrozoa.multisig.persistence

import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}

/** A persisted value paired with the [[ArrivalStamp]] this node recorded it at. Used by the
  * confirmation CFs (which, unlike journal CFs, carry no arrival-stamp framing of their own): a
  * soft/hard confirmation moment is a per-node observation, stored as the monotonic `(generation,
  * monotonicNanos)` stamp rather than a wall-clock instant — the wall-clock time is derived on read
  * from the per-generation zero-time anchor ([[Persistence.zeroTimes]]), so no wall clock is ever
  * persisted.
  */
final case class Timestamped[P](stamp: ArrivalStamp, payload: P)

object Timestamped:
    given arrivalStampEncoder: Encoder[ArrivalStamp] =
        (s: ArrivalStamp) =>
            Json.obj(
              "generation" -> s.generation.asJson,
              "monotonicNanos" -> s.monotonicNanos.asJson
            )

    given arrivalStampDecoder: Decoder[ArrivalStamp] = c =>
        for {
            generation <- c.downField("generation").as[Int]
            monotonicNanos <- c.downField("monotonicNanos").as[Long]
        } yield ArrivalStamp(generation, monotonicNanos)

    given [P: Encoder]: Encoder[Timestamped[P]] =
        (t: Timestamped[P]) => Json.obj("stamp" -> t.stamp.asJson, "payload" -> t.payload.asJson)

    given [P: Decoder]: Decoder[Timestamped[P]] = c =>
        for {
            stamp <- c.downField("stamp").as[ArrivalStamp]
            payload <- c.downField("payload").as[P]
        } yield Timestamped(stamp, payload)
