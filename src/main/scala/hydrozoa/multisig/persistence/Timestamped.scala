package hydrozoa.multisig.persistence

import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import java.time.Instant

/** A persisted value paired with the local wall-clock instant this node recorded it. Used by the
  * confirmation CFs: a soft/hard confirmation moment is a per-node observation (each peer's
  * signature set saturates at its own time), so the stamp is node-local diagnostic data — never
  * consensus state.
  */
final case class Timestamped[P](at: Instant, payload: P)

object Timestamped:
    given [P: Encoder]: Encoder[Timestamped[P]] =
        (t: Timestamped[P]) => Json.obj("at" -> t.at.asJson, "payload" -> t.payload.asJson)

    given [P: Decoder]: Decoder[Timestamped[P]] = c =>
        for {
            at <- c.downField("at").as[Instant]
            payload <- c.downField("payload").as[P]
        } yield Timestamped(at, payload)
