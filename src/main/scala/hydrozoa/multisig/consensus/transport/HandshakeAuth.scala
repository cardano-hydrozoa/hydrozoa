package hydrozoa.multisig.consensus.transport

import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder}

/** How the peer on the other end of a liaison link proved its identity.
  *
  * **One case today: it did not.** A handshake asserts a peer number and nothing verifies it, so
  * any client that can reach the socket can claim to be any peer. Closing that is GUM-322.
  *
  * The slot exists now so that closing it extends this vocabulary rather than reshaping both frame
  * envelopes and every site that builds or matches one. A peer that cannot make sense of a proof it
  * is offered refuses the link, which is why this rides the same frame as [[ProtocolVersion]]
  * rather than arriving later on an already-accepted connection.
  */
enum HandshakeAuth:
    /** The counterpart asserted its identity and offered no proof of it. */
    case Unauthenticated

object HandshakeAuth:

    /** Wire tag for each case. Absent on the wire reads as [[Unauthenticated]], so a counterpart
      * that omits the field is treated as what it is — unproven — rather than rejected.
      */
    given Encoder[HandshakeAuth] = Encoder.instance { case Unauthenticated =>
        "unauthenticated".asJson
    }

    given Decoder[HandshakeAuth] = Decoder.instance(c =>
        c.as[String].flatMap {
            case "unauthenticated" => Right(Unauthenticated)
            case other => Left(DecodingFailure(s"Unknown handshake auth: $other", c.history))
        }
    )
