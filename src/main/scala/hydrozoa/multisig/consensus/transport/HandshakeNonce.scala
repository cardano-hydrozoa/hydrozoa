package hydrozoa.multisig.consensus.transport

import cats.effect.IO
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import java.security.SecureRandom
import scodec.bits.ByteVector

/** The challenge a liaison server issues on a freshly accepted socket, and which the dialer's
  * [[HandshakeAuth.Signed]] signs over.
  *
  * **This is what binds a signature to a channel.** A signature over a fixed payload authenticates
  * a message, so anyone who observed one handshake could replay it onto their own socket. A nonce
  * the server draws per socket makes a captured handshake worthless anywhere else: the server
  * accepts only the nonce it issued on this socket, and the socket carries exactly one handshake.
  *
  * It does **not** authenticate the rest of the stream. Only the handshake is signed, so an
  * attacker able to hijack the TCP stream after the handshake owns the session. The transport
  * supplies channel integrity and the signed handshake supplies identity; neither substitutes for
  * the other.
  */
opaque type HandshakeNonce = ByteVector

object HandshakeNonce {

    /** Nonce width. 32 bytes is far past what a collision argument needs and matches the digest
      * width used everywhere else in the protocol.
      */
    val sizeBytes: Int = 32

    /** Draw a fresh nonce from the platform CSPRNG. One per accepted socket. */
    def random: IO[HandshakeNonce] = IO.delay {
        val bytes = new Array[Byte](sizeBytes)
        SecureRandom().nextBytes(bytes)
        ByteVector(bytes)
    }

    /** Wrap raw bytes. For decoding and for tests that need a fixed nonce; live sockets use
      * [[random]].
      */
    def apply(bytes: ByteVector): HandshakeNonce = bytes

    extension (nonce: HandshakeNonce) {
        def bytes: ByteVector = nonce

        /** Short rendering for a log line — the full 32 bytes say nothing an operator can use. */
        def shortHex: String = nonce.take(4).toHex
    }

    given Encoder[HandshakeNonce] = Encoder.instance(_.toHex.asJson)

    given Decoder[HandshakeNonce] = Decoder.decodeString.emap(s =>
        ByteVector
            .fromHex(s)
            .toRight(s"Invalid hex for a handshake nonce: $s")
            .flatMap(bv =>
                Either.cond(
                  bv.size == sizeBytes.toLong,
                  bv,
                  s"A handshake nonce is $sizeBytes bytes, got ${bv.size}"
                )
            )
    )
}
