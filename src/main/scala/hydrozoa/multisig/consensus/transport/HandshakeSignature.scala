package hydrozoa.multisig.consensus.transport

import io.circe.syntax.*
import io.circe.{Decoder, Encoder}
import scodec.bits.ByteVector

/** A peer's Ed25519 signature over [[HandshakeProof.preimage]] — the one signed message on a
  * liaison link.
  *
  * Its own type, not a bare byte array, for the reason
  * [[hydrozoa.multisig.consensus.ack.SoftAck.Signature]] and
  * [[hydrozoa.multisig.ledger.stack.StandaloneEvacuationCommitment.Signature]] have theirs: the
  * protocol's signatures all render as hex and only the type says which preimage a given one is
  * good for.
  */
opaque type HandshakeSignature = IArray[Byte]

object HandshakeSignature {

    def apply(signature: IArray[Byte]): HandshakeSignature = signature

    extension (signature: HandshakeSignature) {
        def untagged: IArray[Byte] = identity(signature)

        def bytes: Array[Byte] = IArray.genericWrapArray(signature).toArray
    }

    given Encoder[HandshakeSignature] =
        Encoder.instance(sig => ByteVector(sig.bytes).toHex.asJson)

    given Decoder[HandshakeSignature] = Decoder.decodeString.emap(s =>
        ByteVector
            .fromHex(s)
            .toRight(s"Invalid hex for a handshake signature: $s")
            .map(bv => HandshakeSignature(IArray.from(bv.toArray)))
    )
}
