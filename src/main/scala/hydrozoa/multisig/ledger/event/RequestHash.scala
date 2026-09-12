package hydrozoa.multisig.ledger.event

import hydrozoa.lib.crypto.Preimage
import io.circe.{Codec, Decoder, Encoder}
import java.nio.charset.StandardCharsets.UTF_8
import scalus.cardano.ledger.{Blake2b_256, Hash, Hash32}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString
import scodec.bits.ByteVector

type RequestHash = RequestHash.RequestHash

/** The digest of a user request's body — `requestHash` in `design/block-hash.md`.
  *
  * The content counterpart of [[RequestId]]: an id names a position in one peer's sequence, this
  * names the bytes that sit there. A block body carries both, which is what ties the two together —
  * without it, two peers holding different payloads under the same id compare equal.
  *
  * ```
  * requestHash = blake2b_256(
  *      "gummiworm-request-v1"
  *   || u8(variant)                                    -- 0 deposit, 1 transaction
  *   || deposit:     blake2b_256(l1Payload) || blake2b_256(l2Payload)
  *   || transaction: l2Payload
  * )
  * ```
  *
  * The variant tag is what keeps the two kinds apart: without it a transaction whose `l2Payload` is
  * exactly a deposit's 64-byte pair of digests hashes to the same value as that deposit. It leads
  * the payload, so neither variant needs length framing — a deposit contributes two fixed-width
  * digests, a transaction one trailing payload.
  *
  * A deposit's two payloads are hashed before being concatenated so the pair stays injective:
  * hashing them raw would collapse `hash(abc + def) == hash(ab + cdef)`.
  *
  * The [[RequestId]] is deliberately absent. The same bytes hash the same however they were
  * sequenced, which is what lets a submitter compute the digest before the head has assigned an id
  * — and lets two peers that received the same request agree on its digest without agreeing on
  * anything else.
  *
  * The construction is a public interface: a client that cannot reproduce it cannot get a request
  * accepted. It is written out for clients in `docs/user-guide/REQUEST-HASH.md`, and
  * `UserRequestBody.mkHash` is the entry point both the submitter and the head go through.
  */
object RequestHash {
    opaque type RequestHash = Hash32

    /** Mixed in before anything else so this digest can never collide with a hash of the same bytes
      * taken for another purpose. ASCII, no terminator — the variant tag that follows is
      * fixed-width, so the boundary is unambiguous.
      */
    val domainTag: Array[Byte] = "gummiworm-request-v1".getBytes(UTF_8)

    /** Variant tags, in the order [[hydrozoa.multisig.consensus.UserRequestBody]] declares its
      * cases. They separate the two request kinds inside [[domainTag]]'s namespace.
      */
    val depositVariantTag: Int = 0x00
    val transactionVariantTag: Int = 0x01

    /** The digest of a deposit request: the CBOR of its L1 deposit transaction, and the opaque L2
      * payload that transaction pins.
      */
    def hashDeposit(l1Payload: ByteString, l2Payload: ByteString): RequestHash = {
        val out = Preimage()
        out.raw(domainTag)
        out.u8(depositVariantTag)
        out.raw(blake2b_256(l1Payload).bytes)
        out.raw(blake2b_256(l2Payload).bytes)
        out.mkDigest
    }

    /** The digest of a transaction request: one opaque payload, passed to the L2 ledger unmodified.
      */
    def hashTransaction(l2Payload: ByteString): RequestHash = {
        val out = Preimage()
        out.raw(domainTag)
        out.u8(transactionVariantTag)
        out.raw(l2Payload.bytes)
        out.mkDigest
    }

    /** Take a digest that arrived rather than one derived here — off the wire, out of the store, or
      * from a submitter. It is a claim until something compares it against a derived value; see
      * `UserRequest.checkRequestHash`.
      */
    def fromHash(hash: Hash32): RequestHash = hash

    /** Opens `Hash32`'s own members — `bytes`, `toHex` — on a `RequestHash`, so the wrapper costs
      * nothing at a call site that needs the raw digest.
      */
    given Conversion[RequestHash, Hash32] = identity

    given Codec[RequestHash] = Codec.from(
      Decoder.decodeString.emap(hex =>
          ByteVector
              .fromHex(hex)
              .toRight(s"not a hex-encoded request hash: $hex")
              .flatMap(bytes =>
                  if bytes.size == 32 then
                      Right(Hash[Blake2b_256, Any](ByteString.fromArray(bytes.toArray)))
                  else Left(s"request hash must be 32 bytes, got ${bytes.size}")
              )
      ),
      Encoder.encodeString.contramap(_.toHex)
    )
}
