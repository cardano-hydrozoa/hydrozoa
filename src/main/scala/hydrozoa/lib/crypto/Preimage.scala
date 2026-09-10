package hydrozoa.lib.crypto

import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedFiniteDuration, QuantizedInstant}
import java.io.ByteArrayOutputStream
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.ledger.{Blake2b_256, Coin, Hash, Hash32, ScriptHash, TransactionInput}
import scalus.uplc.builtin.{ByteString, platform}

/** Accumulates the preimage of one of Hydrozoa's `blake2b_256` digests, and hashes it.
  *
  * The protocol's digests are written out byte by byte rather than delegated to a JSON or CBOR
  * encoder: a codec tweak that silently moved one of these values would break a live head, whose
  * peers compare the digest and not the bytes behind it. Writing the layout by hand is what keeps
  * the preimage a decision rather than a consequence of somebody else's encoder.
  *
  * The layout rule every writer follows: an ASCII domain tag first, fixed-width values unframed,
  * variable-width values length-framed. That is what makes the encoding injective — no two distinct
  * inputs can produce the same byte string — which is the property the digest rests on.
  *
  * Users: [[hydrozoa.config.head.HeadParamsHash]] (`docs/spec/head-params-hash.md`) and
  * [[hydrozoa.multisig.ledger.block.BlockHash]].
  */
final class Preimage {
    private val buffer = ByteArrayOutputStream()

    /** The preimage accumulated so far. */
    def bytes: Array[Byte] = buffer.toByteArray

    /** `blake2b_256` over [[bytes]] — the digest itself. */
    def digest: Hash32 =
        Hash[Blake2b_256, Any](platform.blake2b_256(ByteString.unsafeFromArray(bytes)))

    /** Append bytes verbatim. For a domain tag, or for a value whose width is fixed by its type. */
    def raw(value: Array[Byte]): Unit = buffer.write(value)

    /** Append a variable-width value behind its `u32` length. */
    def framed(value: Array[Byte]): Unit = {
        u32(value.length)
        buffer.write(value)
    }

    def u8(value: Int): Unit = buffer.write(value & 0xff)

    def u32(value: Int): Unit = {
        buffer.write((value >>> 24) & 0xff)
        buffer.write((value >>> 16) & 0xff)
        buffer.write((value >>> 8) & 0xff)
        buffer.write(value & 0xff)
    }

    def u64(value: Long): Unit = {
        u32((value >>> 32).toInt)
        u32(value.toInt)
    }

    def bool(value: Boolean): Unit = u8(if value then 0x01 else 0x00)

    def coin(value: Coin): Unit = u64(value.value)

    def duration(value: QuantizedFiniteDuration): Unit = finiteDuration(value.finiteDuration)

    def finiteDuration(value: FiniteDuration): Unit = u64(value.toMillis)

    def instant(value: QuantizedInstant): Unit = u64(value.instant.toEpochMilli)

    def hash32(value: Hash32): Unit = raw(value.bytes)

    def scriptHash(value: ScriptHash): Unit = raw(value.bytes)

    def transactionInput(value: TransactionInput): Unit = {
        raw(value.transactionId.bytes)
        u32(value.index)
    }
}
