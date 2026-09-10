package hydrozoa.lib.crypto

import java.nio.charset.StandardCharsets.UTF_8
import org.scalatest.funsuite.AnyFunSuite

/** [[Preimage]]'s byte layout is the layout of every digest built on it — `headParamsHash` sits in
  * live treasury datums and `blockHash` sits in signed soft-acks — so it is pinned here rather than
  * left to be inferred from the writers.
  *
  * A property test cannot catch what this does: a change that shifts every writer the same way
  * still satisfies "the digest moves when a field moves", and only breaks once a running head reads
  * a value written by an older build.
  */
class PreimageTest extends AnyFunSuite {

    private def toHex(bytes: Array[Byte]): String =
        bytes.map(b => f"${b & 0xff}%02x").mkString

    test("Fixed-width values are big-endian and unframed, variable-width ones are length-framed") {
        val out = Preimage()
        out.raw("tag".getBytes(UTF_8))
        out.u8(1)
        out.u32(2)
        out.u64(3)
        out.bool(true)
        out.framed(Array[Byte](7, 8))
        assert(
          toHex(out.bytes) ==
              "746167" + // "tag", verbatim
              "01" + // u8
              "00000002" + // u32
              "0000000000000003" + // u64
              "01" + // bool
              "00000002" + "0708" // framed: u32 length, then the bytes
        )
    }

    test("The digest is blake2b-256 over the accumulated bytes") {
        val out = Preimage()
        out.raw("tag".getBytes(UTF_8))
        out.u8(1)
        out.u32(2)
        out.u64(3)
        out.bool(true)
        out.framed(Array[Byte](7, 8))
        assert(
          out.digest.toHex == "aae1bceab146f024e2710680c931343ea1127afa718a342fb7cd3435ce7a3e9f"
        )
    }

    test("A false bool is a zero byte, so it cannot be confused with an absent field") {
        val out = Preimage()
        out.bool(false)
        assert(toHex(out.bytes) == "00")
    }

    /** Framing is what makes the encoding injective: two adjacent variable-width values cannot be
      * re-split into a different pair.
      */
    test("Framing keeps adjacent variable-width values apart") {
        val split = Preimage()
        split.framed(Array[Byte](1))
        split.framed(Array[Byte](2, 3))

        val other = Preimage()
        other.framed(Array[Byte](1, 2))
        other.framed(Array[Byte](3))

        assert(toHex(split.bytes) != toHex(other.bytes))
    }
}
