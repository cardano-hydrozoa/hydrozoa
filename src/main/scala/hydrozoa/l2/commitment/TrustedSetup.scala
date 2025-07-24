package hydrozoa.l2.commitment

import com.github.plokhotnyuk.jsoniter_scala.core.{
    JsonReader,
    JsonValueCodec,
    JsonWriter,
    readFromStream
}
import com.github.plokhotnyuk.jsoniter_scala.macros.{
    CodecMakerConfig,
    ConfiguredJsonValueCodec,
    JsonCodecMaker
}
import hydrozoa.infra.decodeHex
import hydrozoa.l2.commitment.{infG1Point, infG2Point}
import scalus.Compile
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString}
import scalus.prelude.List as SList
import scalus.prelude.List.asScalus
import scalus.prelude.crypto.bls12_381.{G1, G2}
import supranational.blst.{P1, P2}

import java.io.InputStream

/** @param g1Monomial
  *   `g1 ^ tau * n`
  * @param g2Monomial
  *   `g2 ^ tau* n`
  * @tparam G1
  *   supported values: P1 and BLS12_381_G1_Element
  * @tparam G2
  *   supported values: P2 and BLS12_381_G2_Element
  */
case class TrustedSetup[G1, G2](
    g1Monomial: List[G1],
    g2Monomial: List[G2]
)

given JsonValueCodec[BLS12_381_G1_Element] = new JsonValueCodec[BLS12_381_G1_Element] {
    def decodeValue(in: JsonReader, default: BLS12_381_G1_Element): BLS12_381_G1_Element =
        BLS12_381_G1_Element.apply(ByteString.fromHex(in.readString("").substring(2)))

    def encodeValue(x: BLS12_381_G1_Element, out: JsonWriter): Unit = ???

    def nullValue: BLS12_381_G1_Element = G1.zero
}

given JsonValueCodec[P1] = new JsonValueCodec[P1] {
    def decodeValue(in: JsonReader, default: P1): P1 =
        P1(decodeHex(in.readString("").substring(2)).toArray)

    def encodeValue(x: P1, out: JsonWriter): Unit = ???

    def nullValue: P1 = infG1Point
}

given JsonValueCodec[BLS12_381_G2_Element] = new JsonValueCodec[BLS12_381_G2_Element] {
    def decodeValue(in: JsonReader, default: BLS12_381_G2_Element): BLS12_381_G2_Element =
        BLS12_381_G2_Element.apply(ByteString.fromHex(in.readString("").substring(2)))

    def encodeValue(x: BLS12_381_G2_Element, out: JsonWriter): Unit = ???

    def nullValue: BLS12_381_G2_Element = G2.zero
}

given JsonValueCodec[P2] = new JsonValueCodec[P2] {
    def decodeValue(in: JsonReader, default: P2): P2 =
        P2(decodeHex(in.readString("").substring(2)).toArray)

    def encodeValue(x: P2, out: JsonWriter): Unit = ???

    def nullValue: P2 = infG2Point
}

given [G1, G2](using JsonValueCodec[G1], JsonValueCodec[G2]): JsonValueCodec[TrustedSetup[G1, G2]] =
    JsonCodecMaker.make(CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case2))

object TrustedSetup:
    def readFromResource[G1, G2](using JsonValueCodec[TrustedSetup[G1, G2]]): TrustedSetup[G1, G2] =
        val is: InputStream = getClass.getResourceAsStream("/trusted_setup_32768.json")
        readFromStream[TrustedSetup[G1, G2]](is)

    def g1Monomials(n: Int): SList[P1] =
        readFromResource[P1, P2].g1Monomial.take(n).asScalus

    def g1Monomials1(n: Int): SList[BLS12_381_G1_Element] =
        readFromResource[BLS12_381_G1_Element, BLS12_381_G2_Element].g1Monomial.take(n).asScalus

    val g2Monomials: SList[BLS12_381_G2_Element] =
        readFromResource[BLS12_381_G1_Element, BLS12_381_G2_Element].g2Monomial.asScalus

@main
def main =
    val setup = TrustedSetup.readFromResource[BLS12_381_G1_Element, BLS12_381_G2_Element]
    println(setup.g1Monomial.head)
    println(setup.g2Monomial.head)
