package hydrozoa.bls

import com.github.plokhotnyuk.jsoniter_scala.core.{
    JsonReader,
    JsonValueCodec,
    JsonWriter,
    readFromStream
}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, ByteString}
import scalus.prelude.crypto.bls12_381.{G1, G2}

import java.io.InputStream

case class TrustedSetup(
    g1Monomial: List[BLS12_381_G1_Element],
    g2Monomial: List[BLS12_381_G2_Element]
)

given JsonValueCodec[BLS12_381_G1_Element] = new JsonValueCodec[BLS12_381_G1_Element] {
    def decodeValue(in: JsonReader, default: BLS12_381_G1_Element): BLS12_381_G1_Element =
        BLS12_381_G1_Element.apply(ByteString.fromHex(in.readString("").substring(2)))

    def encodeValue(x: BLS12_381_G1_Element, out: JsonWriter): Unit = ???

    def nullValue: BLS12_381_G1_Element = G1.zero
}

given JsonValueCodec[BLS12_381_G2_Element] = new JsonValueCodec[BLS12_381_G2_Element] {
    def decodeValue(in: JsonReader, default: BLS12_381_G2_Element): BLS12_381_G2_Element =
        BLS12_381_G2_Element.apply(ByteString.fromHex(in.readString("").substring(2)))

    def encodeValue(x: BLS12_381_G2_Element, out: JsonWriter): Unit = ???

    def nullValue: BLS12_381_G2_Element = G2.zero
}

given JsonValueCodec[TrustedSetup] =
    JsonCodecMaker.make(CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case2))

object TrustedSetup:
    def readFromResource: TrustedSetup =
        val is: InputStream = getClass.getResourceAsStream("/trusted_setup_32768.json")
        readFromStream[TrustedSetup](is)

@main
def main =
    val setup = TrustedSetup.readFromResource
    println(setup.g1Monomial.head)
    println(setup.g2Monomial.head)
