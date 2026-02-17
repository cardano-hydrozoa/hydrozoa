package hydrozoa.multisig.ledger.virtual.commitment

import com.bloxbean.cardano.client.util.HexUtil
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, readFromStream}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scalus.cardano.onchain.plutus.prelude.List as SList
import supranational.blst.{P1, P2}

/** Cached trusted setup.
  */
object TrustedSetup:

    private def readFromResource[G1, G2](using
        JsonValueCodec[TrustedSetup[G1, G2]]
    ): TrustedSetup[G1, G2] = {
        println("reading trusted setup from resource")
        val input = getClass.getResourceAsStream("/trusted_setup_32768.json")
        readFromStream[TrustedSetup[G1, G2]](input)
    }

    /** For building commitments, we use the P1 types since P1 is smaller and because `blst` is
      * fast. Reading the setup from a file is slow, so we cache it. For treasury datum we need a
      * limited number of values of type BLS12_381_G2_Element which can be obtained from P2 values
      * when needed.
      */
    lazy val setup: TrustedSetup[P1, P2] = readFromResource[P1, P2]

    def takeSrsG1(n: Int): SList[P1] = SList.from(setup.g1Monomial.take(n))

    def takeSrsG2(n: Int): SList[P2] = SList.from(setup.g2Monomial.take(n))

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  JSON  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/** JSON representation of the trusted setup.
  *
  * @param g1Monomial
  *   `g1 ^ tau * n`, this is the name of the field in the setup
  * @param g2Monomial
  *   `g2 ^ tau* n`, this is the name of the field in the setup
  * @tparam G1
  *   supported values: P1 (blst)
  * @tparam G2
  *   supported values: P2 (blst)
  */
private case class TrustedSetup[G1, G2](
    g1Monomial: List[G1],
    g2Monomial: List[G2]
)

given JsonValueCodec[P1] = new JsonValueCodec[P1] {
    def decodeValue(in: JsonReader, default: P1): P1 =
        P1(HexUtil.decodeHexString(in.readString("").substring(2)))

    def encodeValue(x: P1, out: JsonWriter): Unit = ???

    // This is needed for jsoniter-scala to work, but it's not used
    def nullValue: P1 = P1.generator()
}

given JsonValueCodec[P2] = new JsonValueCodec[P2] {
    def decodeValue(in: JsonReader, default: P2): P2 =
        P2(HexUtil.decodeHexString(in.readString("").substring(2)))

    def encodeValue(x: P2, out: JsonWriter): Unit = ???

    // This is needed for jsoniter-scala to work, but it's not used
    def nullValue: P2 = P2.generator()
}

given [G1, G2](using JsonValueCodec[G1], JsonValueCodec[G2]): JsonValueCodec[TrustedSetup[G1, G2]] =
    JsonCodecMaker.make(
      CodecMakerConfig
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
          .withSkipUnexpectedFields(false)
    )
