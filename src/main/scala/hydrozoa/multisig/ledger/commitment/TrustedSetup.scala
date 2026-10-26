package hydrozoa.multisig.ledger.commitment

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter, readFromStream}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.crypto.accumulator.BilinearAccumulatorProver
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}

/** Cached trusted setup.
  */
object TrustedSetup:

    /** The setup, read from the resource file once at first use and shared by every thread.
      * Decoding uncompresses and checks each of the 32,768 points, which is slow, hence the cache.
      */
    lazy val setup: TrustedSetup = {
        val input = getClass.getResourceAsStream("/trusted_setup_32768.json")
        readFromStream[TrustedSetup](input)
    }

    /** The first `n` points of the G1 ladder. */
    def takeSrsG1(n: Int): SList[G1Element] = SList.from(setup.g1Monomial.take(n))

    /** The whole G1 ladder, which [[BilinearAccumulatorProver]] commits against by multi-scalar
      * multiplication.
      */
    def srsG1Elements: Vector[G1Element] = setup.g1Monomial

    /** The accumulator setup commitments are built against.
      *
      * `g2Powers` is empty because `accumulateG1` reads `g1Powers` alone; the G2 ladder is needed
      * only to *verify* a proof, which goes through [[takeSrsG2]].
      */
    lazy val accumulatorSetupG1: BilinearAccumulatorProver.Setup =
        BilinearAccumulatorProver.Setup.fromPoints(srsG1Elements, Vector.empty)

    /** The first `n` points of the G2 ladder. */
    def takeSrsG2(n: Int): SList[G2Element] = SList.from(setup.g2Monomial.take(n))

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  JSON  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/** JSON representation of the trusted setup.
  *
  * @param g1Monomial
  *   `g1 ^ tau * n`, this is the name of the field in the setup
  * @param g2Monomial
  *   `g2 ^ tau* n`, this is the name of the field in the setup
  */
private case class TrustedSetup(
    g1Monomial: Vector[G1Element],
    g2Monomial: Vector[G2Element]
)

given JsonValueCodec[G1Element] = pointCodec(G1Element(_), G1Element.generator)

given JsonValueCodec[G2Element] = pointCodec(G2Element(_), G2Element.generator)

given JsonValueCodec[TrustedSetup] =
    JsonCodecMaker.make(
      CodecMakerConfig
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
          .withSkipUnexpectedFields(false)
    )

/** A decode-only codec for a curve point stored as a `0x`-prefixed hex string of its compressed
  * bytes.
  *
  * `nullPoint` is taken by value: jsoniter-scala reads `nullValue` once per list element, and
  * `generator` uncompresses a point on every call.
  */
private def pointCodec[P](fromCompressed: ByteString => P, nullPoint: P): JsonValueCodec[P] =
    new JsonValueCodec[P] {
        def decodeValue(in: JsonReader, default: P): P =
            fromCompressed(ByteString.fromHex(in.readString("").substring(2)))

        def encodeValue(x: P, out: JsonWriter): Unit = ???

        def nullValue: P = nullPoint
    }
