package hydrozoa.multisig.ledger.commitment

import hydrozoa.lib.cardano.scalus.Scalar
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.cardano.onchain.plutus.prelude.List as SList
import scalus.cardano.onchain.plutus.prelude.bls12_381.G1
import scalus.uplc.builtin.Builtins.{bls12_381_G1_add, bls12_381_G1_scalarMul}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.bls12_381.G1Element

/** The commitment is a consensus value — it goes into the treasury datum every peer signs and into
  * the on-chain evacuation proof — so the accumulator library it is computed with is not a free
  * choice. This suite pins the current implementation against the textbook one it replaced:
  * multiply the binomials out by schoolbook convolution, then evaluate the polynomial against the
  * SRS one point at a time.
  *
  * The two agree byte for byte or the switch is not a speedup, it is a fork.
  */
class KzgCommitmentTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    /** Distinct scalars, derived from the index so a size is reproducible without a generator. */
    private def scalarsOfSize(n: Int): SList[Scalar] =
        SList.from((1 to n).toList.map(i => Scalar.applyUnsafe(BigInt(i.toLong * 7919))))

    private def agrees(scalars: SList[Scalar]): Boolean =
        KzgCommitment.calculateKzgCommitment(scalars) == Schoolbook.commitment(scalars)

    test("the commitment matches the schoolbook one over a generated set") {
        // Sizes are kept small on purpose: the oracle is quadratic in the set size, so a large
        // generated case would dominate the suite's runtime. The sizes that exercise the
        // library's algorithm switches are covered explicitly below.
        forAll(Gen.listOf(Gen.choose(1L, Long.MaxValue)), minSuccessful(20)) { values =>
            assert(agrees(SList.from(values.map(v => Scalar.applyUnsafe(BigInt(v))))))
        }
    }

    test("the commitment matches the schoolbook one across the library's algorithm switches") {
        // `Poly.product` goes iterative below 33 elements and subproduct-tree above it, and the
        // tree's multiplication goes naive below 256 coefficients and NTT above. A commitment
        // computed on the wrong side of either boundary would still look like a point, so the
        // sizes that straddle both thresholds are named rather than left to the generator.
        val sizes = List(0, 1, 2, 32, 33, 255, 256, 257, 400)
        assert(sizes.filterNot(n => agrees(scalarsOfSize(n))) == Nil)
    }

    test("the empty set commits to the setup's first point") {
        assert(KzgCommitment.empty == Schoolbook.commitment(SList.Nil))
    }

    test("a set larger than the setup is refused") {
        val tooMany = TrustedSetup.srsG1Elements.length
        assertThrows[AssertionError](KzgCommitment.calculateKzgCommitment(scalarsOfSize(tooMany)))
    }
}

/** The commitment computed the textbook way, as an independent check on the accumulator library.
  *
  * Deliberately the slow formulation: `O(n^2)` to multiply the binomials out, and one scalar
  * multiplication per coefficient to evaluate them. Being slow is what makes it a useful oracle —
  * it shares no code with the implementation beyond the SRS and the curve arithmetic.
  */
private object Schoolbook {

    def commitment(scalars: SList[Scalar]): ByteString = {
        val srs = TrustedSetup.takeSrsG1(scalars.length.toInt + 1)
        evaluate(srs, expand(scalars)).toCompressedByteString
    }

    /** Multiplies the normalized binomials `(x + s)` out into the coefficients of their product,
      * lowest degree first, by schoolbook convolution.
      */
    private def expand(binomials: SList[Scalar]): SList[Scalar] = {
        binomials.foldLeft(SList.singleton(Scalar.one)) { (acc, term) =>
            val shifted: SList[Scalar] = SList.Cons(Scalar.zero, acc)
            val multiplied = acc.map(_ * term).appended(Scalar.zero)
            SList.map2(shifted, multiplied)(_ + _)
        }
    }

    /** Evaluates the polynomial in the exponent: one scalar multiplication per coefficient, summed.
      */
    private def evaluate(srsG1: SList[G1Element], coefficients: SList[Scalar]): G1Element = {
        val terms =
            SList.map2(coefficients, srsG1)((c, point) => bls12_381_G1_scalarMul(c.toInt, point))
        terms.foldLeft(G1.zero)(bls12_381_G1_add)
    }
}
