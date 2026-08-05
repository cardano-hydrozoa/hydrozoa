package hydrozoa.rulebased.ledger.l1.script.plutus

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.commitment.TrustedSetup
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.bls12_381.G2Element

/** The deployed-once G2 setup ladder: 7 utxos at the burn address whose inline datums are
  * self-contained prefixes of the KZG trusted setup. Rung `i` holds the first `2^i + 1` compressed
  * G2 points and thus covers exactly `2^i` evacuations per tx (evacuating `k` utxos needs the first
  * `k + 1` points); rung 6 covers
  * [[hydrozoa.rulebased.ledger.l1.tx.EvacuationTx.Assumptions.maxEvacuationsPerTx]].
  *
  * An EvacuationTx references exactly one rung — the smallest that covers its evacuee count — and
  * points the treasury validator at it via the `setupRefInputIdx` redeemer field.
  */
object SetupLadder {

    val rungCount: Int = 7

    /** Points per rung: {2, 3, 5, 9, 17, 33, 65}. */
    val rungSizes: List[Int] = List.tabulate(rungCount)(i => (1 << i) + 1)

    /** The smallest rung index covering `k` evacuations; `k` must be in [1, 2^(rungCount-1)]. */
    def rungForEvacuations(k: Int): Either[UncoveredEvacuationCount, Int] =
        if k < 1 || k > (1 << (rungCount - 1)) then Left(UncoveredEvacuationCount(k))
        else Right(rungSizes.indexWhere(_ >= k + 1))

    /** Rung `i`'s inline datum: the first `rungSizes(i)` compressed G2 setup points. */
    def rungDatum(i: Int): Data =
        TrustedSetup
            .takeSrsG2(rungSizes(i))
            .map(p2 => G2Element(p2).toCompressedByteString)
            .toData

    /** All rung datums in rung order, ready to deploy as [[DeploymentTx]] payloads. */
    def rungDatums: NonEmptyList[Data] =
        NonEmptyList.fromListUnsafe(List.tabulate(rungCount)(rungDatum))

    final case class UncoveredEvacuationCount(k: Int) extends Throwable {
        override def getMessage: String =
            s"No setup ladder rung covers $k evacuations (valid range: 1 to ${1 << (rungCount - 1)})"
    }
}
