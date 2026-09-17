package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber, PeerId}
import org.scalatest.funsuite.AnyFunSuite

/** [[Markers.markerFamilies]]: the families whose highest key *is* a recovery marker.
  *
  * Retention must never empty one of these. Doing so loses no history — it makes the next boot read
  * the store as cold and re-bootstrap from stack 0, which surfaces far from the cause as an
  * out-of-bounds journal cursor.
  *
  * The set is asserted exactly rather than by membership, so adding a marker without telling
  * retention fails here instead of on a node months later.
  */
class MarkerFamiliesTest extends AnyFunSuite:

    private val own: HeadPeerNumber = HeadPeerNumber(1)
    private val head: PeerId = PeerId.Head(own)
    private val coil: PeerId = PeerId.Coil(CoilPeerNumber(3))

    test("a head peer's marker families are exactly the six Markers derives from") {
        assert(
          Markers.markerFamilies(head) == Set(
            Cf.SoftConfirmation,
            Cf.HardConfirmation,
            Cf.HardAck(head),
            Cf.Request(own),
            Cf.BlockResult,
            Cf.EvacuationMap
          )
        )
    }

    /** A coil peer assigns no request numbers, so it authors no Request journal and has none to
      * protect. Every other mark means the same thing on both node types.
      */
    test("a coil peer has the same set without a Request journal") {
        assert(
          Markers.markerFamilies(coil) == Set(
            Cf.SoftConfirmation,
            Cf.HardConfirmation,
            Cf.HardAck(coil),
            Cf.BlockResult,
            Cf.EvacuationMap
          )
        )
    }

    /** No marker reads a `SoftAck` family, so soft-acks prune freely — including the last one.
      * Protecting them would retain a row per peer forever for nothing.
      */
    test("SoftAck is not a marker family") {
        val families = Markers.markerFamilies(head)
        assert(!families.contains(Cf.SoftAck(own)))
        assert(!families.exists { case _: Cf.SoftAck => true; case _ => false })
    }

    /** `hardAcked` derives from the **own** author's journal only. Another peer's hard-acks are
      * ordinary data, and holding a row back in each would retain one per peer forever.
      */
    test("only the own author's HardAck is protected") {
        val families = Markers.markerFamilies(head)
        assert(families.contains(Cf.HardAck(head)))
        assert(!families.contains(Cf.HardAck(PeerId.Head(HeadPeerNumber(2)))))
        assert(!families.contains(Cf.HardAck(coil)))
    }

    /** `Request(own)` is the sharpest of them: `nextRequestNumber` is `max(key) + 1`, so an empty
      * journal re-issues request numbers already handed to users — a CR1 violation, not merely a
      * slow boot.
      */
    test("a head peer's own Request journal is protected, and no other peer's is") {
        val families = Markers.markerFamilies(head)
        assert(families.contains(Cf.Request(own)))
        assert(!families.contains(Cf.Request(HeadPeerNumber(2))))
    }

end MarkerFamiliesTest
