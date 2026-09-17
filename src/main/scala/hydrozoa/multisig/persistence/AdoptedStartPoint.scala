package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.ack.HardAckNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.l2.L2CommandNumber
import hydrozoa.multisig.ledger.stack.StackNumber

/** Where a coil peer was seeded when it joined, written once at adoption and read on every boot
  * after (GUM-312).
  *
  * **Why this exists rather than a marker.** Every other boot anchor falls out of a single-CF scan
  * over what this peer itself produced ([[Markers]]). A seeded coil produced none of it: it has no
  * own hard-ack, so it has no `hardAckedStack` to anchor the slow side on, and no `BlockResult`, so
  * it has no `fastBlockMark` for the fast side. Deriving those from the start point anyway would
  * mean writing an ack this peer never signed into the one journal its hub pulls from — a row that
  * exists only to be misread. The anchor is recorded for what it is instead, and the two recovery
  * seams ask for it by name.
  *
  * @param startStack
  *   the hard-confirmed stack the coil was seeded at. The slow side opens as though it had just
  *   closed this stack.
  * @param lastBlockNum
  *   that stack's last block — the fast side's anchor, and the block whose brief, deposit map and
  *   command number were adopted alongside this record.
  * @param commandNumber
  *   where the L2 ledger stands at [[lastBlockNum]].
  * @param ownHardAckStart
  *   the first hard-ack index this coil will author. Its hub has already moved its own cursor here
  *   and never asks below it, which is what lets a coil with no ack history be pulled from at all.
  */
final case class AdoptedStartPoint(
    startStack: StackNumber,
    lastBlockNum: BlockNumber,
    commandNumber: L2CommandNumber,
    ownHardAckStart: HardAckNumber
) {

    /** The high-water to seed the own-hard-ack outbound lane with, so the hub's first pull at
      * [[ownHardAckStart]] is the lane's *next* number and not past its bound.
      *
      * A lane whose bound is below what the hub asks for reports out of bounds, and that is fatal —
      * the symptom a cold-booted coil shows today.
      */
    def ownHardAckHighWater: Option[HardAckNumber] =
        Option.when((ownHardAckStart: Int) > 0)(HardAckNumber((ownHardAckStart: Int) - 1))
}
