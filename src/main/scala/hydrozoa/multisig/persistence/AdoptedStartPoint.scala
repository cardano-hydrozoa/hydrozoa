package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.ack.HardAckNumber
import hydrozoa.multisig.consensus.liaison.BatchMessages.Population
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.event.RequestNumber
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
  * @param cursors
  *   every inbound population lane's first index, as the hub sent them.
  *
  * These cannot be recomputed at home. A lane cursor is normally `max(journal) + 1`, and a seeded
  * coil's journals are empty, so every one of them would restore cold and the coil would pull from
  * the beginning of a history its hub has very likely pruned. Nor is arithmetic on the start point
  * enough: a stack yields one hard-ack per peer when it is sole and two when it is 2-phase, so
  * those indices are a lookup in the hub's journals and travel with the offer.
  */
final case class AdoptedStartPoint(
    startStack: StackNumber,
    lastBlockNum: BlockNumber,
    commandNumber: L2CommandNumber,
    ownHardAckStart: HardAckNumber,
    cursors: Population.Get
) {

    /** The high-water to seed the own-hard-ack outbound lane with, so the hub's first pull at
      * [[ownHardAckStart]] is the lane's *next* number and not past its bound.
      *
      * A lane whose bound is below what the hub asks for reports out of bounds, and that is fatal —
      * the symptom a cold-booted coil shows today.
      */
    def ownHardAckHighWater: Option[HardAckNumber] =
        Option.when((ownHardAckStart: Int) > 0)(HardAckNumber((ownHardAckStart: Int) - 1))

    /** The per-author request high-water this start point implies: one below the first number each
      * request lane will pull. `BlockWeaver`'s contiguity gate and the replay floor both read a
      * high-water, and [[cursors]] carries the cursor, so the step is taken here rather than at
      * each reader.
      *
      * An author absent from the result has had nothing included — which is how both readers spell
      * "this stream opens at [[RequestNumber.zero]]", and why a lane opening at zero drops out
      * instead of claiming request zero was already counted.
      */
    def requestHighWater: Map[HeadPeerNumber, RequestNumber] =
        cursors.requests.collect {
            case (peer, first) if (first: Long) > 0L => peer -> first.previousOrZero
        }
}
