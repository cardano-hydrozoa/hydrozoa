package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Identifies one single-writer lane — the range-scan prefix.
  *
  * Spines (Block, Stack) are head-global: there is exactly one of each lane for the whole head,
  * round-robin-authored by all head peers. Satellite lanes (Request, SoftAck, HardAck) are
  * per-author: one lane per [[HeadPeerNumber]] per lane type.
  *
  * See `design/persistence-and-crash-recovery.md` §3.1, §7.1.
  */
enum LaneId:
    case BlockSpine
    case StackSpine
    case Request(peer: HeadPeerNumber)
    case SoftAck(peer: HeadPeerNumber)
    case HardAck(peer: HeadPeerNumber)

    /** The column family this lane lives in. The CF is the lane-type discriminant — there is no tag
      * byte in the encoded key (§7.1).
      */
    def cf: Cf = this match
        case BlockSpine => Cf.Block
        case StackSpine => Cf.Stack
        case Request(_) => Cf.Request
        case SoftAck(_) => Cf.SoftAck
        case HardAck(_) => Cf.HardAck
