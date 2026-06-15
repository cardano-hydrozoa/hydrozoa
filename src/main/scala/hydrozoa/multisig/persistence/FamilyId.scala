package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}

/** Identifies one single-writer family — the range-scan prefix.
  *
  * Spines (Block, Stack) are head-global: there is exactly one of each family for the whole head,
  * round-robin-authored by all head peers. Satellite families (Request, SoftAck, HardAck) are
  * per-author: one family per [[HeadPeerNumber]] per family type.
  *
  * See `design/persistence-and-crash-recovery.md` §3.1, §7.1.
  */
enum FamilyId:
    case BlockSpine
    case StackSpine
    case Request(peer: HeadPeerNumber)
    case SoftAck(peer: HeadPeerNumber)
    case HardAck(peer: HeadPeerNumber)
    case CoilHardAck(coil: CoilPeerNumber)
    case HubHardAck(hub: HeadPeerNumber)

    /** The column family this family lives in. The CF is the family-type discriminant — there is no
      * tag byte in the encoded key (§7.1).
      */
    def cf: Cf = this match
        case BlockSpine     => Cf.Block
        case StackSpine     => Cf.Stack
        case Request(p)     => Cf.Request(p)
        case SoftAck(p)     => Cf.SoftAck(p)
        case HardAck(p)     => Cf.HardAck(p)
        case CoilHardAck(c) => Cf.CoilHardAck(c)
        case HubHardAck(h)  => Cf.HubHardAck(h)
