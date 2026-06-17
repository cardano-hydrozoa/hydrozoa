package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}

/** Identifies one append-only family — the range-scan prefix.
  *
  * Spines (Block, Stack) are head-global: there is exactly one of each family for the whole head,
  * round-robin-authored by all head peers. Satellite families (Request, SoftAck, HardAck) are
  * per-author: one family per author per family type. Request/SoftAck are per [[HeadPeerNumber]];
  * HardAck is per [[PeerId]] (one family per head peer and per coil peer).
  *
  * See `design/persistence-and-crash-recovery.md` §3.1, §7.1.
  */
enum FamilyId:
    case BlockSpine
    case StackSpine
    case Request(peer: HeadPeerNumber)
    case SoftAck(peer: HeadPeerNumber)
    case HardAck(peer: PeerId)
    case HubHardAck(hub: HeadPeerNumber)

    /** The column family this family lives in. The CF is the family-type discriminant — there is no
      * tag byte in the encoded key (§7.1).
      */
    def cf: Cf = this match
        case BlockSpine    => Cf.Block
        case StackSpine    => Cf.Stack
        case Request(p)    => Cf.Request(p)
        case SoftAck(p)    => Cf.SoftAck(p)
        case HardAck(p)    => Cf.HardAck(p)
        case HubHardAck(h) => Cf.HubHardAck(h)
