package hydrozoa.multisig.persistence

import hydrozoa.multisig.consensus.peer.{HeadPeerNumber, PeerId}

/** Identifies one append-only journal — the range-scan prefix.
  *
  * Spines (Block, Stack) are head-global: there is exactly one of each journal for the whole head,
  * round-robin-authored by all head peers. Satellite journals (Request, SoftAck, HardAck) are
  * per-author: one journal per author per journal type. Request/SoftAck are per [[HeadPeerNumber]];
  * HardAck is per [[PeerId]] (one journal per head peer and per coil peer).
  *
  * See `design/persistence-and-crash-recovery.md` §3.2, §7.1.
  */
enum JournalId:
    case BlockSpine
    case StackSpine
    case Request(peer: HeadPeerNumber)
    case SoftAck(peer: HeadPeerNumber)
    case HardAck(peer: PeerId)
    case HubHardAck(hub: HeadPeerNumber)

    /** The column family this journal lives in. The CF is the journal-type discriminant — there is
      * no tag byte in the encoded key (§7.1).
      */
    def cf: Cf = this match
        case BlockSpine    => Cf.Block
        case StackSpine    => Cf.Stack
        case Request(p)    => Cf.Request(p)
        case SoftAck(p)    => Cf.SoftAck(p)
        case HardAck(p)    => Cf.HardAck(p)
        case HubHardAck(h) => Cf.HubHardAck(h)
