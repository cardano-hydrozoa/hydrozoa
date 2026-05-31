package hydrozoa.multisig.persistence.recovery

import hydrozoa.multisig.persistence.{ArrivalStamp, LaneValue}

/** One lane entry read back from the store during recovery: its decoded
  * [[hydrozoa.multisig.persistence.LaneKey]], the [[ArrivalStamp]] that orders it across lanes
  * (§5.4), and the **framed** value bytes (`[stamp : 12][wire payload …]`).
  *
  * The payload is left **encoded** on purpose. Recovery ordering ([[ArrivalOrderedMerge]]) needs
  * only the stamp, never the payload — so a scan + merge pays no JSON-decode cost (§5.4: "stamps
  * order streams; they are not a clock"). The consumer decodes the typed payload at dispatch time
  * with the key's own codec:
  *
  * {{{
  *   entry.key match
  *       case k: LaneKey.Block => k.codec.decode(entry.framed).payload  // BlockBrief.Next
  *       …
  * }}}
  *
  * See `design/persistence-and-crash-recovery.md` §5.4, and
  * `design/recovery-implementation-plan.md` R1.
  */
final case class RawLaneEntry(
    key: hydrozoa.multisig.persistence.LaneKey,
    stamp: ArrivalStamp,
    framed: Array[Byte]
):
    /** The payload bytes alone (the wire form), with the stamp prefix stripped. */
    def payloadBytes: Array[Byte] = LaneValue.payload(framed)
