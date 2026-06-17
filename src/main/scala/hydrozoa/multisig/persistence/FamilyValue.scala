package hydrozoa.multisig.persistence

/** A typed family-CF value: the wire payload `P` plus its [[ArrivalStamp]]. The stamp is per-entry
  * runtime data (`(generation, monotonic)` — creation for own entries, receipt for inbound), not
  * derivable from `P`, so it rides alongside the payload here and is framed ahead of it by
  * [[StoreCodec.laneValue]]. See the [[FamilyValue$]] companion for the byte framing.
  */
final case class FamilyValue[P](stamp: ArrivalStamp, payload: P)

/** Framing for the value stored at a family key.
  *
  * Family values are encoded as `[arrivalStamp : 12][wirePayload …]`. The [[ArrivalStamp]] is the
  * durable `(generation, monotonic)` ordering key at which the entry was admitted (receipt for
  * inbound remote entries; creation for own ones — §5.4). The wire-codec payload follows verbatim —
  * stripping the prefix yields the bytes that go on the wire (§5.5, §7.1).
  *
  * Non-family CFs (`Markers` / `Snapshots` / `Confirmations` / `Meta`) do **not** use this framing
  * — their values carry no stamp prefix.
  */
object FamilyValue:

    /** Width of the arrival-stamp prefix, in bytes. */
    val stampWidth: Int = ArrivalStamp.width

    /** Frame `wirePayload` with `stamp` as the fixed-width big-endian prefix. */
    def frame(stamp: ArrivalStamp, wirePayload: Array[Byte]): Array[Byte] =
        val out = new Array[Byte](stampWidth + wirePayload.length)
        System.arraycopy(stamp.toBytes, 0, out, 0, stampWidth)
        System.arraycopy(wirePayload, 0, out, stampWidth, wirePayload.length)
        out

    /** Read the arrival stamp from a framed family value. Throws on a malformed payload (treated as
      * store corruption — fail safe).
      */
    def stamp(framed: Array[Byte]): ArrivalStamp =
        if framed.length < stampWidth then
            throw new IllegalArgumentException(
              s"framed family value too short for stamp: ${framed.length} bytes"
            )
        else ArrivalStamp.fromBytes(framed)

    /** Read the wire-codec payload from a framed family value. Throws on a malformed payload. */
    def payload(framed: Array[Byte]): Array[Byte] =
        if framed.length < stampWidth then
            throw new IllegalArgumentException(
              s"framed family value too short for payload: ${framed.length} bytes"
            )
        else framed.drop(stampWidth)
