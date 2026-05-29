package hydrozoa.multisig.persistence

import java.nio.ByteBuffer

/** A typed lane-CF value: the wire payload `P` plus its 8-byte arrival stamp. The stamp is
  * per-entry runtime data (local monotonic time — creation for own entries, receipt for inbound),
  * not derivable from `P`, so it rides alongside the payload here and is framed ahead of it by
  * [[StoreCodec.laneValue]]. See the [[LaneValue$]] companion for the byte framing.
  */
final case class LaneValue[P](stamp: Long, payload: P)

/** Framing for the value stored at a lane key.
  *
  * Lane values are encoded as `[arrivalStamp : 8 big-endian Long][wirePayload …]`. The arrival
  * stamp is the local monotonic time at which the entry was admitted (receipt for inbound remote
  * entries; creation for own ones). The wire-codec payload follows verbatim — stripping the prefix
  * yields the bytes that go on the wire (§5.5, §7.1).
  *
  * Non-lane CFs (`Markers` / `Snapshots` / `Confirmations` / `Meta`) do **not** use this framing —
  * their values carry no stamp prefix.
  */
object LaneValue:

    /** Width of the arrival-stamp prefix, in bytes. */
    val stampWidth: Int = 8

    /** Frame `wirePayload` with `stamp` as the 8-byte big-endian prefix. */
    def frame(stamp: Long, wirePayload: Array[Byte]): Array[Byte] =
        val out = new Array[Byte](stampWidth + wirePayload.length)
        ByteBuffer.wrap(out).putLong(stamp)
        System.arraycopy(wirePayload, 0, out, stampWidth, wirePayload.length)
        out

    /** Read the arrival stamp from a framed lane value. Throws on a malformed payload (treated as
      * store corruption — fail safe).
      */
    def stamp(framed: Array[Byte]): Long =
        if framed.length < stampWidth then
            throw new IllegalArgumentException(
              s"framed lane value too short for stamp: ${framed.length} bytes"
            )
        else ByteBuffer.wrap(framed, 0, stampWidth).getLong

    /** Read the wire-codec payload from a framed lane value. Throws on a malformed payload. */
    def payload(framed: Array[Byte]): Array[Byte] =
        if framed.length < stampWidth then
            throw new IllegalArgumentException(
              s"framed lane value too short for payload: ${framed.length} bytes"
            )
        else java.util.Arrays.copyOfRange(framed, stampWidth, framed.length)
