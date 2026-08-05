package hydrozoa.multisig.persistence

import cats.effect.IO
import cats.syntax.traverse.*
import java.nio.ByteBuffer

/** Diagnostics over a [[BackendStore]] — a full dump and per-CF statistics.
  *
  * Intended for development / debugging / on-demand inspection from the REPL or a tool entrypoint —
  * never on a hot path. Walks every CF via the `cursor` API, decoding keys where the CF's schema
  * allows it (journals via [[JournalKey]], spine-indexed metadata as big-endian `Int`, `Meta` as
  * UTF-8, singletons as the empty key). Bypasses the typed [[Persistence]] layer — byte-level
  * inspection is the point.
  */
object StoreDump:

    /** Aggregate counters for one column family. */
    final case class CfStats(cf: Cf, entries: Long, keyBytes: Long, valueBytes: Long):
        def totalBytes: Long = keyBytes + valueBytes

    /** Per-CF stats plus a derived total. */
    final case class Stats(perCf: List[CfStats]):
        val total: CfStats =
            CfStats(
              cf = Cf.Meta, // placeholder; the rendered table ignores `cf` for the total row
              entries = perCf.map(_.entries).sum,
              keyBytes = perCf.map(_.keyBytes).sum,
              valueBytes = perCf.map(_.valueBytes).sum
            )

        /** Human-readable table — CF name, entry count, key/value byte totals. */
        def render: String =
            val rows: List[(String, String, String, String)] =
                perCf.map(s =>
                    (
                      s.cf.toString,
                      s.entries.toString,
                      s.keyBytes.toString,
                      s.valueBytes.toString
                    )
                ) :+ (
                  "TOTAL",
                  total.entries.toString,
                  total.keyBytes.toString,
                  total.valueBytes.toString
                )
            val w0 = math.max(2, rows.map(_._1.length).max)
            val w1 = math.max(7, rows.map(_._2.length).max)
            val w2 = math.max(9, rows.map(_._3.length).max)
            val w3 = math.max(11, rows.map(_._4.length).max)
            def row(a: String, b: String, c: String, d: String): String =
                padRight(a, w0) + "  " + padLeft(b, w1) + "  " + padLeft(c, w2) + "  " +
                    padLeft(d, w3)
            val header = row("CF", "Entries", "Key Bytes", "Value Bytes")
            val sep = "-" * (w0 + w1 + w2 + w3 + 6)
            val body = rows.dropRight(1).map { case (a, b, c, d) => row(a, b, c, d) }
            val totalRow = rows.last match
                case (a, b, c, d) => row(a, b, c, d)
            (header :: sep :: body ::: sep :: totalRow :: Nil).mkString("\n")

        private def padLeft(s: String, w: Int): String =
            if s.length >= w then s else " " * (w - s.length) + s
        private def padRight(s: String, w: Int): String =
            if s.length >= w then s else s + " " * (w - s.length)

    /** Walk every CF in `cfs` and tally entry counts + key/value byte totals. With the per-author
      * split the CF set is config-derived (§7.1), so the caller supplies it (e.g.
      * `Cf.mkAll(headPeers, coilPeers, hubs)`).
      */
    def stats(p: BackendStore[IO], cfs: List[Cf]): IO[Stats] =
        cfs.traverse(statsFor(p, _)).map(Stats.apply)

    private def statsFor(p: BackendStore[IO], cf: Cf): IO[CfStats] =
        p.cursor(cf, Array.emptyByteArray).use { c =>
            def loop(entries: Long, keyB: Long, valB: Long): IO[CfStats] =
                c.next.flatMap {
                    case None => IO.pure(CfStats(cf, entries, keyB, valB))
                    case Some((k, v)) =>
                        loop(entries + 1, keyB + k.length.toLong, valB + v.length.toLong)
                }
            loop(0L, 0L, 0L)
        }

    /** Full dump — every CF in `cfs`, every entry, one line per entry. Pretty-prints keys where the
      * CF's schema allows it; falls back to a hex render otherwise. The caller supplies the
      * config-derived CF set (§7.1).
      */
    def dump(p: BackendStore[IO], cfs: List[Cf]): IO[String] =
        cfs
            .traverse(cf =>
                dumpCf(p, cf).map(section => if section.isEmpty then None else Some(section))
            )
            .map(_.flatten.mkString("\n\n"))

    private def dumpCf(p: BackendStore[IO], cf: Cf): IO[String] =
        p.cursor(cf, Array.emptyByteArray).use { c =>
            def loop(acc: List[String]): IO[List[String]] =
                c.next.flatMap {
                    case None         => IO.pure(acc.reverse)
                    case Some((k, v)) => loop(renderEntry(cf, k, v) :: acc)
                }
            loop(Nil).map { lines =>
                if lines.isEmpty then ""
                else (s"== ${cf} (${lines.size} entries) ==" :: lines).mkString("\n")
            }
        }

    /** Render a single `(key, value)` entry for the dump. */
    private def renderEntry(cf: Cf, key: Array[Byte], value: Array[Byte]): String =
        s"  ${renderKey(cf, key)} -> ${value.length} bytes"

    /** Pretty-print a key. Journal CFs go through [[JournalKey.decode]]; spine-indexed metadata CFs
      * decode as 4-byte big-endian `Int`; `RequestBlockIndex` decodes as the packed i64; `Meta`
      * decodes as UTF-8; singleton snapshot CFs show "(singleton)"; anything malformed falls back
      * to a hex dump.
      */
    private def renderKey(cf: Cf, key: Array[Byte]): String =
        cf match
            case Cf.Block | Cf.Stack | Cf.Request(_) | Cf.SoftAck(_) | Cf.HardAck(_) |
                Cf.HubHardAck(_) =>
                try JournalKey.decode(cf, key).toString
                catch case _: IllegalArgumentException => hex(key)
            case Cf.BlockResult | Cf.SoftConfirmation | Cf.RequestHighWater | Cf.L2CommandNumber |
                Cf.EvacuationMap | Cf.UnsignedStack | Cf.BlockStackIndex =>
                if key.length == 4 then s"$cf(${ByteBuffer.wrap(key).getInt})"
                else hex(key)
            case Cf.HardConfirmation =>
                if key.length == 4 then s"HardConfirmation(${ByteBuffer.wrap(key).getInt})"
                else hex(key)
            case Cf.RequestBlockIndex =>
                if key.length == 8 then s"RequestBlockIndex(i64=${ByteBuffer.wrap(key).getLong})"
                else hex(key)
            case Cf.DepositDecisionIndex =>
                if key.length == 8 then s"DepositDecisionIndex(i64=${ByteBuffer.wrap(key).getLong})"
                else hex(key)
            case Cf.WithdrawalEffectIndex =>
                if key.length == 40 then
                    s"WithdrawalEffectIndex(i64=${ByteBuffer.wrap(key, 0, 8).getLong}, " +
                        s"l1TxId=${hex(key.drop(8))})"
                else hex(key)
            case Cf.EffectStackIndex =>
                s"EffectStackIndex(${hex(key)})"
            case Cf.DepositMap | Cf.Treasury | Cf.CoilStampMark =>
                if key.isEmpty then "(singleton)" else hex(key)
            case Cf.Meta =>
                try s"Meta(${new String(key, "UTF-8")})"
                catch case _: Throwable => hex(key)

    /** Lower-case hex render of a byte array — fallback for unrecognised key shapes. */
    private def hex(bytes: Array[Byte]): String =
        if bytes.isEmpty then "[]"
        else bytes.iterator.map(b => f"${b & 0xff}%02x").mkString("[", "", "]")
