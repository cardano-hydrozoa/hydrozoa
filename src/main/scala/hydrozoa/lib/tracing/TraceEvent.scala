package hydrozoa.lib.tracing

/** Protocol trace events — the Scala-side definition of the checker-compatible schema.
  *
  * Each variant corresponds to one JSONL line. The `toJson` method produces the canonical JSON
  * format that the Lean conformance checker (`hydrozoa-check`) can parse.
  */
sealed trait TraceEvent {
    def seq: Long
    def ts: Long
    def node: String
    def eventName: String
    def extraFields: List[(String, String)]

    def toJson: String = {
        val base = List(
          s""""seq":${seq}""",
          s""""ts":${ts}""",
          s""""node":"${escapeJson(node)}"""",
          s""""event":"${eventName}""""
        )
        val extra = extraFields.map { case (k, v) => s""""${k}":${v}""" }
        s"HTRACE|{${(base ++ extra).mkString(",")}}"
    }

    private def escapeJson(s: String): String =
        s.replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\t", "\\t")
}

object TraceEvent {
    private def jsonStr(s: String): String =
        s""""${s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")}""""

    final case class LeaderStarted(seq: Long, ts: Long, node: String, blockNum: Long, peer: Int)
        extends TraceEvent {
        def eventName: String = "leader_started"
        def extraFields: List[(String, String)] = List(
          "block_num" -> blockNum.toString,
          "peer" -> peer.toString
        )
    }

    final case class BriefProduced(
        seq: Long,
        ts: Long,
        node: String,
        blockNum: Long,
        peer: Int,
        blockType: String,
        vMajor: Int,
        vMinor: Int,
        eventCount: Int
    ) extends TraceEvent {
        def eventName: String = "brief_produced"
        def extraFields: List[(String, String)] = List(
          "block_num" -> blockNum.toString,
          "peer" -> peer.toString,
          "block_type" -> jsonStr(blockType),
          "v_major" -> vMajor.toString,
          "v_minor" -> vMinor.toString,
          "event_count" -> eventCount.toString
        )
    }

    final case class Ack(
        seq: Long,
        ts: Long,
        node: String,
        blockNum: Long,
        peer: Int,
        ackType: String
    ) extends TraceEvent {
        def eventName: String = "ack"
        def extraFields: List[(String, String)] = List(
          "block_num" -> blockNum.toString,
          "peer" -> peer.toString,
          "ack_type" -> jsonStr(ackType)
        )
    }

    final case class RoundComplete(
        seq: Long,
        ts: Long,
        node: String,
        blockNum: Long,
        blockType: String,
        round: Int
    ) extends TraceEvent {
        def eventName: String = "round_complete"
        def extraFields: List[(String, String)] = List(
          "block_num" -> blockNum.toString,
          "block_type" -> jsonStr(blockType),
          "round" -> round.toString
        )
    }

    final case class BlockConfirmed(
        seq: Long,
        ts: Long,
        node: String,
        blockNum: Long,
        blockType: String,
        vMajor: Int,
        vMinor: Int
    ) extends TraceEvent {
        def eventName: String = "block_confirmed"
        def extraFields: List[(String, String)] = List(
          "block_num" -> blockNum.toString,
          "block_type" -> jsonStr(blockType),
          "v_major" -> vMajor.toString,
          "v_minor" -> vMinor.toString
        )
    }

    final case class EventProcessed(
        seq: Long,
        ts: Long,
        node: String,
        eventId: String,
        blockNum: Long,
        valid: Boolean
    ) extends TraceEvent {
        def eventName: String = "event_processed"
        def extraFields: List[(String, String)] = List(
          "event_id" -> jsonStr(eventId),
          "block_num" -> blockNum.toString,
          "valid" -> valid.toString
        )
    }

    final case class DepositAbsorbed(
        seq: Long,
        ts: Long,
        node: String,
        depositId: String,
        blockNum: Long,
        amount: Long
    ) extends TraceEvent {
        def eventName: String = "deposit_absorbed"
        def extraFields: List[(String, String)] = List(
          "deposit_id" -> jsonStr(depositId),
          "block_num" -> blockNum.toString,
          "amount" -> amount.toString
        )
    }

    final case class BalanceSnapshot(
        seq: Long,
        ts: Long,
        node: String,
        blockNum: Long,
        l2Total: Long,
        l1Treasury: Long
    ) extends TraceEvent {
        def eventName: String = "balance_snapshot"
        def extraFields: List[(String, String)] = List(
          "block_num" -> blockNum.toString,
          "l2_total" -> l2Total.toString,
          "l1_treasury" -> l1Treasury.toString
        )
    }

    final case class Settlement(seq: Long, ts: Long, node: String, blockNum: Long, vMajor: Int)
        extends TraceEvent {
        def eventName: String = "settlement"
        def extraFields: List[(String, String)] = List(
          "block_num" -> blockNum.toString,
          "v_major" -> vMajor.toString
        )
    }

    final case class TraceError(
        seq: Long,
        ts: Long,
        node: String,
        blockNum: Long,
        errorType: String,
        msg: String
    ) extends TraceEvent {
        def eventName: String = "error"
        def extraFields: List[(String, String)] = List(
          "block_num" -> blockNum.toString,
          "error_type" -> jsonStr(errorType),
          "msg" -> jsonStr(msg)
        )
    }
}
