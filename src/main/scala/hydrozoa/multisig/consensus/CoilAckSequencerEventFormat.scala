package hydrozoa.multisig.consensus

import hydrozoa.lib.logging.LogEvent
import hydrozoa.multisig.consensus.CoilAckSequencerEvent.*
import hydrozoa.multisig.consensus.peer.HeadPeerNumber

/** Renderers from [[CoilAckSequencerEvent]] to [[LogEvent]]. */
object CoilAckSequencerEventFormat:

    /** Routes under `CoilAckSequencer.<hubPeerNum>` — the hub head peer that runs this sequencer.
      */
    def humanFormat(hubPeerNum: HeadPeerNumber)(e: CoilAckSequencerEvent): LogEvent = {
        val hubPn: Int = hubPeerNum
        val ev = LogEvent.From(Map("peer" -> hubPn.toString), s"CoilAckSequencer.$hubPn")
        import ev.*
        e match {
            case SequencedCoilAck(coilNum, hardAckNum, seqNum) =>
                debug(s"sequenced coil ${coilNum.convert} ack $hardAckNum as seq $seqNum")
        }
    }
