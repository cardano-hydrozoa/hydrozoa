package hydrozoa.l2.consensus.network.outbox

import hydrozoa.l2.consensus.network.transport.AnyMsg

/** */
trait Receiver {

    def id: String

    /** @param entries
      *   a list of messages to deliverm may be empty
      * @return
      *   receiver's reply with the match index may be absent
      */
    def appendEntries(entries: List[(OutMsgId, AnyMsg)]): Option[MatchIndex]
}
