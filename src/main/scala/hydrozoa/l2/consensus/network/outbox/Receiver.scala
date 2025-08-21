package hydrozoa.l2.consensus.network.outbox

/** */
trait Receiver {

    def id: String

    /** Note: This message MUST NOT throw exceptions; on failure (e.g., network failure) it should return None.
     * QUESTION: Should this return a Try instead of an Option?
     * @param entries
      *   a list of messages to deliver. May be empty.
      * @return
      *   receiver's reply with the largest message ID successfully received.
     *
      */
    def appendEntries(entries: List[OutMsg]): Option[MatchIndex]

    /**
     * Get the current match index. Equivalent to appendEntries(List.empty)
     * @return
     */
    final def getMatchIndex : Option[MatchIndex] = appendEntries(List.empty)
}
