package hydrozoa.l2.consensus.network.outbox

/**
 * An actor that runs on the node to runs a "receiving loop" that processes responses from the remote peer.
 * This receiver is responsible for "telling" the _remote_ delivery actor to send a new batch of messages, either
 * in response to successfully processeing previously recieved messages or according to a timeout
 * */
trait ReceiverActor {

    def id: String

    val deliveryActorRef : ActorRef[DeliveryActor]

    /** Note: This message MUST NOT throw exceptions; on failure (e.g., network failure) it should return None.
     * QUESTION: Should this return a Try instead of an Option?
     * Should be used with Tell.
     * @param entries
      *   a list of messages to deliver. May be empty.
      * @return
      *   receiver's reply with the largest message ID successfully received.
     *
      */
    def appendEntries(entries: List[OutMsg]): Unit

    /**
     * Get the current match index. Equivalent to appendEntries(List.empty)
     * @return
     */
    final def getMatchIndex : Unit = appendEntries(List.empty)
}