package hydrozoa.l2.consensus.network.mailbox

/** Physically sends a batch of messages to a particular peer.
  *
  * Planned implementations:
  *   - OxChannelTransmitterActor / OxActorTransmitterActor (local)
  *   - SymmetricWSTransmitterActor (remote)
  */
trait TransmitterActor:

    /** Called by node's [[OutboxActor]] when it wants to send a [[batch]] for [[to]] peer.
      *
      * Implementations should execute the actual sending in the background and return immediately
      * to start working on the following actor's inbox messages.
      *
      * @param to
      *   the recipient node
      * @param batch
      *   the messages may be empty
      */
    def appendEntries(to: PeerId, batch: MsgBatch): Unit

    /** Called by node's [[InboxActor]] when it wants to confirm [[matchIndex]] for [[to]] peer.
      *
      * Implementations should execute the actual sending in the background and return immediately
      * to start working on the following actor's inbox messages.
      *
      * @param to
      *   the recipient node
      * @param matchIndex
      *   the current matchIndex for [[to]] peer in the local [[InboxActor]]
      */
    def confirmMatchIndex(to: PeerId, matchIndex: MatchIndex): Unit


/** 
 * Transmits messages locally (for testing) via ox actors
 * */
class OxActorTransmitterActor extends TransmitterActor:
    override def appendEntries(to: PeerId, batch: MsgBatch): Unit = ???

    override def confirmMatchIndex(to: PeerId, matchIndex: MatchIndex): Unit = ???
    
