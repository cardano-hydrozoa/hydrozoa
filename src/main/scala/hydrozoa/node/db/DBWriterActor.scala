package hydrozoa.node.db

import hydrozoa.l2.consensus.network.mailbox.*
import hydrozoa.l2.consensus.network.ProtocolMsg

/** Writing to the database is not concurrent, so we are wrapping it into an only actor.
  */
trait DBWriterActor {

    /** Save an outgoing message and return the message ID assigned to it.
      *
      * @param msg
      *   The message to save
      * @return
      *   The message ID (primary surrogate key) assigned to it
      */
    def persistOutgoingMessage(msg: ProtocolMsg): MsgId[Outbox]

    // TODO: save the whole batch atomically in a db transaction
    /** Save an incoming message. Since we have a natural primary key [[InMsgId]], it doesn't return
      * anything.
      *
      * @param peer
      * @param msg
      * @return
      */
    def persistIncomingMessage(peer: PeerId, msg: MailboxMsg[Inbox]): Unit
}
