package hydrozoa.node.db

import hydrozoa.l2.consensus.network.outbox.{InMsgId, OutMsgId}
import hydrozoa.l2.consensus.network.{Ack, Req}

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
    def persistOutgoingMessage(msg: Req | Ack): OutMsgId

    /** Save an incoming message. Since we have a natural primary key [[InMsgId]], it doesn't return
      * anything.
      *
      * @param msgId
      * @param msg
      * @return
      */
    def persistIncomingMessage(msgId: InMsgId, msg: Req | Ack): Unit
}
