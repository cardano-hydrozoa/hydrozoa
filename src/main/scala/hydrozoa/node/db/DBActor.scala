package hydrozoa.node.db

import hydrozoa.l2.consensus.network.outbox.{OutMsg, OutMsgId}
import hydrozoa.l2.consensus.network.{Ack, Req}

trait DBActor {

    /**
     * Save a message to the database and return the message ID assigned to it
     *
     * @param msg The message to save
     * @return The message ID (primary surrogate key) assigned to it
     */
    def persistOutgoingMessage(msg: Req | Ack): OutMsgId

    /**
     * Returns all outgoing messages that were entered into the outbox after and including the given message ID
     *
     * @param startWithIncluding
     * @return a list of messages and their IDs, in order of increasing message ID
     */
    def getOutgoingMessages(startWithIncluding: OutMsgId): List[OutMsg]

}

/* Data model

table OUTBOX_BROADCAST_MESSAGE
 MSG_ID: Number
 PAYLOAD: Bytes

table PEER_INDICES
 PEER_ID: Varchar
 CONFIRMED_INDEX: Number (MsgId in Scala)
 */
