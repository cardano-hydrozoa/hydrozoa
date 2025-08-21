package hydrozoa.node.db

import hydrozoa.l2.consensus.network.outbox.OutMsgId
import hydrozoa.l2.consensus.network.transport.AnyMsg

trait DBActor {

    def persistOutgoingMessage(msg: AnyMsg): OutMsgId

    def getOutgoingMessages(startWithIncluding: OutMsgId): List[(OutMsgId, AnyMsg)]

}

/* Data model

table OUTBOX_BROADCAST_MESSAGE
 MSG_ID: Number
 PAYLOAD: Bytes

table PEER_INDICES
 PEER_ID: Varchar
 CONFIRMED_INDEX: Number (MsgId in Scala)
 */
