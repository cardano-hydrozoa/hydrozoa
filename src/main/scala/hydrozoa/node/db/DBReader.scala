package hydrozoa.node.db

import hydrozoa.l2.consensus.network.mailbox.{MailboxMsg, MsgBatch, MsgId, PeerId}

/** Reading from the database. Can be used from many threads simultaneously, hence is not required
  * to be an actor, every actor can create and use an instance locally.
  */
trait DBReader {

    /** Returns outgoing messages in range [firstMessage, maxLastMsgId].
      *
      * @param startWithIncluding
      * @return
      *   a list of messages and their IDs, in order of increasing message ID
      */
    def readOutgoingMessages(firstMessage: MsgId, maxLastMsgId: MsgId): MsgBatch

    def readIncomingMessages(peer: PeerId): Seq[MailboxMsg]

}
