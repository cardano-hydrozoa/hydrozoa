package hydrozoa.node.db

import hydrozoa.l2.consensus.network.mailbox.*

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
    def readOutgoingMessages(firstMessage: MsgId[Outbox], maxLastMsgId: MsgId[Outbox]): Either[DbReadOutgoingError, MsgBatch[Outbox]]

    // Why Seq[Msg] and not MsgBatch?
    def readIncomingMessages(peer: PeerId): Seq[MailboxMsg[Inbox]]

}

/**
 * Errors that can be returned by [[readOutgoingMessages]]
 */
enum DbReadOutgoingError:
    /** Returned when maxLastMsgId < firstMessage */
    case MaxIdLessThanFirstID
    /** Returned when firstMessageId is not found  in the database */
    case FirstMsgIdNotFound
    /** Returned when the messages read from the database don't obey the invariants of MsgBatch */
    case ValuesReadAreMalformed


enum DbReadIncomingError:
    /** Returned then the PeerId is not present in the database */
    case PeerIdNotFound

