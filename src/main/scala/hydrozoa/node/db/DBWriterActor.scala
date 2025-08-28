package hydrozoa.node.db

import hydrozoa.l2.consensus.network.mailbox.*
import hydrozoa.l2.consensus.network.ProtocolMsg
import hydrozoa.node.db.DbWriter.Event.{IncomingMessagePersisted, OutgoingMessagePersisted}

object DbWriter:
    enum Event:
      case OutgoingMessagePersisted(mailboxMsg: MailboxMsg[Outbox])
      case IncomingMessagePersisted(peer: PeerId, msg: MailboxMsg[Inbox])
    
    case class Config(outgoingMessagePersistedCallbacks: List[OutgoingMessagePersisted => Unit] = List.empty,
                      incomingMessagePersistedCallbacks: List[IncomingMessagePersisted => Unit] = List.empty)

/** Writing to the database is not concurrent, so we are wrapping it into an only actor.
  */
trait DBWriterActor(config: DbWriter.Config):
    val config_ : DbWriter.Config = config
    
    final def dispatchCallback(dbWriterEvent: DbWriter.Event) : Unit = dbWriterEvent match {
      case e : OutgoingMessagePersisted => config.outgoingMessagePersistedCallbacks.foreach(_(e))
      case e : IncomingMessagePersisted => config.incomingMessagePersistedCallbacks.foreach(_(e))
    }
    
    /** Save an outgoing message and return the message ID assigned to it.
      *
     *  Must issue outgoingMessageCallback on success
      * @param msg
      *   The message to save
      * @return
      *   The message ID (primary surrogate key) assigned to it
      */
    def persistOutgoingMessage(msg: ProtocolMsg): MsgId[Outbox] // TODO: change this to OutgoingMessagePersisted

    // TODO: save the whole batch atomically in a db transaction
    /** Save an incoming message. Since we have a natural primary key [[InMsgId]], it doesn't return
      * anything.
      *
     * Must issue incomingMessageCallback on success
      * @param peer
      * @param msg
      * @return
      */
    def persistIncomingMessage(peer: PeerId, msg: MailboxMsg[Inbox]): Unit // TODO: change this to IncomingMessagePersisted