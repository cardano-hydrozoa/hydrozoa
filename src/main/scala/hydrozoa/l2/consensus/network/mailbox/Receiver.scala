package hydrozoa.l2.consensus.network.mailbox

import hydrozoa.l2.consensus.network.mailbox.{MsgBatch}
import ox.channels.ActorRef

/** Likely, not an actor but something else that physically receives messages from multiple remote
 * [[TransmitterActor]]s and passes them to the local [[InboxActor]] or [[OutboxActor]].
  */
abstract class Receiver(outboxActor: ActorRef[OutboxActor], inboxActor: ActorRef[InboxActor]):

    /** An incoming request from a peer telling us to add new messages to our inbox */
    final def handleAppendEntries(from: PeerId, batch: MsgBatch[Inbox]): Unit =
        inboxActor.tell(_.appendEntries(from, batch))

    /** An incoming request from a peer, indicating the highest message THEY have processed */
    final def handleConfirmMatchIndex(from: PeerId, matchIndex: MatchIndex[Outbox]): Unit =
        outboxActor.tell(_.confirmMatchIndex(from, matchIndex) : Unit)

final class LocalReceiver(outboxActor: ActorRef[OutboxActor], inboxActor: ActorRef[InboxActor])
    extends Receiver(outboxActor, inboxActor)
