package hydrozoa.l2.consensus.network.mailbox

import hydrozoa.l2.consensus.network.mailbox.MsgBatch
import ox.channels.ActorRef

/** Likely, not an actor but something else that physically receives messages from multiple
  * [[TransmitterActor]]s and passes them to the [[InboxActor]] or [[OutboxActor]].
  */
abstract class Receiver(outboxActor: ActorRef[OutboxActor], inboxActor: ActorRef[InboxActor]):

    final def handleAppendEntries(from: PeerId, batch: MsgBatch): Unit =
        inboxActor.tell(_.appendEntries(from, batch))

    final def handleConfirmMatchIndex(from: PeerId, matchIndex: MatchIndex): Unit =
        outboxActor.tell(_.confirmMatchIndex(from, matchIndex))