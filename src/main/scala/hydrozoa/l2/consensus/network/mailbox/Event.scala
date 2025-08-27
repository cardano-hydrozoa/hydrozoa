package hydrozoa.l2.consensus.network.mailbox

/** Node inner events.
  */
enum Event:
    // Unused
    case EventUnit
    // Inbox
    case EventHeartbeat(from: PeerId, counter: Long)
    case EventInboxMessage(from: PeerId, msg: MailboxMsg[Inbox])
    // Outbox
    // ...
