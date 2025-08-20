package hydrozoa.l2.consensus.network.outbox

/** An outbox actor for a particular peer. Every (sender) node will run a dynamic set of such
  * actors.
  *
  * Spinup logic:
  *
  *   - Upon (re)start restore the value of `matchIndex` - the index of the highest message known to
  *     be replicated to the peer, initialized to 0.
  *
  *   - Read all outgoing messages from the database starting from `matchIndex`, adds them to the
  *     queue.
  *
  * Sending logic:
  *
  *   - Reads from the incoming channel, which connects to OutboxBroadcastActor.
  *
  *   - Maintains the queue of msg to send by trying to send AppendEntries (here we use Raft terms)
  *     RPC to the receiver node.
  *
  *   - One AppendEntries RPC may include up to `maxEntriesPerCall` messages.
  *
  *   - The size of queue defines the minimal timeout between calls, such that a msg(s) should be
  *     sent immediate if there are less than `immediateThreshold` in the queue, otherwise `timeout
  *     \= perMsgDelay * (n - immediateThreshold)` but not longer than `maxTimeout`.
  *
  *   - Upon getting a confirmation from the receiver, which may be ONLY `Ok <index>` update the
  *     index of the last delivered message in the local state and in the database, removes
  *     delivered messages from the queue.
  */
trait OutboxPeerActor:
    val maxEntriesPerCall: Int = 10 // msgs
    val immediateThreshold: Int = 5 // msgs
    val maxTimeout: Int = 120 // seconds
    val perMsgDelay = 1 // seconds
