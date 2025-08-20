package hydrozoa.l2.consensus.network.outbox

import hydrozoa.l2.consensus.network.transport.AnyMsg
import hydrozoa.node.state.WalletId

// TODO: wrappers/tags for outbox (and inbox for that matter)

/** Outbox actor for a HEAD. This is the node global outbox, an entry point for all node components
  * that may want to tell others about an event: a block, a deposit, a L2 tx/withdrawal, and so on.
  *
  * NB (remove): I think we better mark all traits/classes that are actors with a corresponding
  * suffix.
  *
  * Another view at this actor: a registry of `OutboxPeerActor`s.
  *
  * Logic:
  *   - Persist a message (not to lose them in case of a crash)
  *   - Respond to the caller with the outgoing message id
  *   - Determine the peers of the HEAD (maybe: all peers should have OutboxPeerActor up and
  *     running)
  *   - Fanout the message to OutboxPeerActor
  */
trait OutboxBroadcastActor:

    // This should be done in the constructor or somewhere else but before staring the actor.
    def run(): Unit = {
        // Now empty.
        ???
    }

    /** Implementation MUST: * persist the message * return the message outgoing number (primary
      * key?)
      * @param msg
      */
    def addToOutbox(msg: AnyMsg): OutMsgId

    /* Data model

    table OUTBOX_BROADCAST_MESSAGE
     MSG_ID: Number
     PAYLOAD: Bytes

    table PEER_INDICES
     PEER_ID: Varchar
     CONFIRMED_INDEX: Number (MsgId in Scala)
     * */

    // Acknowledged messages by peers (should be persisted)
    val peerIndices: Map[WalletId, OutMsgId] = ???

    /** Add newly joined peers to the head.
      * @param newPeers
      */
    def joinNodes(newPeers: List[WalletId]): Unit
