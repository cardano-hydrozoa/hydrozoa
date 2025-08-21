package hydrozoa.l2.consensus.network.outbox

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.{Ack, Req}
import hydrozoa.node.db.DBActor
import ox.channels.ActorRef

import scala.collection.mutable

// TODO: wrappers/tags for outbox (and inbox for that matter)

/** Outbox actor for a HEAD (now for the node since there might be only one head).
  *
  * This is the node global outbox, an entry point for all node components that may want to tell
  * others about an event: a block, a deposit, a L2 tx/withdrawal, and so on.
  *
  * RESPONSIBILITIES:
  *   - sequencing and persisting the outbox messages
  *   - fanout new messages to subscribed deliverers
  */
class OutboxActor(db: ActorRef[DBActor]):

    private val log = Logger(getClass)

    // Initially empty set of subscribed deliverers.
    // Maps a subscribe ID to its actor ref.
    private val subscribers = mutable.Map[String, ActorRef[DeliveryActor]]()

    /** Persist and fanout a message.
      * @param msg
      *   the outgoing message
      * @return
      *   the id of the persisted message
      */
    def addToOutbox(msg: Req | Ack): OutMsgId = {
        log.info(s"Adding to outbox: $msg")
        // 1. Persist a message (not to lose them in case of a crash)
        val msgId = db.ask(_.persistOutgoingMessage(msg))
        // 2. Fanout to subscribers along with msgId
        subscribers.values.foreach(_.tell(_.enqueue(OutMsg(msgId, msg))))
        // n. Return msg id
        // TODO: do we need to? Don't return if we can skip it.
        msgId
    }

    /** Allows a delivery actor to subscribe to the global outbox actor. If a deliverer is already subscribed with the
     * same ID, we will overwrite it.
     *
     * @param deliverer
      *   an actor that wants to subscribe for future outgoing events
      */
    def subscribe(deliverer: ActorRef[DeliveryActor], id: String): Unit = {
        log.info(s"Subscribing deliverer: $id")
        if isSubscribed(id)
        then log.warn(s"Peer ${id} was already subscribed to the outbox. This most likely either indicates failure recovery or that two distinct peers were assigned the same ID." +
            s" Replacing actor ref ${subscribers(id)} with ${deliverer}")
        subscribers.put(id, deliverer): Unit
    }

    /** Checks whether a deliverer for a peer [[id]] is currently subscribed.
      * @param id
      *   peer identifier
      */
    def isSubscribed(id: String): Boolean =
        subscribers.contains(id)
