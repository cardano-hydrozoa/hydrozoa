package hydrozoa.l2.consensus.network.outbox

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.transport.AnyMsg
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
    private val subscribers = mutable.Map[String, ActorRef[DelivererActor]]()

    /** Persist and fanout a message.
      * @param msg
      *   the outgoing message
      * @return
      *   the id of the persisted message
      */
    def addToOutbox(msg: AnyMsg): OutMsgId = {
        log.info(s"Adding to outbox: $msg")
        // 1. Persist a message (not to lose them in case of a crash)
        val msgId = db.ask(_.persistOutgoingMessage(msg))
        // 2. Fanout to subscribers along with msgId
        subscribers.values.foreach(_.tell(_.enqueue(msgId, msg)))
        // n. Return msg id
        // TODO: do we need to? Don't return if we can skip it.
        msgId
    }

    /** Allows a delivery actor to subscribe.
      * @param deliverer
      *   an actor that wants to subscribe for future outgoing events
      */
    def subscribe(deliverer: ActorRef[DelivererActor], id: String): Unit = {
        log.info(s"Subscribing deliverer: $id")
        val _ = subscribers.put(id, deliverer)
    }

    /** Checks whether a deliverer for a peer [[id]] is currently subscribed.
      * @param id
      *   peer identifier
      */
    def isSubscribed(id: String): Boolean =
        subscribers.contains(id)
