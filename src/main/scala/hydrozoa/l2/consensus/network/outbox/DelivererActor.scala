package hydrozoa.l2.consensus.network.outbox

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.LongCompare
import hydrozoa.infra.LongCompare.*
import hydrozoa.l2.consensus.network.transport.AnyMsg
import hydrozoa.node.db.DBActor
import ox.channels.{Actor, ActorRef}
import ox.resilience.{RetryConfig, retryEither}
import ox.{Ox, sleep}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

/** Delivery actor for a particular peer.
  *
  * RESPONSIBILITIES:
  *   - maintain volatile [[matchIndex]] for the corresponding receiver
  *   - pulls missing messages from the database into [[queue]] using [[recover]] method
  *   - subscribe to node's outbox actor once ready
  *   - handle messages from the Outbox actor with [[enqueue]] method (and [[recover]] if needed)
  *   - retry delivering to a particular peer till it succeeds
  *
  * TODO: update the following part
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
 *      sent immediately if there are less than [[immediateThreshold]] messages in the queue,
 *      otherwise `timeout \= perMsgDelay * (n - [[immediateThreshold]])` but not longer than
 *      `maxTimeout`.
  *
  *   - Upon getting a confirmation from the receiver, which may be ONLY `Ok <index>` update the
  *     index of the last delivered message in the local state and in the database, removes
  *     delivered messages from the queue.
  */
private class DelivererActor(
    private val db: ActorRef[DBActor],
    private val outbox: ActorRef[OutboxActor],
    private val remotePeer: Receiver
)(using ox: Ox):

    // Initialization code
    private val log = Logger(getClass)

    // Actor's volatile state

    // Get the receiver's matchIndex - the id of the highest message known to be received
    private var matchIndex: MatchIndex =
        retryEither(RetryConfig.backoffForever(1.second, 60.seconds)) {
            log.info("try to get receiver's matchIndex")
            remotePeer.appendEntries(List.empty).toRight(())
        }.toOption.get

    log.info(s"matchIndex for peer ${remotePeer.id} is $matchIndex")

    // Delivery queue
    private val queue: mutable.Buffer[(OutMsgId, AnyMsg)] = mutable.Buffer.empty

    // -- end of the volatile state section

    // Some parameters

    /** Messages sent with the AppendEntries RPC are grouped into batches. This parameter limits the
     * number of messages that can be grouped into one batch.
     */
    private val maxEntriesPerCall: Int = 10 // msgs

    // -- end of the volatile state section
    private val immediateThreshold: Int = 5 // msgs
    private val maxTimeout = 120.seconds
    private val perMsgDelay = 1.second

    // The self-actor needed to snooze if the RPC call fails
    private var myself: ActorRef[DelivererActor] = _

    // Internal methods
    private def recover(mbMsg: Option[(OutMsgId, AnyMsg)] = None): Unit = {
        (mbMsg, queue.lastOption.map(_._1)) match

            case (None, None) =>
                log.debug(s"recovering to empty queue with entries after matchIndex=$matchIndex")
                val nextMsgId = matchIndex.nextMsgId
                queue.appendAll(db.ask(_.getOutgoingMessages(nextMsgId)))

            case (None, Some(lastMsgInQueue)) =>
                log.debug(
                  s"recover: recover the non-empty queue with entries after the last element=$lastMsgInQueue"
                )
                val nextMsgId = lastMsgInQueue.nextMsgId
                queue.appendAll(db.ask(_.getOutgoingMessages(nextMsgId)))

            // Enqueuing `elem` to the non empty `queue`
            case (Some(elem @ (msgId, _)), Some(lastMsgInQueue)) =>
                val nextMsgId = lastMsgInQueue.nextMsgId
                (msgId.toLong ? nextMsgId.toLong) match
                    case LongCompare.LT =>
                        log.debug(
                          s"recover: the queue already longer than $msgId, recovery is not needed"
                        )
                        () //
                    case LongCompare.EQ =>
                        log.debug(s"recover: enqueuing $msgId, recovery is not needed")
                        queue.append(elem)
                    case LongCompare.GT =>
                        log.debug(
                          s"recover: missing some entries, recovering with entries after the last element=$lastMsgInQueue"
                        )
                        queue.appendAll(db.ask(_.getOutgoingMessages(nextMsgId)))
                        assert(queue.last._1.toLong >= msgId.toLong)

            // Enqueuing `elem` to the empty `queue`
            // Since we always recover before subscribing,
            // this case may happen only for msgId = 1.
            // So we are missing exactly one (and the only) message.
            case (Some(elem), None) =>
                assert(elem._1.toLong == 1L)
                log.debug(s"recover: adding the first outgoing message to the queue")
                queue.append(elem)

        tryDelivery()
    }

    /** Attempts to deliver a batches of messages from the queue (FIFO) according to the following
     * logic.
     *
     *   - The messages are taken from the queue and grouped into "batches". The size of a batch is
     *     limited by [[maxEntriesPerCall]].
     *   - There is timeout (`silencePeriod`) between batches.
     *   - If the size of the queue is less than the [[immediateThreshold]], then the
     *     silencePeriond is 0. Otherwise, the timeout is calculated based on the number of
     *     messages (see the implementation) and limited by the maxTimeout parameter.
     *    - The method waits for a response from the remote peer, which contains the messageId of the last message
     *      successfully recieved (known as the remote peer's `matchIndex`). The DelevererActor stores this in memory,
     *      but does not persist it; on recovery, it can be queried again. The delivered messages are removed from the
     *      queue.
     *    - Regardless of whether delivery is successful, the actor calls itself again to deliver more messages.
     *      This has the effect of delivering all messages in batches until no more messages remain.
     */
    private def tryDelivery(): Unit = {
        val (msgs, silencePeriod) =
            if queue.size < immediateThreshold then (queue.take(maxEntriesPerCall), 0.seconds)
            else
                val n = queue.size - immediateThreshold
                val timeout = perMsgDelay.length.toInt * n
                (queue.take(maxEntriesPerCall), math.min(timeout, maxTimeout.length.toInt).seconds)
        if msgs.nonEmpty then {
            log.info(
              s"tryDelivery: sending ${msgs.size} messages after silencePeriod=$silencePeriod"
            )
            sleep(silencePeriod)
            // RPC call
            remotePeer.appendEntries(msgs.toList) match {
                case Some(matchIndex) =>
                    log.info(s"tryDelivery: got response $matchIndex")
                    this.matchIndex = matchIndex
                    queue.dropWhileInPlace((msgId,_) => msgId.toLong <= matchIndex.toLong)
                    log.debug(s"queue after dropWhileInPlace: $queue")
                // TODO: update matchIndex and queue
                case None =>
                    log.warn(s"tryDelivery: RPC call failed, snoozing")
            }
            myself.tell(_.tryDelivery())
        } else log.info(s"tryDelivery: no messages to deliver")
    }

    // NOTE: Partial, will throw a runtime error if an actor for this peer is already subscribed to the outbox.
    private val subscribe: () => Unit = () =>
        log.info(s"Subscribing to outbox for peer ${remotePeer.id}")
        this.myself = Actor.create(this)
        outbox.tell(_.subscribe(myself, remotePeer.id)) match
            case Right(()) => ()
            case Left(e) => throw runtime

    /** [[enqueue]] is just a case of [[recover]].
      * @param msgId
      * @param msg
      */
    def enqueue(msgId: OutMsgId, msg: AnyMsg): Unit = recover(Some(msgId, msg))

    def currentMatchIndex: MatchIndex = matchIndex

object DelivererActor:
    def initialize(db: ActorRef[DBActor], outbox: ActorRef[OutboxActor], remotePeer: Receiver)(using
        ox: Ox
    ) =
        val d = DelivererActor(db, outbox, remotePeer)
        d.recover()
        d.subscribe()
        d
