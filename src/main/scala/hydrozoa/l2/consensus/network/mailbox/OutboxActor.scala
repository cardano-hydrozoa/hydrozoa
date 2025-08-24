package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.{Ack, Heartbeat, Req}
import hydrozoa.node.db.{DBReader, DBWriterActor}
import ox.channels.ActorRef

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

// TODO: wrappers/tags for outbox (and inbox for that matter)

/** Outbox actor for a HEAD (now for the node since there might be only one head).
  *
  * This is the node global outbox, an entry point for all node components that may want to tell
  * others about an event: a block, a deposit, a L2 tx/withdrawal, and so on.
  *
  * RESPONSIBILITIES:
  *   - persisting and sequencing messages that a node produces
  *   - handling peers' matchIndex messages
  *   - maintaining the volatile delivery queue for _all_ peers
  *   - maintaining the volatile map of peers' matchIndex
  *   - sending batches to peers
  *   - pulling missing messages from the database when needed
  */
class OutboxActor(
    private val dbReader: DBReader,
    private val dbWriter: ActorRef[DBWriterActor],
    private val transmitter: ActorRef[TransmitterActor]
) extends Watchdog:

    // ---------------------------------------------  Volatile state
    // The common delivery queue
    private val queue: mutable.Buffer[MailboxMsg] = mutable.Buffer.empty

    private val matchIndices: mutable.Map[PeerId, MatchIndex] = mutable.Map.empty

    // TODO: When a peer is caught-up, add him to this set.
    //  When we receive a new outgoing message, we'll broadcast it to all peers in the set and then empty it.
    //  This avoids endless ping-ponging of addEntries/confirmMatchIndex with caught-up peers.
    val peersAwaitingMessages: mutable.Set[PeerId] = mutable.Set.empty

    // --------------------------- Some parameters

    /** Messages sent with the "AppendEntries RPC" are grouped into batches. This parameter limits
      * the number of messages that can be grouped into one batch.
      */
    private val maxEntriesPerBatch: Long = 10 // msgs

    // ---------------------------------------------  Misc
    private val log = Logger(getClass)

    // ---------------------------------------------  Public API

    /** Persist and enqueue an outgoing message for delivering.
      *
      * @param msg
      *   the outgoing message
      * @return
      *   the id of the persisted message
      */
    def addToOutbox(msg: Req | Ack): MsgId = {
        log.info(s"Adding to outbox: $msg")
        // 1. Persist a message (not to lose them in case of a crash)
        val msgId = dbWriter.ask(_.persistOutgoingMessage(msg))
        // 2. Append to the end of the delivery queue
        queue.append(MailboxMsg(msgId, msg))
        // TODO: remove? 3. Return the msg ID
        msgId
    }

    /** Brings a possibly new [[matchIndex]] from a [[peer]].
      *
      * @param peer
      * @param matchIndex
      */
    def confirmMatchIndex(peer: PeerId, matchIndex: MatchIndex): Unit = {

        log.info(s"Confirming match index from $peer: $matchIndex")

        // TODO: highestOwnOutMsgId may be the last element of the queue or the last known msg if from the database(?)
        // TODO: assert(matchIndex < highestOwnOutMsgId)

        // Store a possibly new value
        val _ = matchIndices.put(peer, matchIndex)

        // Make the batch
        val batch = mkBatch(matchIndex)

        // Send the batch or add to the set of awaiting peers
        if (batch.nonEmpty) {
            transmitter.tell(_.appendEntries(peer, batch))
        } else {
            val _ = peersAwaitingMessages.add(peer)
        }

        // Clean up the queue

        // Now the map has at least one value, so `minBy` won't throw.
        val lowestIndex: MatchIndex = matchIndices.values.minBy(_.toLong)
        lowestIndex.toMsgId match {
            case Some(cleanUpTo) =>
                // Drop the prefix in the queue
                log.info(s"Cleaning up messages up to id=$cleanUpTo (including) from the queue.")
                queue.dropWhileInPlace(msg => msg.id.toLong < lowestIndex.toLong)

            case None => ()
        }
    }

    // ---------------------------------------------  Private API

    /** Makes the next batch for sending, either from the database, from the queue, or from the
      * both.
      * @param matchIndex
      *   index to start with
      * @return
      *   possibly empty batch
      */
    private def mkBatch(matchIndex: MatchIndex): Batch =

        // TODO: if matchIndex == highestOwnOutMsgId (queue.lastOption) return List.empty

        val firstMsgId = matchIndex.nextMsgId
        val maxLastMsgId = MsgId(matchIndex.toLong + maxEntriesPerBatch)

        // First, we need to read from the database if needed
        queue.headOption match {
            case None =>
                // This prevents from a db trip if `matchIndex` is zero
                if !matchIndex.isZero then dbReader.readOutgoingMessages(firstMsgId, maxLastMsgId)
                else Batch.empty
            case Some(queueHead) =>
                if queueHead.id.toLong > firstMsgId.toLong then {
                    // Reading up to the queue's head or till the end of the batch
                    val readUpTo = MsgId(
                      math.min(queueHead.id.toLong - 1, maxLastMsgId.toLong)
                    )
                    val dbPart = dbReader.readOutgoingMessages(firstMsgId, readUpTo)

                    // Then, if there is some space left in the batch, we can try adding messages from the queue.
                    if dbPart.length < maxEntriesPerBatch then {
                        val firstMsgIdFromQueue =
                            dbPart.lastOption.map(_.id.nextMsgId).getOrElse(firstMsgId)
                        val maxLastMsgIdFromQueue = MsgId(
                          firstMsgIdFromQueue.toLong + maxEntriesPerBatch - dbPart.length
                        )
                        Try(
                          Batch
                              .fromList(
                                dbPart ++ readFromQueue(firstMsgIdFromQueue, maxLastMsgIdFromQueue)
                              )
                              .get
                        ) match
                            case Success(b) => b
                            // FIXME: Better exception type
                            case Failure(e) =>
                                throw RuntimeException(
                                  "Error constructing message batch: db part and queue part do not obey the required invariants."
                                )

                    } else { dbPart }
                } else {
                    // No need to read from the db
                    readFromQueue(firstMsgId, maxLastMsgId)
                }
        }

    /** Reads messages in range [firstMessage, maxLastMsgId] from the delivery queue.
      *
      * @param firstMessage
      * @param maxLastMsgId
      * @return
      *   possibly empty list of messages
      */
    // TODO: use MsgId + Int/Long
    private def readFromQueue(firstMessage: MsgId, maxLastMsgId: MsgId): Batch =
        val n = maxLastMsgId.toLong - firstMessage.toLong
        queue.headOption match {
            case None => Batch.empty
            case Some(head) =>
                val startOffset = firstMessage.toLong - head.id.toLong
                val slice = queue.slice(startOffset.toInt, (startOffset + n).toInt)
                // TODO: remove option
                Batch.fromList(slice.toList).get
        }

    /** Unconditionally triggers a heartbeat message to be broadcasted to all peers.
      */
    override def wakeUp(): Unit =
        if matchIndices.nonEmpty then
            val _ = addToOutbox(Heartbeat())
            // TODO: Initially this map is empty, so we need to wait till the first `matchIndex`
            //      pops up from a peer or add them proactively.
            matchIndices.foreach((peer, index) =>
                val batch = mkBatch(index)
                assert(batch.nonEmpty, "Batch in wakeUp should contain at least heartbeat message.")
                transmitter.tell(_.appendEntries(peer, batch))
            )
