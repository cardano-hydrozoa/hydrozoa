package hydrozoa.l2.consensus.network.mailbox

import cats.syntax.all.*
import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.mailbox.OutboxActorError.DatabaseReadError
import hydrozoa.l2.consensus.network.{Ack, Heartbeat, Req}
import hydrozoa.node.db.*
import ox.channels.ActorRef

import scala.collection.mutable

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
                 ) extends Watchdog[OutboxActorError]:

    // ---------------------------------------------  Volatile state
    // The common delivery queue
    // TODO: This queue SHOULD be a data structure that obeys the following properties:
    // - The entiries in the queue must be strictly monotonically increasing and in sequential order
    // - Efficient slicing
    // - Efficient append
    // - Efficient dropping (from memory) of a "head slice"
    // (If we can obey the invariant, we can have an API that returns `MsgBatch`s directly rather than returning
    // List[Msg] and needing to call MsgBatch.fromList)
    private val queue: mutable.Buffer[Msg[Outbox]] = mutable.Buffer.empty

    private val matchIndices: mutable.Map[PeerId, MatchIndex[Outbox]] = mutable.Map.empty

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

    /** Called by the [[Receiver]] when we receive a message from a remote peer indicating the highest message ID that
     * they have successfully processed. Will send a new message to the peer with the next batch.
     *
      * @param peer
      * @param matchIndex
      */
    def confirmMatchIndex(peer: PeerId, matchIndex: MatchIndex[Outbox]): Either[OutboxActorError, Unit] = {

        log.info(s"Confirming match index from $peer: $matchIndex")

        // TODO: highestOwnOutMsgId may be the last element of the queue or the last known msg if from the database(?)
        // TODO: assert(matchIndex < highestOwnOutMsgId)

        // Store a possibly new value
        val _ = matchIndices.put(peer, matchIndex)

        for
            // Make the batch
            batch <- mkBatch(matchIndex)

            // Send the batch or add to the set of awaiting peers
            _ = if (batch.nonEmpty) {
                transmitter.tell(_.appendEntries(peer, batch))
            } else {
                peersAwaitingMessages.add(peer)
            }

            // Clean up the queue

            // Now the map has at least one value, so `minBy` won't throw.
            lowestIndex: MatchIndex[Outbox] = matchIndices.values.minBy(_.toLong)
        yield lowestIndex.toMsgId match {
            case Some(cleanUpTo) =>
                // Drop the prefix in the queue
                log.info(s"Cleaning up messages up to id=${cleanUpTo} (including) from the queue.")
                queue.dropWhileInPlace(msg => msg.id.toLong < lowestIndex.toLong)

            case None => ()
        }
    }

    /** Makes the next batch for sending, either from the database, from the queue, or from the
      * both.
      * @param matchIndex
      *   index to start with
      * @return
      *   possibly empty batch
      */
    private def mkBatch(matchIndex: MatchIndex[Outbox]): Either[OutboxActorError, MsgBatch[Outbox]] =

        // TODO: if matchIndex == highestOwnOutMsgId (queue.lastOption) return List.empty

        val firstMsgId = matchIndex.nextMsgId
        val maxLastMsgId = MsgId[Outbox](matchIndex.toLong + maxEntriesPerBatch)

        // First, we need to read from the database if needed
        queue.headOption match {
            // Empty Queue, read from the DB if needed
            case None =>
                // This prevents from a db trip if `matchIndex` is zero
                if !matchIndex.isZero
                then dbReader.readOutgoingMessages(firstMsgId, maxLastMsgId) match {
                    case Left(err) => Left(DatabaseReadError(err))
                    case Right(msgBatch) => Right(msgBatch)
                }
                else Right(MsgBatch.empty)

            // Non-empty queue; read from the DB if needed or pull directly from the queue
            case Some(queueHead) =>
                if queueHead.id.toLong > firstMsgId.toLong then {
                    // Reading up to the queue's head or till the end of the batch
                    val readUpTo = MsgId[Outbox](
                        math.min(queueHead.id.toLong - 1, maxLastMsgId.toLong)
                    )
                    dbReader.readOutgoingMessages(firstMsgId, readUpTo) match {
                        case Left(err) => Left(DatabaseReadError(err))
                        case Right(dbPart) =>
                            // Then, if there is some space left in the batch, we can try adding messages from the queue.
                            if dbPart.length < maxEntriesPerBatch then {
                                val firstMsgIdFromQueue =
                                    dbPart.lastOption.map(_.id.nextMsgId).getOrElse(firstMsgId)
                                val maxLastMsgIdFromQueue = MsgId[Outbox](
                                    firstMsgIdFromQueue.toLong + maxEntriesPerBatch - dbPart.length
                                )
                                readFromQueue(firstMsgIdFromQueue, maxLastMsgIdFromQueue) match {
                                    case Right(queuePart) => MsgBatch
                                        .fromList[Outbox](
                                            dbPart ++ queuePart
                                        ) match
                                        case Some(b) => Right(b)
                                        case None => Left(OutboxActorError.DbPartAndQueueNotCoherent)
                                    case Left(err) => Left(err)
                                }

                            } else {
                                Right(dbPart)
                            }
                    }
                }
                else {
                    // No need to read from the db
                    readFromQueue(firstMsgId, maxLastMsgId)

                }
                }


    // ---------------------------------------------  Private API

    /** Reads messages in range [firstMessage, maxLastMsgId] from the delivery queue.
      *
      * @param firstMessage
      * @param maxLastMsgId
      * @return
      *   possibly empty list of messages
      */
    // TODO: use MsgId + Int/Long
    private def readFromQueue(firstMessage: MsgId[Outbox], maxLastMsgId: MsgId[Outbox]): Either[OutboxActorError, MsgBatch[Outbox]] =
        val n = maxLastMsgId.toLong - firstMessage.toLong
        queue.headOption match {
            case None => Right(MsgBatch.empty)
            case Some(head) =>
                val startOffset = firstMessage.toLong - head.id.toLong
                val slice = queue.slice(startOffset.toInt, (startOffset + n).toInt)
                MsgBatch.fromList[Outbox](slice.toList) match
                    case None => Left(OutboxActorError.QueueMalformed)
                    case Some(batch) => Right(batch)
        }

    /** Unconditionally triggers a heartbeat message to be broadcasted to all peers.
     */
    override def wakeUp(): Either[OutboxActorError, Unit] = {
        if matchIndices.nonEmpty then
            val _ = addToOutbox(Heartbeat())
            // TODO: Initially this map is empty, so we need to wait till the first `matchIndex`
            //      pops up from a peer or add them proactively.
            // TODO: Improve error reporting to indicate _which_ peer(s) had an invalid batch.
            // TODO: This should be in a Validation traversable, but I cant get `cats` to work
            val results: List[Either[OutboxActorError, Unit]] = matchIndices.map((peer, index) =>
                mkBatch(index) match {
                    case Left(err) => Left(err)
                    case Right(batch) if batch.isEmpty => Left(OutboxActorError.WakeupBatchEmpty)
                    case Right(batch) => Right(transmitter.tell(_.appendEntries(peer, batch))
                    )
                }
            ).toList
            results.sequence match
                case Left(e) => Left(e)
                case Right(_) => Right(())
        else Right(())
    }

    /** Persist and enqueue an outgoing message for delivering.
      *
      * @param msg
      *   the outgoing message
      * @return
      *   the id of the persisted message
      */
    def addToOutbox(msg: Req | Ack): MsgId[Outbox] = {
        log.info(s"Adding to outbox: $msg")
        // 1. Persist a message (not to lose them in case of a crash)
        val msgId = dbWriter.ask(_.persistOutgoingMessage(msg))
        // 2. Append to the end of the delivery queue
        queue.append(Msg(msgId, msg))
        // TODO: remove? 3. Return the msg ID
        msgId
    }

enum OutboxActorError extends Throwable:
    case DatabaseReadError(e: DbReadOutgoingError)
    /** Returned during recovery if the dbPart and the queuePart, taken together, don't obey the invariants of a MsgBatch */
    case DbPartAndQueueNotCoherent
    /** Returned when the queue part, taken in isolation, doesn't obey the invariants of a MsgBatch */
    case QueueMalformed
    /** Returned when the wakeUp batch is empty (should contain at least a heartbeat message) */
    case WakeupBatchEmpty

