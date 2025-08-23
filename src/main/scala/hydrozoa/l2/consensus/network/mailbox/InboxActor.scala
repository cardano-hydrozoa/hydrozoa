package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.consensus.network.Heartbeat
import hydrozoa.l2.consensus.network.mailbox.MsgBatch.MsgBatch
import hydrozoa.node.db.DBWriterActor
import ox.*
import ox.channels.ActorRef

import scala.annotation.unused
import scala.collection.mutable
import scala.concurrent.duration.DurationInt

/** An actor that runs on the node to keep a "receiving loop" that handles from all the remote
  * peers. This [[InboxActor]] is responsible for periodically "telling" the _remote_
  * [[OutboxActor]] to make them send a new batch of messages, either in response to successfully
  * processing previously received messages or according to a timeout.
  *
  *   - Comment (Peter, 2025-08-22): Unsure if this should be private?
  */
private class InboxActor(
    val dbWriter: ActorRef[DBWriterActor],
    val transmitter: ActorRef[TransmitterActor],
    val ownPeerId: PeerId,
    val others: Set[PeerId]
) extends Watchdog:

    private val log = Logger(getClass)

    // N.B.: see InMemoryDbMemoryActor

    private val inboxes: mutable.Map[PeerId, mutable.Buffer[Msg]] = mutable.Map.empty

    private val headPeers: mutable.Set[PeerId] = mutable.Set.empty
    private val pendingHeartbeats: mutable.Set[PeerId] = mutable.Set.empty

    /** It's more convenient to have a separate method so we can make connections first and then
      * start.
      *
      * The inbox starts with a `pendingHeartbeats` set that includes all peer IDs.
      */
    def start(): Unit =
        // Initialization code
        others.foreach(id =>
            inboxes.put(id, mutable.Buffer.empty): Unit
            headPeers.addAll(others)

            pendingHeartbeats.addAll(others)
        )
        wakeUp()

    /** In the case of not seeing any incoming messages from a peer, the [[InboxActor]] should
      * actively confirm its [[matchIndex]] to trigger the next batch. This value determines
      */
    @unused
    private val breakSilenceTimeout = 60.seconds

    /** @param from
      *   the sender
      * @param batch
      *   the batch, may be empty
      */
    def appendEntries(from: PeerId, batch: MsgBatch): Unit =

        log.debug(s"Got a batch from $from: $batch");

        // Persist incoming messages to DB
        batch.foreach(inMsg => dbWriter.tell(_.persistIncomingMessage(from, inMsg)): Unit)

        // Persist messages in memory of Inbox Actor
        batch.foreach(msg => this.inboxes(from).append(msg))

        // - When an inbox receives a heartbeat from a peer, it marks it
        // by removing that peer ID from `pendingHeartbeats`.
        batch.find(_ == Heartbeat()).foreach(_ => pendingHeartbeats.remove(from))

        // Calculate updated match index
        val newMatchIndex: MatchIndex =
            batch.newMatchIndex match {
                // Received empty list of messages, return the current match index.
                case None => inboxes.get(from).map(_.last.id.toMatchIndex).getOrElse(MatchIndex(0))
                case Some(index) => index
            }

        transmitter.tell(_.confirmMatchIndex(from, newMatchIndex))

    /** When an inbox receives a check-heartbeat (a watchdog signal, internally):
      *   - For each peer ID in pendingHeartbeats, it sends out its current match index with that
      *     peer to prompt the peer to send the next batch.
      *   - Then it resets pendingHeartbeats to include all peer IDs for the next watchdog cycle.
      */
    override def wakeUp(): Unit = {
        // TODO: separate types for receiver and sender
        pendingHeartbeats.foreach(peer =>
            // Should always exist
            val matchIndex =
                inboxes(peer).lastOption.map(_.id.toMatchIndex).getOrElse(MatchIndex(0))
            transmitter.tell(_.confirmMatchIndex(peer, matchIndex))
        )
        pendingHeartbeats.addAll(headPeers)
    }

