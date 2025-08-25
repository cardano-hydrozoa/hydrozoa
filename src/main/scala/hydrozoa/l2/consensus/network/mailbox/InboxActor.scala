package hydrozoa.l2.consensus.network.mailbox

import cats.syntax.all.*
import com.typesafe.scalalogging.Logger
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

    private val inboxes: mutable.Map[PeerId, mutable.Buffer[Msg[Inbox]]] = mutable.Map.empty

    // Match indexes per remote peer for the highest message of THEIRS that WE have successfully processed
    private val matchIndices: mutable.Map[PeerId, MatchIndex[Inbox]] = mutable.Map.from(this.others.map(peer => (peer, MatchIndex[Inbox](0))))

    private val headPeers: mutable.Set[PeerId] = mutable.Set.empty
    val pendingHeartbeats: mutable.Set[PeerId] = mutable.Set.from(others)

    /** It's more convenient to have a separate method so we can make connections first and then
     * start.
     *
     * The inbox starts with a `pendingHeartbeats` set that includes all peer IDs.
     */
    def start(): Either[InboxActorError, Unit] =
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

    /** When an inbox receives a check-heartbeat (a watchdog signal, internally):
      *   - For each peer ID in pendingHeartbeats, it sends out its current match index with that
      *     peer to prompt the peer to send the next batch.
      *   - Then it resets pendingHeartbeats to include all peer IDs for the next watchdog cycle.
      */
    override def wakeUp(): Either[InboxActorError, Unit] = {
        // TODO: separate types for receiver and sender
        pendingHeartbeats.map(peer =>
            matchIndices.get(peer) match {
                case None => Left(InboxActorError.MatchIndexForPeerNotFound)
                case Some(matchIndex) =>
                    // N.B.: Deliberately swallows errors! The inbox is fire-and-forget
                    Right(transmitter.tellDiscard(_.confirmMatchIndex(peer, matchIndex)))
            }
        ).toList.traverse(identity) match {
            case Left(err) => Left(err)
            case Right(_) => Right(pendingHeartbeats.addAll(headPeers))
        }
    }

    /** @param from
      *   the sender
      * @param batch
      *   the batch, may be empty
      */
    def appendEntries(from: PeerId, batch: MsgBatch[Inbox]): Unit =

        log.debug(s"Got a batch from $from: $batch");

        // Persist incoming messages to DB
        batch.foreach(inMsg => dbWriter.tell(_.persistIncomingMessage(from, inMsg)))

        // Persist messages in memory of Inbox Actor
        batch.foreach(msg => this.inboxes(from).append(msg))

        // - When an inbox receives a batch from a peer, it marks it
        // by removing that peer ID from `pendingHeartbeats`.
        pendingHeartbeats.remove(from) : Unit

        // Calculate updated match index
        val newMatchIndex: MatchIndex[Inbox] =
            batch.newMatchIndex match {
                // Received empty list of messages, return the current match index.
                case None => inboxes.get(from).map(inbox => if inbox.isEmpty then MatchIndex(0) else inbox.last.id.toMatchIndex).getOrElse(MatchIndex(0))
                case Some(index) => index
            }
        matchIndices.update(from, newMatchIndex)


        transmitter.tellDiscard(_.confirmMatchIndex(from, newMatchIndex))

    /** Send a new MatchIndex[Inbox] to a remove peer.
     *
     * @param peer
     * @param matchIndex
     */
    def confirmMatchIndex(peer: PeerId): Either[InboxActorError, Unit] = {
        this.matchIndices.get(peer) match {
            case None => Left(InboxActorError.MatchIndexForPeerNotFound)
            case Some(matchIndex) =>
                log.info(s"Confirming match index from $peer: $matchIndex")
                Right(transmitter.tellDiscard(_.confirmMatchIndex(peer, matchIndex)))
        }
    }


enum InboxActorError extends Throwable:
    case MatchIndexForPeerNotFound