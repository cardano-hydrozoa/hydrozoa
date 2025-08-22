package hydrozoa.l2.consensus.network.mailbox

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
  * NOTE: You should NOT call Actor.create on this directly! Use the [[create]] method from the
  * companion object
  *   - Comment (Peter, 2025-08-22): Unsure if this should be private?
  */
private class InboxActor(
    val dbWriter: ActorRef[DBWriterActor],
    val transmitter: ActorRef[TransmitterActor]
)  extends Watchdog :
    val matchIndex: MatchIndex = ???

    // N.B.: see InMemoryDbMemoryActor
    @unused
    private val inboxes: mutable.Map[PeerId, mutable.Buffer[Msg]] = ???

    // TODO: in the constructor we need to set up a timer that will check [[breakSilenceTimeout]]
    //   for _every_ peer separately.

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

        // Persist incoming messages to DB
        batch.foreach(inMsg => dbWriter.tell(_.persistIncomingMessage(from, inMsg)): Unit)

//        // Persist messages in memory of Inbox Actor
//        batch.foreach(msg => this.inbox.append(msg))

        // Calculate updated match index
        val newMatchIndex: MatchIndex =
            batch.newMatchIndex match {
                // Received empty list of messages, return the current match index.
                case None        => this.matchIndex
                case Some(index) => index
            }

        transmitter.tell(_.confirmMatchIndex(from, newMatchIndex))

    /** Get the current match index. Equivalent to appendEntries(List.empty)
      *
      * TODO: remove?
      */
    final def getMatchIndex(from: PeerId): Unit = appendEntries(from, MsgBatch.empty)

    /** Called with `tell` every [[WatchdogTimeoutSeconds]] in scope, exceptions are not handled.
     */
    override def wakeUp(): Unit = ???

///**
// *
// */
//object ActorWatchdog:
//    /** Should spawn an actor that shoots off heartbeats with the match index at a periodic interval when no messages
//     * have been received.
//     *
//     * @param dbWriter
//     * @param transmitter
//     * @return
//     */
//    def create(peerId: PeerId, dbWriter: ActorRef[DBWriterActor], transmitter: ActorRef[TransmitterActor]): ActorRef[ActorWithWatchdog] =
//        ActorWithTimeout[ActorWithWatchdog, Unit](timeout = 5.second,
//            action = (inbox => inbox.transmitter.tell(_.confirmMatchIndex(peerId, inbox.matchIndex))))
//            .create(ActorWithWatchdog(peerId, dbWriter, transmitter))
//
