package hydrozoa.l2.consensus.network.outbox

import hydrozoa.node.db.DBWriterActor
import ox.channels.ActorRef

import scala.annotation.unused
import scala.concurrent.duration.DurationInt

/** An actor that runs on the node to keep a "receiving loop" that handles from all the remote
  * peers. This [[InboxActor]] is responsible for periodically "telling" the _remote_
  * [[OutboxActor]] to make them send a new batch of messages, either in response to successfully
  * processing previously received messages or according to a timeout.
  */
class InboxActor(
    @unused
    private val dbWriter: ActorRef[DBWriterActor],
    @unused
    private val transmitter: ActorRef[TransmitterActor]
):

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
    def appendEntries(from: PeerId, batch: List[OutMsg]): Unit = ???

    /** Get the current match index. Equivalent to appendEntries(List.empty)
      *
      * TODO: remove?
      */
    final def getMatchIndex(from: PeerId): Unit = appendEntries(from, List.empty)
