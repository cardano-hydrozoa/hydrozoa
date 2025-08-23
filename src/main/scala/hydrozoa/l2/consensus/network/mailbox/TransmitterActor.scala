package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger

import scala.collection.mutable

/** Physically sends a batch of messages to a particular peer.
  *
  * Planned implementations:
  *   - LocalTransmitterActor
  *   - SymmetricWSTransmitterActor (remote)
  */
trait TransmitterActor:

    /** Called by node's [[OutboxActor]] when it wants to send a [[batch]] for [[to]] peer.
      *
      * Implementations should execute the actual sending in the background and return immediately
      * to start working on the following actor's inbox messages.
      *
      * @param to
      *   the recipient node
      * @param batch
      *   the messages may be empty
      */
    def appendEntries(to: PeerId, batch: MsgBatch): Unit

    /** Called by node's [[InboxActor]] when it wants to confirm [[matchIndex]] for [[to]] peer.
      *
      * Implementations should execute the actual sending in the background and return immediately
      * to start working on the following actor's inbox messages.
      *
      * @param to
      *   the recipient node
      * @param matchIndex
      *   the current matchIndex for [[to]] peer in the local [[InboxActor]]
      */
    def confirmMatchIndex(to: PeerId, matchIndex: MatchIndex): Unit

/** Transmits messages locally (for testing) directly using ox actors
  */
final class LocalTransmitterActor(myself: PeerId) extends TransmitterActor:

    private val log = Logger(getClass)

    val peers: mutable.Map[PeerId, Receiver] = mutable.Map.empty

    override def appendEntries(to: PeerId, batch: MsgBatch): Unit = {
        log.debug(s"appendEntries to: $to, batch: $batch")
        peers(to).handleAppendEntries(myself, batch)
    }

    override def confirmMatchIndex(to: PeerId, matchIndex: MatchIndex): Unit =
        log.debug(s"confirmMatchIndex to: $to, matchIndex: $matchIndex")
        peers(to).handleConfirmMatchIndex(myself, matchIndex)

    def connect(to: PeerId, receiver: Receiver): Unit = peers.put(to, receiver): Unit

    def disconnect(to: PeerId): Unit = peers.remove(to): Unit
