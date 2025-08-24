package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import sttp.client4.ws.SyncWebSocket
import sttp.client4.ws.sync.asWebSocketAlways
import sttp.client4.{DefaultSyncBackend, basicRequest}
import sttp.model.Uri

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
    def appendEntries(to: PeerId, batch: Batch): Unit

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

    override def appendEntries(to: PeerId, batch: Batch): Unit = {
        log.debug(s"appendEntries to: $to, batch: $batch")
        peers(to).handleAppendEntries(myself, batch)
    }

    override def confirmMatchIndex(to: PeerId, matchIndex: MatchIndex): Unit =
        log.debug(s"confirmMatchIndex to: $to, matchIndex: $matchIndex")
        peers(to).handleConfirmMatchIndex(myself, matchIndex)

    def connect(to: PeerId, receiver: Receiver): Unit = peers.put(to, receiver): Unit

    // def disconnect(to: PeerId): Unit = peers.remove(to): Unit

class WSTransmitterActor() extends TransmitterActor:

    private val log = Logger(getClass)

    // One global backend for all connections
    val backend = DefaultSyncBackend() // AsyncHttpClientSyncBackend()

    private val sockets: mutable.Map[PeerId, SyncWebSocket] = mutable.Map.empty

    override def appendEntries(to: PeerId, batch: Batch): Unit =
        sockets.get(to) match {
            case Some(socket) =>
                val msg = batchMsgWSFCodec.encode(to, batch)
                socket.send(msg)
            case None =>
                log.error(s"Trying to send to $to, but no socket is connected.")
        }

    override def confirmMatchIndex(to: PeerId, matchIndex: MatchIndex): Unit = {
        log.info("confirmMatchIndex to: $to, matchIndex: $matchIndex")
        sockets.get(to) match {
            case Some(socket) =>
                val msg = matchIndexMsgWSFCodec.encode(to, matchIndex)
                log.info(s"Sending: $msg")
                socket.send(msg)
            case None =>
                log.error(s"Trying to send to $to, but no socket is connected.")
        }
    }

    def connect(to: PeerId, uri: Uri): Unit =

        log.info(s"connecting to peer $to at $uri")

        val response = basicRequest
            .get(uri)
            .response(asWebSocketAlways(ws => ws)) // keep the WebSocket
            .send(backend)

        // Why .body?
        val socket = response.body

        sockets.put(to, socket): Unit

// def disconnect(to: PeerId): Unit = peers.remove(to): Unit
