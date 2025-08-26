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
      * @return
      *   Must return a Left-throwable if transmission is unsuccessful; otherwise unit.
      */
    def appendEntries(to: PeerId, batch: MsgBatch[Outbox]): Either[Throwable, Unit]

    /** Called by node's [[InboxActor]] when it wants to confirm [[matchIndex]] for [[to]] peer.
      *
      * Implementations should execute the actual sending in the background and return immediately
      * to start working on the following actor's inbox messages.
      *
      * @param to
      *   the recipient node
      * @param matchIndex
      *   the current matchIndex for [[to]] peer in the local [[InboxActor]]
      * @return
      *   Must return a Left-throwable if transmission is unsuccessful; otherwise unit.
      */
    def confirmMatchIndex(to: PeerId, matchIndex: MatchIndex[Inbox]): Either[Throwable, Unit]

/** Transmits messages locally (for testing) directly using ox actors
  */
final class LocalTransmitterActor(myself: PeerId) extends TransmitterActor:

    private val log = Logger(getClass)

    val peers: mutable.Map[PeerId, Receiver] = mutable.Map.empty

    /** Replicate messages in out outbox to the peer's inbox */
    override def appendEntries(
        to: PeerId,
        batch: MsgBatch[Outbox]
    ): Either[LocalTransmitterError, Unit] = {

        // The batch we send is OUR outbox, but must be received at the PEERS inbox
        val inBatch = MsgBatch
            .fromList[Inbox](
              batch.toList.map(msg => MailboxMsg[Inbox](MsgId[Inbox](msg.id.toLong), msg.content))
            )
            .get
        peers.get(to) match {
            case None => {
                log.error(
                  s"[${myself.asString}] transmission failed: appendEntries(${to}, ${batch})"
                )
                Left(LocalTransmitterError.PeerNotFound)
            }
            case Some(peer) => {
                peer.handleAppendEntries(myself, inBatch)
                log.debug(s"[${myself.asString}] appendEntries to: $to, batch: $batch")
                Right(())
            }
        }
    }

    /** Confirm to the peer the highest message id that WE have processed from the peer */
    override def confirmMatchIndex(
        to: PeerId,
        matchIndex: MatchIndex[Inbox]
    ): Either[LocalTransmitterError, Unit] =

        // The index we send is the highest index WE have processed for the remote peer. This is reflected in THEIR
        // outbox.
        val outIndex = MatchIndex[Outbox](matchIndex.toLong)
        peers.get(to) match {
            case None => {
                log.error(
                  s"[${myself.asString}] transmission failed for confirmMatchIndex(${to}, ${matchIndex})"
                )
                Left(LocalTransmitterError.PeerNotFound)
            }
            case Some(peer) => {
                peer.handleConfirmMatchIndex(myself, outIndex)
                log.debug(
                  s"[${myself.asString}] confirmMatchIndex to: $to, matchIndex: $matchIndex"
                )
                Right(())
            }
        }

    def connect(to: PeerId, receiver: Receiver): Unit = peers.put(to, receiver): Unit

    def disconnect(to: PeerId): Unit = peers.remove(to): Unit

enum LocalTransmitterError extends Throwable:
    /** Returned when the peer is not found in the peers map */
    case PeerNotFound
