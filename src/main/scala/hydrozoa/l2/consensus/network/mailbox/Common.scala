package hydrozoa.l2.consensus.network.mailbox

import hydrozoa.l2.consensus.network.{Ack, Req}
import ox.channels.ActorRef

// TODO: find a better place?
opaque type PeerId = String

object PeerId:
    def apply(s: String): PeerId = s
    extension (self: PeerId) {
        def asString: String = self
    }

// Do we want this?
//opaque type OutMsg = Msg
//case class InMsg(msg : Msg, from: PeerId)

/** Requests or acknowledgements tagged with a sequence ID
  */
case class Msg[M <: Mailbox](id: MsgId[M], content: Req | Ack)

object MsgBatch:
    /** Opaque newtype around `List[Msg]`. Invariants:
      *   - The messages in the batch must be in sorted order of MsgId, strictly sequential (no
      *     gaps).
      */
    opaque type MsgBatch[M <: Mailbox] = List[Msg[M]]

    given [M <: Mailbox]: Conversion[MsgBatch[M], List[Msg[M]]] = identity

    def empty[M <: Mailbox]: MsgBatch[M] = List.empty

    extension [M <: Mailbox](batch: MsgBatch[M])
        /** Returns none on an empty batch, otherwise returns the highest MsgId of the batch
          * @return
          */
        def newMatchIndex: Option[MatchIndex[M]] = {
            // N.B.: we only know this works if we keep the invariant!
            batch.lastOption.map(_.id.toLong)
        }

    /** Create a batch from a list of messages. Will return None if the list passed contains gaps or
      * is not in sorted order
      * @param list
      * @return
      */
    def fromList[M <: Mailbox](list: List[Msg[M]]): Option[MsgBatch[M]] =
        // TODO add checks
        Some(list)

type MsgBatch[M <: Mailbox] = MsgBatch.MsgBatch[M]

object MsgId:
    // Surrogate primary key for outgoing messages, starts with 1.
    opaque type MsgId[M <: Mailbox] = Long

    def apply[M <: Mailbox](n: Long): MsgId[M] = {
        assert(n > 0, "MsgIds must be positive")
        n
    }

    extension [M <: Mailbox](self: MsgId[M]) {
        def nextMsgId: MsgId[M] = self.toLong + 1
        def toMatchIndex: MatchIndex[M] = self
        def toLong: Long = self
    }

type MsgId[M <: Mailbox] = MsgId.MsgId[M]


sealed trait Mailbox derives CanEqual

sealed trait Inbox extends Mailbox derives CanEqual

sealed trait Outbox extends Mailbox derives CanEqual

/** Matching index for a remote peer i.e., the position in the outbox that is confirmed by a
  * recipient. It is a non-negative Long under the hood. A MatchIndex of 0 indicates that the remote
  * peer has not told us about their match index yet.
 *
 * We maintain two types of match indicies:
 * - "Inbox" match indicies refer to the highest message id from a remote peer that _we_ have processed
 * - "Outbox" match indicies refer to the highest message id that we have received confirmation from a peer that _they_
 * have processed
  */
opaque type MatchIndex[Mailbox] = Long

object MatchIndex:
    def apply[M <: Mailbox](n: Long): MatchIndex[M] = {
        assert(n >= 0, "Match Indicies must be non-negative")
        n
    }

    extension [M <: Mailbox](self: MatchIndex[M]) {
        def nextMsgId: MsgId[M] = MsgId(self.toLong + 1)
        def toMsgId: Option[MsgId[M]] = if self.isZero then None else Some(MsgId(self.toLong))
        def toLong: Long = self
        def isZero: Boolean = self == 0
    }


extension [T](actor: ActorRef[T])
    def tellThrow[A](f: T => Either[Throwable, A]): Unit =
        actor.tell(t => f(t) match {
            case Left(e) => throw RuntimeException(e)
            case Right(_) => ()
        })