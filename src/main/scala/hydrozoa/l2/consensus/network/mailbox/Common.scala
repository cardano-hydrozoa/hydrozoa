package hydrozoa.l2.consensus.network.mailbox

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.l2.consensus.network.{ProtocolMsg, *, given}
import ox.channels.ActorRef
import sttp.tapir.Schema

// Actor message: What Ox does with _.tell, _.ask.
// Protocol Message: Broadcast messages from miniprotocol actors to their counterparts at other peers
// Mailbox Messages: messages sent from an outbox to inbox or vice versa
// Client messages: when a peer communicates with a node over HTTP

// TODO: find a better place?
opaque type PeerId = String

object PeerId:
    def apply(s: String): PeerId = s
    extension (self: PeerId) {
        def asString: String = self
    }

object Heartbeat:
    /** Heartbeats are psuedo-messages that can be put in the outbox only. They do not have sequence
      * numbers. When processed, they trigger the outbox to send an empty message batch to all peers
      * that are awaiting messages
      */
    opaque type Heartbeat = Unit
    def apply(): Heartbeat = ()

enum AnyMsg:
    case ReqVerKeyMsg(content: ReqVerKey)
    case AckVerKeyMsg(content: AckVerKey)
    case ReqInitMsg(content: ReqInit)
    case AckInitMsg(content: AckInit)
    case ReqRefundLaterMsg(content: ReqRefundLater)
    case AckRefundLaterMsg(content: AckRefundLater)
    case ReqEventL2Msg(content: ReqEventL2)
    case AckUnitMsg
    case ReqMinorMsg(content: ReqMinor)
    case AckMinorMsg(content: AckMinor)
    case ReqMajorMsg(content: ReqMajor)
    case AckMajorMsg(content: AckMajor)
    case AckMajor2Msg(content: AckMajor2)
    case ReqFinalMsg(content: ReqFinal)
    case AckFinalMsg(content: AckFinal)
    case AckFinal2Msg(content: AckFinal2)
    case ReqDeinitMsg(content: ReqDeinit)
    case AckDeinitMsg(content: AckDeinit)

object AnyMsg:
    def apply(msg: Req | Ack): AnyMsg = msg match
        case content: ReqVerKey      => ReqVerKeyMsg(content)
        case content: ReqInit        => ReqInitMsg(content)
        case content: ReqRefundLater => ReqRefundLaterMsg(content)
        case content: ReqEventL2     => ReqEventL2Msg(content)
        case content: ReqMinor       => ReqMinorMsg(content)
        case content: ReqMajor       => ReqMajorMsg(content)
        case content: ReqFinal       => ReqFinalMsg(content)
        case content: ReqDeinit      => ReqDeinitMsg(content)
        case content: AckVerKey      => AckVerKeyMsg(content)
        case content: AckInit        => AckInitMsg(content)
        case content: AckRefundLater => AckRefundLaterMsg(content)
        case _: AckUnit              => AckUnitMsg
        case content: AckMinor       => AckMinorMsg(content)
        case content: AckMajor       => AckMajorMsg(content)
        case content: AckMajor2      => AckMajor2Msg(content)
        case content: AckFinal       => AckFinalMsg(content)
        case content: AckFinal2      => AckFinal2Msg(content)
        case content: AckDeinit      => AckDeinitMsg(content)

given anyMsgCodec: JsonValueCodec[AnyMsg] =
    JsonCodecMaker.make

given anyMsgSchema: Schema[AnyMsg] =
    Schema.binary[AnyMsg]

/** Requests or acknowledgements tagged with a sequence ID
  */
case class MailboxMsg[M <: Mailbox](id: MsgId[M], content: ProtocolMsg)

given msgCodec[M <: Mailbox]: JsonValueCodec[MailboxMsg[M]] =
    JsonCodecMaker.make

given msgSchema[M <: Mailbox]: Schema[MailboxMsg[M]] =
    Schema.binary[MailboxMsg[M]]

object Batch:
    /** Opaque newtype around `List[Msg]`. Invariants:
      *   - The messages in the batch must be in sorted order of MsgId, strictly sequential (no
      *     gaps).
      */
    opaque type Batch[M <: Mailbox] = List[MailboxMsg[M]]

    given msgBatchCodec[M <: Mailbox]: JsonValueCodec[Batch[M]] =
        JsonCodecMaker.make

    given msgBatchSchema[M <: Mailbox]: Schema[Batch[M]] =
        Schema.binary[Batch[M]]

    given [M <: Mailbox]: Conversion[Batch[M], List[MailboxMsg[M]]] = identity

    def empty[M <: Mailbox]: Batch[M] = List.empty

    extension [M <: Mailbox](batch: Batch[M])
        /** Returns none on an empty batch, otherwise returns the highest MsgId of the batch
          * @return
          */
        def newMatchIndex: Option[MatchIndex[M]] = {
            // N.B.: we only know this works if we keep the invariant!
            batch.lastOption.map(last => MatchIndex(last.id.toLong))
        }

    /** Create a batch from a list of messages. Will return None if the list passed contains gaps or
      * is not in sorted order
      * @param list
      * @return
      */
    def fromList[M <: Mailbox](list: List[MailboxMsg[M]]): Option[Batch[M]] =
        // TODO add checks
        Some(list)
    def singleton[M <: Mailbox](msg: MailboxMsg[M]): Batch[M] = List(msg)

type Batch[M <: Mailbox] = Batch.Batch[M]

object MsgId:
    // Surrogate primary key for outgoing messages, starts with 1.
    opaque type MsgId[M <: Mailbox] = Long

    given msgIdCodec[M <: Mailbox]: JsonValueCodec[MsgId.MsgId[M]] = JsonCodecMaker.make

    given msgIdSchema[M <: Mailbox]: Schema[MsgId.MsgId[M]] = Schema.binary[MsgId.MsgId[M]]

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
  *   - "Inbox" match indicies refer to the highest message id from a remote peer that _we_ have
  *     processed
  *   - "Outbox" match indicies refer to the highest message id that we have received confirmation
  *     from a peer that _they_ have processed
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

// TODO: move away
extension [T](actor: ActorRef[T])
    /** Turn an `ask` that returns an Either into a throwing ask */
    def askThrow[A](f: T => Either[Throwable, A]): A =
        actor.ask(t =>
            f(t) match {
                case Left(e)    => throw RuntimeException(e)
                case Right(res) => res
            }
        )

// TODO: opaque types?
type BatchMsg[M <: Mailbox] = (PeerId, Batch[M])

given batchMsgCodec[M <: Mailbox]: JsonValueCodec[BatchMsg[M]] =
    JsonCodecMaker.make

given batchMsgSchema[M <: Mailbox]: Schema[BatchMsg[M]] =
    Schema.binary[BatchMsg[M]]

type MatchIndexMsg[M <: Mailbox] = (PeerId, MatchIndex[M])

given matchIndexMasgCodec[M <: Mailbox]: JsonValueCodec[MatchIndexMsg[M]] =
    JsonCodecMaker.make

given matchIndexMsgSchema[M <: Mailbox]: Schema[MatchIndexMsg[M]] =
    Schema.binary[MatchIndexMsg[M]]

enum BatchMsgOrMatchIndexMsg[M <: Mailbox]:
    case CaseBatchMsg(batchMsg: BatchMsg[M])
    case CaseMatchIndexMsg(matchIndexMsg: MatchIndexMsg[M])

given batchMsgOrMatchIndexMsgCodec[M <: Mailbox]: JsonValueCodec[BatchMsgOrMatchIndexMsg[M]] =
    JsonCodecMaker.make

given batchMsgOrMatchIndexMsgSchema[M <: Mailbox]: Schema[BatchMsgOrMatchIndexMsg[M]] =
    Schema.binary[BatchMsgOrMatchIndexMsg[M]]
