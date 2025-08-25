package hydrozoa.l2.consensus.network.mailbox

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.l2.consensus.network.{*, given}
import sttp.tapir.Schema

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
    case AckHearbeatMsg(content: Heartbeat)

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
        case heartbeat: Heartbeat    => AckHearbeatMsg(heartbeat)

given anyMsgCodec: JsonValueCodec[AnyMsg] =
    JsonCodecMaker.make

given anyMsgSchema: Schema[AnyMsg] =
    Schema.binary[AnyMsg]

/** Requests or acknowledgements tagged with a sequence ID
  */
case class MailboxMsg(id: MsgId, content: AnyMsg)

given msgCodec: JsonValueCodec[MailboxMsg] =
    JsonCodecMaker.make

given msgSchema: Schema[MailboxMsg] =
    Schema.binary[MailboxMsg]

object Batch:
    /** Opaque newtype around `List[Msg]`. Invariants:
      *   - The messages in the batch must be in sorted order of MsgId, strictly sequential (no
      *     gaps).
      */
    opaque type Batch = List[MailboxMsg]

    given msgBatchCodec: JsonValueCodec[Batch] =
        JsonCodecMaker.make

    given msgBatchSchema: Schema[Batch] =
        Schema.binary[Batch]

    given Conversion[Batch, List[MailboxMsg]] = identity

    def empty: Batch = List.empty

    extension (batch: Batch)
        /** Returns none on an empty batch, otherwise returns the highest MsgId of the batch
          * @return
          */
        def newMatchIndex: Option[MatchIndex] = {
            // N.B.: we only know this works if we keep the invariant!
            batch.lastOption.map(last => MatchIndex(last.id.toLong))
        }

    /** Create a batch from a list of messages. Will return None if the list passed contains gaps or
      * is not in sorted order
      * @param list
      * @return
      */
    def fromList(list: List[MailboxMsg]): Option[Batch] =
        // TODO add checks
        Some(list)

type Batch = Batch.Batch

object MsgId:
    // Surrogate primary key for outgoing messages, starts with 1.
    opaque type MsgId = Long

    given msgIdCodec: JsonValueCodec[MsgId.MsgId] = JsonCodecMaker.make

    given msgIdSchema: Schema[MsgId.MsgId] = Schema.binary[MsgId.MsgId]

    def apply(n: Long): MsgId = {
        assert(n > 0, "MsgIds must be positive")
        n
    }

    extension (self: MsgId) {
        def nextMsgId: MsgId = self.toLong + 1
        def toMatchIndex: MatchIndex = self
        def toLong: Long = self
    }

type MsgId = MsgId.MsgId

/** Matching index for a remote peer i.e., the position in the outbox that is confirmed by a
  * recipient. It is a non-negative Long under the hood. A MatchIndex of 0 indicates that the remote
  * peer has not told us about their match index yet.
  */
opaque type MatchIndex = Long

object MatchIndex:
    def apply(n: Long): MatchIndex = {
        assert(n >= 0, "Match Indicies must be non-negative")
        n
    }

    extension (self: MatchIndex) {
        def nextMsgId: MsgId = MsgId(self.toLong + 1)
        def toMsgId: Option[MsgId] = if self.isZero then None else Some(MsgId(self.toLong))
        def toLong: Long = self
        def isZero: Boolean = self == 0
    }

// TODO: opaque types?
type BatchMsg = (PeerId, Batch)

given batchMsgCodec: JsonValueCodec[BatchMsg] =
    JsonCodecMaker.make

given batchMsgSchema: Schema[BatchMsg] =
    Schema.binary[BatchMsg]

type MatchIndexMsg = (PeerId, MatchIndex)

given matchIndexMasgCodec: JsonValueCodec[MatchIndexMsg] =
    JsonCodecMaker.make

given matchIndexMsgSchema: Schema[MatchIndexMsg] =
    Schema.binary[MatchIndexMsg]

enum BatchMsgOrMatchIndexMsg:
    case CaseBatchMsg(batchMsg: BatchMsg)
    case CaseMatchIndexMsg(matchIndexMsg: MatchIndexMsg)

given batchMsgOrMatchIndexMsgCodec: JsonValueCodec[BatchMsgOrMatchIndexMsg] =
    JsonCodecMaker.make

given batchMsgOrMatchIndexMsgSchema: Schema[BatchMsgOrMatchIndexMsg] =
    Schema.binary[BatchMsgOrMatchIndexMsg]
