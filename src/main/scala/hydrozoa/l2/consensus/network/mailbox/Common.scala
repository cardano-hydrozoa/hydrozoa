package hydrozoa.l2.consensus.network.mailbox

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.l2.consensus.network.{Msg, given}
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

/** Requests or acknowledgements tagged with a sequence ID
  */
case class MailboxMsg(id: MsgId, content: Msg)

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
