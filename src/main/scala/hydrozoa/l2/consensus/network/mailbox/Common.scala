package hydrozoa.l2.consensus.network.mailbox

import cats.effect.*
import cats.effect.concurrent.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import hydrozoa.l2.consensus.network.{*, given}
import ox.channels.ActorRef
import sttp.tapir.Schema

import scala.concurrent.ExecutionContext

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


private type Stream[A] = MVar2[IO, Item[A]]

private case class Item[A](value: A, stream: Stream[A])

/**
 * See: https://github.com/simonmar/parconc-examples/blob/master/chan2.hs
 * And page 135 of Simon Marlow's "Parallel and Concurrent Programming in Haskell"
 *
 * Note that I don't know if this implementation is exception safe; if an exception
 * arises at certain points (for instance, in the middle of writeChan or readChan),
 * we may deadlock.
 *
 * An exception-safe implementation in haskell can be found page 163 in Marlow.
 * https://github.com/simonmar/parconc-examples/blob/master/chan3.hs
 *
 * The underlying data structure of a broadcast channel can be thought of as a linked list (i.e., [[Stream]])
 * of [[Item]]s, where each Item[A] contains a value of type A and a reference to the next [[Item]].*
 *
 * An instance of BroadcastChannel[A] contains two "pointers" to positions in this stream: a "read" position
 * and a "write" position. The "write" is the same for all consumers, meaning that a write from any consumer
 * of the channel will appear on all duplicated channels. The Read position is individual, meaning that reading
 * from one duplicated instances does not remove the value from other duplicated instances.
 */
sealed abstract case class BroadcastChannel[A] private( private val readVar: MVar2[IO, Stream[A]], private val writeVar: MVar2[IO, Stream[A]]):
    // TODO: MVar2 exposes non-semantically-blocking tryRead and tryPut; we can thus expose tryReadChan and
    //  tryWriteChan
    //
    // TODO: I _think_ that if any consumer is holding a reference to an Item in the stream, all subsequent
    // items will be held in memory and not get GC'd.
    // My hypothesis is that once all consumers have moved passed an item, that item should be subject to garbage
    // collection.
    // Thus, if we hold a reference to a Broadcast channel, but never read from it, the entire collection will
    // always be held in memory.
    // We can expose a "write only" version that will permanently empty its readVar so that it will not hold any
    // references, and only the consumers of the channel decide which references are kept.


    /** Reading an item from the broadcast channel inspects the head of the channel and advances the read pointer
     * to the next item.
     *
     * FIXME: Not exception safe.
     * */
    def readChan: IO[A] = {
        for {
            stream <- readVar.take
            item <- stream.read
            _ <- readVar.put(item.stream)
        } yield item.value
    }

    /** Writing to a channel inspects the write pointer to obtain the current (empty) item, creates a new (empty)
     * item for the stream, and writes the given value and pointer to the new item to the current item.
     *
     * FIXME: Not exception safe.
     */
    def writeChan(value: A): IO[Unit] = {
        implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.Implicits.global)
        for {
            newHole: Stream[A] <- MVar[IO].empty
            oldHole <- writeVar.take
            _ <- oldHole.put(Item(value, newHole))
            _ <- writeVar.put(newHole)
        } yield ()
    }

    /** Duplicating an instance "B1" of BroadcastChannel returns a new instance "B2" such that:
     * - The readVar of B2 is a _new_ MVar that _points_ to the same place as the writeVar of B1.
     * - The writeVar of B1 and B2 are the same.
     *
     * This means that B2 will start _reading_ from the point that B1 is currently _writing_.
     *
     * TODO: an alternate implementation could be exposed that gives B2 a distinct readVar that points to
     * the same place as the readVar of B1; thus any messages (even ones in the past) that are visible to B1
     * are also visible to B2.
     * */
    def dupe: IO[BroadcastChannel[A]] = {
        implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.Implicits.global)
        for {
            hole <- writeVar.read
            newReadVar <- MVar[IO].of(hole)
        } yield (new BroadcastChannel(readVar = newReadVar, writeVar = writeVar) {})
    }

object BroadcastChannel:
    /** Create an empty broadcast channel */
    def apply[A](): IO[BroadcastChannel[A]] = {
        implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.Implicits.global)
        for {
            hole <- MVar[IO].empty[Item[A]]
            readVar <- MVar[IO].of(hole)
            writeVar <- MVar[IO].of(hole)

        } yield new BroadcastChannel[A](readVar, writeVar) {}
    }