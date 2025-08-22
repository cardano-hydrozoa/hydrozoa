package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.Piper
import hydrozoa.l2.consensus.network.mailbox.MsgId.toLong
import hydrozoa.l2.consensus.network.{Ack, AckUnit, Req}
import hydrozoa.node.db.DBWriterActor
import munit.{FunSuite, ScalaCheckSuite}
import ox.channels.{Actor, ActorRef}
import ox.resilience.{RetryConfig, retry}
import ox.{Ox, sleep, supervised}

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.concurrent.duration.{Duration, DurationInt, MILLISECONDS}

///**
// * Helper function to poll a deliverer actor to determine when it's queue is empty.
// * Retries up to 10 times with a 200 ms delay
// * TODO: Maybe move away from polling?
// * */
//def waitForEmptyQueue(deliverer: ActorRef[DeliveryActor])(using ox: Ox): Unit = {
//    retry(RetryConfig.delay(10, Duration(200, MILLISECONDS))) {
//        if deliverer.ask(_.currentQueueSize == 0) then () else "Queue not empty"
//    } : Unit
//}

//class MailboxSpec extends ScalaCheckSuite:
//    test("Mailbox initialization (clean slate)"):
//        supervised {
//            val db: ActorRef[DBWriterActor] = Actor.create(InMemoryDBWriterActor())
//            val outbox = Actor.create(OutboxActor(db))
//            val peerId = "tellMeTwice"
//            val peer = TellMeNTimesInboxFixture(2, peerId)
//            val deliverer = DeliveryActor.initialize(db, outbox, peer)
//
//            assertEquals(peer.counter.get(), 2L)
//            assertEquals(deliverer.ask(_.currentMatchIndex), MatchIndex(0L))
//            assertEquals(outbox.ask(_.isSubscribed(peerId)), true)
//        }
////
//    test("Handle a message"):
//        supervised {
//            val db: ActorRef[DBWriterActor] = Actor.create(InMemoryDBWriterActor())
//            val outbox = Actor.create(OutboxActor(db))
//            val peerId = "tellMeTwice"
//            val peer = TellMeNTimesInboxFixture(2, peerId)
//            val deliverer = DeliveryActor.initialize(db, outbox, peer)
//
//            assertEquals(peer.counter.get(), 2L)
//            assertEquals(deliverer.ask(_.currentMatchIndex), MatchIndex(0L))
//            assertEquals(outbox.ask(_.isSubscribed(peerId)), true)
//
//            val outMsgId = outbox.ask(_.addToOutbox(AckUnit()))
//            sleep(1.second)
//
//            assertEquals(peer.counter.get(), 4L)
//            assertEquals(deliverer.ask(_.currentMatchIndex), MatchIndex(outMsgId.toLong))
//        }
//
//    test("Test delayed delivery of multiple messages"):
//        /* First create the db and enqueue multiple messages; then create a peer actor and ensure the peer catches up */
//        supervised:
//            val db: ActorRef[DBWriterActor] = Actor.create(InMemoryDBWriterActor())
//            val outbox = Actor.create(OutboxActor(db))
//
//            // Add 3 messages to the outbox
//            val _ = outbox.ask(_.addToOutbox(AckUnit()))
//            val _ = outbox.ask(_.addToOutbox(AckUnit()))
//            val outMsgId3 = outbox.ask(_.addToOutbox(AckUnit()))
//
//            // Create a peer
//            val peerId = "tellMeTwice"
//            val n = 2
//            val peer = TellMeNTimesInboxFixture(n, peerId)
//            assertEquals(peer.getMatchIndex, None)
//
//            // Create a deliverer
//            val deliverer = DeliveryActor.initialize(db, outbox, peer)
//            waitForEmptyQueue(deliverer)
//
//            // This may need to be retried multiple times, because the peer simulates failure for
//            // every second request.
//            val peerMatchIndex = retry(RetryConfig.immediate(n)) {peer.getMatchIndex.get}
//
//            assertEquals(peerMatchIndex, MatchIndex(outMsgId3.toLong))
//            assertEquals(deliverer.ask(_.currentMatchIndex), MatchIndex(outMsgId3.toLong))
//

class InMemoryDBWriterActor extends DBWriterActor:

    private val log = Logger(getClass)

    private val outboxSeq = AtomicLong(0L)
    private val outbox = mutable.Buffer[Req | Ack]()
    private val inboxes: mutable.Map[PeerId, mutable.Buffer[Msg]] = mutable.Map.empty

    override def persistOutgoingMessage(msg: Req | Ack): MsgId =
        outbox.append(msg)
        val outMsgId = outboxSeq.incrementAndGet() |> MsgId.apply
        log.info(s"persistOutgoingMessage: persisted $msg with outMsgId=$outMsgId")
        outMsgId

    override def getOutgoingMessages(startWithIncluding: MsgId): List[Msg] = {
        val msgIdLong = startWithIncluding.toLong
        val ret = outbox
            .clone()
            .toList
            .drop(msgIdLong.toInt - 1)
            .zipWithIndex
            .map { case (msg, i) =>
                Msg(MsgId(i.toLong + msgIdLong), msg)
            }
        log.debug(
            s"getOutgoingMessages: found ${ret.size} entries starting with outMsgId=$startWithIncluding"
        )
        ret
    }

    override def persistIncomingMessage(peerId: PeerId, msg: Msg): Unit = {
        val key = peerId
        val newVal = (msg)
        inboxes.updateWith(key) {
            case None => Some(mutable.Buffer(newVal))
            case Some(buffer) => Some(buffer.append(newVal))
        }: Unit
    }


/**
 * A test fixture capable of receiving messages from a (counterparty) outbox and responding. It maintains a counter and drops all incoming/outgoing messages
 * that arrive modulo `n` (default 5, i.e. ignoring every fifth message).
 * NOTE: This will result in log.warn messages with failed RPC calls.
 *
 * @param n
 * @param peerId
 */
class DropModNInboxFixture(n: Int = 5, peerId: PeerId, dbWriter: ActorRef[DBWriterActor], transmitterActor: ActorRef[OxActorTransmitterActor])(using Ox):

    // N.B.: InboxActor.create should create an ActorWithTimeout
    val inboxActor: ActorRef[InboxActor] = InboxActor.create(peerId, dbWriter, transmitterActor)
    val counter = AtomicLong(0L)
    private val log = Logger(getClass)

    override def id: String = peerId

    override def appendEntries(from: PeerId, batch: MsgBatch): Unit = {
        if counter.incrementAndGet() % n == 0
        then log.info(s"appendEntries: ignoring $batch")
        else    
            log.info(s"appendEntries: appending $batch")
            inboxActor.tell(_.appendEntries(from, batch))
    }
