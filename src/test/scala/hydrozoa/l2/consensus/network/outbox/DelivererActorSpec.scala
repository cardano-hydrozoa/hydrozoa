package hydrozoa.l2.consensus.network.outbox

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.Piper
import hydrozoa.l2.consensus.network.ReqVerKey
import hydrozoa.l2.consensus.network.outbox.OutMsgId.toLong
import hydrozoa.l2.consensus.network.transport.AnyMsg.ReqVerKeyMsg
import hydrozoa.l2.consensus.network.transport.{AnyMsg, ReqAux}
import hydrozoa.node.TestPeer.{Alice, Bob, Carol}
import hydrozoa.node.db.DBActor
import munit.{FunSuite, ScalaCheckSuite}
import ox.channels.{Actor, ActorRef}
import ox.resilience.{RetryConfig, retry}
import ox.{Ox, sleep, supervised}

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.concurrent.duration.{Duration, DurationInt, MILLISECONDS}

/**
 * Helper function to poll a deliverer actor to determine when it's queue is empty.
 * Retries up to 10 times with a 200 ms delay
 * TODO: Maybe move away from polling?
 * */
def waitForEmptyQueue(deliverer: ActorRef[DelivererActor])(using ox: Ox): Unit = {
    retry(RetryConfig.delay(10, Duration(200, MILLISECONDS))) {
        if deliverer.ask(_.currentQueueSize == 0) then () else "Queue not empty"
    } : Unit
}

class DelivererActorSpec extends ScalaCheckSuite:
    /** Temporary function to ensure that some redundancy on IDs in AnyMsg's aux data
     * is consistent with the ID assigned by the outbox actor */
    def addToOutboxAndEnsureIdMatch(outbox: ActorRef[OutboxActor], msg: AnyMsg): OutMsgId = {
        val outboxId = outbox.ask(_.addToOutbox(msg))
        assertEquals(outboxId.toLong, msg.getFromSeq._2)
        outboxId
    }

    test("Deliverer initialization for the clean slate"):
        supervised {
            val db: ActorRef[DBActor] = Actor.create(InMemoryDBActor())
            val outbox = Actor.create(OutboxActor(db))
            val peerId = "tellMeTwice"
            val peer = TellMeNTimesReceiverFixture(2, peerId)
            val deliverer = DelivererActor.initialize(db, outbox, peer)

            assertEquals(peer.counter.get(), 2L)
            assertEquals(deliverer.ask(_.currentMatchIndex), MatchIndex(0L))
            assertEquals(outbox.ask(_.isSubscribed(peerId)), true)
        }

    test("Handle a message"):
        supervised {
            val db: ActorRef[DBActor] = Actor.create(InMemoryDBActor())
            val outbox = Actor.create(OutboxActor(db))
            val peerId = "tellMeTwice"
            val peer = TellMeNTimesReceiverFixture(2, peerId)
            val deliverer = DelivererActor.initialize(db, outbox, peer)

            assertEquals(peer.counter.get(), 2L)
            assertEquals(deliverer.ask(_.currentMatchIndex), MatchIndex(0L))
            assertEquals(outbox.ask(_.isSubscribed(peerId)), true)

            // handle anyMsg
            val anyMsg = ReqVerKeyMsg(ReqVerKey(), ReqAux(Alice, 1))
            val outMsgId = addToOutboxAndEnsureIdMatch(outbox, anyMsg)
            sleep(1.second)

            assertEquals(peer.counter.get(), 4L)
            assertEquals(deliverer.ask(_.currentMatchIndex), MatchIndex(outMsgId.toLong))
        }

    test("Test delayed delivery of multiple messages"):
        /* First create the db and enqueue multiple messages; then create a peer actor and ensure the peer catches up */
        supervised:
            val db: ActorRef[DBActor] = Actor.create(InMemoryDBActor())
            val outbox = Actor.create(OutboxActor(db))

            // Add 3 messages to the outbox
            val msg1 = ReqVerKeyMsg(ReqVerKey(), ReqAux(Alice, 1))
            val _ = addToOutboxAndEnsureIdMatch(outbox, msg1)
            val msg2 = ReqVerKeyMsg(ReqVerKey(), ReqAux(Bob, 2))
            val _ = addToOutboxAndEnsureIdMatch(outbox, msg2)
            val msg3 = ReqVerKeyMsg(ReqVerKey(), ReqAux(Carol, 3))
            val outMsgId3 = addToOutboxAndEnsureIdMatch(outbox, msg3)

            // Create a peer
            val peerId = "tellMeTwice"
            val n = 2
            val peer = TellMeNTimesReceiverFixture(n, peerId)
            assertEquals(peer.getMatchIndex, None)

            // Create a deliverer
            val deliverer = DelivererActor.initialize(db, outbox, peer)
            waitForEmptyQueue(deliverer)

            // This may need to be retried multiple times, because the peer simulates failure for
            // every second request.
            val peerMatchIndex = retry(RetryConfig.immediate(n)) {peer.getMatchIndex.get}

            assertEquals(peerMatchIndex, MatchIndex(outMsgId3.toLong))
            assertEquals(deliverer.ask(_.currentMatchIndex), MatchIndex(outMsgId3.toLong))

class InMemoryDBActor extends DBActor:

    private val log = Logger(getClass)

    private val outboxSeq = AtomicLong(0L)
    private val outbox = mutable.Buffer[AnyMsg]()

    override def persistOutgoingMessage(msg: AnyMsg): OutMsgId =
        outbox.append(msg)
        val outMsgId = outboxSeq.incrementAndGet() |> OutMsgId.apply
        log.info(s"persistOutgoingMessage: persisted $msg with outMsgId=$outMsgId")
        outMsgId

    override def getOutgoingMessages(startWithIncluding: OutMsgId): List[(OutMsgId, AnyMsg)] = {
        val msgIdLong = startWithIncluding.toLong
        val ret = outbox
            .clone()
            .toList
            .drop(msgIdLong.toInt - 1)
            .zipWithIndex
            .map { case (msg, i) =>
                (OutMsgId(i.toLong + msgIdLong), msg)
            }
        log.debug(
          s"getOutgoingMessages: found ${ret.size} entries starting with outMsgId=$startWithIncluding"
        )
        ret
    }

/**
 * A test fixture capable of receiving messages from an outbox. It maintains a counter and ignores all messages
 * except those that arrive modulo `n` (default 2, i.e. ignoring every other message).
 * NOTE: This will result in log.warn messages with failed RPC calls, since acknowledgements to messages wont be
 * received by the deliverer
 *
 * @param n
 * @param peerId
 */
class TellMeNTimesReceiverFixture(n: Int = 2, peerId: String) extends Receiver:

    private val log = Logger(getClass)

    val counter = AtomicLong(0L)

    private val entries = mutable.Buffer[(OutMsgId, AnyMsg)]()

    override def id: String = peerId

    override def appendEntries(newEntries: List[(OutMsgId, AnyMsg)]): Option[MatchIndex] =

        if counter.incrementAndGet() % n == 0 then
            log.info(s"appendEntries: appending $newEntries")

            entries.appendAll(newEntries)
            entries.lastOption match {
                case None              => Some(MatchIndex(0L))
                case Some((lastId, _)) => lastId.toLong |> MatchIndex.apply |> Some.apply
            }
        else
            log.info(s"appendEntries: ignoring $newEntries")
            None
