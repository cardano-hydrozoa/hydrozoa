package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.Piper
import hydrozoa.l2.consensus.network.{Ack, Req}
import hydrozoa.node.db.{DBReader, DBWriterActor}
import munit.ScalaCheckSuite
import ox.channels.{Actor, ActorRef, BufferCapacity}
import ox.{Ox, sleep, supervised}
import sttp.client4.UriContext

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.concurrent.duration.DurationInt

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

class MailboxSpec extends ScalaCheckSuite:

    test("Heartbeat mini-protocol works with local transport"):

        def startNode(peerId: PeerId, others: Set[PeerId])(using
            ox: Ox
        ): (
            DBWriterActor & DBReader,
            LocalReceiver,
            ActorRef[LocalTransmitterActor],
            ActorRef[InboxActor]
        ) =
            val db = InMemoryDBActor()
            val dbWriteOnly = OnlyWriteDbActor(db)
            val dbActor: ActorRef[DBWriterActor] = Actor.create(dbWriteOnly)
            val transmitter: ActorRef[TransmitterActor] =
                Actor.create(LocalTransmitterActor(peerId))
            val outbox = ActorWatchdog.create(OutboxActor(dbWriteOnly, dbActor, transmitter))(using
              timeout = WatchdogTimeoutSeconds(3)
            )
            val inbox = ActorWatchdog.create(InboxActor(dbActor, transmitter, peerId, others))(using
              timeout = WatchdogTimeoutSeconds(10)
            )

            (
              db,
              LocalReceiver(outbox, inbox),
              transmitter.asInstanceOf[ActorRef[LocalTransmitterActor]],
              inbox
            )

        supervised {

            // Start nodes
            val aliceId = PeerId("Alice")
            val bobId = PeerId("Bob")
            val carolId = PeerId("Carol")

            val (dbAlice, receiverAlice, transmitterAlice, inboxAlice) =
                startNode(aliceId, Set(bobId, carolId))
            val (dbBob, receiverBob, transmitterBob, inboxBob) =
                startNode(bobId, Set(aliceId, carolId))
            val (dbCarol, receiverCarol, transmitterCarol, inboxCarol) =
                startNode(carolId, Set(aliceId, bobId))

            // Connect nodes to each other
            transmitterAlice.tell(_.connect(bobId, receiverBob))
            transmitterAlice.tell(_.connect(carolId, receiverCarol))

            transmitterBob.tell(_.connect(aliceId, receiverAlice))
            transmitterBob.tell(_.connect(carolId, receiverCarol))

            transmitterCarol.tell(_.connect(aliceId, receiverAlice))
            transmitterCarol.tell(_.connect(bobId, receiverBob))

            inboxAlice.tell(_.start())
            inboxBob.tell(_.start())
            inboxCarol.tell(_.start())

            sleep(10.seconds)

            assertEquals(dbBob.readIncomingMessages(aliceId).length, 3)
            assertEquals(dbAlice.readIncomingMessages(bobId).length, 3)
            assertEquals(dbAlice.readIncomingMessages(carolId).length, 3)

            assertEquals(dbBob.readIncomingMessages(aliceId), dbCarol.readIncomingMessages(aliceId))
            assertEquals(dbAlice.readIncomingMessages(bobId), dbCarol.readIncomingMessages(bobId))
            assertEquals(dbAlice.readIncomingMessages(carolId), dbBob.readIncomingMessages(carolId))

            // + implicitly by using [[OnlyWriteDbActor]] - no read from the db during execution
        }

    test("Heartbeat mini-protocol works using WS transport"):

        def startNode(peerId: PeerId, others: Set[PeerId], port: Int)(using
            ox: Ox
        ): (
            DBWriterActor & DBReader,
            WSReceiver,
            ActorRef[WSTransmitterActor],
            ActorRef[InboxActor]
        ) =
            val db = InMemoryDBActor()
            val dbWriteOnly = OnlyWriteDbActor(db)
            val dbActor: ActorRef[DBWriterActor] = Actor.create(dbWriteOnly)
            val transmitter: ActorRef[TransmitterActor] =
                Actor.create(WSTransmitterActor())
            val outbox = ActorWatchdog.create(OutboxActor(dbWriteOnly, dbActor, transmitter))(using
              timeout = WatchdogTimeoutSeconds(3)
            )
            val inbox = ActorWatchdog.create(InboxActor(dbActor, transmitter, peerId, others))(using
              timeout = WatchdogTimeoutSeconds(10)
            )

            (
              db,
              WSReceiver(outbox, inbox, port),
              transmitter.asInstanceOf[ActorRef[WSTransmitterActor]],
              inbox
            )

        supervised {

            // Start nodes
            val aliceId = PeerId("Alice")
            val bobId = PeerId("Bob")
            val carolId = PeerId("Carol")

            val (dbAlice, receiverAlice, transmitterAlice, inboxAlice) =
                startNode(aliceId, Set(bobId, carolId), 4937)
            val (dbBob, receiverBob, transmitterBob, inboxBob) =
                startNode(bobId, Set(aliceId, carolId), 4938)
            val (dbCarol, receiverCarol, transmitterCarol, inboxCarol) =
                startNode(carolId, Set(aliceId, bobId), 4939)

            val uriAlice = uri"ws://localhost:4937/ws"
            val uriBob = uri"ws://localhost:4938/ws"
            val uriCarol = uri"ws://localhost:4939/ws"

            // Connect nodes to each other
            transmitterAlice.tell(_.connect(bobId, uriBob))
            transmitterAlice.tell(_.connect(carolId, uriCarol))

            transmitterBob.tell(_.connect(aliceId, uriAlice))
            transmitterBob.tell(_.connect(carolId, uriCarol))

            transmitterCarol.tell(_.connect(aliceId, uriAlice))
            transmitterCarol.tell(_.connect(bobId, uriBob))

            sleep(2.seconds)

            inboxAlice.tell(_.start())
            inboxBob.tell(_.start())
            inboxCarol.tell(_.start())

            sleep(600.seconds)

            assertEquals(dbBob.readIncomingMessages(aliceId).length, 3)
            assertEquals(dbAlice.readIncomingMessages(bobId).length, 3)
            assertEquals(dbAlice.readIncomingMessages(carolId).length, 3)

            assertEquals(dbBob.readIncomingMessages(aliceId), dbCarol.readIncomingMessages(aliceId))
            assertEquals(dbAlice.readIncomingMessages(bobId), dbCarol.readIncomingMessages(bobId))
            assertEquals(dbAlice.readIncomingMessages(carolId), dbBob.readIncomingMessages(carolId))

            // + implicitly by using [[OnlyWriteDbActor]] - no read from the db during execution
        }

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

/** Use it for tests that MUST never READ from the database (though can write).
  */
class OnlyWriteDbActor(inMemoryDBActor: InMemoryDBActor) extends DBWriterActor, DBReader {
    lazy val noRead = throw RuntimeException("This test should not READ from the database!")

    override def persistOutgoingMessage(msg: Req | Ack): MsgId =
        inMemoryDBActor.persistOutgoingMessage(msg)

    override def readOutgoingMessages(firstMessage: MsgId, maxLastMsgId: MsgId): Batch = noRead

    override def persistIncomingMessage(peerId: PeerId, msg: MailboxMsg): Unit =
        inMemoryDBActor.persistIncomingMessage(peerId, msg)

    override def readIncomingMessages(peer: PeerId): Seq[MailboxMsg] = noRead
}

class InMemoryDBActor extends DBWriterActor, DBReader:

    private val log = Logger(getClass)

    private val outboxSeq = AtomicLong(0L)
    private val outbox = mutable.Buffer[Req | Ack]()
    private val inboxes: mutable.Map[PeerId, mutable.Buffer[MailboxMsg]] = mutable.Map.empty

    override def persistOutgoingMessage(msg: Req | Ack): MsgId =
        outbox.append(msg)
        val outMsgId = outboxSeq.incrementAndGet() |> MsgId.apply
        log.info(s"persistOutgoingMessage: persisted $msg with outMsgId=$outMsgId")
        outMsgId

    override def readOutgoingMessages(firstMessage: MsgId, maxLastMsgId: MsgId): Batch = ???
//    override def getOutgoingMessages(startWithIncluding: MsgId): List[Msg] = {
//        val msgIdLong = startWithIncluding.toLong
//        val ret = outbox
//            .clone()
//            .toList
//            .drop(msgIdLong.toInt - 1)
//            .zipWithIndex
//            .map { case (msg, i) =>
//                Msg(MsgId(i.toLong + msgIdLong), msg)
//            }
//        log.debug(
//          s"getOutgoingMessages: found ${ret.size} entries starting with outMsgId=$startWithIncluding"
//        )
//        ret
//    }

    override def persistIncomingMessage(peerId: PeerId, msg: MailboxMsg): Unit = {
        val key = peerId
        val newVal = (msg)
        inboxes.updateWith(key) {
            case None         => Some(mutable.Buffer(newVal))
            case Some(buffer) => Some(buffer.append(newVal))
        }: Unit
    }

    override def readIncomingMessages(peer: PeerId): Seq[MailboxMsg] =
        inboxes.get(peer).map(_.toSeq).getOrElse(Seq.empty)

/** A test fixture capable of receiving messages from a (counterparty) outbox and responding. It
  * maintains a counter and drops all incoming/outgoing messages that arrive modulo `n` (default 5,
  * i.e. ignoring every fifth message). NOTE: This will result in log.warn messages with failed RPC
  * calls.
  *
  * @param n
  * @param peerId
  */
class DropModNInboxFixture(
    n: Int = 5,
    dbWriter: ActorRef[DBWriterActor],
    transmitterActor: ActorRef[TransmitterActor]
)(using ox: Ox, sc: BufferCapacity):

    // N.B.: InboxActor.create should create an ActorWithTimeout
    val inboxActor: ActorRef[InboxActor] =
        ActorWatchdog.create(InboxActor(dbWriter, transmitterActor, ???, ???))(using
          ox,
          sc,
          WatchdogTimeoutSeconds.apply(5)
        )
    val counter = AtomicLong(0L)
    private val log = Logger(getClass)

//    override def id: String = peerId

    def appendEntries(from: PeerId, batch: Batch): Unit = {
        if counter.incrementAndGet() % n == 0
        then log.info(s"appendEntries: ignoring $batch")
        else
            log.info(s"appendEntries: appending $batch")
            inboxActor.tell(_.appendEntries(from, batch))
    }
