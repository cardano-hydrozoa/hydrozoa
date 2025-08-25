package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.Piper
import hydrozoa.l2.consensus.network.{Ack, AckUnit, Req, ReqVerKey}
import hydrozoa.node.db.DbReadOutgoingError.ValuesReadAreMalformed
import hydrozoa.node.db.{DBReader, DBWriterActor, DbReadOutgoingError}
import munit.ScalaCheckSuite
import ox.channels.{Actor, ActorRef, BufferCapacity}
import ox.resilience.{RetryConfig, retry}
import ox.{Ox, sleep, supervised}

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.concurrent.duration.DurationInt


val aliceId = PeerId("Alice")
val bobId = PeerId("Bob")
val carolId = PeerId("Carol")

/** Helper methods for swapping mailbox tags during testing. For instance, when Alice sends messages to Bob,
 * she sees it as a MsgBatch[Outbox], but he sees it as a MsgBatch[Inbox */
def swapMailbox[M1 <: Mailbox, M2 <: Mailbox](msgId: MsgId[M1]): MsgId[M2] = MsgId[M2](msgId.toLong)


/** The composite set of information needed to work with a node within this test suite.
 * Functions are exposed to work with a single peer's node, or a set of peers/nodes all at once.
 * Full initialization of a set of peers requires:
 *   - first making the node contexts
 *   - Setting up bi-directional interconnects between all peers
 *   - Starting the inbox
 *     Use [[TestNodeContext.initializePeers]] to for full initialization of a set of peers, or the individual methods
 *     of the companion object for testing various partial initializations or simulated failures.
 *     N.B.: I named this "TestNodeContext", but aside from `LocalTransmitterActor`, I think it might be the same as production node context (API included) */
case class TestNodeContext(peerId: PeerId, db: DBWriterActor & DBReader, receiver: Receiver, transmitterActor: ActorRef[LocalTransmitterActor], inbox: ActorRef[InboxActor], outbox: ActorRef[OutboxActor])

object TestNodeContext:


  /** Terminate bi-directional connections between a set of nodes */
  def disconnectNodes(nodes: Set[TestNodeContext]): Unit =
    nodes.foreach(thisNode => {
      val otherNodes = nodes - thisNode
      otherNodes.foreach(otherNode => disconnectNodeUnidirectional(thisNode, otherNode))
    })

  /** Terminate a unidirectional connection from sender to reciever. Note that the connection with roles reversed
   * may still exist */
  def disconnectNodeUnidirectional(sender: TestNodeContext, receiver: TestNodeContext): Unit =
    sender.transmitterActor.tell(_.disconnect(receiver.peerId))

  /** Start nodes for each peer, set up the interconnects, and start the inbox watchdogs.
   * Note that fully initializing may take some time, and this MAY be dependent on the number of peers.
   * This function blocks until initialization is complete.
   */
  def initializePeers[E, T](peers: Set[PeerId], retryConfig: RetryConfig[Throwable, Unit] = RetryConfig.backoff(10, 100.millis, 10.seconds))(using Ox): Map[PeerId, TestNodeContext] = {
    // Start each node
    val peerNodes: Map[PeerId, TestNodeContext] = mkNodeContexts(peers)
    val contexts = peerNodes.values.toSet

    // Connect Nodes
    connectNodes(contexts)

    // Start inboxes
    startInboxes(contexts)

    val totalPeers: Int = peerNodes.size

    retry(retryConfig) {
      peerNodes.foreach(peerNode =>
        if peerNode._2.transmitterActor.ask(_.peers.size) == totalPeers - 1 then (()) else throw RuntimeException("Peers not ready"))
    }: Unit

    peerNodes
  }

  def startInboxes(nodes: Set[TestNodeContext]): Unit =
    nodes.foreach(node => node.inbox.tell(_.start() : Unit))

  /** Start a set of nodes all at once. Inform them of each other, but do not connect them. */
  def mkNodeContexts(peers: Set[PeerId])(using Ox): Map[PeerId, TestNodeContext] =
    peers.foldLeft(Map.empty)((map, peer) => map.updated(peer, mkNodeContext(peer, peers - peer)))

  /** Initialize Database, Receiver, Transmitter, Inbox, and Outbox for peer a single node.
   * Use this when you want to test starting nodes at different times. To start a set of nodes all at once,
   * see [[mkNodeContexts]]. This method informs the peers of each other's existence, but does not open connections.
   * */
  def mkNodeContext(peerId: PeerId, others: Set[PeerId])(using
                                                         ox: Ox
  ): TestNodeContext =
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

    TestNodeContext(
      peerId,
      db,
      LocalReceiver(outbox, inbox),
      transmitter.asInstanceOf[ActorRef[LocalTransmitterActor]],
      inbox,
      outbox
    )

  def startInbox(node: TestNodeContext)(using Ox): Unit =
    node.inbox.tellDiscard(_.start())

  /** Open bi-directional connections among a set of nodes */
  def connectNodes(nodes: Set[TestNodeContext]): Unit =
    // Connect each peer
    nodes.foreach(thisNode => {
      val otherNodes = nodes - thisNode
      otherNodes.foreach(otherNode => connectNodeUnidirectional(thisNode, otherNode))
    })

  /** Begin a uni-directional connection between two nodes. If you want bi-directional connections between two or
   * more nodes, see [[connectNodes]] */
  def connectNodeUnidirectional(sender: TestNodeContext, receiver: TestNodeContext): Unit =
    sender.transmitterActor.tell(_.connect(receiver.peerId, receiver.receiver))




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

  test("Heartbeat mini-protocol works, inboxes are the same"):


    supervised {
      val peerNodes = TestNodeContext.initializePeers(Set(aliceId, bobId, carolId))

      val dbAlice = peerNodes(aliceId).db
      val dbBob = peerNodes(bobId).db
      val dbCarol = peerNodes(carolId).db

      sleep(10.seconds)

      assertEquals(dbBob.readIncomingMessages(aliceId).length, 3)
      assertEquals(dbAlice.readIncomingMessages(bobId).length, 3)
      assertEquals(dbAlice.readIncomingMessages(carolId).length, 3)

      assertEquals(dbBob.readIncomingMessages(aliceId), dbCarol.readIncomingMessages(aliceId))
      assertEquals(dbAlice.readIncomingMessages(bobId), dbCarol.readIncomingMessages(bobId))
      assertEquals(dbAlice.readIncomingMessages(carolId), dbBob.readIncomingMessages(carolId))

      // + implicitly by using [[OnlyWriteDbActor]] - no read from the db during execution
    }

  test("Handle a message"):
    supervised {
      val peers = Set(aliceId, bobId, carolId)
      val peerNodes = TestNodeContext.initializePeers(peers)

      val outMsg = Msg(peerNodes(aliceId).outbox.ask(_.addToOutbox(msg = AckUnit())), AckUnit)

      sleep(10.second)


      peerNodes(aliceId).db.readOutgoingMessages(outMsg.id, outMsg.id) match {
        case Left(e) => throw RuntimeException(s"Error reading message from sender's outbox db: ${e}")
        case Right(batch) => if batch.contains(Msg[Outbox](outMsg.id, AckUnit())) then
          () else throw RuntimeException(s"batch returned from db did not contain message `${outMsg}`. Batch was: ${batch}.")
      }


      // check if the message got into the receiver's databases
      (peerNodes - aliceId).foreach(receiverPeerNode => receiverPeerNode._2.db.readIncomingMessages(aliceId).contains(Msg(outMsg.id, AckUnit())))
    }

  test("Test delayed delivery of multiple messages"):
    /* First create the db and enqueue multiple messages; then create a peer actor and ensure the peer catches up */
    supervised:
      val peers = Set(aliceId, bobId)
      // Start two nodes. Do not connect them
      val peerNodes = TestNodeContext.mkNodeContexts(peers)
      val nodes = peerNodes.values.toSet

      // Add 10 messages to alice's outbox
      val aliceAckMsgIds = (0 until 10).map(_ => peerNodes(aliceId).outbox.ask(_.addToOutbox(AckUnit())))

      // add 20 messages to Bob's outbox
      val bobAckMsgIds = (0 until 20).map(_ => peerNodes(bobId).outbox.ask(_.addToOutbox(ReqVerKey())))

      // Open bi-directional connections between the nodes
      TestNodeContext.connectNodes(nodes)

      // Start inboxes on all nodes
      TestNodeContext.startInboxes(nodes)

      sleep(10.seconds)

      // Check that Alice's message IDs have gotten into Bob's database
      val recievedMessagesAliceToBob = peerNodes(bobId).db.readIncomingMessages(aliceId)
      aliceAckMsgIds.map(swapMailbox[Outbox, Inbox]).foreach(msgIdFromAlice => assert(recievedMessagesAliceToBob.map(_.id).contains(msgIdFromAlice), s"Expected to find MsgId ${msgIdFromAlice} from Alice in Bob's db, but did not."))

      // Check that Bob's messages have gotten into Alice's database
      val recievedMessagesBobToAlice = peerNodes(aliceId).db.readIncomingMessages(bobId)
      bobAckMsgIds.map(swapMailbox[Outbox, Inbox]).foreach(msgIdFromBob => assert(recievedMessagesBobToAlice.map(_.id).contains(msgIdFromBob), s"Expected to find MsgId ${msgIdFromBob} from Bob in Alice's db, but did not."))

      // Check that Alice's match index for bob is high enough.
      val minimalMatchIndexAliceForBob = aliceAckMsgIds.map(_.toLong).max
      assert(peerNodes(aliceId).outbox.ask(_.matchIndex(bobId)).toLong >= minimalMatchIndexAliceForBob)


  test("Test heartbeat for outbox"):
      supervised {
        val peers = Set(aliceId, bobId)
        val peerNodes = TestNodeContext.initializePeers(peers)

        // Verify initial state
        assert(peerNodes(aliceId).outbox.ask(_.peersAwaitingMessages).isEmpty, s"Immediately after initialization, no peers of Alice should be awaiting messages")
        assert(peerNodes(aliceId).outbox.ask(_.matchIndex(bobId)) == MatchIndex(0), s"Immediately after initialization, Alice should have match index 0 for Bob")
        assert(peerNodes(bobId).inbox.ask(_.pendingHeartbeats).contains(aliceId), "After initialization, Bob's `pendingHeartbeats` must contain Alice's peerId")

        // Terminate Alice's connection to Bob to avoid her outbox sending another heartbeat
        TestNodeContext.disconnectNodeUnidirectional(peerNodes(aliceId), peerNodes(bobId))

        sleep(10.second)
        assert(peerNodes(aliceId).outbox.ask(_.peersAwaitingMessages).contains(bobId)
          , "after 10 seconds, Bob's inbox should have noticed no new messages from Alice, and he should send a match index. This should result in Alice's outbox adding Bob to peerAwaitingMessages")
        assert(peerNodes(aliceId).outbox.ask(_.matchIndex(bobId)) == MatchIndex(0)
          , "after receiving the match index, the match index should be still be 0; i.e., a heartbeat (empty batch) from Alice should not result in Bob updating his match index.")


        // Reconnect Alice to Bob
        TestNodeContext.connectNodeUnidirectional(peerNodes(aliceId), peerNodes(bobId))
        sleep(3.seconds)
        assert(!peerNodes(bobId).inbox.ask(_.pendingHeartbeats).contains((aliceId))
          , "after 3 seconds, Alice's outbox heart beat should trigger sending an empty batch to Bob. This should result in Bob removing Alice from pendingHeartbeats")

        // Disconnect Alice from Bob again
        TestNodeContext.disconnectNodeUnidirectional(peerNodes(aliceId), peerNodes(bobId))
        sleep(10.seconds)
        assert(peerNodes(bobId).inbox.ask(_.pendingHeartbeats).contains(aliceId), "After 10 seconds since Bob's inbox last checked the pending heartbeats, it should notice that it has not received any messages from Alice." +
          "This should result in Alice being added back into pendingHeartbeats ")


      }

/** Use it for tests that MUST never READ from the database (though can write).
  */
class OnlyWriteDbActor(inMemoryDBActor: InMemoryDBActor) extends DBWriterActor, DBReader {
    lazy val noRead = throw RuntimeException("This test should not READ from the database!")

    override def persistOutgoingMessage(msg: Req | Ack): MsgId[Outbox] =
        inMemoryDBActor.persistOutgoingMessage(msg)

    override def readOutgoingMessages(firstMessage: MsgId[Outbox], maxLastMsgId: MsgId[Outbox]): Either[DbReadOutgoingError, MsgBatch[Outbox]] = noRead

    override def persistIncomingMessage(peerId: PeerId, msg: Msg[Inbox]): Unit =
        inMemoryDBActor.persistIncomingMessage(peerId, msg)

    override def readIncomingMessages(peer: PeerId): Seq[Msg[Inbox]] = noRead
}

// TODO: I think this can essentially just wrap the inbox and outbox actors with some transmitters/receivers that just
// swallow network requests
class InMemoryDBActor extends DBWriterActor, DBReader:

    private val log = Logger(getClass)

    private val outboxSeq = AtomicLong(0L)
    private val outbox = mutable.Buffer[Msg[Outbox]]()
    private val inboxes: mutable.Map[PeerId, mutable.Buffer[Msg[Inbox]]] = mutable.Map.empty

    override def persistOutgoingMessage(msg: Req | Ack): MsgId[Outbox] =
        val outMsgId = outboxSeq.incrementAndGet() |> MsgId.apply[Outbox]
        outbox.append(Msg[Outbox](outMsgId, msg))
        log.info(s"persistOutgoingMessage: persisted $msg with outMsgId=$outMsgId")
        outMsgId

    override def readOutgoingMessages(firstMessage: MsgId[Outbox], maxLastMsgId: MsgId[Outbox]): Either[DbReadOutgoingError, MsgBatch[Outbox]] =
        outbox.lastOption match {
            case _ if maxLastMsgId.toLong < firstMessage.toLong => Left(DbReadOutgoingError.MaxIdLessThanFirstID)
            case None => Left(DbReadOutgoingError.FirstMsgIdNotFound)
            case Some(msg) if firstMessage.toLong > outboxSeq.get() => Left(DbReadOutgoingError.FirstMsgIdNotFound)
            case Some(msg) => {
              val msgs = outbox.clone().slice(firstMessage.toLong.toInt - 1, maxLastMsgId.toLong.toInt)
                log.debug(
                    s"getOutgoingMessages: found ${msgs.size} entries starting with msgId=$firstMessage"
                )
                MsgBatch.fromList(msgs.toList) match {
                    case None => Left(ValuesReadAreMalformed)
                    case (Some(batch)) => Right(batch)
                }
            }
        }


    override def persistIncomingMessage(peerId: PeerId, msg: Msg[Inbox]): Unit = {
        val key = peerId
        val newVal = (msg)
        inboxes.updateWith(key) {
            case None         => Some(mutable.Buffer(newVal))
            case Some(buffer) => Some(buffer.append(newVal))
        }: Unit
    }

    override def readIncomingMessages(peer: PeerId): Seq[Msg[Inbox]] =
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

  def appendEntries(from: PeerId, batch: MsgBatch[Inbox]): Unit = {
    if counter.incrementAndGet() % n == 0
    then log.info(s"appendEntries: ignoring $batch")
    else
      log.info(s"appendEntries: appending $batch")
      inboxActor.tell(_.appendEntries(from, batch))
  }
