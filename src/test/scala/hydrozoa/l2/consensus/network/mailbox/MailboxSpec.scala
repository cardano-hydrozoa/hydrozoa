package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.Piper
import hydrozoa.l2.consensus.network.{AckUnit, ProtocolMsg, ReqVerKey}
import hydrozoa.node.db.DbReadOutgoingError.ValuesReadAreMalformed
import hydrozoa.node.db.{DBReader, DBWriterActor, DbReadOutgoingError}
import munit.ScalaCheckSuite
import ox.channels.{Actor, ActorRef, BufferCapacity}
import ox.logback.InheritableMDC
import ox.resilience.{RetryConfig, retry}
import ox.{Ox, sleep, supervised}

import java.net.URI
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.concurrent.duration.DurationInt

val aliceId = PeerId("Alice")
val bobId = PeerId("Bob")
val carolId = PeerId("Carol")

/** Helper methods for swapping mailbox tags during testing. For instance, when Alice sends messages
  * to Bob, she sees it as a MsgBatch[Outbox], but he sees it as a MsgBatch[Inbox
  */
def swapMailbox[M1 <: Mailbox, M2 <: Mailbox](msgId: MsgId[M1]): MsgId[M2] = MsgId[M2](msgId.toLong)

/** The composite set of information needed to work with a node within this test suite. Functions
  * are exposed to work with a single peer's node, or a set of peers/nodes all at once. Full
  * initialization of a set of peers requires:
  *   - first making the node contexts
  *   - Setting up bi-directional interconnects between all peers
  *   - Starting the inbox Use [[TestNodeContext.initializePeers]] to for full initialization of a
  *     set of peers, or the individual methods of the companion object for testing various partial
  *     initializations or simulated failures. N.B.: I named this "TestNodeContext", but aside from
  *     `LocalTransmitterActor`, I think it might be the same as production node context (API
  *     included, except for "networking" stuff)
  */
case class TestNodeContext(
    peerId: PeerId,
    config: TestNodeConfig,
    db: DBWriterActor & DBReader,
    receiver: Receiver,
    transmitterActor: ActorRef[LocalTransmitterActor],
    inbox: ActorRef[InboxActor],
    outbox: ActorRef[OutboxActor]
)

case class TestNodeConfig(outboxWatchdogSeconds: Int = 3, inboxWatchdogSeconds: Int = 10)

object TestNodeContext:

    val log = Logger(getClass)

    /** Terminate bi-directional connections between a set of nodes */
    def disconnectNodes(nodes: Set[TestNodeContext]): Unit =
        log.warn(s"Disconnecting nodes: $nodes")

        nodes.foreach(thisNode => {
            val otherNodes = nodes - thisNode
            otherNodes.foreach(otherNode => disconnectNodeUnidirectional(thisNode, otherNode))
        })

    /** Terminate a unidirectional connection from sender to receiver. Note that the connection with
      * roles reversed may still exist
      */
    def disconnectNodeUnidirectional(sender: TestNodeContext, receiver: TestNodeContext): Unit =
        log.warn(s"Disconnecting unidirectionally (${sender.peerId} -> ${receiver.peerId}))")
        sender.transmitterActor.tell(_.disconnect(receiver.peerId))

    /** Start nodes for each peer, set up the interconnects, and start the inbox watchdogs. Note
      * that fully initializing may take some time, and this MAY be dependent on the number of
      * peers. This function blocks until initialization is complete.
      * @param peerConfigs
      *   A set containing Peer IDs and the test node configurations
      * @param retryConfig
      *   A RetryConfig indicating the type of retry to poll if the peers are full initialized
      */
    def initializePeers[E, T](
        peerConfigs: Set[(PeerId, TestNodeConfig)],
        retryConfig: RetryConfig[Throwable, Unit] = RetryConfig.backoff(10, 100.millis, 10.seconds)
    )(using Ox): Map[PeerId, TestNodeContext] = {
        // Start each node
        val peerNodes: Map[PeerId, TestNodeContext] = mkNodeContexts(peerConfigs)
        val contexts = peerNodes.values.toSet

        // Connect Nodes
        connectNodes(contexts)

        // Start inboxes
        startInboxes(contexts)

        val totalPeers: Int = peerNodes.size

        retry(retryConfig) {
            peerNodes.foreach(peerNode =>
                if peerNode._2.transmitterActor.ask(_.peers.size) == totalPeers - 1 then (())
                else throw RuntimeException("Peers not ready")
            )
        }: Unit

        peerNodes
    }

    def startInboxes(nodes: Set[TestNodeContext]): Unit =
        nodes.foreach(node => node.inbox.tell(_.start(): Unit))

    /** Start a set of nodes all at once. Inform them of each other, but do not connect them. */
    def mkNodeContexts(
        peerConfigs: Set[(PeerId, TestNodeConfig)]
    )(using Ox): Map[PeerId, TestNodeContext] =
        peerConfigs.foldLeft(Map.empty)((map, peerConfig) =>
            map.updated(
              peerConfig._1,
              mkNodeContext(peerConfig._1, peerConfig._2, peerConfigs.map(_._1) - peerConfig._1)
            )
        )

    /** Initialize Database, Receiver, Transmitter, Inbox, and Outbox for peer a single node. Use
      * this when you want to test starting nodes at different times. To start a set of nodes all at
      * once, see [[mkNodeContexts]]. This method informs the peers of each other's existence, but
      * does not open connections.
      */
    def mkNodeContext(peerId: PeerId, testNodeConfig: TestNodeConfig, others: Set[PeerId])(using
        ox: Ox
    ): TestNodeContext =

        val db = InMemoryDBActor()
        val dbWriteOnly = OnlyWriteDbActor(db)
        val dbActor: ActorRef[DBWriterActor] = Actor.create(db)
        val transmitter: ActorRef[TransmitterActor] =
            Actor.create(LocalTransmitterActor(peerId))
        val outbox = ActorWatchdog.create(OutboxActor(dbWriteOnly, dbActor, transmitter, peerId))(
          using timeout = WatchdogTimeoutSeconds(testNodeConfig.outboxWatchdogSeconds)
        )
        val inbox = ActorWatchdog.create(InboxActor(dbActor, transmitter, peerId, others))(using
          timeout = WatchdogTimeoutSeconds(testNodeConfig.inboxWatchdogSeconds)
        )

        TestNodeContext(
          peerId,
          testNodeConfig,
          db,
          LocalReceiver(outbox, inbox),
          transmitter.asInstanceOf[ActorRef[LocalTransmitterActor]],
          inbox,
          outbox
        )

    def startInbox(node: TestNodeContext)(using Ox): Unit =
        node.inbox.tell(_.start())

    /** Open bi-directional connections among a set of nodes */
    def connectNodes(nodes: Set[TestNodeContext]): Unit =
        // Connect each peer
        nodes.foreach(thisNode => {
            val otherNodes = nodes - thisNode
            otherNodes.foreach(otherNode => connectNodeUnidirectional(thisNode, otherNode))
        })

    /** Begin a uni-directional connection between two nodes. If you want bi-directional connections
      * between two or more nodes, see [[connectNodes]]
      */
    def connectNodeUnidirectional(sender: TestNodeContext, receiver: TestNodeContext): Unit = {
        log.info(s"Connecting nodes unidirectionally: ${sender.peerId} -> ${receiver.peerId}")
        sender.transmitterActor.tell(_.connect(receiver.peerId, receiver.receiver))
    }

class MailboxSpec extends ScalaCheckSuite:

    override def beforeEach(context: BeforeEach): Unit =
        InheritableMDC.init

    test("Heartbeat mini-protocol works, inboxes are the same"):
        supervised {
            val config = TestNodeConfig()
            val peers = Set(aliceId, bobId, carolId)
            val peerNodes = TestNodeContext.initializePeers(peers.map((_, config)))

            sleep(config.inboxWatchdogSeconds.seconds)

            peers.foreach(thisPeer => {
                val otherPeers = peers - thisPeer
                otherPeers.foreach(otherPeer =>
                    assertEquals(peerNodes(thisPeer).inbox.ask(_.heartbeatCounters(otherPeer)), 3L)
                )
            })
        }

    test("Handle a message"):
        supervised {
            val peerConfigs = Set(aliceId, bobId, carolId).map((_, TestNodeConfig()))
            val peerNodes = TestNodeContext.initializePeers(peerConfigs)

            val outMsg =
                MailboxMsg(peerNodes(aliceId).outbox.ask(_.addToOutbox(msg = AckUnit())), AckUnit)

            sleep(10.second)

            peerNodes(aliceId).db.readOutgoingMessages(outMsg.id, outMsg.id) match {
                case Left(e) =>
                    throw RuntimeException(s"Error reading message from sender's outbox db: ${e}")
                case Right(batch) =>
                    if batch.contains(MailboxMsg[Outbox](outMsg.id, AckUnit())) then ()
                    else
                        throw RuntimeException(
                          s"batch returned from db did not contain message `${outMsg}`. Batch was: ${batch}."
                        )
            }

            // check if the message got into the receiver's databases
            (peerNodes - aliceId).foreach(receiverPeerNode =>
                receiverPeerNode._2.db
                    .readIncomingMessages(aliceId)
                    .contains(MailboxMsg(outMsg.id, AckUnit()))
            )
        }

    test("Test delayed delivery of multiple messages"):
        /* First create the db and enqueue multiple messages; then create a peer actor and ensure the peer catches up */
        supervised:
            val peers = Set(aliceId, bobId).map((_, TestNodeConfig()))
            // Start two nodes. Do not connect them
            val peerNodes = TestNodeContext.mkNodeContexts(peers)
            val nodes = peerNodes.values.toSet

            // Add 10 messages to alice's outbox
            val aliceAckMsgIds =
                (0 until 10).map(_ => peerNodes(aliceId).outbox.ask(_.addToOutbox(AckUnit())))

            // add 20 messages to Bob's outbox
            val bobAckMsgIds =
                (0 until 20).map(_ => peerNodes(bobId).outbox.ask(_.addToOutbox(ReqVerKey())))

            // Open bi-directional connections between the nodes
            TestNodeContext.connectNodes(nodes)

            // Start inboxes on all nodes
            TestNodeContext.startInboxes(nodes)

            sleep(10.seconds)

            // Check that Alice's message IDs have gotten into Bob's database
            val receivedMessagesAliceToBob = peerNodes(bobId).db.readIncomingMessages(aliceId)
            aliceAckMsgIds
                .map(swapMailbox[Outbox, Inbox])
                .foreach(msgIdFromAlice =>
                    assert(
                      receivedMessagesAliceToBob.map(_.id).contains(msgIdFromAlice),
                      s"Expected to find MsgId ${msgIdFromAlice} from Alice in Bob's db, but did not."
                    )
                )

            // Check that Bob's messages have gotten into Alice's database
            val receivedMessagesBobToAlice = peerNodes(aliceId).db.readIncomingMessages(bobId)
            bobAckMsgIds
                .map(swapMailbox[Outbox, Inbox])
                .foreach(msgIdFromBob =>
                    assert(
                      receivedMessagesBobToAlice.map(_.id).contains(msgIdFromBob),
                      s"Expected to find MsgId ${msgIdFromBob} from Bob in Alice's db, but did not."
                    )
                )

            // Check that Alice's match index for bob is high enough.
            val minimalMatchIndexAliceForBob = aliceAckMsgIds.map(_.toLong).max
            assert(
              peerNodes(aliceId).outbox
                  .ask(_.matchIndex(bobId))
                  .toLong >= minimalMatchIndexAliceForBob
            )

    test("Heartbeat when Inbox -> Outbox connection lost"):
        InheritableMDC.supervisedWhere("node" -> "Test harness") {

            supervised {
                val config = TestNodeConfig()
                // We add Carol into this test even though we don't reference her at all. Her presence should not affect the validity of the test.
                val peers = Set(aliceId, bobId).map((_, config))
                val peerNodes = TestNodeContext.initializePeers(peers)

                // Verify initial state
                assert(
                  peerNodes(aliceId).outbox.ask(_.matchIndex(bobId)) == MatchIndex(0),
                  s"Immediately after initialization, Alice should have match index 0 for Bob"
                )

                assert(
                  peerNodes(aliceId).outbox.ask(_.peersAwaitingMessages).contains(bobId),
                  s"Alice should have added bob to peersAwaitingMessages when Bob send her a matchIndex of 0 during the initialization of Bob's Node"
                )

                // Terminate connection from Bob to Alice to allow Alice to clear bob from Peers awaiting messages
                TestNodeContext.disconnectNodeUnidirectional(peerNodes(bobId), peerNodes(aliceId))

                sleep((config.outboxWatchdogSeconds + 1).seconds)

                assert(
                  !peerNodes(aliceId).outbox.ask(_.peersAwaitingMessages).contains(bobId),
                  s"Alice's peersAwaitingMessages should no longer contain Bob, because his inbox's heartbeat is blocked"
                )

                val bobsCountBeforeReconnect: Long =
                    peerNodes(bobId).inbox.ask(_.heartbeatCounters(aliceId))

                // Reconnect Bob to Alice and wait
                TestNodeContext.connectNodeUnidirectional(peerNodes(bobId), peerNodes(aliceId))

                // In the worst case (see the diagram for details):
                // - the rest of the first inbox timeout (10s) should end, and no matchIndex will be sent to Alice since Bob got the heartbeat=1
                // - then the next inbox timeout (10s)
                // - outbox timeout, since we don't send heartbeats immediately
                // - an additional second for good measure
                sleep((2 * config.inboxWatchdogSeconds + config.outboxWatchdogSeconds + 1).seconds)

                val bobsCountAfterReconnect =
                    peerNodes(bobId).inbox.ask(_.heartbeatCounters(aliceId))
                assert(
                  bobsCountAfterReconnect > bobsCountBeforeReconnect,
                  s"Bobs count after the reconnect(${bobsCountAfterReconnect}) should be greater than before (${bobsCountBeforeReconnect})"
                )
            }
        }

    test("Heartbeat mini-protocol works using WS transport"):

        def startNode(myself: PeerId, others: Set[PeerId], port: Int)(using
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
                WSTransmitterActor.create(myself)
            val outbox =
                ActorWatchdog.create(OutboxActor(dbWriteOnly, dbActor, transmitter, myself))(using
                  timeout = WatchdogTimeoutSeconds(3)
                )
            val inbox = ActorWatchdog.create(InboxActor(dbActor, transmitter, myself, others))(using
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

            val uriAlice = URI("ws://localhost:4937/ws")
            val uriBob = URI("ws://localhost:4938/ws")
            val uriCarol = URI("ws://localhost:4939/ws")

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

/** Use it for tests that MUST never READ from the database (though can write).
  */
class OnlyWriteDbActor(inMemoryDBActor: InMemoryDBActor) extends DBWriterActor, DBReader {
    lazy val noRead: Nothing = throw RuntimeException(
      "This test should not READ from the database!"
    )

    override def persistOutgoingMessage(msg: ProtocolMsg): MsgId[Outbox] =
        inMemoryDBActor.persistOutgoingMessage(msg)

    override def readOutgoingMessages(
        firstMessage: MsgId[Outbox],
        maxLastMsgId: MsgId[Outbox]
    ): Either[DbReadOutgoingError, Batch[Outbox]] = noRead

    override def persistIncomingMessage(peerId: PeerId, msg: MailboxMsg[Inbox]): Unit =
        inMemoryDBActor.persistIncomingMessage(peerId, msg)

    override def readIncomingMessages(peer: PeerId): Seq[MailboxMsg[Inbox]] = noRead
}

// TODO: I think this can essentially just wrap the inbox and outbox actors with some transmitters/receivers that just
// swallow network requests
class InMemoryDBActor extends DBWriterActor, DBReader:

    private val log = Logger(getClass)

    private val outboxSeq = AtomicLong(0L)
    private val outbox = mutable.Buffer[MailboxMsg[Outbox]]()
    private val inboxes: mutable.Map[PeerId, mutable.Buffer[MailboxMsg[Inbox]]] = mutable.Map.empty

    override def persistOutgoingMessage(msg: ProtocolMsg): MsgId[Outbox] =
        val outMsgId = outboxSeq.incrementAndGet() |> MsgId.apply[Outbox]
        outbox.append(MailboxMsg[Outbox](outMsgId, msg))
        log.info(s"persistOutgoingMessage: persisted $msg with outMsgId=$outMsgId")
        outMsgId

    override def readOutgoingMessages(
        firstMessage: MsgId[Outbox],
        maxLastMsgId: MsgId[Outbox]
    ): Either[DbReadOutgoingError, Batch[Outbox]] =
        outbox.lastOption match {
            case _ if maxLastMsgId.toLong < firstMessage.toLong =>
                Left(DbReadOutgoingError.MaxIdLessThanFirstID)
            case None => Left(DbReadOutgoingError.FirstMsgIdNotFound)
            case Some(msg) if firstMessage.toLong > outboxSeq.get() =>
                Left(DbReadOutgoingError.FirstMsgIdNotFound)
            case Some(msg) => {
                val msgs =
                    outbox.clone().slice(firstMessage.toLong.toInt - 1, maxLastMsgId.toLong.toInt)
                log.debug(
                  s"getOutgoingMessages: found ${msgs.size} entries starting with msgId=$firstMessage"
                )
                Batch.fromList(msgs.toList) match {
                    case None          => Left(ValuesReadAreMalformed)
                    case (Some(batch)) => Right(batch)
                }
            }
        }

    override def persistIncomingMessage(peerId: PeerId, msg: MailboxMsg[Inbox]): Unit = {
        val key = peerId
        val newVal = (msg)
        inboxes.updateWith(key) {
            case None         => Some(mutable.Buffer(newVal))
            case Some(buffer) => Some(buffer.append(newVal))
        }: Unit
    }

    override def readIncomingMessages(peer: PeerId): Seq[MailboxMsg[Inbox]] =
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

    def appendEntries(from: PeerId, batch: Batch[Inbox]): Unit = {
        if counter.incrementAndGet() % n == 0
        then log.info(s"appendEntries: ignoring $batch")
        else
            log.info(s"appendEntries: appending $batch")
            inboxActor.tell(_.appendEntries(from, batch))
    }
