package hydrozoa.l2.consensus.network.mailbox

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.Piper
import hydrozoa.infra.transitionary.{emptyContext, emptyState}
import hydrozoa.l2.consensus.network.actor.BlockActor
import hydrozoa.l2.consensus.network.mailbox.Event.{EventInboxMessage, EventUnit}
import hydrozoa.l2.consensus.network.{AckUnit, ProtocolMsg, ReqEventL2, ReqVerKey}
import hydrozoa.l2.ledger.generators.genL2EventGenesisFromPeer
import hydrozoa.l2.ledger.{HydrozoaL2Mutator, L2EventGenesis}
import hydrozoa.node.TestPeer.Alice
import hydrozoa.node.db.*
import hydrozoa.node.db.DbReadOutgoingError.ValuesReadAreMalformed
import hydrozoa.node.l2EventWithdrawalFromInputsAndPeer
import munit.ScalaCheckSuite
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Test as ScalaCheckTest
import ox.*
import ox.channels.{Actor, ActorRef, BufferCapacity, Channel}
import ox.logback.InheritableMDC
import ox.resilience.{RetryConfig, retry, retryEither}

import java.net.URI
import java.util.concurrent.atomic.AtomicLong
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.TimeoutException
import scala.concurrent.duration.{DurationInt, FiniteDuration}

val aliceId = PeerId("Alice")
val bobId = PeerId("Bob")
val carolId = PeerId("Carol")

/** Helper methods for swapping mailbox tags during testing. For instance, when Alice sends messages
  * to Bob, she sees it as a MsgBatch[Outbox], but he sees it as a MsgBatch[Inbox
  */
def swapMailbox[M1 <: Mailbox, M2 <: Mailbox](msgId: MsgId[M1]): MsgId[M2] = MsgId[M2](msgId.toLong)
def swapMailbox[M1 <: Mailbox, M2 <: Mailbox](mm: MailboxMsg[M1]): MailboxMsg[M2] =
    MailboxMsg[M2](swapMailbox(mm.id), mm.content)

/** Retry an action until a predicate is true
  *
  * @param retryConfig:
  *   The retry config, with a default exponential backoff
  * @param action:
  *   The action to retry. If the predicate passes on the result, the result is returned
  * @param predicate:
  *   The predicate to evaluate on the result of the action
  * @param onFailure:
  *   A function to turn a failing result into an error.
  *
  * Note that when used for polling within a property test:
  *   - If we give up too quickly, the test will be flaky due to race conditions
  *   - Polling may issue blocking calls to actor, meaning that polling too frequently will flood
  *     their mailbox and prevent them from processing the message you are waiting for.
  *   - Polling occurs in the hot-loop of the test. Since this is a property test, it will slow down
  *     _each_ test case run. A 1 second slow down for 60 test cases will increase running time by 1
  *     minute. *
  */
def retryEitherPredicate[E, A](
    retryConfig: RetryConfig[E, A] = RetryConfig.backoff[E, A](
      maxRetries = 100,
      initialDelay = 10.millis,
      maxDelay = 500.millis
    ),
    action: Unit => A,
    predicate: A => Boolean,
    onFailure: A => E
): Either[E, A] = {
    retryEither(retryConfig) {
        val res = action(())
        if predicate(res)
        then Right(res)
        else Left(onFailure(res))
    }
}

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
    outbox: ActorRef[OutboxActor],
    blockActor: ActorRef[BlockActor]
)

// NOTE: the real-world default for timeouts are currently 3 seconds and 10 seconds, but these can likely be reduced
// substantially without the tests failing due to races.
case class TestNodeConfig(
    outboxWatchdogTimeout: FiniteDuration = 3.seconds,
    inboxWatchdogTimeout: FiniteDuration = 10.seconds,
    callbackInboxMsg: Option[EventInboxMessage => Unit] = None,
    dbWriterConfig: DbWriter.Config = DbWriter.Config()
)

object TestNodeContext:

    private val log = Logger(getClass)

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

        val db = InMemoryDBActor(testNodeConfig.dbWriterConfig)
        val dbWriteOnly = OnlyWriteDbActor(db)
        val dbActor: ActorRef[DBWriterActor] = Actor.create(db)
        val transmitter: ActorRef[TransmitterActor] =
            Actor.create(LocalTransmitterActor(peerId))
        val outbox = ActorWatchdog.create(OutboxActor(dbWriteOnly, dbActor, transmitter, peerId))(
          using timeout = WatchdogTimeout(testNodeConfig.outboxWatchdogTimeout)
        )

        val blockActor = Actor.create(BlockActor())
        val inbox =
            ActorWatchdog.create(
              InboxActor(
                dbActor,
                transmitter,
                peerId,
                others,
                blockActor,
                callbackMessageReceived = testNodeConfig.callbackInboxMsg
              )
            )(using
              timeout = WatchdogTimeout(testNodeConfig.inboxWatchdogTimeout)
            )

        TestNodeContext(
          peerId,
          testNodeConfig,
          db,
          LocalReceiver(outbox, inbox),
          transmitter.asInstanceOf[ActorRef[LocalTransmitterActor]],
          inbox,
          outbox,
          blockActor
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
    override def scalaCheckTestParameters: ScalaCheckTest.Parameters = {
        /* NOTE: This is very low for sufficient test coverage, but we're keeping it low for now because
       most of these tests contain race conditions. We'll often need to poll things like databases to wait for
       messages to appear, which will slow down the tests
         */
        ScalaCheckTest.Parameters.default.withMinSuccessfulTests(10)
    }

    override def beforeEach(context: BeforeEach): Unit =
        InheritableMDC.init

    test("Heartbeat mini-protocol works 10 secs, all peers got 3 heartbeats"):
        supervised {
            val config = TestNodeConfig()
            val peers = Set(aliceId, bobId, carolId)
            val peerNodes = TestNodeContext.initializePeers(peers.map((_, config)))

            sleep(config.inboxWatchdogTimeout)

            peers.foreach(thisPeer => {
                val otherPeers = peers - thisPeer
                otherPeers.foreach(otherPeer =>
                    assertEquals(peerNodes(thisPeer).inbox.ask(_.heartbeatCounters(otherPeer)), 3L)
                )
            })
        }

    /** The simplest form of test callback: the first observed event of that type should be what we
      * expect to see.
      */
    object TestCallback:
        def apply[E <: Event](finiteDuration: FiniteDuration)(expectedEvent: E)(using
            ox: Ox
        ): E => Unit =
            val channel = Channel.rendezvous[E]
            forkUserDiscard {
                timeoutOption(finiteDuration) {
                    val event = channel.receive()
                    assertEquals(event, expectedEvent)
                }.getOrElse(
                  throw new TimeoutException(
                    s"timeout waiting for an expected event ${expectedEvent}"
                  )
                )
            }
            channel.send

        /** Establish an unlimited channel and a callback such that only filtered events appear on
          * it
          */
        def filteredChannel[E](filter: E => Boolean)(using Ox): (Channel[E], E => Unit) = {
            val channel = Channel.unlimited[E]
            val callback = (event: E) => if filter(event) then channel.send(event) else (())
            (channel, callback)
        }

        /** Wait on a channel until all specified events arrive, in any order. The arrival of an
          * unspecified event raises an exception, as does the arrival of a specified event multiple
          * times.
          */
        @tailrec
        def awaitOnlySingleExpected[E](
            channel: Channel[E],
            expectedEvents: Set[E],
            timeout: FiniteDuration
        ): Unit = {
            if expectedEvents.isEmpty
            then (())
            else {
                val event = timeoutOption(timeout) {
                    channel.receive()
                } match {
                    case None =>
                        throw new TimeoutException(s"Did not receive expected event before timeout")
                    case Some(e) => e
                }
                if expectedEvents.contains(event)
                then awaitOnlySingleExpected(channel, expectedEvents - event, timeout)
                else throw new RuntimeException(s"Unexpected event found: ${event}")
            }
        }

    test("test callback works (positive test)"):
        supervised:
            val callback = TestCallback.apply(2.seconds)((EventUnit))
            callback.apply(EventUnit)

    test("test callback works (negative test)"):
        intercept[TimeoutException]:
            supervised:
                TestCallback.apply(2.seconds)((EventUnit))

    test("Handle an AckUnit message from Alice to Bob and Carol"):

        val ackUnit: AckUnit = AckUnit()

        supervised {

            val aliceConfig = TestNodeConfig()

            val callback = TestCallback.apply[EventInboxMessage](1.seconds)(
              EventInboxMessage(aliceId, MailboxMsg(MsgId.apply(1L), ackUnit))
            )

            val othersConfig = TestNodeConfig(callbackInboxMsg = Some(callback))

            val peerConfigs =
                Set((aliceId, aliceConfig)) ++ Set(bobId, carolId).map((_, othersConfig))
            val peerNodes = TestNodeContext.initializePeers(peerConfigs)

            peerNodes(aliceId).outbox.tell(_.addToOutbox(msg = ackUnit): Unit)

            // TODO: would be better to test one thing in one test; this should be a separate set of tests for the database
//            peerNodes(aliceId).db.readOutgoingMessages(outMsg.id, outMsg.id) match {
//                case Left(e) =>
//                    throw RuntimeException(s"Error reading message from sender's outbox db: ${e}")
//                case Right(batch) =>
//                    if batch.contains(MailboxMsg[Outbox](outMsg.id, AckUnit())) then ()
//                    else
//                        throw RuntimeException(
//                          s"batch returned from db did not contain message `${outMsg}`. Batch was: ${batch}."
//                        )
//            }
        }

    test("Test Outbox delivers multiple delayed messages to inbox"):
        /* First create the db and enqueue multiple messages; then create a peer actor and ensure the peer catches up */
        supervised:
            // Contains events from Alice's DB writer when incoming messages are persisted
            val (aliceIncomingMessagesChannel, aliceIncomingMessagesCallback) =
                TestCallback.filteredChannel[DbWriter.Event](e =>
                    e.isInstanceOf[DbWriter.Event.IncomingMessagePersisted]
                )

            // Contains events from Bob's DB writer when incoming messages are persisted
            val (bobIncomingMessagesChannel, bobIncomingMessagesCallback) =
                TestCallback.filteredChannel[DbWriter.Event](e =>
                    e.isInstanceOf[DbWriter.Event.IncomingMessagePersisted]
                )

            val peerConfigs =
                Set(
                  (
                    aliceId,
                    TestNodeConfig(dbWriterConfig =
                        DbWriter.Config(incomingMessagePersistedCallbacks =
                            List(aliceIncomingMessagesCallback)
                        )
                    )
                  ),
                  (
                    bobId,
                    TestNodeConfig(dbWriterConfig =
                        DbWriter.Config(incomingMessagePersistedCallbacks =
                            List(bobIncomingMessagesCallback)
                        )
                    )
                  )
                )
            // Start two nodes. Do not connect them
            val peerNodes = TestNodeContext.mkNodeContexts(peerConfigs)
            val nodes = peerNodes.values.toSet

            // Add 10 messages to alice's outbox
            val aliceAckMsgs =
                (0 until 10).map(_ =>
                    (peerNodes(aliceId).outbox.ask(_.addToOutbox(AckUnit())), AckUnit())
                )

            // add 20 messages to Bob's outbox
            val bobAckMsgs =
                (0 until 20).map(_ =>
                    (peerNodes(bobId).outbox.ask(_.addToOutbox(ReqVerKey())), ReqVerKey())
                )

            // Open bi-directional connections between the nodes
            TestNodeContext.connectNodes(nodes)

            // Start inboxes on all nodes
            TestNodeContext.startInboxes(nodes)

            // Check that Alice's messages have gotten into Bob's database
            TestCallback.awaitOnlySingleExpected(
              channel = bobIncomingMessagesChannel,
              expectedEvents = aliceAckMsgs
                  .map(mm =>
                      DbWriter.Event.IncomingMessagePersisted(
                        aliceId,
                        MailboxMsg[Inbox](swapMailbox(mm._1), mm._2)
                      )
                  )
                  .toSet,
              timeout = 1.seconds
            )

            // TODO: The above await replaces the previous "sleep". We still need to check that we can read the messages from the DB
            // Check that Alice's message IDs have gotten into Bob's database
//            val receivedMessagesAliceToBob = peerNodes(bobId).db.readIncomingMessages(aliceId)
//            aliceAckMsgIds
//                .map(swapMailbox[Outbox, Inbox])
//                .foreach(msgIdFromAlice =>
//                    assert(
//                      receivedMessagesAliceToBob.map(_.id).contains(msgIdFromAlice),
//                      s"Expected to find MsgId ${msgIdFromAlice} from Alice in Bob's db, but did not."
//                    )
//                )

            // Check that Bob's messages have gotten into Alice's database
            TestCallback.awaitOnlySingleExpected(
              channel = aliceIncomingMessagesChannel,
              expectedEvents = bobAckMsgs
                  .map(mm =>
                      DbWriter.Event.IncomingMessagePersisted(
                        bobId,
                        MailboxMsg[Inbox](swapMailbox(mm._1), mm._2)
                      )
                  )
                  .toSet,
              timeout = 1.seconds
            )

            // TODO: same as above
//          val receivedMessagesBobToAlice = peerNodes(aliceId).db.readIncomingMessages(bobId)
//                 bobAckMsgIds
//                .map(swapMailbox[Outbox, Inbox])
//                .foreach(msgIdFromBob =>
//                    assert(
//                        receivedMessagesBobToAlice.map(_.id).contains(msgIdFromBob),
//                        s"Expected to find MsgId ${msgIdFromBob} from Bob in Alice's db, but did not."
//                    )
//                )

    //
    //            // Check that Alice's match index for bob is high enough.
    //            val minimalMatchIndexAliceForBob = aliceAckMsgIds.map(_.toLong).max
    //            assert(
    //              peerNodes(aliceId).outbox
    //                  .ask(_.matchIndex(bobId))
    //                  .toLong >= minimalMatchIndexAliceForBob
    //            )

    test("Heartbeat when Inbox -> Outbox connection lost"):
        InheritableMDC.supervisedWhere("node" -> "Test harness") {

            supervised {
                val config = TestNodeConfig()
                // We add Carol into this test even though we don't reference her at all. Her presence should not affect the validity of the test.
                val peers = Set(aliceId, bobId, carolId).map((_, config))
                val peerNodes = TestNodeContext.initializePeers(peers)

                // Verify initial state
                assert(
                  peerNodes(aliceId).outbox.ask(_.matchIndex(bobId)) == MatchIndex(0),
                  s"Immediately after initialization, Alice should have match index 0 for Bob"
                )

                assert(
                  peerNodes(aliceId).outbox.ask(_.peersAwaitingMessages).contains(bobId),
                  s"Alice should have added bob to peersAwaitingMessages when Bob sent her a matchIndex of 0 during the initialization of Bob's Node"
                )

                // Terminate connection from Bob to Alice to allow Alice to clear bob from Peers awaiting messages
                TestNodeContext.disconnectNodeUnidirectional(peerNodes(bobId), peerNodes(aliceId))

                sleep(config.outboxWatchdogTimeout + 1.seconds)

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
                sleep(config.inboxWatchdogTimeout * 2d + config.outboxWatchdogTimeout + 1.seconds)

                val bobsCountAfterReconnect =
                    peerNodes(bobId).inbox.ask(_.heartbeatCounters(aliceId))
                assert(
                  bobsCountAfterReconnect > bobsCountBeforeReconnect,
                  s"Bobs count after the reconnect(${bobsCountAfterReconnect}) should be greater than before (${bobsCountBeforeReconnect})"
                )
            }
        }

    test("Heartbeat mini-protocol works using WS transport".ignore):

        def startNode(myself: PeerId, others: Set[PeerId], port: Int)(using
            ox: Ox
        ): (
            DBWriterActor & DBReader,
            WSReceiver,
            ActorRef[WSTransmitterActor],
            ActorRef[InboxActor]
        ) =
            val db = InMemoryDBActor(???)
            val dbWriteOnly = OnlyWriteDbActor(db)
            val dbActor: ActorRef[DBWriterActor] = Actor.create(dbWriteOnly)
            val transmitter: ActorRef[TransmitterActor] =
                WSTransmitterActor.create(myself)
            val outbox =
                ActorWatchdog.create(OutboxActor(dbWriteOnly, dbActor, transmitter, myself))(using
                  timeout = WatchdogTimeout(3.seconds)
                )

            val blockActor = Actor.create(BlockActor())
            val inbox =
                ActorWatchdog.create(InboxActor(dbActor, transmitter, myself, others, blockActor))(
                  using timeout = WatchdogTimeout(10.seconds)
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

    property("Random Single Withdrawal Event")(
      forAll(genL2EventGenesisFromPeer(Alice)) { (event: L2EventGenesis) =>
          supervised:
              // General setup. Note that this is unique /per test/, so is not suitable if node initialization
              // takes a non-trivial amount of time (e.g., establishing websocket connections)
              val config = TestNodeConfig()
              val peers = Set(aliceId, bobId).map((_, config))
              val peerNodes = TestNodeContext.initializePeers(peers)

              val res =
                  for
                      // Generate initial state and withdrawal event
                      state <- HydrozoaL2Mutator(emptyContext, emptyState, event)
                      l2Event = l2EventWithdrawalFromInputsAndPeer(state.utxo.keySet, Alice)
                      req = ReqEventL2(l2Event)

                      // Verify test preconditions hold (initial match index == 0)
                      _ <-
                          if peerNodes(aliceId).outbox.ask(_.matchIndex(bobId)) == MatchIndex(0)
                          then Right(())
                          else Left("Alice's match index for Bob is not 0")

                      // Send l2 withdrawal to outbox
                      msgId = peerNodes(aliceId).outbox.ask(_.addToOutbox(req))

                      mailboxMsg = MailboxMsg[Outbox](msgId, req)

                      ///// Observe message
                      // In Alice's outgoing db. Note that there should not be a race condition here, because
                      // the persistence must happen before the call to (_.ask(_.addToOutbox(req)) completes
                      _ <- retryEitherPredicate(
                        action = (_ => peerNodes(aliceId).db.readOutgoingMessages(msgId, msgId)),
                        predicate = {
                            case Left(e)     => false
                            case Right(msgs) => msgs.contains(swapMailbox(mailboxMsg))
                        },
                        onFailure =
                            _ => s"Error reading Alice's database. msg id ${msgId} not found!"
                      )

                      // In Bob's DB. Note there is a race condition here, so we retry until successful.
                      _ <- retryEitherPredicate(
                        action = (_ => peerNodes(bobId).db.readIncomingMessages(aliceId)),
                        predicate = _.contains(swapMailbox(mailboxMsg)),
                        onFailure = (
                            dbMessages =>
                                s"Message was not found (persisted) in Bob's db! The Message was: \n ${mailboxMsg}"
                        )
                      )

                      // Check that Alice has successfully received Bob's updated match ID
                      expectedMatchIndex: MatchIndex[Outbox] = MatchIndex(msgId.toLong)
                      _ <- retryEitherPredicate(
                        action = (_ => peerNodes(aliceId).outbox.ask(_.matchIndex(bobId))),
                        predicate = (expectedMatchIndex == _),
                        onFailure = (
                            actualMatchIndex =>
                                s"Alice's matchIndex for Bob should be ${expectedMatchIndex}, but it is ${actualMatchIndex}"
                        )
                      )

                      // Check that Bob's block actor has the event in it's mempool
                      _ <- retryEitherPredicate(
                        action = (_ => peerNodes(bobId).blockActor.ask(_.getMempool)),
                        predicate = (mempool => mempool.contains(l2Event)),
                        onFailure = mempool =>
                            s"Mempool should contain \n\n ${l2Event} \n\n but was: \n\n ${mempool} \n\n"
                      )
                  yield Right(())

              res match {
                  case Left(e)  => false :| s"Test failed. Error: ${e}"
                  case Right(_) => true :| s"Test succeeded."
              }
      }
    )

/** Use it for tests that MUST never READ from the database (though can write).
  */
class OnlyWriteDbActor(inMemoryDBActor: InMemoryDBActor)
    extends DBWriterActor(inMemoryDBActor.config_),
      DBReader {
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
class InMemoryDBActor(config: DbWriter.Config) extends DBWriterActor(config), DBReader:

    private val log = Logger(getClass)

    private val outboxSeq = AtomicLong(0L)
    private val outbox = mutable.Buffer[MailboxMsg[Outbox]]()
    private val inboxes: mutable.Map[PeerId, mutable.Buffer[MailboxMsg[Inbox]]] = mutable.Map.empty

    override def persistOutgoingMessage(msg: ProtocolMsg): MsgId[Outbox] =
        val outMsgId = outboxSeq.incrementAndGet() |> MsgId.apply[Outbox]
        val mm = MailboxMsg[Outbox](outMsgId, msg)
        outbox.append(mm)
        log.info(s"persistOutgoingMessage: persisted $msg with outMsgId=$outMsgId")
        dispatchCallback(DbWriter.Event.OutgoingMessagePersisted(mm))
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
        dispatchCallback(DbWriter.Event.IncomingMessagePersisted(peerId, msg))
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
        ActorWatchdog.create(InboxActor(dbWriter, transmitterActor, ???, ???, ???))(using
          ox,
          sc,
          WatchdogTimeout.apply(5.seconds)
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
