package hydrozoa.multisig.consensus

import cats.Monad
import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.UtxoIdL1
import hydrozoa.lib.cardano.scalus.QuantizedTime.toEpochQuantizedInstant
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.protocol.types.Block.Number.first
import hydrozoa.multisig.protocol.types.Block.blockEvents
import hydrozoa.multisig.protocol.types.{Block, LedgerEvent, LedgerEventId, Peer}
import scalus.cardano.ledger.SlotConfig

/** Block weaver actor.
  *   - When the node is leading a block, the weaver packages all known unprocessed (by the time
  *     block is stared) and continue streaming all incoming events (until the block is completed)
  *     into that block. These events include L1 deposits and L2 txs.
  *   - When the node is a follower, before any block arrives, the weaver simply stores incoming
  *     events in the mempool, keeping the order or arrival.
  *   - When a block arrives, the weaver feeds all the block events to the joint ledger or switches
  *     to Awaiting mode if some events are not here yet.
  *
  * There are several diagrams in the Hydrozoa docs that illustrate the work of the weaver.
  */
object BlockWeaver:
    def apply(
        config: Config,
        pendingConnections: MultisigRegimeManager.PendingConnections | BlockWeaver.Connections
    ): IO[BlockWeaver] =
        IO(new BlockWeaver(config = config, pendingConnections = pendingConnections) {})

    /** Configuration for the block weaver actor.
      *
      * @param lastKnownBlock
      *   Normally this is always the initial block, the only block known to the head upfront. In *
      *   the case of recovery it may be any block that was fully finished and synced to the *
      *   persistence.
      * @param peerId
      *   this peer's own ID, which is used to determine the block numbers for which it should be
      *   leader.
      * @param recoveredMempool
      *   Recovered mempool, i.e., all ledger events outstanding the indices of the last known *
      *   block. Upon initialization is always an empty set by definition.
      *
      * @param slotConfig
      *   the slot configuration.
      */
    final case class Config(
        lastKnownBlock: Block.Number,
        peerId: Peer.Id,
        // TODO: shall we use just Seq[LedgerEvent here] not to expose Mempool?
        recoveredMempool: Mempool,
        slotConfig: SlotConfig
    )

    final case class Connections(
        jointLedger: JointLedger.Handle
    )

    // ===================================
    // Weaver internal state
    // ===================================

    sealed trait State

    final case class Idle(
        mempool: Mempool
    ) extends State

    final case class Leader(
        blockNumber: Block.Number
    ) extends State {
        def isFirstBlock: Boolean = blockNumber == first
    }

    final case class Awaiting(
        block: Block.Next,
        eventIdAwaited: LedgerEventId,
        mempool: Mempool
    ) extends State

    object State:
        def mkInitialState: State = Idle(Mempool.empty)

    // ===================================
    // Request + ActorRef + apply
    // ===================================

    /** Block confirmation.
      *
      * @param blockNumber
      */
    final case class BlockConfirmed(
        blockNumber: Block.Number,
        finalizationRequested: Boolean = false
    )

    /** So-called "poll results" from the Cardano Liaison, i.e., a set of all utxos ids found at the
      * multisig head address.
      *
      * @param utxos
      *   all utxos found
      */
    final case class PollResults(utxos: Set[UtxoIdL1])

    object PollResults:
        val empty: PollResults = PollResults(Set.empty)

    object FinalizationLocallyTriggered

    type Handle = ActorRef[IO, Request]
    type Request = LedgerEvent | Block.Next | BlockConfirmed | PollResults |
        FinalizationLocallyTriggered.type

    // ===================================
    // Immutable mempool state
    // ===================================

    /** Simple immutable mempool implementation. Duplicate ledger events IDs are NOT allowe and a
      * runtime exception is thrown since this should never happen. Other components, particularly
      * the peer liaison is in charge or maintaining the integrity of the stream of messages.
      *
      * @param events
      *   map to srtore events
      * @param receivingOrder
      *   vector to store order of event ids
      */
    case class Mempool(
        events: Map[LedgerEventId, LedgerEvent] = Map.empty,
        receivingOrder: Vector[LedgerEventId] = Vector.empty
    )

    object Mempool {
        val empty: Mempool = Mempool()

        def apply(events: Seq[LedgerEvent]): Mempool =
            events.foldLeft(Mempool.empty)((mempool, event) => mempool.add(event))

        // Extension methods
        extension (mempool: Mempool)

            // Add event - returns new state
            /** Throws if a duplicate is detected.
              * @param event
              *   an event to add
              * @return
              *   an updated mempool
              */
            def add(
                event: LedgerEvent
            ): Mempool = {

                val eventId = event.eventId

                require(
                  !mempool.events.contains(eventId),
                  s"panic - duplicate event id in the pool: $eventId"
                )

                mempool.copy(
                  events = mempool.events + (eventId -> event),
                  receivingOrder = mempool.receivingOrder :+ eventId
                )
            }

            // Remove event - returns new state
            def remove(id: LedgerEventId): Mempool = {
                require(
                  mempool.events.contains(id),
                  "panic - an attempt to remove a missing event from the mempool"
                )
                mempool.copy(
                  events = mempool.events - id,
                  receivingOrder = mempool.receivingOrder.filterNot(_ == id)
                )
            }

            // Find by ID
            def findById(id: LedgerEventId): Option[LedgerEvent] =
                mempool.events.get(id)

            // Iterate in insertion order
            def inOrder: Iterator[LedgerEvent] =
                mempool.receivingOrder.iterator.flatMap(mempool.events.get)

            def isEmpty: Boolean = mempool.events.isEmpty
    }
end BlockWeaver

trait BlockWeaver(
    config: BlockWeaver.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | BlockWeaver.Connections
) extends Actor[IO, BlockWeaver.Request]:
    import BlockWeaver.*

    private val stateRef = Ref.unsafe[IO, BlockWeaver.State](State.mkInitialState)

    // Having this field separately rids of the need to weave it through state changes.
    private val pollResultsRef = Ref.unsafe[IO, BlockWeaver.PollResults](PollResults.empty)

    private val connections = Ref.unsafe[IO, Option[BlockWeaver.Connections]](None)

    /** This ref initialized with false value intially and gets true value upon receiving
      * [[FinalizationLocallyTriggered]]. Regardless the peers' role for the current block, that
      * causes the eponymous flag `finalizationLocallyTriggeredRef` in the next
      * [[CompleteBlockRegular]] be set to true. This tells the joint ledger to set up the
      * finalization flag `finalizationRequested` in the peer's ack, so the leader of the next block
      * observe it in their [[BlockWeaver.BlockConfirmed]] and produce the final block.
      */
    private val finalizationLocallyTriggeredRef = Ref.unsafe[IO, Boolean](false)

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error(
              "Block weaver is missing its connections to other actors."
            )
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                _connections <- x.get
                _ <- this.connections.set(
                  Some(BlockWeaver.Connections(jointLedger = _connections.jointLedger))
                )
            } yield ()
        case x: BlockWeaver.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] =
        if config.lastKnownBlock.toInt == 0 then
            require(
              config.recoveredMempool.isEmpty,
              "panic: recovered mempool should be empty before the first block"
            )
        for {
            _ <- initializeConnections
            _ <- switchToIdle(config.lastKnownBlock.increment, config.recoveredMempool)
        } yield ()

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] =
        req match {
            case msg: LedgerEvent             => handleLedgerEvent(msg)
            case b: Block.Next                => handleNewBlock(b)
            case bc: BlockConfirmed           => handleBlockConfirmed(bc)
            case pr: PollResults              => handlePollResults(pr)
            case finalizationLocallyTriggered => finalizationLocallyTriggeredRef.set(true)
        }

    // ===================================
    // Mailbox message handlers
    // ===================================

    private def handleLedgerEvent(event: LedgerEvent): IO[Unit] = {
        import FeedResult.*

        for {
            // _ <- IO.println("handleLedgerEvent")
            state <- stateRef.get

            _ <- state match {
                case Idle(mempool) =>
                    // Just add event to the mempool
                    stateRef.set(Idle(mempool.add(event)))
                case leader @ Leader(blockNumber) =>
                    for {
                        conn <- getConnections
                        // Pass-through to the joint ledger
                        _ <- conn.jointLedger ! event

                        // Complete the first block immediately
                        _ <- IO.whenA(leader.isFirstBlock)(for {
                            pollResults <- pollResultsRef.get
                            finalizationLocallyTriggered <- finalizationLocallyTriggeredRef.get
                            // It's always CompleteBlockRegular since we haven't
                            // seen any confirmations so far.
                            _ <- conn.jointLedger ! CompleteBlockRegular(
                              None,
                              pollResults.utxos,
                              finalizationLocallyTriggered
                            )
                            _ <- switchToIdle(blockNumber.increment)
                        } yield ())
                    } yield ()

                case awaiting @ Awaiting(block, eventIdAwaited, mempool) =>
                    if event.eventId == eventIdAwaited
                    then {
                        // We got the event we waited for, try to finish the block
                        for {
                            result <- continueFeedBlock(block, event, mempool)
                            _ <- result match {
                                case Done(residualMempool) =>
                                    for {
                                        // We are done with the block
                                        // _ <- IO.println("Done after awaiting for missing events")
                                        _ <- completeReferenceBlock(block)
                                        // Switch to Idle with the residual of mempool
                                        _ <- switchToIdle(block.id.increment, residualMempool)
                                    } yield ()
                                case EventMissing(newEventIdAwaited, newMempool) =>
                                    // Stay in Awaiting with new eventIdAwaited and new mempool
                                    stateRef.set(
                                      awaiting.copy(
                                        eventIdAwaited = newEventIdAwaited,
                                        mempool = newMempool
                                      )
                                    )
                            }
                        } yield ()
                    } else {
                        // This is not an event we are waiting for, just add it to the mempool and keep going
                        stateRef.set(
                          awaiting.copy(mempool = mempool.add(event))
                        )
                    }
            }
        } yield ()
    }

    private def handleNewBlock(block: Block.Next): IO[Unit] = {
        import FeedResult.*
        import WeaverError.*

        for {
            // _ <- IO.println("handleNewBlock")
            state <- stateRef.get

            _ <- state match {
                case Idle(mempool) =>
                    for {
                        conn <- getConnections
                        // This is sort of ephemeral Follower state
                        _ <- conn.jointLedger ! StartBlock(
                          block.header.blockNum,
                          block.header.timeCreation
                        )
                        result <- tryFeedBlock(block, mempool)
                        _ <- result match {
                            case Done(residualMempool) =>
                                for {
                                    // We are done with the block
                                    // _ <- IO.println("Done")
                                    _ <- completeReferenceBlock(block)
                                    _ <- switchToIdle(block.id.increment, residualMempool)
                                } yield ()
                            case EventMissing(eventIdAwaited, residualMempool) =>
                                for {
                                    // _ <- IO.println(s"EventMissing: ${eventIdAwaited}")
                                    _ <- stateRef.set(
                                      Awaiting(block, eventIdAwaited, residualMempool)
                                    )
                                } yield ()
                        }
                    } yield ()
                case _ => IO.raiseError(UnexpectedBlockAnnouncement)
            }
        } yield ()
    }

    /** When completing a refernce block, we decide upon the type of completion message
      * CompleteBlockRegular/CompleteBlockFinal based on the type of the block only.
      *
      * `finalizationLocallyTriggered` is passed only if [[CompleteBlockRegular]] was chosen to
      * force the joint ledger to produce an acknowledgement with `finalizationRequested` flag set.
      *
      * @param block
      * @return
      */
    private def completeReferenceBlock(block: Block.Next): IO[Unit] = for {
        pollResults <- pollResultsRef.get
        finalizationLocallyTriggered <- finalizationLocallyTriggeredRef.get
        completeBlock <- block match {
            case _: Block.Minor =>
                IO.pure(
                  CompleteBlockRegular(Some(block), pollResults.utxos, finalizationLocallyTriggered)
                )
            case _: Block.Major =>
                IO.pure(
                  CompleteBlockRegular(Some(block), pollResults.utxos, finalizationLocallyTriggered)
                )
            case _: Block.Final =>
                IO.pure(CompleteBlockFinal(Some(block)))
        }
        conn <- getConnections
        _ <- conn.jointLedger ! completeBlock
    } yield ()

    /** This one a bit cumbersome due to the presence of TWO finalization flags being used in this
      * function:
      *   - The `finalizationLocallyTriggered` flag that indicates that a finalization API was
      *     called locally sets the flag in `CompleteBlockRegular` to ask joint ledger add
      *     finalization flag to the ack.
      *   - the `blockConfirmed.finalizationRequested` forces [[CompleteBlockFinal]] so joint ledger
      *     is to produce a final block immediately.
      */
    private def handleBlockConfirmed(blockConfirmed: BlockConfirmed): IO[Unit] = {
        for {
            // _ <- IO.println("handleBlockConfirmed")
            state <- stateRef.get
            _ <- state match {
                case Leader(blockNumber) =>
                    // Iff the block confirmed is the previous block
                    IO.whenA(blockConfirmed.blockNumber.increment == blockNumber)
                    for {
                        pollResults <- pollResultsRef.get
                        finalizationLocallyTriggered <- finalizationLocallyTriggeredRef.get
                        conn <- getConnections

                        // Finish the current block immediately
                        _ <- conn.jointLedger !
                            (if blockConfirmed.finalizationRequested
                             then CompleteBlockFinal(None)
                             else
                                 CompleteBlockRegular(
                                   None,
                                   pollResults.utxos,
                                   finalizationLocallyTriggered
                                 ))
                        // Switch to Idle
                        _ <- switchToIdle(blockNumber.increment)
                    } yield ()
                case _ =>
                    // This may happen, but we should ignore it since that signal is useless for the weaver
                    // when the node is node a leader.
                    IO.pure(())
            }
        } yield ()
    }

    // ===================================
    // Feeding, i.e. pushing all block events into the joint ledger being a follower
    // ===================================

    private enum FeedResult:
        case Done(mempool: Mempool)
        case EventMissing(eventId: LedgerEventId, mempool: Mempool)

    /** Make an attempt to feed the whole block to the joint ledger
      * @param block
      *   the next block to handle
      * @param initialMempool
      *   the current state of mempool
      * @param startWith
      * @return
      *   [[FeedResult]]
      */
    private def tryFeedBlock(
        block: Block.Next,
        initialMempool: Mempool,
        startWith: Option[LedgerEventId] = None
    ): IO[FeedResult] = {
        import FeedResult.*

        for {
            // _ <- IO.println(s"tryFeedBlock: start with: $startWith")
            blockEvents <- IO.pure(block.blockEvents)

            eventsToFeed = startWith match {
                case Some(eventId) => blockEvents.splitAt(blockEvents.indexOf(eventId))._2
                case None          => blockEvents
            }

            // _ <- IO.println(s"events to feed: ${eventsToFeed}")
            // _ <- IO.println(s"initial mempool: ${initialMempool}")

            // Tries to feed block events, until it's over or an event is missing.
            mbFirstEventIdMissingAndResidualMempool <- Monad[IO].tailRecM(
              (eventsToFeed, initialMempool)
            ) {
                case (Nil, mempool) => IO.pure(Right(None, mempool))
                // TODO maybe add tryRemove later on
                case (e :: es, mempool) =>
                    mempool.findById(e) match {
                        case Some(event) =>
                            for {
                                conn <- getConnections
                                _ <- conn.jointLedger ! event
                            } yield Left(es, mempool.remove(e))
                        case None =>
                            for {
                                _ <- IO.unit // IO.println(s"new missing event: $e")
                            } yield Right(Some(e), mempool)
                    }
            }

            result <- mbFirstEventIdMissingAndResidualMempool match {
                case (None, residualMempool) =>
                    IO.pure(Done(residualMempool))
                case (Some(eventId), residualMempool) =>
                    IO.pure(EventMissing(eventId, residualMempool))
            }
        } yield result
    }

    private def continueFeedBlock(
        block: Block.Next,
        event: LedgerEvent,
        mempool: Mempool
    ): IO[FeedResult] =
        tryFeedBlock(block, mempool.add(event), Some(event.eventId))

    // ===================================
    // Switch to Idle
    // ===================================

    /** Switch to the Idle state. If node is a leader of the next block, starts a new block, send
      * all events existing in the mempool to the joint ledger and immediately switches to the
      * Leader state. If not, stays in Idle until we see the next block announcement.
      *
      * @param nextBlockNum
      *   the next block number
      * @param mempool
      *   if we are switching from follower/awaiting there may be the rest of mempool
      * @return
      */
    private def switchToIdle(
        nextBlockNum: Block.Number,
        mempool: Mempool = Mempool.empty
    ): IO[Unit] =
        if config.peerId.isLeader(nextBlockNum)
        then
            for {
                conn <- getConnections
                // _ <- IO.println(s"becoming leader for block: $nextBlockNum")
                now <- IO.realTime.map(_.toEpochQuantizedInstant(config.slotConfig))
                _ <- conn.jointLedger ! StartBlock(nextBlockNum, now)
                _ <- IO.traverse_(mempool.receivingOrder)(event =>
                    conn.jointLedger ! mempool.findById(event).get
                )
                _ <- stateRef.set(Leader(nextBlockNum))
            } yield ()
        else stateRef.set(Idle(mempool))

    private def handlePollResults(pollResults: PollResults): IO[Unit] =
        pollResultsRef.set(pollResults)

    private enum WeaverError extends Throwable:
        case UnexpectedBlockAnnouncement

        def msg: String = this match {
            case UnexpectedBlockAnnouncement => "Weaver got an unexpected new block announcement"
        }

end BlockWeaver
