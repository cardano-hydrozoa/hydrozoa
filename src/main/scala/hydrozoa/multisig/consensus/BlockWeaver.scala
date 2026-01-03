package hydrozoa.multisig.consensus

import cats.Monad
import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.BlockWeaver.*
import hydrozoa.multisig.protocol.types.Block.Number.firstBlockNumber
import hydrozoa.multisig.protocol.types.Block.blockEvents
import hydrozoa.multisig.protocol.types.{Block, LedgerEvent, LedgerEventId, Peer}

/** Block weaver actor.
  *   - When the node is leading a block, packages known unprocessed and incoming events, i.e., L1
  *     deposits and L2 txs into a new block.
  *   - When follower, before any block arrives, simply store incoming events in the mempool,
  *     keeping their order or arrival.
  *   - When a block arrives, feeds all the block to the joint ledger or uses Awaiting mode to wait
  *     till all events get through.
  *
  * There are several diagrams in the Hydrozoa docs that illustrate the work of the weaver.
  */
object BlockWeaver {

    final case class Config(
        /** Normally this is always the initial block, the only block known to the head upfront. In
          * the case of recovery it may be any block that was fully finished and synced to the
          * persistence.
          */
        lastKnownBlock: Block.Number,

        /** Own peer number */
        peerId: Peer.Number,

        /** This is needed for [[isLeaderForBlock]] function, the total number of peers in the head.
          *
          * Invariant: >= 1
          *
          * TODO: likely we are going to move it to the head config
          */
        numberOfPeers: Int,

        /** Round-robin peer's turn. see [[isLeaderForBlock]] function.
          *
          * Invariant: blockLeadTurn ∈ [1, numberOfPeers]
          *
          * TODO: likely we are going to move it to the head config
          */
        blockLeadTurn: Int,

        /** Recovered mempool, i.e., all ledger events outstanding the indices of the last known
          * block. Upon initialization is always an empty set by definition.
          *
          * TODO: shall we use just Seq[LedgerEvent here] not to expose Mempool?
          */
        recoveredMempool: Mempool,
        jointLedger: JointLedger.Ref,

        // persistence: Persistence.Ref
    )

    def apply(config: Config): IO[BlockWeaver] =
        IO(new BlockWeaver(config = config) {})

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
        def isFirstBlock: Boolean = blockNumber == firstBlockNumber
    }

    final case class Awaiting(
        block: Block.Next,
        eventIdAwaited: LedgerEventId,
        mempool: Mempool
    ) extends State

    private def mkInitialState: Idle = Idle(Mempool.empty)

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
}

trait BlockWeaver(config: BlockWeaver.Config) extends Actor[IO, Request] {

    import hydrozoa.multisig.consensus.BlockWeaver.*

    private val stateRef: Ref[IO, State] = Ref.unsafe(mkInitialState)

    // Having this field separately rids of the need to weave it through state changes.
    private val pollResultsRef: Ref[IO, PollResults] = Ref.unsafe(PollResults(Set.empty))

    override def preStart: IO[Unit] =
        if config.lastKnownBlock.toInt == 0 then
            require(
              config.recoveredMempool.isEmpty,
              "panic: recovered mempool should be empty before the first block"
            )
        switchToIdle(config.lastKnownBlock.increment, config.recoveredMempool)

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Request): IO[Unit] =
        req match {
            case msg: LedgerEvent   => handleLedgerEvent(msg)
            case b: Block.Next      => handleNewBlock(b)
            case bc: BlockConfirmed => handleBlockConfirmed(bc)
            case pr: PollResults    => handlePollResults(pr)
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
                        // Pass-through to the joint ledger
                        _ <- config.jointLedger ! event
                        // Complete the first block immediately
                        pollResults <- pollResultsRef.get
                        _ <- IO.whenA(leader.isFirstBlock) {
                            // It's always CompleteBlockRegular since we haven't
                            // seen any confirmations so far.
                            (config.jointLedger ! CompleteBlockRegular(None, pollResults.utxos))
                                >> switchToIdle(blockNumber.increment)
                        }
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
                        // This is sort of ephemeral Follower state
                        _ <- config.jointLedger ! StartBlock(
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

    private def completeReferenceBlock(block: Block.Next): IO[Unit] = for {
        pollResults <- pollResultsRef.get
        completeBlock <- block match {
            case _: Block.Minor =>
                IO.pure(CompleteBlockRegular(Some(block), pollResults.utxos))
            case _: Block.Major =>
                IO.pure(CompleteBlockRegular(Some(block), pollResults.utxos))
            case _: Block.Final =>
                IO.pure(CompleteBlockFinal(Some(block)))
        }
        _ <- config.jointLedger ! completeBlock
    } yield ()

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
                        // Finish the current block immediately
                        _ <- config.jointLedger !
                            (if blockConfirmed.finalizationRequested
                             then CompleteBlockFinal(None)
                             else CompleteBlockRegular(None, pollResults.utxos))
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
                            (config.jointLedger ! event) >> IO.pure(Left(es, mempool.remove(e)))
                        case None =>
                            // IO.println(s"new missing event: $e") >>
                            IO.pure(Right(Some(e), mempool))
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
        if isLeaderForBlock(nextBlockNum)
        then
            for {
                // _ <- IO.println(s"becoming leader for block: $nextBlockNum")
                now <- IO.monotonic
                _ <- config.jointLedger ! StartBlock(nextBlockNum, now)
                _ <- IO.traverse_(mempool.receivingOrder)(event =>
                    config.jointLedger ! mempool.findById(event).get
                )
                _ <- stateRef.set(Leader(nextBlockNum))
            } yield ()
        else stateRef.set(Idle(mempool))

    /** Determines whether the weaver is a leader for a block n:
      *   - The leader for block n = 1 is those that has blockLeadTurn = 1
      *   - ...
      *   - The leader for block n = numberOfPeers is that with blockLeadTurn = numberOfPeers
      *   - .. and so on and so forth
      *
      * @param blockNumber
      *   the number of the block, n ∈ [1 .. inf). Technically it's defined for block number 0,
      *   pointing to the last peer, but we don't need it in practice.
      */
    private def isLeaderForBlock(blockNumber: Block.Number): Boolean =
        blockNumber % config.numberOfPeers == config.blockLeadTurn % config.numberOfPeers

    private def handlePollResults(pollResults: PollResults): IO[Unit] =
        pollResultsRef.set(pollResults)

    private enum WeaverError extends Throwable:
        case UnexpectedBlockAnnouncement

        def msg: String = this match {
            case UnexpectedBlockAnnouncement => "Weaver got an unexpected new block announcement"
        }
}
