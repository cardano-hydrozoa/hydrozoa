package hydrozoa.multisig.consensus

import cats.Monad
import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.multisig.ledger.JointLedger
import hydrozoa.multisig.ledger.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, LedgerEvent, StartBlock}
import hydrozoa.multisig.protocol.ConsensusProtocol.*
import hydrozoa.multisig.protocol.ConsensusProtocol.BlockWeaver.*
import hydrozoa.multisig.protocol.types.Block.Header
import hydrozoa.multisig.protocol.types.Block.Number.firstBlockNumber
import hydrozoa.multisig.protocol.types.{AckBlock, Block, LedgerEventId, Peer}
import scala.collection.immutable.SortedSet

/** Block weaver actor:
  *
  * TODO: update the description
  *
  *   - When leader, receives L1 deposits + L2 txs and packages them into a new block.
  *   - When follower, before any block arrives, simply store incoming events in their corresponding
  *     queues in the mempool.
  *   - When leader or follower, collects L2 block acks to confirm block effects and trigger
  *     leader/follower switch.
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
          * TODO: shall we use just Seq[LedgerEvent here]?
          */
        recoveredMempool: Mempool,
        jointLedger: JointLedger.Ref,

        // persistence: Persistence.Ref
    )

    def apply(config: Config): IO[BlockWeaver] =
        IO(new BlockWeaver(config = config) {})

    //// TODO: shall we use a separate package?
    //// This is useful fot testing purposes
    // private [consensus] def applyWithState(config: Config, someState: State): IO[BlockWeaver] = for {
    //    weaver <- IO(new BlockWeaver(config = config) {})
    //    _ <- weaver.stateRef.set(someState)
    // } yield weaver

    // ===================================
    // Weaver internal state
    // ===================================

    sealed trait State

    final case class Idle(
        mempool: Mempool
    ) extends State

    final case class Leader(
        blockNumber: Block.Number,
        isFinal: Boolean
    ) extends State {
        def isFirstBlock: Boolean = blockNumber == firstBlockNumber
    }

    final case class Awaiting(
        block: Block,
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
      * @param numbersByPeer
      * @param receivingOrder
      */
    case class Mempool(
        events: Map[LedgerEventId, LedgerEvent] = Map.empty,
        numbersByPeer: Map[Peer.Number, SortedSet[Int]] = Map.empty,
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

                val newNumbers = mempool.numbersByPeer
                    .getOrElse(eventId.peerNum, SortedSet.empty[Int]) + eventId.eventNum

                mempool.copy(
                  events = mempool.events + (eventId -> event),
                  numbersByPeer = mempool.numbersByPeer + (eventId.peerNum -> newNumbers),
                  receivingOrder = mempool.receivingOrder :+ eventId
                )
            }

            // Remove event - returns new state
            def remove(id: LedgerEventId): Mempool = {
                mempool.events.get(id) match {
                    // TODO: shall we raise here?
                    case None => mempool // Not found, no change
                    case Some(_) =>
                        val newNumbersByPeer = mempool.numbersByPeer.get(id.peerNum) match {
                            case None => mempool.numbersByPeer
                            case Some(eventNums) =>
                                val updatedNums = eventNums - id.eventNum
                                if updatedNums.isEmpty then mempool.numbersByPeer - id.peerNum
                                else mempool.numbersByPeer + (id.peerNum -> updatedNums)
                        }

                        mempool.copy(
                          events = mempool.events - id,
                          numbersByPeer = newNumbersByPeer,
                          receivingOrder = mempool.receivingOrder.filterNot(_ == id)
                        )
                }
            }

            // Query max number for peer
            def getMaxNumber(peer: Peer.Number): Option[Int] =
                mempool.numbersByPeer.get(peer).flatMap(_.lastOption)

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

    private val subscribers = Ref.unsafe[IO, Option[Subscribers]](None)

    private final case class Subscribers(
        ackBlock: List[AckBlock.Subscriber],
        newBlock: List[Block.Subscriber],
        confirmBlock: List[ConfirmBlock.Subscriber],
        confirmMajorFinalBlock: ConfirmMajorFinalBlock.Subscriber
    )

    override def preStart: IO[Unit] =
        if config.lastKnownBlock.toInt == 0 then
            require(
              config.recoveredMempool.isEmpty,
              "panic: recovered mempool should be empty before the first block"
            )
        switchToIdle(config.lastKnownBlock.increment, config.recoveredMempool)

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction(receiveTotal)

    // private def receiveTotal(req: Request, _subs: Subscribers): IO[Unit] =
    private def receiveTotal(req: Request): IO[Unit] =
        req match {
            case msg: NewLedgerEvent => handleNewLedgerEvent(msg)
            case b: Block            => handleNewBlock(b)
            case bc: BlockConfirmed  => handleBlockConfirmed(bc)
        }

    // ===================================
    // Mailbox message handlers
    // ===================================

    private def handleNewLedgerEvent(msg: NewLedgerEvent): IO[Unit] = {
        import FeedResult.*

        for {
            _ <- IO.println("handleNewLedgerEvent")
            state <- stateRef.get

            _ <- state match {
                case Idle(mempool) =>
                    // Just add event to the mempool
                    stateRef.set(Idle(mempool.add(msg.event)))
                case leader @ Leader(blockNumber, _) =>
                    for {
                        // Pass-through to the joint ledger
                        _ <- config.jointLedger ! msg.event
                        // Complete the first block immediately
                        _ <- IO.whenA(leader.isFirstBlock) {
                            // It's always CompleteBlockRegular since we haven't
                            // seen any confirmations so far.
                            (config.jointLedger ! CompleteBlockRegular(None))
                                >> switchToIdle(blockNumber.increment)
                        }
                    } yield ()

                case awaiting @ Awaiting(block, eventIdAwaited, mempool) =>
                    if msg.event.eventId == eventIdAwaited
                    then {
                        // We got the event we waited for, try to finish the block
                        for {
                            result <- continueFeedBlock(block, msg, mempool)
                            _ <- result match {
                                case Done(residualMempool) =>
                                    // Switch to Idle with the residual of mempool
                                    switchToIdle(block.id.increment, residualMempool)
                                case EventMissing(neweventIdAwaited, newMempool) =>
                                    // Stay in Awaiting with new eventIdAwaited and new mempool
                                    stateRef.set(
                                      awaiting.copy(
                                        eventIdAwaited = neweventIdAwaited,
                                        mempool = newMempool
                                      )
                                    )
                            }
                        } yield ()
                    } else {
                        // This is not an event we are waiting for, just add it to the mempool and keep going
                        stateRef.set(
                          awaiting.copy(mempool = mempool.add(msg.event))
                        )
                    }
            }
        } yield ()
    }

    private def handleNewBlock(block: Block): IO[Unit] = {
        import FeedResult.*
        import WeaverError.*

        for {
            _ <- IO.println("handleNewBlock")
            state <- stateRef.get

            _ <- state match {
                case Idle(mempool) =>
                    for {
                        // This is sort of ephemeral Follower state
                        // TODO: I don't think that this is what we want in terms of time
                        _ <- config.jointLedger ! StartBlock(
                          block.header.timeCreation.toMillis,
                          Set.empty
                        )
                        result <- tryFeedBlock(block, mempool)
                        _ <- result match {
                            case Done(residualMempool) =>
                                for {
                                    _ <- IO.println("Done")
                                    // We are done with the block
                                    completeBlock <- block match {
                                        case _: Block.Initial => ??? // TODO: Impossible
                                        case _: Block.Minor =>
                                            IO.pure(CompleteBlockRegular(Some(block)))
                                        case _: Block.Major =>
                                            IO.pure(CompleteBlockRegular(Some(block)))
                                        case _: Block.Final =>
                                            IO.pure(CompleteBlockFinal(Some(block)))
                                    }
                                    _ <- config.jointLedger ! completeBlock
                                    _ <- switchToIdle(block.id.increment, residualMempool)
                                } yield ()
                            case EventMissing(eventIdAwaited, residualMempool) =>
                                for {
                                    _ <- IO.println(s"EventMissing: ${eventIdAwaited}")
                                    _ <- stateRef.set(
                                      Awaiting(block, eventIdAwaited, residualMempool)
                                    )
                                } yield ()
                        }
                    } yield ()
                case _: Leader   => IO.raiseError(UnexpectedBlockAnnouncement)
                case _: Awaiting => IO.raiseError(UnexpectedBlockAnnouncement)
            }
        } yield ()
    }

    private def handleBlockConfirmed(confirmation: BlockConfirmed): IO[Unit] = {
        import WeaverError.*

        for {
            _ <- IO.println("handleBlockConfirmed")
            state <- stateRef.get
            _ <- state match {
                case Idle(_) =>
                    // This may happen, but we should ignore it since that signal is useless for the weaver
                    // when the node is node a leader.
                    IO.pure(())
                case Leader(blockNumber, isFinal) =>
                    for {
                        // Finish the current block immediately
                        _ <- config.jointLedger !
                            (if isFinal
                             // TODO: add pollResults thingy, which is gone now
                             // then CompleteBlockRegular(pollResults = ???, None)
                             then CompleteBlockRegular(None)
                             else CompleteBlockFinal(None))
                        // Switch to Idle
                        _ <- switchToIdle(blockNumber.increment)
                    } yield ()
                case Awaiting(block, eventIdAwaited, mempool) =>
                    IO.raiseError(UnexpectedBlockConfirmation)
            }
        } yield ()
    }

    // ===================================
    // Feeding, i.e. pushing all block events into the joint ledger being a follower
    // ===================================

    private enum FeedResult:
        case Done(mempool: Mempool)
        case EventMissing(eventId: LedgerEventId, mempool: Mempool)

    private def tryFeedBlock(
        block: Block,
        initialMempool: Mempool,
        startWith: Option[LedgerEventId] = None
    ): IO[FeedResult] = {
        import FeedResult.*

        for {

            // TODO fix inference, see the first case match
            _ <- IO.whenA(block.isInstanceOf[Block.Initial])(
              IO.raiseError(WeaverError.InitialBlockAnnouncement)
            )

            blockEvents <- IO.pure(block match {
                // case Block.Initial(_)     => IO.raiseError(WeaverError.InitialBlockAnnouncement)
                case Block.Minor(_, body) => body.events.map(_._1)
                case Block.Major(_, body) => body.events.map(_._1)
                case Block.Final(_, body) => body.events.map(_._1)
            })

            eventsToFeed = startWith match {
                // TODO: check indexOf/splitAt word as expected
                case Some(eventId) => blockEvents.splitAt(blockEvents.indexOf(eventId))._1
                case None          => blockEvents
            }

            // Tries to feed block events, until it's over or an event is missing.
            mbFirstEventIdMissingAndResidualMempool <- Monad[IO].tailRecM(
              (eventsToFeed, initialMempool)
            ) {
                case (Nil, mempool) => IO.pure(Right(None, mempool))
                // TODO add tryRemove
                case (e :: es, mempool) =>
                    mempool.findById(e) match {
                        case Some(event) =>
                            (config.jointLedger ! event) >> IO.pure(Left(es, mempool.remove(e)))
                        case None => IO.pure(Right(Some(e), mempool))
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
        block: Block,
        msg: NewLedgerEvent,
        mempool: Mempool
    ): IO[FeedResult] =
        tryFeedBlock(block, mempool.add(msg.event), Some(msg.event.eventId))

    // ===================================
    // Switch to Idle
    // ===================================

    /** Switch to the Idle state. If node is a leader of the next block, starts a new block, send
      * all events existing in the mempool to the joint ledger and immediately switches to the
      * Leader state. If not, stays in Idle until we see the next block announcement.
      *
      * @param nextBlock
      *   the next block number
      * @param mempool
      *   if we are switching from follower/awaiting there may be the rest of mempool
      * @return
      */
    private def switchToIdle(
        nextBlock: Block.Number,
        mempool: Mempool = Mempool.empty
    ): IO[Unit] =
        if isLeaderForBlock(nextBlock)
        then
            for {
                _ <- IO.println(s"becoming leader for block: $nextBlock")
                now <- IO.monotonic
                // TODO: revert FiniteDuration, now I see why we had it there before
                // pollResults now live here though George's intent was to send them in the finish block event
                // besides they are not needed when we commence building/checking a block another thing is
                // that there is a chance results are fresher by the time we finish a block
                // TODO: personally I see this as mingling in two separate things together
                // This function is supposed to be called in preStart as well,
                // and we only can pass empty Set
                // TODO: don't we need to pass the number of the block?
                _ <- config.jointLedger ! StartBlock(now.toMillis, Set.empty)
                _ <- IO.traverse_(mempool.receivingOrder)(event =>
                    config.jointLedger ! mempool.findById(event).get
                )
                // TODO: add isFinal to BlockConfirmed or use AckBlock as it was done before
                // TODO: this is always false for the initialization block
                _ <- stateRef.set(Leader(nextBlock, false))
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

    private enum WeaverError extends Throwable:
        case InitialBlockAnnouncement
        case UnexpectedBlockAnnouncement
        case UnexpectedBlockConfirmation

        def msg: String = this match {
            case InitialBlockAnnouncement    => "The initial block is not expected to be broadcast"
            case UnexpectedBlockAnnouncement => "Weaver got an unexpected new block announcement"
            case UnexpectedBlockConfirmation => "Weaver got an unexpected block confirmation"
        }
}
