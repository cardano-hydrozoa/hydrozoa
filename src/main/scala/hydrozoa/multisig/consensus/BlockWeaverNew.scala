package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockHeader, BlockNumber, BlockStatus, BlockType}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedger
import org.typelevel.log4cats.Logger
import scalus.cardano.ledger.TransactionInput

final case class BlockWeaverNew(
    config: BlockWeaverNew.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | BlockWeaverNew.Connections
) extends Actor[IO, BlockWeaverNew.Request] {
    import BlockWeaverNew.*

    private val loggerIO = Logging.loggerIO("BlockWeaver")

    override def preStart: IO[Unit] = for {
        _ <- context.self ! BlockWeaverNew.PreStart
        _ <- context.become(receive)
    } yield ()

    private def become(state: BlockWeaverNew.State.Reactive): IO[Unit] =
        context.become(
          PartialFunction.fromFunction(req =>
              for {
                  // Handle the request using the current state's handler
                  mNewState <- state.react(config)(req)
                  // If the handler returns a new state, become that state.
                  // Otherwise, stop the actor.
                  _ <- mNewState.fold(context.self.stop)(newState => become(newState))
              } yield ()
          )
        )

    override def receive: Receive[IO, BlockWeaverNew.Request] = PartialFunction.fromFunction {
        case PreStart =>
            for {
                connections <- initializeConnections
                startingState <- State.start(config, connections, loggerIO)
                _ <- become(startingState)
            } yield ()
        case x =>
            val msg = s"Unexpected message received before PreStart: $x"
            loggerIO.error(msg) >> IO.raiseError(RuntimeException(msg))
    }

    private def initializeConnections: IO[BlockWeaverNew.Connections] = pendingConnections match {
        case pc: MultisigRegimeManager.PendingConnections =>
            for {
                c <- pc.get
            } yield BlockWeaverNew.Connections(
              jointLedger = c.jointLedger,
              tracer = c.tracer,
            )
        case c: BlockWeaverNew.Connections => IO.pure(c)
    }
}

object BlockWeaverNew {
    export BlockWeaverDataStructures.*

    type Config = CardanoNetwork.Section & OwnHeadPeerPublic.Section

    final case class Connections(
        jointLedger: JointLedger.Handle,
        tracer: hydrozoa.lib.tracing.ProtocolTracer = hydrozoa.lib.tracing.ProtocolTracer.noop,
    )

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | UserRequestWithId | BlockBrief.Next | BlockConfirmed |
        PollResults | LocalFinalizationTrigger.Triggered.type

    type BlockConfirmed = BlockHeader.Section & Block.Fields.HasFinalizationRequested &
        BlockStatus.MultiSigned & BlockType.Next

    case object PreStart

    sealed trait LocalFinalizationTrigger

    object LocalFinalizationTrigger {
        case object Triggered extends LocalFinalizationTrigger
        case object NotTriggered extends LocalFinalizationTrigger
    }

    sealed trait State {

        /** See [[State.Active]] and [[State.Reactive]]. */
        type NextState <: State.Reactive

        def connections: Connections
        def logger: Logger[IO]
        def pollResults: PollResults
        def finalizationLocallyTriggered: LocalFinalizationTrigger
    }

    object State {
        import Idle.*

        def start(
            config: Config,
            connections: Connections,
            logger: Logger[IO]
        ): IO[IdleReactive | Leader] =
            for {
                state: Some[IdleReactive | Leader] <- IdleActive(
                  connections = connections,
                  logger = logger,
                  pollResults = PollResults.empty,
                  finalizationLocallyTriggered = LocalFinalizationTrigger.NotTriggered,
                  mempool = Mempool.empty
                ).act(config)
            } yield state.get

        private def continue[S <: State.Reactive](state: S): IO[Some[S]] =
            IO.pure(Some(state))

        // TODO: implement state machine termination.
        // private def stop(): IO[None.type] = IO.pure(None)

        /** A caching state can store requests in its mempool. */
        sealed trait Caching extends State {
            def mempool: Mempool
        }

        /** An active state can immediately transition into another state, without waiting for a new
          * message.
          *
          * An active state cannot be reactive, and it can only transition into a reactive state (or
          * terminate).
          *
          * An active state can transition to a reactive state via a chain of active states, but the
          * [[Active.act]] function must statically prove that the chain of [[Active.act]] calls
          * terminates in a reactive state.
          */
        sealed trait Active extends State {
            def act(config: Config): IO[Option[NextState]]
        }

        /** A reactive state can receive a message, reacting by transitioning to another state.
          *
          * A reactive state cannot be active, and it can only transition into a reactive state (or
          * terminate).
          *
          * A reactive state can transition to a reactive state via a chain of active states, but *
          * the [[Reactive.react]] function must statically prove that the chain of [[Active.act]]
          * calls terminates in a reactive * state.
          */
        sealed trait Reactive extends State {
            type Unexpected <: Request

            def react(config: Config)(req: Request): IO[Option[NextState]]

            final def panicUnexpectedRequest(
                state: State.Reactive,
                unexpected: Unexpected
            ): IO[None.type] =
                val msg =
                    s"Unexpectedly received in ${state.toString} state: ${unexpected.toString}"
                logger.error(msg) >>
                    IO.raiseError(RuntimeException(msg))
        }

        sealed trait Idle

        object Idle {
            final case class IdleActive private[State] (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
            ) extends Active,
                  Caching {
                override type NextState = IdleReactive | Leader

                private def idleReactive(newMempool: Mempool): IdleReactive =
                    IdleReactive(this, newMempool)

                private def leader(blockNumber: BlockNumber, startedBlock: Leader.StartedBlock) =
                    Leader(this, blockNumber, startedBlock)

                override def act(config: Config): IO[Some[NextState]] =
                    if config.ownHeadPeerId.isLeader(BlockNumber.zero)
                    then {
                        logger.info("Starting as Leader.") >>
                            continue(leader(BlockNumber.zero, Leader.StartedBlock.NotStarted))
                    } else {
                        logger.info("Starting as Idle.") >>
                            continue(idleReactive(mempool))
                    }
            }

            object IdleActive {
                private[State] def apply(state: State, mempool: Mempool): IdleActive =
                    import state.*
                    IdleActive(
                      connections,
                      logger,
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool
                    )
            }

            final case class IdleReactive private[State] (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
            ) extends Caching,
                  Reactive {
                override type NextState = IdleReactive | FollowerAwaiting

                override type Unexpected = PreStart.type | BlockConfirmed

                private def idleReactive(newMempool: Mempool): IdleReactive =
                    IdleReactive(this, newMempool)

                private def follower(
                    newMempool: Mempool,
                    reproducingBlockBrief: BlockBrief.Next
                ): Follower =
                    Follower(this, newMempool, reproducingBlockBrief)

                override def react(
                    config: Config
                )(req: Request): IO[Option[NextState]] =
                    req match {
                        case ur: UserRequestWithId =>
                            logger.trace(s"Adding ${ur.requestId} to mempool.") >>
                                continue(idleReactive(mempool.add(ur)))
                        case bb: BlockBrief.Next =>
                            logger.trace(s"New block brief ${bb.blockNum}.") >>
                                follower(mempool, bb).act(config)
                        case pr: PollResults =>
                            logger.trace("New poll results.") >>
                                continue(copy(pollResults = pr))
                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            logger.trace("Finalization was locally triggered.") >>
                                continue(copy(finalizationLocallyTriggered = ft))
                        case m: Unexpected =>
                            panicUnexpectedRequest(this, m)
                    }
            }

            object IdleReactive {
                private[Idle] def apply(state: State, mempool: Mempool): IdleReactive =
                    import state.*
                    IdleReactive(
                      connections,
                      logger,
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool
                    )
            }
        }

        final case class Follower private (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            override val finalizationLocallyTriggered: LocalFinalizationTrigger,
            override val mempool: Mempool,
            reproducingBlockBrief: BlockBrief.Next,
        ) extends Active,
              Caching {
            override type NextState = IdleReactive | FollowerAwaiting

            def act(config: Config): IO[Some[NextState]] = ???
        }

        object Follower {
            private[State] def apply(
                state: State,
                mempool: Mempool,
                reproducingBlockBrief: BlockBrief.Next
            ): Follower =
                import state.*
                Follower(
                  connections,
                  logger,
                  pollResults,
                  finalizationLocallyTriggered,
                  mempool,
                  reproducingBlockBrief
                )
        }

        final case class FollowerAwaiting private (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            override val finalizationLocallyTriggered: LocalFinalizationTrigger,
            override val mempool: Mempool,
            reproducingBlockBrief: BlockBrief.Next,
            awaitingRequestId: RequestId,
        ) extends Caching,
              Reactive {
            override type NextState = IdleReactive | FollowerAwaiting

            override type Unexpected = PreStart.type | BlockBrief.Next | BlockConfirmed

            override def react(config: Config)(req: Request): IO[Option[NextState]] =
                req match {
                    case ur: UserRequestWithId => ???
                    case pr: PollResults =>
                        logger.trace("New poll results.") >>
                            continue(copy(pollResults = pr))
                    case ft: LocalFinalizationTrigger.Triggered.type =>
                        logger.trace("Finalization was locally triggered.") >>
                            continue(copy(finalizationLocallyTriggered = ft))
                    case unexpected: Unexpected =>
                        panicUnexpectedRequest(this, unexpected)
                }
        }

        object FollowerAwaiting {
            private[State] def apply(
                state: State,
                mempool: Mempool,
                reproducingBlockBrief: BlockBrief.Next,
                awaitingRequestId: RequestId
            ): FollowerAwaiting =
                import state.*
                FollowerAwaiting(
                  connections,
                  logger,
                  pollResults,
                  finalizationLocallyTriggered,
                  mempool,
                  reproducingBlockBrief,
                  awaitingRequestId
                )
        }

        final case class Leader private (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            override val finalizationLocallyTriggered: LocalFinalizationTrigger,
            leadingBlockNumber: BlockNumber,
            startedBlock: Leader.StartedBlock
        ) extends Reactive {
            override type NextState = IdleReactive | Leader | LeaderAwaiting

            override type Unexpected = PreStart.type | BlockBrief.Next

            override def react(config: Config)(req: Request): IO[Option[NextState]] =
                req match {
                    case ur: UserRequestWithId => ???
                    case bc: BlockConfirmed    => ???
                    case pr: PollResults =>
                        logger.trace("New poll results.") >>
                            continue(copy(pollResults = pr))
                    case ft: LocalFinalizationTrigger.Triggered.type =>
                        logger.trace("Finalization was locally triggered.") >>
                            continue(copy(finalizationLocallyTriggered = ft))
                    case unexpected: Unexpected =>
                        panicUnexpectedRequest(this, unexpected)
                }
        }

        object Leader {
            private[State] def apply(
                state: State,
                blockNumber: BlockNumber,
                startedBlock: StartedBlock
            ): Leader =
                import state.*
                Leader(
                  connections,
                  logger,
                  pollResults,
                  finalizationLocallyTriggered,
                  blockNumber,
                  startedBlock
                )

            enum StartedBlock:
                case Started, NotStarted
        }

        final case class LeaderAwaiting private (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            override val finalizationLocallyTriggered: LocalFinalizationTrigger,
            previousBlockBriefConfirmed: BlockBrief.NonFinal
        ) extends Reactive {
            override type NextState = IdleReactive | LeaderAwaiting

            override type Unexpected = PreStart.type | BlockBrief.Next | BlockConfirmed

            override def react(config: Config)(req: Request): IO[Option[NextState]] =
                req match {
                    case ur: UserRequestWithId => ???
                    case pr: PollResults =>
                        logger.trace("New poll results.") >>
                            continue(copy(pollResults = pr))
                    case ft: LocalFinalizationTrigger.Triggered.type =>
                        logger.trace("Finalization was locally triggered.") >>
                            continue(copy(finalizationLocallyTriggered = ft))
                    case unexpected: Unexpected =>
                        panicUnexpectedRequest(this, unexpected)
                }
        }

        object LeaderAwaiting {
            private[State] def apply(
                state: State,
                previousBlockBriefConfirmed: BlockBrief.NonFinal
            ): LeaderAwaiting =
                import state.*
                LeaderAwaiting(
                  connections,
                  logger,
                  pollResults,
                  finalizationLocallyTriggered,
                  previousBlockBriefConfirmed
                )
        }

    }
}

private object BlockWeaverDataStructures {

    /** Simple immutable mempool implementation. Duplicate ledger request IDs are NOT allowed and a
      * runtime exception is thrown since this should never happen. Other components, particularly
      * the peer liaison is in charge or maintaining the integrity of the stream of messages.
      *
      * @param requests
      *   map to store requests
      * @param receivingOrder
      *   vector to store order of request ids
      */
    final case class Mempool(
        requests: Map[RequestId, UserRequestWithId] = Map.empty,
        receivingOrder: Vector[RequestId] = Vector.empty
    ) {

        /** Throws if a duplicate is detected.
          *
          * @param request
          *   an request to add
          * @return
          *   an updated mempool
          */
        def add(
            request: UserRequestWithId
        ): Mempool = {

            val requestId = request.requestId

            require(
              !requests.contains(requestId),
              s"panic - duplicate event id in the pool: $requestId"
            )

            copy(
              requests = requests + (requestId -> request),
              receivingOrder = receivingOrder :+ requestId
            )
        }

        // Remove event - returns new state
        def remove(id: RequestId): Mempool = {
            require(
              requests.contains(id),
              "panic - an attempt to remove a missing event from the mempool"
            )
            copy(
              requests = requests - id,
              receivingOrder = receivingOrder.filterNot(_ == id)
            )
        }

        // Find by ID
        def findById(id: RequestId): Option[UserRequestWithId] = requests.get(id)

        // Iterate in insertion order
        def inOrder: Iterator[UserRequestWithId] =
            receivingOrder.iterator.flatMap(requests.get)

        def isEmpty: Boolean = requests.isEmpty
    }

    object Mempool {
        val empty: Mempool = Mempool()

        def apply(events: Seq[UserRequestWithId]): Mempool =
            events.foldLeft(Mempool.empty)((mempool, request) => mempool.add(request))
    }

    /** So-called "poll results" from the Cardano Liaison, i.e., a set of all utxos ids found at the
      * multisig head address.
      *
      * @param utxos
      *   all utxos found
      */
    final case class PollResults(utxos: Set[TransactionInput])

    object PollResults:
        val empty: PollResults = PollResults(Set.empty)
}
