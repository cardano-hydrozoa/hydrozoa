package hydrozoa.multisig.consensus

import cats.effect.IO
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.mempool.Mempool
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockHeader, BlockNumber, BlockStatus, BlockType}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import org.typelevel.log4cats.Logger

final case class BlockWeaverNew(
    config: BlockWeaverNew.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | BlockWeaverNew.Connections
) extends Actor[IO, BlockWeaverNew.Request] {
    import BlockWeaverNew.*

    private val logger = Logging.loggerIO("BlockWeaver")

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
                startingState <- State.start(config, connections, logger)
                _ <- become(startingState)
            } yield ()
        case x =>
            val msg = s"Unexpected message received before PreStart: $x"
            logger.error(msg) >> IO.raiseError(RuntimeException(msg))
    }

    private def initializeConnections: IO[BlockWeaverNew.Connections] = pendingConnections match {
        case pc: MultisigRegimeManager.PendingConnections =>
            for {
                c <- pc.get
            } yield BlockWeaverNew.Connections(
              jointLedger = c.jointLedger
            )
        case c: BlockWeaverNew.Connections => IO.pure(c)
    }
}

object BlockWeaverNew {

    type Config = CardanoNetwork.Section & OwnHeadPeerPublic.Section

    final case class Connections(
        jointLedger: JointLedger.Handle
    )

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | UserRequestWithId | BlockBrief.Next | BlockConfirmed |
        PollResults | LocalFinalizationTrigger.Triggered.type

    type BlockConfirmed = BlockHeader.Section & Block.Fields.HasFinalizationRequested &
        BlockStatus.MultiSigned & BlockType.Next

    case object PreStart

    sealed trait LocalFinalizationTrigger(val asBoolean: Boolean)

    object LocalFinalizationTrigger {
        case object Triggered extends LocalFinalizationTrigger(true)
        case object NotTriggered extends LocalFinalizationTrigger(false)
    }

    sealed trait State {
        def stateName: String

        /** See [[State.Active]] and [[State.Reactive]]. */
        type NextState <: State.Reactive

        def connections: Connections
        def logger: Logger[IO]
        def pollResults: PollResults
        def finalizationLocallyTriggered: LocalFinalizationTrigger

        final def logStateTransition: IO[Unit] =
            logger.info(s"Becoming $stateName.")

        final def sendStartBlock(config: Config)(blockNumber: BlockNumber): IO[Unit] = for {
            now <- realTimeQuantizedInstant(config.slotConfig)
            blockCreationStartTime = BlockCreationStartTime(now)
            startBlockMsg = StartBlock(blockNumber, blockCreationStartTime)
            _ <- connections.jointLedger ! startBlockMsg
        } yield ()

        final def sendCompleteRegularBlockAsLeader(config: Config): IO[Unit] = for {
            now <- realTimeQuantizedInstant(config.slotConfig)
            blockCreationEndTime = BlockCreationEndTime(now)
            completeBlockMsg = ???
            _ <- connections.jointLedger ! completeBlockMsg
        } yield ()

        final def sendCompleteBlockAsFollower(config: Config)(
            blockBrief: BlockBrief.Next
        ): IO[Unit] = for {
            now <- realTimeQuantizedInstant(config.slotConfig)
            blockCreationEndTime = BlockCreationEndTime(now)
            completeBlockMsg = blockBrief match {
                case x: BlockBrief.Intermediate =>
                    CompleteBlockRegular(
                      Some(x),
                      pollResults.utxos,
                      finalizationLocallyTriggered.asBoolean,
                      blockCreationEndTime
                    )
                case x: BlockBrief.Final =>
                    CompleteBlockFinal(Some(x), blockCreationEndTime)
            }
            _ <- connections.jointLedger ! completeBlockMsg
        } yield ()

    }

    object State {
        def start(
            config: Config,
            connections: Connections,
            logger: Logger[IO]
        ): IO[Idle.AwaitingBlockBrief | Leader.AwaitingConfirmation] =
            for {
                state: Some[Idle.AwaitingBlockBrief | Leader.AwaitingConfirmation] <- Idle(
                  connections = connections,
                  logger = logger,
                  pollResults = PollResults.empty,
                  finalizationLocallyTriggered = LocalFinalizationTrigger.NotTriggered,
                  mempool = Mempool.empty,
                  nextBlockNumber = BlockNumber.zero.increment
                ).act(config)
            } yield state.get

        /** If the next state is reactive, then the transition into it is pure because no immediate
          * actions need to be taken.
          */
        private def pure[S <: State.Reactive](state: S): IO[Some[S]] =
            state.logStateTransition >> IO.pure(Some(state))

        // TODO: implement state machine termination.
        // private def stop(): IO[None.type] = IO.pure(None)

        /** A state with a mempool can store requests in its mempool. */
        sealed trait WithMempool extends State {
            def mempool: Mempool

            def storeRequest(request: UserRequestWithId): IO[Mempool] = for {
                _ <- logger.trace(s"Adding request ID ${request.requestId} to mempool.")
                newMempool <- mempool.addRequest(request) match {
                    case Some(newMempool) => IO.pure(newMempool)
                    case None =>
                        val msg =
                            s"Request ID ${request.requestId} is already in the mempool."
                        logger.error(msg) >>
                            IO.raiseError(RuntimeException(msg))
                }
            } yield newMempool
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

        final case class Idle private[State] (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            override val finalizationLocallyTriggered: LocalFinalizationTrigger,
            override val mempool: Mempool,
            nextBlockNumber: BlockNumber,
        ) extends Active,
              WithMempool {
            override transparent inline def stateName: String = "Idle"

            override type NextState = Idle.AwaitingBlockBrief | Leader.AwaitingConfirmation

            override def act(config: Config): IO[Some[NextState]] = for {
                _ <- logStateTransition
                newState <-
                    if config.ownHeadPeerId.isLeader(nextBlockNumber)
                    then {
                        Leader(this, mempool, nextBlockNumber).act(config)
                    } else {
                        pure(Idle.AwaitingBlockBrief(this, mempool, nextBlockNumber))
                    }
            } yield newState

        }

        object Idle {
            private[State] def apply(
                state: State,
                mempool: Mempool,
                nextBlockNumber: BlockNumber
            ): Idle =
                import state.*
                Idle(
                  connections,
                  logger,
                  pollResults,
                  finalizationLocallyTriggered,
                  mempool,
                  nextBlockNumber
                )

            final case class AwaitingBlockBrief private[State] (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                nextBlockNumber: BlockNumber
            ) extends Reactive,
                  WithMempool {
                override transparent inline def stateName: String = "Idle.AwaitingBlockBrief"

                override type NextState = Idle.AwaitingBlockBrief | Leader.AwaitingConfirmation |
                    Follower.AwaitingRequest

                override type Unexpected = PreStart.type | BlockConfirmed

                override def react(
                    config: Config
                )(req: Request): IO[Option[NextState]] =
                    req match {
                        case ur: UserRequestWithId =>
                            for {
                                newMempool <- storeRequest(ur)
                                newState <- pure(copy(mempool = newMempool))
                            } yield newState

                        case bb: BlockBrief.Next =>
                            logger.info(s"New block brief ${bb.blockNum}.") >>
                                Follower(this, mempool, bb).act(config)

                        case pr: PollResults =>
                            logger.trace("New poll results.") >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            logger.info("Finalization was locally triggered.") >>
                                pure(copy(finalizationLocallyTriggered = ft))

                        case m: Unexpected =>
                            panicUnexpectedRequest(this, m)
                    }
            }

            object AwaitingBlockBrief {
                private[Idle] def apply(
                    state: State,
                    mempool: Mempool,
                    nextBlockNumber: BlockNumber
                ): Idle.AwaitingBlockBrief =
                    import state.*
                    Idle.AwaitingBlockBrief(
                      connections,
                      logger,
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool,
                      nextBlockNumber
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
              WithMempool {
            override transparent inline def stateName: String = "Follower"

            override type NextState = Idle.AwaitingBlockBrief | Leader.AwaitingConfirmation |
                Follower.AwaitingRequest

            def act(config: Config): IO[Some[NextState]] = for {
                _ <- logStateTransition
                extractionResult <- extractAndSendRequestsFromMempool
                newState <- extractionResult match {
                    case Mempool.Extraction.Complete(extractedRequests, survivingMempool) =>
                        val nextBlockNumber = reproducingBlockBrief.blockNum.increment
                        for {
                            newState <- Idle(this, survivingMempool, nextBlockNumber).act(config)
                        } yield newState
                    case result: Mempool.Extraction.Incomplete =>
                        pure(Follower.AwaitingRequest(this, reproducingBlockBrief, result))
                }
            } yield newState

            private def extractAndSendRequestsFromMempool: IO[Mempool.Extraction.Result] = {
                val requestIds: List[RequestId] = reproducingBlockBrief.events.map(_._1)
                val newExtractionResult = mempool.extractRequestsWhile(requestIds)
                import newExtractionResult.*
                for {
                    _ <- logger.trace(
                      "Extracted requests from mempool. Sending them to joint ledger: " +
                          s"${extractedRequests.map(_.requestId.asI64)}"
                    )
                    _ <- extractedRequests.traverse_(connections.jointLedger ! _)
                } yield mempool.extractRequestsWhile(requestIds)
            }
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

            final case class AwaitingRequest private (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                reproducingBlockBrief: BlockBrief.Next,
                incompleteExtraction: Mempool.Extraction.Incomplete
            ) extends Reactive,
                  WithMempool {
                override transparent inline def stateName: String = "Follower.AwaitingRequest"

                override type NextState = Idle.AwaitingBlockBrief | Leader.AwaitingConfirmation |
                    Follower.AwaitingRequest

                override type Unexpected = PreStart.type | BlockBrief.Next | BlockConfirmed

                override def react(config: Config)(req: Request): IO[Option[NextState]] =
                    req match {
                        case ur: UserRequestWithId =>
                            if ur.requestId == incompleteExtraction.awaitingRequestId then
                                for {
                                    newExtractionResult <- extractAndSendRequestsFromMempool
                                    newState <- newExtractionResult match {
                                        case Mempool.Extraction
                                                .Complete(extractedRequests, survivingMempool) =>
                                            val nextBlockNumber =
                                                reproducingBlockBrief.blockNum.increment
                                            Idle(this, survivingMempool, nextBlockNumber).act(
                                              config
                                            )
                                        case result: Mempool.Extraction.Incomplete =>
                                            pure(
                                              Follower.AwaitingRequest(
                                                this,
                                                reproducingBlockBrief,
                                                result
                                              )
                                            )
                                    }
                                } yield newState
                            else
                                for {
                                    newMempool <- storeRequest(ur)
                                    newState <- pure(copy(mempool = newMempool))
                                } yield newState

                        case pr: PollResults =>
                            logger.trace("New poll results.") >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            logger.info("Finalization was locally triggered.") >>
                                pure(copy(finalizationLocallyTriggered = ft))

                        case unexpected: Unexpected =>
                            panicUnexpectedRequest(this, unexpected)
                    }

                private def extractAndSendRequestsFromMempool: IO[Mempool.Extraction.Result] = {
                    val allRequestIds: List[RequestId] = reproducingBlockBrief.events.map(_._1)
                    val requestIds =
                        allRequestIds.dropWhile(_ != incompleteExtraction.awaitingRequestId)
                    val newExtractionResult = mempool.extractRequestsWhile(requestIds)
                    import newExtractionResult.*
                    for {
                        _ <- logger.trace(
                          "Extracted more requests from mempool: " +
                              s"${newExtractionResult.extractedRequests.map(_.requestId.asI64)}"
                        )
                        _ <- extractedRequests.traverse_(connections.jointLedger ! _)
                    } yield newExtractionResult
                }
            }

            object AwaitingRequest {
                private[State] def apply(
                    state: State,
                    reproducingBlockBrief: BlockBrief.Next,
                    incompleteExtraction: Mempool.Extraction.Incomplete
                ): Follower.AwaitingRequest =
                    import state.*
                    Follower.AwaitingRequest(
                      connections,
                      logger,
                      pollResults,
                      finalizationLocallyTriggered,
                      incompleteExtraction.survivingMempool,
                      reproducingBlockBrief,
                      incompleteExtraction
                    )
            }
        }

        final case class Leader(
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            override val finalizationLocallyTriggered: LocalFinalizationTrigger,
            override val mempool: Mempool,
            leadingBlockNum: BlockNumber
        ) extends Active,
              WithMempool {
            override transparent inline def stateName: String = "Leader"

            override type NextState = Leader.AwaitingConfirmation

            override def act(config: Config): IO[Some[NextState]] = for {
                _ <- logStateTransition
                now <- realTimeQuantizedInstant(config.slotConfig)
                _ <- connections.jointLedger ! StartBlock(
                  leadingBlockNum,
                  BlockCreationStartTime(now)
                )
                newState <- IO.pure(Some(???))
            } yield newState

            private def extractAndSendRequestsInOrder: IO[List[UserRequestWithId]] = {
                val requests = mempool.extractRequestsInOrder
                for {
                    _ <- logger.trace(
                      "Extracting remaining requests from mempool in order of arrival. " +
                          s"First twenty request IDs: ${requests.iterator.take(20).map(_.requestId.asI64)}"
                    )
                    _ <- requests.traverse_(connections.jointLedger ! _)
                } yield requests
            }
        }

        object Leader {
            private[State] def apply(
                state: State,
                mempool: Mempool,
                leadingBlockNum: BlockNumber,
            ): Leader = {
                import state.*
                Leader(
                  connections,
                  logger,
                  pollResults,
                  finalizationLocallyTriggered,
                  mempool,
                  leadingBlockNum
                )
            }

            final case class AwaitingConfirmation private (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                leadingBlockNumber: BlockNumber,
                // TODO: Add the MajorBlockWakeupTime, so we can set the timeout.
                startedBlock: Leader.AwaitingConfirmation.StartedBlock
            ) extends Reactive {
                override transparent inline def stateName: String = "Leader.AwaitingConfirmation"

                override type NextState = Idle.AwaitingBlockBrief | Leader.AwaitingConfirmation |
                    Leader.AwaitingRequest

                override type Unexpected = PreStart.type | BlockBrief.Next

                override def react(config: Config)(req: Request): IO[Option[NextState]] =
                    req match {
                        case ur: UserRequestWithId => ???

                        case bc: BlockConfirmed => ???

                        case pr: PollResults =>
                            logger.trace("New poll results.") >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            logger.info("Finalization was locally triggered.") >>
                                pure(copy(finalizationLocallyTriggered = ft))

                        case unexpected: Unexpected =>
                            panicUnexpectedRequest(this, unexpected)
                    }
            }

            object AwaitingConfirmation {
                private[State] def apply(
                    state: State,
                    blockNumber: BlockNumber,
                    startedBlock: StartedBlock
                ): Leader.AwaitingConfirmation =
                    import state.*
                    Leader.AwaitingConfirmation(
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

            final case class AwaitingRequest private (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                previousBlockBriefConfirmed: BlockBrief.NonFinal
            ) extends Reactive {
                override transparent inline def stateName: String = "Leader.AwaitingConfirmation"

                override type NextState = Idle.AwaitingBlockBrief | Leader.AwaitingConfirmation |
                    Leader.AwaitingRequest

                override type Unexpected = PreStart.type | BlockBrief.Next | BlockConfirmed

                override def react(config: Config)(req: Request): IO[Option[NextState]] =
                    req match {
                        case ur: UserRequestWithId => ???

                        case pr: PollResults =>
                            logger.trace("New poll results.") >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            logger.info("Finalization was locally triggered.") >>
                                pure(copy(finalizationLocallyTriggered = ft))

                        case unexpected: Unexpected =>
                            panicUnexpectedRequest(this, unexpected)
                    }
            }

            object AwaitingRequest {
                private[State] def apply(
                    state: State,
                    previousBlockBriefConfirmed: BlockBrief.NonFinal
                ): Leader.AwaitingRequest =
                    import state.*
                    Leader.AwaitingRequest(
                      connections,
                      logger,
                      pollResults,
                      finalizationLocallyTriggered,
                      previousBlockBriefConfirmed
                    )
            }
        }
    }
}
