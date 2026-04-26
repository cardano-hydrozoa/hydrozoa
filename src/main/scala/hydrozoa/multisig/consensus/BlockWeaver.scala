package hydrozoa.multisig.consensus
import cats.effect.{Fiber, IO}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.BlockWeaver.State.Leader.AwaitingConfirmation.StartedBlock.{NotStarted, Started}
import hydrozoa.multisig.consensus.mempool.Mempool
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockNumber, BlockType}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import org.typelevel.log4cats.Logger

final case class BlockWeaver(
    config: BlockWeaver.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | BlockWeaver.ConnectionsPartial
) extends Actor[IO, BlockWeaver.Request] {
    import BlockWeaver.*

    private val logger = Logging.loggerIO(s"BlockWeaver.${config.ownHeadPeerNum}")

    override def preStart: IO[Unit] = for {
        _ <- context.self ! BlockWeaver.PreStart
        _ <- context.become(receive)
    } yield ()

    private def become(state: BlockWeaver.State.Reactive): IO[Unit] =
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

    override def receive: Receive[IO, BlockWeaver.Request] = PartialFunction.fromFunction {
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

    private def initializeConnections: IO[BlockWeaver.Connections] = pendingConnections match {
        case pc: MultisigRegimeManager.PendingConnections =>
            for {
                c <- pc.get
            } yield BlockWeaver.Connections(
              blockWeaver = context.self,
              jointLedger = c.jointLedger
            )
        case c: BlockWeaver.ConnectionsPartial => IO.pure(c(context.self))
    }
}

object BlockWeaver {
    type Config = CardanoNetwork.Section & OwnHeadPeerPublic.Section &
        NodeOperationMultisigConfig.Section

    final case class Connections private[BlockWeaver] (
        blockWeaver: BlockWeaver.Handle,
        jointLedger: JointLedger.Handle
    )

    final case class ConnectionsPartial(jointLedger: JointLedger.Handle) {
        def apply(blockWeaver: BlockWeaver.Handle): Connections = Connections(
          blockWeaver = blockWeaver,
          jointLedger = jointLedger
        )
    }

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | UserRequestWithId | BlockBrief.Next | Block.MultiSigned |
        PollResults | LocalFinalizationTrigger.Triggered.type | Wakeup

    case object PreStart

    case class Wakeup(blockNumber: BlockNumber)

    sealed trait LocalFinalizationTrigger(val asBoolean: Boolean)

    object LocalFinalizationTrigger {
        case object Triggered extends LocalFinalizationTrigger(true)
        case object NotTriggered extends LocalFinalizationTrigger(false)
    }

    sealed trait State {
        def stateName: String

        /** See [[State.Active]] and [[State.Reactive]]. */
        type NextReactiveState <: State.Reactive

        def connections: Connections
        def logger: Logger[IO]
        def pollResults: PollResults
        def finalizationLocallyTriggered: LocalFinalizationTrigger

        final def stop(): IO[None.type] =
            logger.info("Stopping") >> IO.pure(None)

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
            completeBlockMsg = CompleteBlockRegular(
              None,
              pollResults,
              finalizationLocallyTriggered,
              blockCreationEndTime
            )
            _ <- connections.jointLedger ! completeBlockMsg
        } yield ()

        final def sendCompleteFinalBlockAsLeader(config: Config): IO[Unit] = for {
            now <- realTimeQuantizedInstant(config.slotConfig)
            blockCreationEndTime = BlockCreationEndTime(now)
            completeBlockMsg = CompleteBlockFinal(
              None,
              blockCreationEndTime
            )
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
                      pollResults,
                      finalizationLocallyTriggered,
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
        ): IO[Follower.AwaitingBlockBrief | Leader.AwaitingConfirmation] =
            for {
                state: Some[Follower.AwaitingBlockBrief | Leader.AwaitingConfirmation] <-
                    DecidingRole(
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
            def act(config: Config): IO[Option[NextReactiveState]]
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

            def react(config: Config)(req: Request): IO[Option[NextReactiveState]]

            final def panicUnexpectedRequest(
                state: State.Reactive,
                unexpected: Unexpected
            ): IO[None.type] =
                val msg =
                    s"Unexpectedly received in ${state.stateName.toString} state: ${unexpected.toString}"
                logger.error(msg) >>
                    IO.raiseError(RuntimeException(msg))
        }

        final case class DecidingRole private[State] (
            override val connections: Connections,
            override val logger: Logger[IO],
            override val pollResults: PollResults,
            override val finalizationLocallyTriggered: LocalFinalizationTrigger,
            override val mempool: Mempool,
            nextBlockNumber: BlockNumber,
        ) extends Active,
              WithMempool {
            override transparent inline def stateName: String = "DecidingRole"

            export DecidingRole.NextReactiveState

            override def act(config: Config): IO[Some[NextReactiveState]] = for {
                _ <- logStateTransition
                newState <-
                    if config.ownHeadPeerId.isLeader(nextBlockNumber)
                    then {
                        Leader.ProcessingReadyRequests(this, mempool, nextBlockNumber).act(config)
                    } else {
                        pure(Follower.AwaitingBlockBrief(this, mempool, nextBlockNumber))
                    }
            } yield newState

        }

        object DecidingRole {
            type NextReactiveState = Follower.AwaitingBlockBrief |
                Leader.ProcessingReadyRequests.NextReactiveState

            private[State] def apply(
                stateToTransitionFrom: State,
                mempool: Mempool,
                nextBlockNumber: BlockNumber
            ): DecidingRole =
                import stateToTransitionFrom.*
                DecidingRole(
                  connections,
                  logger,
                  pollResults,
                  finalizationLocallyTriggered,
                  mempool,
                  nextBlockNumber
                )
        }

        object Follower {
            final case class AwaitingBlockBrief private (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                nextBlockNumber: BlockNumber
            ) extends Reactive,
                  WithMempool {
                override transparent inline def stateName: String = "Follower.AwaitingBlockBrief"

                export Follower.AwaitingBlockBrief.{NextReactiveState, Unexpected}

                override def react(
                    config: Config
                )(req: Request): IO[Option[NextReactiveState]] =
                    req match {
                        case ur: UserRequestWithId =>
                            for {
                                newMempool <- storeRequest(ur)
                                newState <- pure(copy(mempool = newMempool))
                            } yield newState

                        case bb: BlockBrief.Next =>
                            logger.info(s"New block brief ${bb.blockNum}.") >>
                                Follower.ProcessingReadyRequests(this, mempool, bb).act(config)

                        case pr: PollResults =>
                            logger.trace("New poll results.") >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            logger.info("Finalization was locally triggered.") >>
                                pure(copy(finalizationLocallyTriggered = ft))

                        case w: Wakeup =>
                            logger.trace(
                              s"Unexpected wakeup for block ${w.blockNumber}, ignoring."
                            ) >>
                                pure(this)

                        case bc: Block.MultiSigned =>
                            logger.trace(
                              s"Ignoring confirmed block ${bc.blockNum} — was leader, now follower."
                            ) >> pure(this)

                        case unexpected: Unexpected =>
                            panicUnexpectedRequest(this, unexpected)
                    }
            }

            object AwaitingBlockBrief {
                type NextReactiveState = Follower.AwaitingBlockBrief |
                    Follower.ProcessingReadyRequests.NextReactiveState
                type Unexpected = PreStart.type

                private[State] def apply(
                    state: State,
                    mempool: Mempool,
                    nextBlockNumber: BlockNumber
                ): Follower.AwaitingBlockBrief =
                    import state.*
                    Follower.AwaitingBlockBrief(
                      connections,
                      logger,
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool,
                      nextBlockNumber
                    )
            }

            /** Processing ready requests, i.e. those, which are already in the mempool.
              */
            final case class ProcessingReadyRequests private (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                reproducingBlockBrief: BlockBrief.Next,
            ) extends Active,
                  WithMempool {
                override transparent inline def stateName: String =
                    "Follower.ProcessingReadyRequests"

                export Follower.ProcessingReadyRequests.NextReactiveState

                def act(config: Config): IO[Some[NextReactiveState]] = for {
                    _ <- logStateTransition
                    _ <- connections.jointLedger ! StartBlock(
                      reproducingBlockBrief.blockNum,
                      reproducingBlockBrief.startTime
                    )
                    extractionResult <- extractAndSendRequestsFromMempool
                    newState <- extractionResult match {
                        case Mempool.Extraction.Complete(extractedRequests, survivingMempool) =>
                            val nextBlockNumber = reproducingBlockBrief.blockNum.increment
                            for {
                                _ <- sendCompleteBlockAsFollower(config)(reproducingBlockBrief)
                                newState <- DecidingRole(this, survivingMempool, nextBlockNumber)
                                    .act(config)
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
                              s"${extractedRequests.map(r =>
                                      s"(${r.requestId.peerNum}:${r.requestId.requestNum})"
                                  )}"
                        )
                        _ <- extractedRequests.traverse_(connections.jointLedger ! _)
                    } yield newExtractionResult
                }
            }

            object ProcessingReadyRequests {
                type NextReactiveState = DecidingRole.NextReactiveState | Follower.AwaitingRequest

                private[State] def apply(
                    state: State,
                    mempool: Mempool,
                    reproducingBlockBrief: BlockBrief.Next
                ): Follower.ProcessingReadyRequests = {
                    import state.*
                    Follower.ProcessingReadyRequests(
                      connections,
                      logger,
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool,
                      reproducingBlockBrief
                    )
                }
            }

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

                export Follower.AwaitingRequest.{NextReactiveState, Unexpected}

                override def react(config: Config)(req: Request): IO[Option[NextReactiveState]] =
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
                                            for {
                                                _ <- sendCompleteBlockAsFollower(config)(
                                                  reproducingBlockBrief
                                                )
                                                newState <- DecidingRole(
                                                  this,
                                                  survivingMempool,
                                                  nextBlockNumber
                                                ).act(config)
                                            } yield newState
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

                        case w: Wakeup =>
                            logger.trace(
                              s"Unexpected wakeup for block ${w.blockNumber}, ignoring."
                            ) >>
                                pure(this)

                        case bc: Block.MultiSigned =>
                            logger.trace(
                              s"Ignoring confirmed block ${bc.blockNum} — was leader, now follower."
                            ) >> pure(this)

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
                              s"${newExtractionResult.extractedRequests.map(r =>
                                      s"(${r.requestId.peerNum}:${r.requestId.requestNum})"
                                  )}"
                        )
                        _ <- extractedRequests.traverse_(connections.jointLedger ! _)
                    } yield newExtractionResult
                }
            }

            private object AwaitingRequest {
                type NextReactiveState = DecidingRole.NextReactiveState | Follower.AwaitingRequest
                type Unexpected = PreStart.type | BlockBrief.Next

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

        object Leader {
            final case class ProcessingReadyRequests private (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                leadingBlockNum: BlockNumber
            ) extends Active,
                  WithMempool {
                override transparent inline def stateName: String = "Leader.ProcessingReadyRequests"

                export Leader.ProcessingReadyRequests.NextReactiveState

                override def act(config: Config): IO[Some[NextReactiveState]] = for {
                    _ <- logStateTransition
                    now <- realTimeQuantizedInstant(config.slotConfig)
                    requests <- extractRequestsInOrder
                    isBlockStarted <-
                        if requests.isEmpty
                        then IO.pure(NotStarted)
                        else
                            for {
                                _ <- sendStartBlock(config)(leadingBlockNum)
                                _ <- requests.traverse_(connections.jointLedger ! _)
                            } yield Started
                    newState <- pure(
                      Leader.AwaitingConfirmation(
                        this,
                        leadingBlockNum,
                        isBlockStarted
                      )
                    )
                } yield newState

                private def extractRequestsInOrder: IO[List[UserRequestWithId]] = {
                    val requests = mempool.extractRequestsInOrder
                    for {
                        _ <- logger.trace(
                          "Extracting remaining requests from mempool in order of arrival. " +
                              s"First twenty request IDs: ${requests.iterator.take(20).map(r => s"(${r.requestId.peerNum}:${r.requestId.requestNum})").mkString(", ")}"
                        )

                    } yield requests
                }
            }

            object ProcessingReadyRequests {
                type NextReactiveState = Leader.AwaitingConfirmation

                private[State] def apply(
                    state: State,
                    mempool: Mempool,
                    leadingBlockNum: BlockNumber,
                ): Leader.ProcessingReadyRequests = {
                    import state.*
                    Leader.ProcessingReadyRequests(
                      connections,
                      logger,
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool,
                      leadingBlockNum
                    )
                }
            }

            final case class AwaitingConfirmation private (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                leadingBlockNumber: BlockNumber,
                isBlockStarted: Leader.AwaitingConfirmation.StartedBlock
            ) extends Reactive {
                override transparent inline def stateName: String = "Leader.AwaitingConfirmation"

                export Leader.AwaitingConfirmation.{NextReactiveState, Unexpected}

                override def react(config: Config)(req: Request): IO[Option[NextReactiveState]] = {
                    req match {
                        case ur: UserRequestWithId =>
                            // First block is implicitly confirmed, so we exit immediately back to
                            // the DecidingRole state.
                            def completeFirstBlock = for {
                                _ <- logger.trace(
                                  s"Completing first block immediately with request (${ur.requestId.peerNum}:${ur.requestId.requestNum})"
                                ) >> sendCompleteRegularBlockAsLeader(config)
                                newState <- DecidingRole(
                                  connections,
                                  logger,
                                  pollResults,
                                  finalizationLocallyTriggered,
                                  mempool = Mempool.empty,
                                  nextBlockNumber = leadingBlockNumber.increment
                                ).act(config)
                            } yield newState

                            for {
                                _ <- IO.whenA(isBlockStarted == NotStarted)(
                                  sendStartBlock(config)(leadingBlockNumber)
                                )

                                _ <- logger.trace(
                                  s"Sending to joint ledger: request (${ur.requestId.peerNum}:${ur.requestId.requestNum})"
                                )
                                _ <- connections.jointLedger ! ur
                                res <-
                                    if leadingBlockNumber == BlockNumber.zero.increment
                                    then completeFirstBlock
                                    else pure(this.copy(isBlockStarted = Started))
                            } yield res

                        case bc: Block.MultiSigned.NonFinal =>
                            def completeBlockRegular =
                                sendCompleteRegularBlockAsLeader(config) >>
                                    DecidingRole(
                                      connections,
                                      logger,
                                      pollResults,
                                      finalizationLocallyTriggered = finalizationLocallyTriggered,
                                      mempool = Mempool.empty,
                                      nextBlockNumber = leadingBlockNumber.increment
                                    ).act(config)

                            def completeBlockFinal =
                                sendCompleteFinalBlockAsLeader(config) >> IO.pure(None)

                            def completeNextBlock =
                                if bc.finalizationRequested || finalizationLocallyTriggered.asBoolean
                                then completeBlockFinal
                                else completeBlockRegular

                            // Iff the block confirmed is the previous block
                            if bc.blockNum.increment == leadingBlockNumber
                            then
                                if isBlockStarted == Started
                                then completeNextBlock
                                else
                                    for {
                                        now <- realTimeQuantizedInstant(config.slotConfig)
                                        sleepDuration = bc.headerNonFinal.majorBlockWakeupTime - now
                                        fiber <- sleepSendWakeup(sleepDuration).start
                                        ret <- pure(
                                          Leader.AwaitingRequest(
                                            this,
                                            previousBlockConfirmed = bc,
                                            fiber
                                          )
                                        )
                                    } yield ret
                            else {
                                val msg = "Received wrong block number for confirmed block. We are producing" +
                                    s" $leadingBlockNumber, but the confirmed block that we received is ${bc.blockNum}"
                                logger.error(msg) >> IO.raiseError(RuntimeException(msg))
                            }

                        case pr: PollResults =>
                            logger.trace("New poll results.") >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            logger.info("Finalization was locally triggered.") >>
                                pure(copy(finalizationLocallyTriggered = ft))

                        case w: Wakeup =>
                            logger.trace(
                              s"Unexpected wakeup for block ${w.blockNumber}, ignoring."
                            ) >>
                                pure(this)

                        case unexpected: Unexpected =>
                            panicUnexpectedRequest(this, unexpected)
                    }
                }

                private def sleepSendWakeup(sleepDuration: QuantizedFiniteDuration): IO[Unit] = {
                    IO.sleep(sleepDuration.finiteDuration) >>
                        (connections.blockWeaver ! Wakeup(this.leadingBlockNumber))
                }
            }

            object AwaitingConfirmation {
                type NextReactiveState = DecidingRole.NextReactiveState |
                    Leader.AwaitingConfirmation | Leader.AwaitingRequest

                type Unexpected = PreStart.type | BlockBrief.Next |
                    (Block.MultiSigned & BlockType.Final)

                private[State] def apply(
                    state: State,
                    blockNumber: BlockNumber,
                    isBlockStarted: StartedBlock
                ): Leader.AwaitingConfirmation =
                    import state.*
                    Leader.AwaitingConfirmation(
                      connections,
                      logger,
                      pollResults,
                      finalizationLocallyTriggered,
                      blockNumber,
                      isBlockStarted
                    )

                enum StartedBlock:
                    case Started, NotStarted
            }

            final case class AwaitingRequest private (
                override val connections: Connections,
                override val logger: Logger[IO],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                previousBlockConfirmed: Block.MultiSigned.NonFinal,
                wakeupFiber: Fiber[IO, Throwable, Unit]
            ) extends Reactive {
                override transparent inline def stateName: String = "Leader.AwaitingRequest"

                export Leader.AwaitingRequest.{NextReactiveState, Unexpected}

                private val currentBlockNumber = previousBlockConfirmed.blockNum.increment

                override def react(config: Config)(req: Request): IO[Option[NextReactiveState]] = {
                    def completeBlockRegular = sendCompleteRegularBlockAsLeader(config) >>
                        DecidingRole(
                          this,
                          mempool = Mempool.empty,
                          nextBlockNumber = currentBlockNumber.increment
                        ).act(config)

                    def completeBlock = {
                        if finalizationLocallyTriggered.asBoolean || previousBlockConfirmed.finalizationRequested
                        then sendCompleteFinalBlockAsLeader(config) >> stop()
                        else completeBlockRegular
                    }

                    req match {
                        case ur: UserRequestWithId =>
                            for {
                                _ <- sendStartBlock(config)(currentBlockNumber)
                                _ <- connections.jointLedger ! ur
                                newState <- completeBlock
                            } yield newState

                        case w: Wakeup =>
                            def forceMajorBlock =
                                for {
                                    _ <- logger.info(
                                      "Wakeup for the current block is received, force major block"
                                    )
                                    _ <- sendStartBlock(config)(currentBlockNumber)
                                    newState <- completeBlock
                                } yield newState

                            if w.blockNumber == currentBlockNumber
                            then forceMajorBlock
                            else {
                                (if w.blockNumber > currentBlockNumber
                                 then
                                     logger.warn(
                                       s"Ignoring wakeup for a future block ${w.blockNumber}, current block: ${currentBlockNumber}"
                                     )
                                 else
                                     logger.info(
                                       s"Ignoring wakeup for preceding block ${w.blockNumber}, current block: ${currentBlockNumber}"
                                     )
                                ) >> pure(this)
                            }

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
            }

            private object AwaitingRequest {
                type NextReactiveState = DecidingRole.NextReactiveState | Leader.AwaitingRequest

                type Unexpected = PreStart.type | BlockBrief.Next | Block.MultiSigned

                private[State] def apply(
                    state: State,
                    previousBlockConfirmed: Block.MultiSigned.NonFinal,
                    wakeupFiber: Fiber[IO, Throwable, Unit]
                ): Leader.AwaitingRequest =
                    import state.*
                    Leader.AwaitingRequest(
                      connections,
                      logger,
                      pollResults,
                      finalizationLocallyTriggered,
                      previousBlockConfirmed,
                      wakeupFiber
                    )
            }
        }
    }
}
