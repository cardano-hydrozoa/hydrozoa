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
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.mempool.Mempool
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockNumber, BlockType}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}

final case class BlockWeaver(
    config: BlockWeaver.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | BlockWeaver.Connections
) extends Actor[IO, BlockWeaver.Request] {
    import BlockWeaver.*

    private val logger = Logging.loggerIO("BlockWeaver")

    private def wakeup: IO[Unit] = context.self ! Wakeup

    override def preStart: IO[Unit] = for {
        _ <- context.self ! BlockWeaver.PreStart
        _ <- context.become(receive)
    } yield ()

    private def become(state: this.State.Reactive): IO[Unit] =
        context.become(
          PartialFunction.fromFunction(req =>
              for {
                  // Handle the request using the current state's handler
                  mNewState <- state.react(req)
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
                startingState <- State.start(connections)
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
              jointLedger = c.jointLedger
            )
        case c: BlockWeaver.Connections => IO.pure(c)
    }

    sealed trait State {
        def stateName: String

        /** See [[State.Active]] and [[State.Reactive]]. */
        type NextReactiveState <: State.Reactive

        def connections: Connections
        def pollResults: PollResults
        def finalizationLocallyTriggered: LocalFinalizationTrigger

        final def stop: IO[None.type] =
            logger.info("Stopping") >> IO.pure(None)

        final def logStateTransition: IO[Unit] =
            logger.info(s"Becoming $stateName.")

        final def sendStartBlock(blockNumber: BlockNumber): IO[Unit] = for {
            now <- realTimeQuantizedInstant(config.slotConfig)
            blockCreationStartTime = BlockCreationStartTime(now)
            startBlockMsg = StartBlock(blockNumber, blockCreationStartTime)
            _ <- connections.jointLedger ! startBlockMsg
        } yield ()

        final def sendCompleteRegularBlockAsLeader: IO[Unit] = for {
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

        final def sendCompleteFinalBlockAsLeader: IO[Unit] = for {
            now <- realTimeQuantizedInstant(config.slotConfig)
            blockCreationEndTime = BlockCreationEndTime(now)
            completeBlockMsg = CompleteBlockFinal(
              None,
              blockCreationEndTime
            )
            _ <- connections.jointLedger ! completeBlockMsg
        } yield ()

        final def sendCompleteBlockAsFollower(
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
            connections: Connections,
        ): IO[Follower.AwaitingBlockBrief | Leader.AwaitingConfirmation] =
            for {
                state: Some[Follower.AwaitingBlockBrief | Leader.AwaitingConfirmation] <-
                    DecidingRole(
                      connections = connections,
                      pollResults = PollResults.empty,
                      finalizationLocallyTriggered = LocalFinalizationTrigger.NotTriggered,
                      mempool = Mempool.empty,
                      nextBlockNumber = BlockNumber.zero.increment
                    ).act
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
            def act: IO[Option[NextReactiveState]]
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

            def react(req: Request): IO[Option[NextReactiveState]]

            final def panicUnexpectedRequest(
                state: State.Reactive,
                unexpected: Unexpected
            ): IO[None.type] =
                val msg =
                    s"Unexpectedly received in ${state.toString} state: ${unexpected.toString}"
                logger.error(msg) >>
                    IO.raiseError(RuntimeException(msg))
        }

        final case class DecidingRole private[State] (
            override val connections: Connections,
            override val pollResults: PollResults,
            override val finalizationLocallyTriggered: LocalFinalizationTrigger,
            override val mempool: Mempool,
            nextBlockNumber: BlockNumber,
        ) extends Active,
              WithMempool {
            override transparent inline def stateName: String = "DecidingRole"

            export DecidingRole.NextReactiveState

            override def act: IO[Some[NextReactiveState]] = for {
                _ <- logStateTransition
                newState <-
                    if config.ownHeadPeerId.isLeader(nextBlockNumber)
                    then {
                        Leader.ProcessingReadyRequests(this, mempool, nextBlockNumber).act
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
                  pollResults,
                  finalizationLocallyTriggered,
                  mempool,
                  nextBlockNumber
                )
        }

        object Follower {
            final case class AwaitingBlockBrief private (
                override val connections: Connections,
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                nextBlockNumber: BlockNumber
            ) extends Reactive,
                  WithMempool {
                override transparent inline def stateName: String = "Follower.AwaitingBlockBrief"

                export Follower.AwaitingBlockBrief.{NextReactiveState, Unexpected}

                override def react(req: Request): IO[Option[NextReactiveState]] =
                    req match {
                        case ur: UserRequestWithId =>
                            for {
                                newMempool <- storeRequest(ur)
                                newState <- pure(copy(mempool = newMempool))
                            } yield newState

                        case bb: BlockBrief.Next =>
                            logger.info(s"New block brief ${bb.blockNum}.") >>
                                Follower.ProcessingReadyRequests(this, mempool, bb).act

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
                type NextReactiveState = Follower.AwaitingBlockBrief |
                    Follower.ProcessingReadyRequests.NextReactiveState
                type Unexpected = PreStart.type | Block.MultiSigned | Wakeup.type

                private[State] def apply(
                    state: State,
                    mempool: Mempool,
                    nextBlockNumber: BlockNumber
                ): Follower.AwaitingBlockBrief =
                    import state.*
                    Follower.AwaitingBlockBrief(
                      connections,
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool,
                      nextBlockNumber
                    )
            }

            final case class ProcessingReadyRequests private (
                override val connections: Connections,
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                reproducingBlockBrief: BlockBrief.Next,
            ) extends Active,
                  WithMempool {
                override transparent inline def stateName: String =
                    "Follower.ProcessingReadyRequests"

                export Follower.ProcessingReadyRequests.NextReactiveState

                def act: IO[Some[NextReactiveState]] = for {
                    _ <- logStateTransition
                    extractionResult <- extractAndSendRequestsFromMempool
                    newState <- extractionResult match {
                        case Mempool.Extraction.Complete(extractedRequests, survivingMempool) =>
                            val nextBlockNumber = reproducingBlockBrief.blockNum.increment
                            for {
                                newState <- DecidingRole(
                                  this,
                                  survivingMempool,
                                  nextBlockNumber
                                ).act
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
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool,
                      reproducingBlockBrief
                    )
                }
            }

            final case class AwaitingRequest private (
                override val connections: Connections,
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                reproducingBlockBrief: BlockBrief.Next,
                incompleteExtraction: Mempool.Extraction.Incomplete
            ) extends Reactive,
                  WithMempool {
                override transparent inline def stateName: String = "Follower.AwaitingRequest"

                export Follower.AwaitingRequest.{NextReactiveState, Unexpected}

                override def react(req: Request): IO[Option[NextReactiveState]] =
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
                                            DecidingRole(
                                              this,
                                              survivingMempool,
                                              nextBlockNumber
                                            ).act
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

            private object AwaitingRequest {
                type NextReactiveState = DecidingRole.NextReactiveState | Follower.AwaitingRequest
                type Unexpected = PreStart.type | BlockBrief.Next | Block.MultiSigned | Wakeup.type

                private[State] def apply(
                    state: State,
                    reproducingBlockBrief: BlockBrief.Next,
                    incompleteExtraction: Mempool.Extraction.Incomplete
                ): Follower.AwaitingRequest =
                    import state.*
                    Follower.AwaitingRequest(
                      connections,
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
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                leadingBlockNum: BlockNumber
            ) extends Active,
                  WithMempool {
                override transparent inline def stateName: String = "Leader.ProcessingReadyRequests"

                export Leader.ProcessingReadyRequests.NextReactiveState

                override def act: IO[Some[NextReactiveState]] = for {
                    _ <- logStateTransition
                    now <- realTimeQuantizedInstant(config.slotConfig)
                    requests <- extractRequestsInOrder
                    isBlockStarted <-
                        if requests.isEmpty
                        then IO.pure(Leader.AwaitingConfirmation.StartedBlock.NotStarted)
                        else
                            for {
                                _ <- sendStartBlock(leadingBlockNum)
                                _ <- requests.traverse_(connections.jointLedger ! _)
                            } yield Leader.AwaitingConfirmation.StartedBlock.Started
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
                              s"First twenty request IDs: ${requests.iterator.take(20).map(_.requestId.asI64)}"
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
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool,
                      leadingBlockNum
                    )
                }
            }

            final case class AwaitingConfirmation private (
                override val connections: Connections,
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                leadingBlockNumber: BlockNumber,
                isBlockStarted: Leader.AwaitingConfirmation.StartedBlock
            ) extends Reactive {
                override transparent inline def stateName: String = "Leader.AwaitingConfirmation"

                export Leader.AwaitingConfirmation.{NextReactiveState, Unexpected}

                override def react(req: Request): IO[Option[NextReactiveState]] =
                    req match {
                        case ur: UserRequestWithId =>
                            // First block is implicitly confirmed, so we exit immediately back to
                            // the DecidingRole state.
                            def completeFirstBlock = for {
                                _ <- logger.trace(
                                  s"Completing first block immediately with request ${ur.requestId.asI64}"
                                ) >> sendCompleteRegularBlockAsLeader
                                newState <- DecidingRole(
                                  connections,
                                  pollResults,
                                  finalizationLocallyTriggered,
                                  mempool = Mempool.empty,
                                  nextBlockNumber = leadingBlockNumber.increment
                                ).act
                            } yield newState

                            for {
                                _ <- IO.whenA(
                                  isBlockStarted ==
                                      Leader.AwaitingConfirmation.StartedBlock.NotStarted
                                )(
                                  sendStartBlock(leadingBlockNumber)
                                )

                                _ <- logger.trace(
                                  s"Sending to joint ledger: request ${ur.requestId.asI64}"
                                )
                                _ <- connections.jointLedger ! ur
                                res <-
                                    if leadingBlockNumber == BlockNumber.zero.increment
                                    then completeFirstBlock
                                    else
                                        pure(
                                          this.copy(isBlockStarted =
                                              Leader.AwaitingConfirmation.StartedBlock.Started
                                          )
                                        )
                            } yield res

                        case bc: Block.MultiSigned.NonFinal =>
                            def completeBlockRegular =
                                sendCompleteRegularBlockAsLeader >>
                                    DecidingRole(
                                      connections,
                                      pollResults,
                                      finalizationLocallyTriggered = finalizationLocallyTriggered,
                                      mempool = Mempool.empty,
                                      nextBlockNumber = leadingBlockNumber.increment
                                    ).act

                            def completeBlockFinal =
                                sendCompleteFinalBlockAsLeader >> IO.pure(None)

                            def completeNextBlock =
                                if bc.finalizationRequested || finalizationLocallyTriggered.asBoolean
                                then completeBlockFinal
                                else completeBlockRegular

                            // Iff the block confirmed is the previous block
                            if bc.blockNum.increment == leadingBlockNumber
                            then
                                if isBlockStarted == AwaitingConfirmation.StartedBlock.Started
                                then completeNextBlock
                                else
                                    for {
                                        now <- realTimeQuantizedInstant(config.slotConfig)
                                        sleepDuration = bc.headerNonFinal.majorBlockWakeupTime - now
                                        fiber <- (IO.sleep(
                                          sleepDuration.finiteDuration
                                        ) >> wakeup).start
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

                        case unexpected: Unexpected =>
                            panicUnexpectedRequest(this, unexpected)
                    }
            }

            object AwaitingConfirmation {
                type NextReactiveState = DecidingRole.NextReactiveState |
                    Leader.AwaitingConfirmation | Leader.AwaitingRequest

                type Unexpected = PreStart.type | BlockBrief.Next |
                    (Block.MultiSigned & BlockType.Final) | Wakeup.type

                private[State] def apply(
                    state: State,
                    blockNumber: BlockNumber,
                    isBlockStarted: StartedBlock
                ): Leader.AwaitingConfirmation =
                    import state.*
                    Leader.AwaitingConfirmation(
                      connections,
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
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                previousBlockConfirmed: Block.MultiSigned.NonFinal,
                wakeupFiber: Fiber[IO, Throwable, Unit]
            ) extends Reactive {
                override transparent inline def stateName: String = "Leader.AwaitingRequest"

                export Leader.AwaitingRequest.{NextReactiveState, Unexpected}

                private val currentBlockNumber = previousBlockConfirmed.blockNum.increment

                override def react(req: Request): IO[Option[NextReactiveState]] = {
                    def completeBlockRegular = sendCompleteRegularBlockAsLeader >>
                        DecidingRole(
                          this,
                          mempool = Mempool.empty,
                          nextBlockNumber = currentBlockNumber.increment
                        ).act

                    def completeBlock = {
                        wakeupFiber.cancel >>
                            (if finalizationLocallyTriggered.asBoolean || previousBlockConfirmed.finalizationRequested
                             then sendCompleteFinalBlockAsLeader >> stop
                             else completeBlockRegular)
                    }

                    req match {
                        case ur: UserRequestWithId =>
                            for {
                                _ <- sendStartBlock(currentBlockNumber)
                                _ <- connections.jointLedger ! ur
                                newState <- completeBlock
                            } yield newState

                        case pr: PollResults =>
                            logger.trace("New poll results.") >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            logger.info("Finalization was locally triggered.") >>
                                pure(copy(finalizationLocallyTriggered = ft))

                        case _: BlockWeaver.Wakeup.type =>
                            def forceMajorBlock =
                                for {
                                    _ <- sendStartBlock(currentBlockNumber)
                                    newState <- completeBlock
                                } yield newState

                            for {
                                _ <- logger.info("Wakeup is received, force major block")
                                newState <- forceMajorBlock
                            } yield newState

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
                      pollResults,
                      finalizationLocallyTriggered,
                      previousBlockConfirmed,
                      wakeupFiber
                    )
            }
        }
    }
}

object BlockWeaver {
    object Wakeup

    type Config = CardanoNetwork.Section & OwnHeadPeerPublic.Section &
        NodeOperationMultisigConfig.Section

    final case class Connections(
        jointLedger: JointLedger.Handle
    )

    type Handle = ActorRef[IO, Request]

    type Request = PreStart.type | UserRequestWithId | BlockBrief.Next | Block.MultiSigned |
        PollResults | LocalFinalizationTrigger.Triggered.type | Wakeup.type

    case object PreStart

    sealed trait LocalFinalizationTrigger(val asBoolean: Boolean)

    object LocalFinalizationTrigger {
        case object Triggered extends LocalFinalizationTrigger(true)
        case object NotTriggered extends LocalFinalizationTrigger(false)
    }
}