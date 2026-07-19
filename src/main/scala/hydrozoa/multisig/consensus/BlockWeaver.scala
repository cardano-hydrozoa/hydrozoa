package hydrozoa.multisig.consensus
import cats.effect.IO
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedFiniteDuration
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.HeadMultisigRegimeManager
import hydrozoa.multisig.consensus.BlockWeaver.State.Leader.AwaitingConfirmation.StartedBlock.{NotStarted, Started}
import hydrozoa.multisig.consensus.mempool.Mempool
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.ledger.block.{Block, BlockBrief, BlockNumber}
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.{CompleteBlockFinal, CompleteBlockRegular, StartBlock}
import scala.collection.immutable.Queue

final case class BlockWeaver(
    config: BlockWeaver.Config,
    pendingConnections: HeadMultisigRegimeManager.PendingConnections |
        BlockWeaver.ConnectionsPartial,
    tracer: ContraTracer[IO, BlockWeaverEvent],
) extends Actor[IO, BlockWeaver.Request] {
    import BlockWeaver.*

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
                startingState <- State.start(config, connections, tracer)
                _ <- become(startingState)
            } yield ()
        case x =>
            IO.raiseError(RuntimeException(s"Unexpected message received before PreStart: $x"))
    }

    private def initializeConnections: IO[BlockWeaver.Connections] = pendingConnections match {
        case pc: HeadMultisigRegimeManager.PendingConnections =>
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
    type Config = CardanoNetwork.Section & OwnPeerPublic.Section &
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

    type Request = PreStart.type | UserRequestWithId | BlockBrief.Next | Block.SoftConfirmed |
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
        def tracer: ContraTracer[IO, BlockWeaverEvent]
        def pollResults: PollResults
        def finalizationLocallyTriggered: LocalFinalizationTrigger

        final def stop(): IO[None.type] =
            tracer.traceWith(BlockWeaverEvent.Stopped) >> IO.pure(None)

        final def logStateTransition: IO[Unit] =
            tracer.traceWith(BlockWeaverEvent.BecameState(stateName))

        /** Wrap a reactive state in `Some` and emit the "Becoming X" log only if the receiver (the
          * previous state) has a different [[stateName]]. Same-name self-loops (e.g. `pure(this)`
          * or `pure(copy(...))`) suppress the log so it doesn't drown the rest of the trace.
          */
        final def pure[S <: State.Reactive](newState: S): IO[Some[S]] = {
            val log =
                if newState.stateName == this.stateName then IO.unit
                else newState.logStateTransition
            log >> IO.pure(Some(newState))
        }

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

        final def sendCompleteBlockAsFollower(
            blockBrief: BlockBrief.Next
        ): IO[Unit] = {
            val completeBlockMsg = blockBrief match {
                case x: BlockBrief.Intermediate =>
                    CompleteBlockRegular(
                      Some(x),
                      pollResults,
                      finalizationLocallyTriggered,
                      x.endTime
                    )
                case x: BlockBrief.Final =>
                    CompleteBlockFinal(Some(x), x.endTime)
            }
            connections.jointLedger ! completeBlockMsg
        }

    }

    object State {
        def start(
            config: Config,
            connections: Connections,
            tracer: ContraTracer[IO, BlockWeaverEvent]
        ): IO[Follower.AwaitingBlockBrief | Leader.AwaitingConfirmation] =
            for {
                state: Some[Follower.AwaitingBlockBrief | Leader.AwaitingConfirmation] <-
                    DecidingRole(
                      connections = connections,
                      tracer = tracer,
                      pollResults = PollResults.empty,
                      finalizationLocallyTriggered = LocalFinalizationTrigger.NotTriggered,
                      mempool = Mempool.empty,
                      nextBlockNumber = BlockNumber.zero.increment
                    ).act(config)
            } yield state.get

        /** A state with a mempool can store requests in its mempool. */
        sealed trait WithMempool extends State {
            def mempool: Mempool

            def storeRequest(request: UserRequestWithId): IO[Mempool] =
                mempool.addRequest(request) match {
                    case Some(newMempool) => IO.pure(newMempool)
                    case None =>
                        IO.raiseError(
                          RuntimeException(
                            s"Request ID ${request.requestId} is already in the mempool."
                          )
                        )
                }
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
                IO.raiseError(
                  RuntimeException(
                    s"Unexpectedly received in ${state.stateName.toString} state: ${unexpected.toString}"
                  )
                )
        }

        final case class DecidingRole private[State] (
            override val connections: Connections,
            override val tracer: ContraTracer[IO, BlockWeaverEvent],
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
                    if config.canLeadFast(nextBlockNumber)
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
                  tracer,
                  pollResults,
                  finalizationLocallyTriggered,
                  mempool,
                  nextBlockNumber
                )
        }

        object Follower {
            final case class AwaitingBlockBrief private (
                override val connections: Connections,
                override val tracer: ContraTracer[IO, BlockWeaverEvent],
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
                            tracer.traceWith(BlockWeaverEvent.BlockBriefReceived(bb.blockNum)) >>
                                Follower.ProcessingReadyRequests(this, mempool, bb).act(config)

                        case pr: PollResults =>
                            tracer.traceWith(BlockWeaverEvent.PollResultsUpdated) >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            tracer.traceWith(BlockWeaverEvent.FinalizationTriggered) >>
                                pure(copy(finalizationLocallyTriggered = ft))

                        case w: Wakeup =>
                            tracer.traceWith(BlockWeaverEvent.WakeupDropped(w.blockNumber)) >>
                                pure(this)

                        case bc: Block.SoftConfirmed =>
                            tracer.traceWith(
                              BlockWeaverEvent.SoftConfirmationIgnored(bc.blockNum)
                            ) >>
                                pure(this)

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
                      tracer,
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
                override val tracer: ContraTracer[IO, BlockWeaverEvent],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                reproducingBlockBrief: BlockBrief.Next,
            ) extends Active,
                  WithMempool {
                override transparent inline def stateName: String =
                    "Follower.ProcessingReadyRequests"

                export Follower.ProcessingReadyRequests.NextReactiveState

                def act(config: Config): IO[Option[NextReactiveState]] = for {
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
                                _ <- sendCompleteBlockAsFollower(reproducingBlockBrief)
                                newState <- reproducingBlockBrief match {
                                    // The reproduced block was the final one: no further block
                                    // will ever be woven, so retire instead of arming to lead.
                                    case _: BlockBrief.Final =>
                                        tracer.traceWith(
                                          BlockWeaverEvent.RetiredOnFinalBlock(
                                            reproducingBlockBrief.blockNum
                                          )
                                        ) >> IO.pure(None)
                                    case _ =>
                                        DecidingRole(this, survivingMempool, nextBlockNumber)
                                            .act(config)
                                }
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
                        _ <- tracer.traceWith(
                          BlockWeaverEvent.MempoolExtracted(extractedRequests.map(_.requestId))
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
                      tracer,
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool,
                      reproducingBlockBrief
                    )
                }
            }

            final case class AwaitingRequest private (
                override val connections: Connections,
                override val tracer: ContraTracer[IO, BlockWeaverEvent],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                override val mempool: Mempool,
                reproducingBlockBrief: BlockBrief.Next,
                incompleteExtraction: Mempool.Extraction.Incomplete,
                // Briefs for later blocks that arrived while we were blocked on this block's
                // requests. The hub relays briefs faster than the requests they need, so a coil peer (or
                // any follower) can see the next brief before finishing the current block. Buffer
                // them in order and replay (re-send to self) once this block completes — they are
                // then handled normally from the next AwaitingBlockBrief.
                stashedBriefs: Queue[BlockBrief.Next] = Queue.empty
            ) extends Reactive,
                  WithMempool {
                override transparent inline def stateName: String = "Follower.AwaitingRequest"

                export Follower.AwaitingRequest.{NextReactiveState, Unexpected}

                override def react(config: Config)(req: Request): IO[Option[NextReactiveState]] =
                    req match {
                        case ur: UserRequestWithId =>
                            for {
                                // Store first
                                _ <- tracer.traceWith(
                                  BlockWeaverEvent.RequestAddedToMempool(ur.requestId)
                                )
                                newMempool <- storeRequest(ur)
                                // Then decide what to do
                                newState <-
                                    if ur.requestId == incompleteExtraction.awaitingRequestId then
                                        for {
                                            _ <- tracer.traceWith(
                                              BlockWeaverEvent.AwaitedRequestReceived(ur.requestId)
                                            )
                                            newExtractionResult <-
                                                extractAndSendRequestsFromMempool(newMempool)
                                            newState <- newExtractionResult match {
                                                case Mempool.Extraction
                                                        .Complete(
                                                          extractedRequests,
                                                          survivingMempool
                                                        ) =>
                                                    val nextBlockNumber =
                                                        reproducingBlockBrief.blockNum.increment
                                                    for {
                                                        _ <- sendCompleteBlockAsFollower(
                                                          reproducingBlockBrief
                                                        )
                                                        newState <- reproducingBlockBrief match {
                                                            // The reproduced block was the final
                                                            // one: no further block will ever be
                                                            // woven, so retire (dropping any
                                                            // buffered briefs) instead of arming
                                                            // to lead.
                                                            case _: BlockBrief.Final =>
                                                                tracer.traceWith(
                                                                  BlockWeaverEvent
                                                                      .RetiredOnFinalBlock(
                                                                        reproducingBlockBrief.blockNum
                                                                      )
                                                                ) >> IO.pure(None)
                                                            case _ =>
                                                                // Replay buffered later-block
                                                                // briefs in order; they re-enter
                                                                // from the next AwaitingBlockBrief.
                                                                stashedBriefs.traverse_(
                                                                  connections.blockWeaver ! _
                                                                ) >> DecidingRole(
                                                                  this,
                                                                  survivingMempool,
                                                                  nextBlockNumber
                                                                ).act(config)
                                                        }
                                                    } yield newState
                                                case result: Mempool.Extraction.Incomplete =>
                                                    // Still missing a request — keep waiting, but
                                                    // carry the buffered briefs over.
                                                    pure(
                                                      copy(
                                                        mempool = result.survivingMempool,
                                                        incompleteExtraction = result
                                                      )
                                                    )
                                            }
                                        } yield newState
                                    else
                                        for {
                                            _ <- tracer.traceWith(
                                              BlockWeaverEvent.WaitingForRequest(
                                                ur.requestId,
                                                incompleteExtraction.awaitingRequestId
                                              )
                                            )
                                            newState <- pure(copy(mempool = newMempool))
                                        } yield newState
                            } yield newState

                        case pr: PollResults =>
                            tracer.traceWith(BlockWeaverEvent.PollResultsUpdated) >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            tracer.traceWith(BlockWeaverEvent.FinalizationTriggered) >>
                                pure(copy(finalizationLocallyTriggered = ft))

                        case w: Wakeup =>
                            tracer.traceWith(BlockWeaverEvent.WakeupDropped(w.blockNumber)) >>
                                pure(this)

                        case bc: Block.SoftConfirmed =>
                            tracer.traceWith(
                              BlockWeaverEvent.SoftConfirmationIgnored(bc.blockNum)
                            ) >>
                                pure(this)

                        case bb: BlockBrief.Next =>
                            // A later block's brief arrived while we're still blocked on this
                            // block's requests (the hub relays briefs faster than the requests they
                            // need). Buffer it; we replay it once this block completes.
                            tracer.traceWith(
                              BlockWeaverEvent.EarlyBriefBuffered(
                                bb.blockNum,
                                incompleteExtraction.awaitingRequestId,
                                reproducingBlockBrief.blockNum
                              )
                            ) >> pure(copy(stashedBriefs = stashedBriefs :+ bb))

                        case unexpected: Unexpected =>
                            panicUnexpectedRequest(this, unexpected)
                    }

                // TODO: pass awaited request not the whole mempool
                private def extractAndSendRequestsFromMempool(
                    mempool: Mempool
                ): IO[Mempool.Extraction.Result] = {
                    val allRequestIds: List[RequestId] = reproducingBlockBrief.events.map(_._1)
                    val requestIds =
                        allRequestIds.dropWhile(_ != incompleteExtraction.awaitingRequestId)
                    val newExtractionResult = mempool.extractRequestsWhile(requestIds)
                    import newExtractionResult.*
                    for {
                        _ <- tracer.traceWith(
                          BlockWeaverEvent.MempoolExtracted(extractedRequests.map(_.requestId))
                        )
                        _ <- extractedRequests.traverse_(connections.jointLedger ! _)
                    } yield newExtractionResult
                }
            }

            private object AwaitingRequest {
                type NextReactiveState = DecidingRole.NextReactiveState | Follower.AwaitingRequest
                type Unexpected = PreStart.type

                private[State] def apply(
                    state: State,
                    reproducingBlockBrief: BlockBrief.Next,
                    incompleteExtraction: Mempool.Extraction.Incomplete
                ): Follower.AwaitingRequest =
                    import state.*
                    Follower.AwaitingRequest(
                      connections,
                      tracer,
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
                override val tracer: ContraTracer[IO, BlockWeaverEvent],
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
                    _ <- realTimeQuantizedInstant(config.slotConfig)
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
                    tracer.traceWith(
                      BlockWeaverEvent.MempoolExtracted(requests.map(_.requestId))
                    ) >> IO.pure(requests)
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
                      tracer,
                      pollResults,
                      finalizationLocallyTriggered,
                      mempool,
                      leadingBlockNum
                    )
                }
            }

            final case class AwaitingConfirmation private (
                override val connections: Connections,
                override val tracer: ContraTracer[IO, BlockWeaverEvent],
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
                            // First block is implicitly confirmed, so we complete it immediately —
                            // as the final block when finalization was requested (mirroring
                            // completeNextBlock below), regular otherwise.
                            def completeFirstBlock =
                                if finalizationLocallyTriggered.asBoolean
                                then sendCompleteFinalBlockAsLeader(config) >> stop()
                                else
                                    for {
                                        _ <- sendCompleteRegularBlockAsLeader(config)
                                        newState <- DecidingRole(
                                          connections,
                                          tracer,
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

                                _ <- tracer.traceWith(
                                  BlockWeaverEvent.RequestSentToJointLedger(ur.requestId)
                                )
                                _ <- connections.jointLedger ! ur
                                res <-
                                    if leadingBlockNumber == BlockNumber.zero.increment
                                    then completeFirstBlock
                                    else pure(this.copy(isBlockStarted = Started))
                            } yield res

                        case bc: Block.SoftConfirmed.NonFinal =>
                            def completeBlockRegular =
                                sendCompleteRegularBlockAsLeader(config) >>
                                    DecidingRole(
                                      connections,
                                      tracer,
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

                            // TODO: introduce thee-way data Confirmation = Expected | Belated | Unexpected
                            // Iff the block confirmed is the previous block
                            if bc.blockNum.increment == leadingBlockNumber
                            then
                                if isBlockStarted == Started
                                then completeNextBlock
                                else
                                    tracer.traceWith(
                                      BlockWeaverEvent.PreviousBlockConfirmation(bc.blockNum)
                                    ) >> scheduleWakeupFiber(config, bc) >>
                                        pure(
                                          Leader.AwaitingRequest(this, previousBlockConfirmed = bc)
                                        )
                            else {
                                // If it's not for the previous block, two cases are possible.
                                // It might be a late confirmation for a previous block, consider this sequence:
                                // INFO  BlockWeaver.0 New block brief 20. // Start 20 as follower
                                // INFO  BlockWeaver.0 Becoming Follower.ProcessingReadyRequests. // Feed the content
                                // INFO  BlockWeaver.0 Becoming DecidingRole. // Done
                                // INFO  BlockWeaver.0 Becoming Leader.ProcessingReadyRequests. // Leading 21
                                // INFO  BlockWeaver.0 Becoming Leader.AwaitingConfirmation. // Waiting for confirmation for 20, but may receive her for 19
                                //
                                // This is totally fine, just ignore the confirmation we are not interested in.
                                // On the other hand, since we are the leader working on the edge, a confirmation for a higher block number is a panic.
                                val belatedPreviousConfirmation =
                                    bc.blockNum.increment < leadingBlockNumber

                                if belatedPreviousConfirmation
                                then
                                    tracer.traceWith(
                                      BlockWeaverEvent.BelatedConfirmation(
                                        bc.blockNum,
                                        leadingBlockNumber
                                      )
                                    ) >> pure(this)
                                else
                                    IO.raiseError(
                                      RuntimeException(
                                        "Received block confirmation for the current or a future block. We are producing" +
                                            s" $leadingBlockNumber, but the confirmed block that we received is ${bc.blockNum}"
                                      )
                                    )
                            }

                        case bc: Block.SoftConfirmed.Final =>
                            // Defensive: the weaver retires when it completes the final block, so
                            // a Final confirmation should not reach an armed leader — but a late
                            // or duplicate fan-out must retire it, not panic the node.
                            tracer.traceWith(
                              BlockWeaverEvent.RetiredOnFinalBlock(bc.blockNum)
                            ) >> stop()

                        case pr: PollResults =>
                            tracer.traceWith(BlockWeaverEvent.PollResultsUpdated) >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            // The first block completes on its first request — there is no
                            // previous-block confirmation and no wakeup to drive it otherwise — so
                            // on the first block the trigger itself completes the (possibly empty)
                            // block as final. On later blocks the flag is recorded here and honored
                            // at completion.
                            if leadingBlockNumber == BlockNumber.zero.increment
                            then
                                for {
                                    _ <- tracer.traceWith(BlockWeaverEvent.FinalizationTriggered)
                                    _ <- IO.whenA(isBlockStarted == NotStarted)(
                                      sendStartBlock(config)(leadingBlockNumber)
                                    )
                                    _ <- sendCompleteFinalBlockAsLeader(config)
                                    newState <- stop()
                                } yield newState
                            else
                                tracer.traceWith(BlockWeaverEvent.FinalizationTriggered) >>
                                    pure(copy(finalizationLocallyTriggered = ft))

                        case w: Wakeup =>
                            tracer.traceWith(BlockWeaverEvent.WakeupDropped(w.blockNumber)) >>
                                pure(this)

                        case unexpected: Unexpected =>
                            panicUnexpectedRequest(this, unexpected)
                    }
                }

                private def scheduleWakeupFiber(
                    config: Config,
                    bc: Block.SoftConfirmed.NonFinal
                ): IO[Unit] =
                    for {
                        now <- realTimeQuantizedInstant(config.slotConfig)
                        fmbwtInstant = bc.headerNonFinal.forcedMajorBlockWakeupTime.convert
                        wakeupInstant = bc.headerNonFinal.mDepositDecisionWakeupTime
                            .fold(fmbwtInstant)(ddwt =>
                                val ddwtInstant = ddwt.convert
                                if ddwtInstant.instant.isBefore(fmbwtInstant.instant)
                                then ddwtInstant
                                else fmbwtInstant
                            )
                        sleepDuration = wakeupInstant - now
                        _ <-
                            if sleepDuration.finiteDuration.toMillis <= 0L
                            then {
                                // Non-positive sleep duration: the chosen wakeup target is already
                                // at or before `now`, which is legitimate. The dominant case is a
                                // deposit-driven wakeup: `mDepositDecisionWakeupTime` is set from
                                // a pending deposit's `absorptionStartTime` (a fixed wall-time
                                // anchor from when the deposit was registered) and is selected
                                // here when it precedes `forcedMajorBlockWakeupTime`. Between the
                                // moment the previous block's header was sealed (carrying that
                                // ddwt) and the moment confirmation for that block reaches us
                                // here, virtual time can advance past the ddwt. Observed in the
                                // stage4 20-peer head: a major-block transition (block 45 in
                                // run logged 2026-02-11) advanced virtual time ~43s during
                                // cross-peer ack collection, leaving sleepDuration = -43s.
                                //
                                // The right action is "fire the wakeup immediately" — the same
                                // Wakeup message `sleepSendWakeup` would send after sleeping.
                                // The `Ignoring wakeup for preceding block N` guard handles any
                                // race between this dispatch and a subsequent state change.
                                //
                                // See:
                                //   https://linear.app/gummiworm-labs/issue/GUM-111/should-negative-weavers-wakeups-be-permitted
                                tracer.traceWith(
                                  BlockWeaverEvent.NonPositiveWakeupDelay(this.leadingBlockNumber)
                                ) >> (connections.blockWeaver ! Wakeup(this.leadingBlockNumber))
                            } else
                                tracer.traceWith(
                                  BlockWeaverEvent.WakeupFiberStarted(this.leadingBlockNumber)
                                ) >> sleepSendWakeup(sleepDuration).start.void
                    } yield ()

                private def sleepSendWakeup(sleepDuration: QuantizedFiniteDuration): IO[Unit] =
                    IO.sleep(sleepDuration.finiteDuration) >>
                        (connections.blockWeaver ! Wakeup(this.leadingBlockNumber))
            }

            object AwaitingConfirmation {
                type NextReactiveState = DecidingRole.NextReactiveState |
                    Leader.AwaitingConfirmation | Leader.AwaitingRequest

                type Unexpected = PreStart.type | BlockBrief.Next

                private[State] def apply(
                    state: State,
                    blockNumber: BlockNumber,
                    isBlockStarted: StartedBlock
                ): Leader.AwaitingConfirmation =
                    import state.*
                    Leader.AwaitingConfirmation(
                      connections,
                      tracer,
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
                override val tracer: ContraTracer[IO, BlockWeaverEvent],
                override val pollResults: PollResults,
                override val finalizationLocallyTriggered: LocalFinalizationTrigger,
                previousBlockConfirmed: Block.SoftConfirmed.NonFinal,
                // wakeupFiber: Fiber[IO, Throwable, Unit]
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
                                    _ <- tracer.traceWith(
                                      BlockWeaverEvent.ForcedBlockCompletion(currentBlockNumber)
                                    )
                                    _ <- sendStartBlock(config)(currentBlockNumber)
                                    newState <- completeBlock
                                } yield newState

                            if w.blockNumber == currentBlockNumber
                            then forceMajorBlock
                            else
                                tracer.traceWith(
                                  BlockWeaverEvent.WakeupIgnored(
                                    w.blockNumber,
                                    currentBlockNumber,
                                    isFuture = w.blockNumber > currentBlockNumber
                                  )
                                ) >> pure(this)

                        case pr: PollResults =>
                            tracer.traceWith(BlockWeaverEvent.PollResultsUpdated) >>
                                pure(copy(pollResults = pr))

                        case ft: LocalFinalizationTrigger.Triggered.type =>
                            tracer.traceWith(BlockWeaverEvent.FinalizationTriggered) >>
                                pure(copy(finalizationLocallyTriggered = ft))

                        case unexpected: Unexpected =>
                            panicUnexpectedRequest(this, unexpected)
                    }
                }
            }

            private object AwaitingRequest {
                type NextReactiveState = DecidingRole.NextReactiveState | Leader.AwaitingRequest

                type Unexpected = PreStart.type | BlockBrief.Next | Block.SoftConfirmed

                private[State] def apply(
                    state: State,
                    previousBlockConfirmed: Block.SoftConfirmed.NonFinal,
                    // wakeupFiber: Fiber[IO, Throwable, Unit]
                ): Leader.AwaitingRequest =
                    import state.*
                    Leader.AwaitingRequest(
                      connections,
                      tracer,
                      pollResults,
                      finalizationLocallyTriggered,
                      previousBlockConfirmed,
                      // wakeupFiber
                    )
            }
        }
    }
}
