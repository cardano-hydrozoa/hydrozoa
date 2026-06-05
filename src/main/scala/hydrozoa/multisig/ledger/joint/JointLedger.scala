package hydrozoa.multisig.ledger.joint

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, FallbackTxStartTime}
import hydrozoa.config.head.multisig.timing.TxTiming.RequestTimes.{RequestValidityEndTime, RequestValidityStartTime}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.{ContraTracer, Traced}
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.BlockWeaver.LocalFinalizationTrigger
import hydrozoa.multisig.consensus.BlockWeaver.LocalFinalizationTrigger.NotTriggered
import hydrozoa.multisig.consensus.ack.SoftAck
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.consensus.{FastConsensusActor, PeerLiaison, StackComposer, UserRequestWithId, pollresults}
import hydrozoa.multisig.ledger.block.*
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag.{Invalid, Valid}
import hydrozoa.multisig.ledger.joint.JointLedger.*
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.*
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.l1.txseq.DepositRefundTxSeq
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import hydrozoa.multisig.ledger.l2.{L2Ledger, L2LedgerCommand, L2LedgerError, L2LedgerState}
import hydrozoa.multisig.persistence.{LaneKey, LaneValue, Persistence, StoreKey, WriteBatch}
import monocle.Focus.focus

private case class UserRequestState(
    requests: List[(RequestId, ValidityFlag)],
    postDatedRefundTxs: Vector[RefundTx.PostDated]
)

final case class JointLedger(
    config: JointLedger.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | JointLedger.Connections,
    l2Ledger: L2Ledger[IO],
    tracer: ContraTracer[IO, JointLedgerEvent],
    persistence: Persistence[IO]
) extends Actor[IO, Requests.Request] {
    import config.*

    /** `config` is a `CardanoNetwork.Section` transitively (`HeadConfig.Section`); expose it as a
      * given so the typed `WriteBatch.put` calls in [[persistOwnBlock]] pick it up.
      */
    private given CardanoNetwork.Section = config

    /** Bridge for pure functions that return `Traced[A]` (e.g. `BlockHeader.nextHeader*`): emits
      * each [[hydrozoa.lib.logging.LogEvent]] through the typed JL tracer as a
      * [[JointLedgerEvent.HeaderLog]] pass-through, then yields the result `A`.
      */
    private def emitTraced[A](traced: Traced[A]): IO[A] =
        traced._2
            .traverse_(e => tracer.traceWith(JointLedgerEvent.HeaderLog(e.level, e.msg, e.ctx)))
            .as(traced._1)

    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    val state: Ref[IO, JointLedger.State] =
        Ref.unsafe[IO, JointLedger.State](JointLedger.State.initialize(config))

    private def executeL2Command(
        state: JointLedger.Producing,
        command: L2LedgerCommand.Real
    ): IO[L2LedgerState] = for {
        either <- runL2Command(state, command)
        ret <- either match {
            case Left(err) =>
                tracer.traceWith(JointLedgerEvent.L2CommandFailed(err)) *>
                    IO.raiseError(err)
            case Right(ret) => IO.pure(ret)
        }
    } yield ret

    private def executeL2ProxyCommand(
        command: L2LedgerCommand.Proxy
    ): IO[Unit] = L2LedgerState
        .executeProxyCommand(l2Ledger, command)
        .handleErrorWith { err =>
            tracer.traceWith(JointLedgerEvent.L2ProxyCommandFailed(err)) *>
                IO.raiseError(err)
        }

    private def runL2Command(
        state: JointLedger.Producing,
        command: L2LedgerCommand.Real
    ): IO[Either[L2LedgerError, L2LedgerState]] =
        state.runL2CommandReal(l2Ledger, command)

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error(
              "Joint ledger is missing its connections to other actors."
            )
          )
        )(IO.pure)
    } yield conn

    private def initializeConnections: IO[Unit] = pendingConnections match {
        case x: MultisigRegimeManager.PendingConnections =>
            for {
                _connections <- x.get
                _ <- connections.set(
                  Some(
                    Connections(
                      fastConsensusActor = _connections.consensusActor,
                      stackComposer = _connections.stackComposer,
                      peerLiaisons = _connections.peerLiaisons
                    )
                  )
                )
            } yield ()
        case x: JointLedger.Connections => connections.set(Some(x))
    }

    // TODO: Refactor to use "become" and use different receive functions

    /** Get _only_ a [[Producing]] State or throw an exception QUESTION: What type of exception
      * should this be?
      */
    private val unsafeGetProducing: IO[Producing] = for {
        s <- state.get
        p <- s match {
            case _: Done =>
                val msg = "Expected a `Producing` State, but got `Done`. This indicates" +
                    " that a request was issued to the JointLedger that is only valid when the hydrozoa node is producing" +
                    " a block."
                tracer.traceWith(JointLedgerEvent.InvalidStateExpectedProducing) >>
                    IO.raiseError(RuntimeException(msg))
            case p: Producing => IO.pure(p)
        }
    } yield p

    /** Get _only_ a [[Done]] State or throw an exception QUESTION: What type of exception should
      * this be?
      */
    private val unsafeGetDone: IO[Done] = for {
        s <- state.get
        p <- s match {
            case _: Producing =>
                throw new RuntimeException(
                  "Expected a `Done` State, but got `Producing`. This indicates" +
                      " that a request was issued to the JointLedger that is only valid when the hydrozoa node is not producing" +
                      " a block."
                )
            case d: Done => IO.pure(d)
        }
    } yield p

    override def preStart: IO[Unit] =
        context.self ! Requests.PreStart

    override def receive: Receive[IO, Requests.Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Requests.Request): IO[Unit] =
        req match {
            case Requests.PreStart       => preStartLocal
            case e: UserRequestWithId    => applyUserRequestWithId(e)
            case s: StartBlock           => startBlock(s)
            case c: CompleteBlockRegular => completeBlockRegular(c)
            case f: CompleteBlockFinal   => completeBlockFinal(f)
            case req: SyncRequest.Any =>
                req.request match {
                    case r: GetState.type => r.handleSync(req, _ => state.get)
                }
            case p: Block.SoftConfirmed.Next => proxyConfirmation(p)
        }

    /** Notify the L2 ledger that the brief was soft-confirmed, recording the block's refund-tx
      * CBORs in the L2 ledger's per-block `confirmations` map for L2 clients (SugarRush) to
      * retrieve.
      *
      * TODO(GUM-133): incomplete — passes an empty refund-tx list. Post-dated refunds now live on
      * the slow side (`BlockResult.postDatedRefundTxs` → slow consensus → L1), so this fast path
      * has no refund CBORs to hand the L2 ledger, and a client querying `confirmations` sees none.
      * The proxy was a temporary measure; simply relocating it to slow consensus is not the answer
      * — the refund-surfacing contract needs a joint design with the SugarRush team.
      */
    private def proxyConfirmation(next: Block.SoftConfirmed.Next): IO[Unit] = {
        val l2Command = L2LedgerCommand.ProxyBlockConfirmation(
          next.blockNum,
          Vector.empty
        )
        executeL2ProxyCommand(l2Command)
    }

    private def preStartLocal: IO[Unit] = initializeConnections

    private def applyUserRequestWithId(e: UserRequestWithId): IO[Unit] = e match {
        case req: UserRequestWithId.DepositRequest     => registerDeposit(req)
        case req: UserRequestWithId.TransactionRequest => applyTransaction(req)
    }

    private def checkRequestValidityInterval(
        req: UserRequestWithId,
        blockCreationStartTime: BlockCreationStartTime
    ): Boolean = {
        val header = req.request.header
        TxTiming.checkRequestValidityInterval(
          blockCreationStartTime,
          header.validityStart,
          header.validityEnd
        )
    }

    private def rejectEvent(
        requestId: RequestId,
        e: JointLedger.UserRequestError | JointLedger.DepositLedgerError | L2LedgerError
    ): IO[Unit] =
        for {
            oldState <- unsafeGetProducing
            currentBlockNum = oldState.nextBlockNumber
            newState = oldState
                .focus(_.userRequestState.requests)
                .modify(_.appended((requestId, Invalid)))
            _ <- state.set(newState)
            _ <- tracer.traceWith(
              JointLedgerEvent.RequestRejected(requestId, currentBlockNum, e.toString)
            )
            l2Command = L2LedgerCommand.ProxyRequestError(
              requestId = requestId,
              message = e.toString
            )
            // FIXME: Should we retry?
            _ <- executeL2ProxyCommand(l2Command)
        } yield ()

    /** Pure deposit-ledger op: parse the deposit tx, check its submission-deadline TTL, and append
      * the produced deposit utxo to the L1 deposits map — this actor's only L1-ledger surface.
      * Returns the new map + the produced deposit utxo and its post-dated refund tx, or a
      * [[DepositLedgerError]] (rejected via `rejectEvent`, never raised).
      *
      * NOTE: checks SOME time bounds — specifically that the deposit's submission deadline matches
      * the one expected from its validity-end.
      */
    private def registerDepositInMap(
        deposits: DepositsMap,
        requestWithId: UserRequestWithId.DepositRequest
    ): Either[DepositLedgerError, (DepositsMap, (DepositUtxo, RefundTx.PostDated))] = {
        import requestWithId.*
        import request.*
        val validityEndTime = RequestValidityEndTime(header.validityEnd)
        for {
            depositRefundTxSeq <- DepositRefundTxSeq
                .Parse(config)(
                  depositTxBytes = body.l1Payload,
                  l2Payload = body.l2Payload,
                  requestId = requestWithId.requestId,
                  requestValidityEndTime = validityEndTime
                )
                .result
                .left
                .map(DepositLedgerError.ParseError(_))
            expectedSubmissionDeadline = config.txTiming.depositSubmissionDeadline(validityEndTime)
            depositProduced <-
                if depositRefundTxSeq.depositTx.submissionDeadline == expectedSubmissionDeadline
                then Right(depositRefundTxSeq.depositTx.depositProduced)
                else
                    Left(
                      DepositLedgerError.DepositTxInvalidTTL(
                        expectedSubmissionDeadline,
                        depositRefundTxSeq.depositTx.submissionDeadline
                      )
                    )
        } yield (
          deposits.append(DepositsMap.Entry(requestId, depositProduced)),
          (depositProduced, depositRefundTxSeq.refundTx)
        )
    }

    /** Update the work-in-progress block to accept or reject the deposit, depending on whether the
      * L2 ledger can register it.
      */
    private def registerDeposit(req: UserRequestWithId.DepositRequest): IO[Unit] = {
        import req.*
        import request.*
        import body.*

        for {
            _ <- tracer.traceWith(JointLedgerEvent.DepositRegistrationStarted(requestId))

            p <- unsafeGetProducing
            blockStartTime = p.BlockCreationStartTime
            currentBlockNum = p.nextBlockNumber

            _ <-
                if !checkRequestValidityInterval(req, blockStartTime) then
                    rejectEvent(
                      requestId,
                      JointLedger.UserRequestError.BlockOutOfRequestValidityInterval(
                        blockStartTime,
                        req.request.header.validityStart,
                        req.request.header.validityEnd
                      )
                    )
                else {
                    val l1Res = registerDepositInMap(p.deposits, req)
                    l1Res match {
                        case Left(error) => rejectEvent(requestId, error)
                        case Right((newDeposits, (depositProduced, refundTx))) => {
                            val l2Command = L2LedgerCommand.RegisterDeposit(
                              requestId = requestId,
                              userVKey = req.request.userVk,
                              blockNumber = currentBlockNum,
                              blockCreationStartTime = p.BlockCreationStartTime.toPosixTime,
                              depositId = depositProduced.utxoId,
                              depositFee = depositProduced.depositFee,
                              depositL2Value = depositProduced.l2Value,
                              refundDestination = refundTx.refundDestination,
                              l2Payload = l2Payload
                            )
                            for {
                                res <- runL2Command(p, l2Command)
                                _ <- res match {
                                    // FIXME: Should we distinguish between genuine L2 failures and things like
                                    // network errors?
                                    case Left(e) => rejectEvent(requestId, e)
                                    case Right(newL2State) =>
                                        for {
                                            _ <- state.set(
                                              p.setDeposits(newDeposits)
                                                  .setL2LedgerState(newL2State)
                                                  .focus(_.userRequestState.requests)
                                                  .modify(_.appended((requestId, Valid)))
                                                  .focus(_.userRequestState.postDatedRefundTxs)
                                                  .modify(_.appended(refundTx))
                                            )
                                            _ <- tracer.traceWith(
                                              JointLedgerEvent.DepositRegistrationCompleted(
                                                requestId,
                                                currentBlockNum
                                              )
                                            )
                                        } yield ()
                                }
                            } yield ()
                        }
                    }
                }
        } yield ()
    }

    /** Apply a transaction request to the L2 ledger and record its outcome (valid / invalid) on the
      * work-in-progress block.
      */
    private def applyTransaction(
        req: UserRequestWithId.TransactionRequest
    ): IO[Unit] = {
        import req.*
        import request.*
        import body.*

        for {
            _ <- tracer.traceWith(JointLedgerEvent.TransactionApplicationStarted(requestId))

            p <- unsafeGetProducing
            blockStartTime = p.BlockCreationStartTime
            currentBlockNum = p.nextBlockNumber

            _ <-
                if !checkRequestValidityInterval(req, blockStartTime) then
                    rejectEvent(
                      requestId,
                      JointLedger.UserRequestError.BlockOutOfRequestValidityInterval(
                        blockStartTime,
                        req.request.header.validityStart,
                        req.request.header.validityEnd
                      )
                    )
                else {
                    val l2Command: L2LedgerCommand.ApplyTransaction = L2LedgerCommand
                        .ApplyTransaction(
                          requestId = req.requestId,
                          userVKey = req.request.userVk,
                          blockNumber = p.nextBlockNumber,
                          blockCreationStartTime = p.BlockCreationStartTime.toPosixTime,
                          l2Payload = l2Payload
                        )

                    for {
                        res <- runL2Command(p, l2Command)
                        _ <- res match {
                            case Left(e) => rejectEvent(requestId, e)
                            case Right(newL2State) =>
                                for {
                                    _ <- state.set(
                                      p.setL2LedgerState(newL2State)
                                          .focus(_.userRequestState.requests)
                                          .modify(_.appended((requestId, Valid)))
                                    )
                                    _ <- tracer.traceWith(
                                      JointLedgerEvent.TransactionApplicationCompleted(
                                        requestId,
                                        currentBlockNum
                                      )
                                    )
                                } yield ()
                        }
                    } yield ()
                }
        } yield ()
    }

    /** Move the JointLedger from `Done` to `Producing` for the next block: set the creation start
      * time and re-initialize the per-block transient fields (L2 ledger state, user-request state).
      */
    private def startBlock(args: StartBlock): IO[Unit] = {
        import args.*
        for {
            _ <- tracer.traceWith(
              JointLedgerEvent.BlockStarted(args.blockNum, blockCreationStartTime)
            )
            d <- unsafeGetDone
            newState = d.producing(
              l2LedgerState = L2LedgerState.empty,
              startTime = blockCreationStartTime,
              userRequestState = UserRequestState(
                requests = List.empty,
                postDatedRefundTxs = Vector.empty
              )
            )
            _ <- state.set(newState)
        } yield ()
    }

    /** Complete a Minor or Major block. */
    private def completeBlockRegular(
        args: CompleteBlockRegular
    ): IO[Unit] = {
        import args.*
        unsafeGetProducing.flatMap { p =>
            val partition = p.deposits.partition(
              blockCreationEndTime = blockCreationEndTime,
              settlementTxEndTime = config.txTiming.newSettlementEndTime(p.competingFallbackTxTime),
              pollResults = pollResults
            )
            val split = partition.split(maxDepositsAbsorbedPerBlock)
            for {
                _ <- tracer.traceWith(
                  JointLedgerEvent.BlockCompleting(
                    p.nextBlockNumber,
                    blockCreationEndTime,
                    p.competingFallbackTxTime,
                    split.toString
                  )
                )

                blockBriefRes <- mkBlockBriefIntermediate(
                  p,
                  blockCreationEndTime,
                  split.decisions
                )
                (pBlockBrief, blockBrief, evacDiffs) = blockBriefRes

                // Verify the produced brief against the reference brief (follower mode).
                _ <- panicOnMismatchWithExpectedBrief(referenceBlockBrief, blockBrief)

                // Drop the deposits we just absorbed/refunded from the L1 deposits map.
                newJlState = pBlockBrief.setDeposits(split.surviving)

                _ <- state.set(newJlState.done(blockBrief.header))

                // Slow side: emit per-block result for the StackComposer to assemble into
                // stacks. Independent of soft-confirmation.
                blockResult = BlockResult(
                  brief = blockBrief,
                  evacuationMapDiff = evacDiffs,
                  payoutObligations = newJlState.l2LedgerState.payouts.toList,
                  postDatedRefundTxs = pBlockBrief.userRequestState.postDatedRefundTxs.toList,
                  absorbedDeposits = split.decisions.absorbed.depositUtxos,
                  competingFallbackTxTime = pBlockBrief.competingFallbackTxTime
                )

                // Hand off the brief: emit our soft-ack and broadcast the brief.
                _ <- handleBlock(blockBrief, finalizationLocallyTriggered, blockResult)
            } yield ()
        }
    }

    /** Build the next block's header and intermediate brief from the current `Producing` state and
      * the deposit decisions. Returns the updated `Producing`, the brief, and the block's
      * evacuation-map diffs.
      *
      * TODO: I don't like definitions like that - too many things in one place.
      */
    def mkBlockBriefIntermediate(
        p: JointLedger.Producing,
        blockCreationEndTime: BlockCreationEndTime,
        decisions: DepositsMap.Decisions
    ): IO[(JointLedger.Producing, BlockBrief.Intermediate, Seq[EvacuationDiff])] = {
        val blockCreationStartTime = p.BlockCreationStartTime
        val previousHeader = p.previousBlockHeader
        val blockWithdrawnUtxos = p.l2LedgerState.payouts
        val events = p.userRequestState.requests
        for {
            _ <- tracer.traceWith(
              JointLedgerEvent.BlockBriefBuilding(
                previousHeader,
                blockCreationStartTime,
                p.competingFallbackTxTime,
                events,
                decisions.absorbed.requestIds,
                decisions.refunded.requestIds
              )
            )

            depositEventDecisions: L2LedgerCommand.ApplyDepositDecisions =
                L2LedgerCommand.ApplyDepositDecisions(
                  blockNumber = p.nextBlockNumber,
                  blockCreationEndTime = blockCreationEndTime.toPosixTime,
                  absorbedDeposits = decisions.absorbed.requestIds,
                  refundedDeposits = decisions.refunded.requestIds
                )

            // Block header
            headerRes: (JointLedger.Producing, BlockHeader.Intermediate, Seq[EvacuationDiff]) <-
                if decisions.absorbed.isEmpty && blockWithdrawnUtxos.isEmpty
                then
                    val evacDiffs = p.l2LedgerState.diffs
                    for {
                        newL2State <-
                            if decisions.refunded.isEmpty then IO.pure(p.l2LedgerState)
                            else executeL2Command(p, depositEventDecisions)

                        // `evacDiffs` are surfaced to the slow side via `BlockResult`.
                        newJLState = p.setL2LedgerState(newL2State)

                        headerIntermediate <- emitTraced(
                          previousHeader.nextHeaderIntermediate(
                            txTiming,
                            blockCreationStartTime,
                            blockCreationEndTime,
                            decisions.mNextAbsorptionStartTime,
                          )
                        )
                    } yield (newJLState, headerIntermediate, evacDiffs)
                else {
                    for {
                        newL2State <- executeL2Command(p, depositEventDecisions)
                        evacDiffs = newL2State.diffs
                        newJLState = p.setL2LedgerState(newL2State)

                        headerIntermediate <- emitTraced(
                          previousHeader.nextHeaderMajor(
                            txTiming,
                            blockCreationStartTime,
                            blockCreationEndTime,
                            decisions.mNextAbsorptionStartTime,
                          )
                        )
                    } yield (newJLState, headerIntermediate, evacDiffs)
                }
            (newJlState, headerIntermediate, evacDiffs) = headerRes

            // Block brief
            blockBrief: BlockBrief.Intermediate = headerIntermediate match {
                case header: BlockHeader.Minor =>
                    val blockBody = BlockBody.Minor(events, decisions.refunded.requestIds)
                    BlockBrief.Minor(header, blockBody)
                case header: BlockHeader.Major =>
                    val blockBody = BlockBody.Major(
                      events,
                      decisions.absorbed.requestIds,
                      decisions.refunded.requestIds
                    )
                    BlockBrief.Major(header, blockBody)
            }

            _ <- tracer.traceWith(JointLedgerEvent.BlockBriefBuilt(blockBrief))
        } yield (newJlState, blockBrief, evacDiffs)
    }

    // Settlement / fallback / rollout / refund / finalization transactions are slow-cycle
    // responsibility and live in [[hydrozoa.multisig.consensus.StackComposer]]. The fast cycle
    // only handles briefs + header signatures.

    // Block completion Signal is provided to the joint ledger when the block weaver says it's time.
    // If it's a final block, we don't pass poll results from the cardano liaison. Otherwise, we do.
    // We need to:
    //   - Compile the information from the transient fields into a block
    //   - put it into "previous block"
    //   - wipe the "transient fields"
    // If a "reference block" is passed, this means that the block we produce must be equal to the reference block.
    // If the produced block is NOT equal to a passed reference block, then:
    //   - Consensus is broken
    //   - Send a panic to the multisig regime manager in a suicide note
    def completeBlockFinal(args: CompleteBlockFinal): IO[Unit] = {
        import args.*
        unsafeGetProducing.flatMap { p =>
            for {
                blockBrief <- IO.pure {
                    import p.userRequestState.*
                    val blockHeader = p.previousBlockHeader.nextHeaderFinal(
                      p.BlockCreationStartTime,
                      args.blockCreationEndTime
                    )
                    val blockBody = BlockBody.Final(
                      events = requests,
                      // Final block should reject all the deposits known.
                      depositsRefunded = p.deposits.requestIds
                    )
                    BlockBrief.Final(blockHeader, blockBody)
                }

                _ <- panicOnMismatchWithExpectedBrief(referenceBlockBrief, blockBrief)

                _ <- state.set(p.done(blockBrief.header))

                // Final block: the fast side does not maintain the cumulative evacuation
                // map, so it cannot enumerate the drain. evacuationMapDiff / payoutObligations
                // are empty here; the slow side fills them from its own cumulative state (see
                // StackEffectsBuilder).
                // TODO: verify - don't we get the diff to drain everything from L2?
                blockResult = BlockResult(
                  brief = blockBrief,
                  evacuationMapDiff = Nil,
                  payoutObligations = Nil,
                  postDatedRefundTxs = Nil,
                  absorbedDeposits = Nil,
                  competingFallbackTxTime = p.competingFallbackTxTime
                )

                _ <- handleBlock(blockBrief, NotTriggered, blockResult)
            } yield ()
        }
    }

    /** When the joint ledger finishes producing (or reproducing) a brief:
      *   1. Broadcast the brief to peer liaisons — only when leading the block.
      *   2. Sign the brief and forward both brief + own soft-ack to the consensus actor for
      *      soft-confirmation. L1 effect signing (slow consensus) does not happen here.
      */
    private def handleBlock(
        brief: BlockBrief.Next,
        localFinalization: LocalFinalizationTrigger,
        blockResult: BlockResult
    ): IO[Unit] =
        for {
            conn <- getConnections
            _ <- brief match {
                case b: BlockBrief.Intermediate =>
                    tracer.traceWith(JointLedgerEvent.BriefProduced(b))
                case _ => IO.unit
            }
            softAck = ownHeadWallet.mkSoftAck(brief, localFinalization.asBoolean)
            // 0. Persist this peer's own per-soft-ack bundle before the soft-ack leaves (CR4
            //    write-before-send): own brief (leader) + own soft-ack lanes + BlockResult +
            //    deposits snapshot, in one atomic WriteBatch.
            _ <- persistOwnBlock(brief, softAck, blockResult)
            // 1. Broadcast the brief to peer liaisons (leader only).
            _ <- IO.whenA(config.ownHeadPeerId.isLeader(brief.blockNum))(
              (conn.peerLiaisons ! brief).parallel
            )
            // 2. Ship brief + own soft-ack to the consensus actor.
            _ <- conn.fastConsensusActor ! brief
            _ <- conn.fastConsensusActor ! softAck
            // 3. Slow side: hand the block result to the stack composer (independent of fast
            //    cycle).
            _ <- conn.stackComposer ! blockResult
        } yield ()

    /** Persist this peer's per-soft-ack bundle in one atomic `WriteBatch` (CR4/CR6/CR8, §6):
      *   - own `BlockBrief` → `Block` lane (**leader only** — the BlockLane author for this block);
      *   - own `SoftAck` → `SoftAck` lane;
      *   - the per-block `BlockResult` → `BlockResult` CF;
      *   - the current deposits snapshot → `DepositMap` CF.
      *
      * Lane values carry the 8-byte arrival stamp (creation time — local monotonic; §5.4/§7.1).
      */
    private def persistOwnBlock(
        brief: BlockBrief.Next,
        softAck: SoftAck,
        blockResult: BlockResult
    ): IO[Unit] =
        for {
            stamp <- persistence.arrivalStamp
            deposits <- state.get.map(_.deposits)
            briefBatch =
                if config.ownHeadPeerId.isLeader(brief.blockNum) then
                    WriteBatch.start.put(LaneKey.Block(brief.blockNum))(LaneValue(stamp, brief))
                else WriteBatch.start
            _ <- persistence.write(
              briefBatch
                  .put(LaneKey.SoftAck(softAck.peerNum, softAck.ackNum))(LaneValue(stamp, softAck))
                  .put(StoreKey.BlockResult(brief.blockNum))(blockResult)
                  .put(StoreKey.DepositMap)(deposits)
            )
        } yield ()

    // TODO: classify the mismatch instead of emitting a generic "consensus is broken" panic.
    //   One specific subcase worth singling out is "a deposit absorbed by the leader was not
    //   found onchain by this peer": the leader's brief lists `depositsAbsorbed` containing a
    //   request whose deposit utxo is missing from this peer's pollResults — i.e. the peer
    //   would have classified that deposit as `NotInPollResults` (refunded) and produced a
    //   different block. This typically reflects a polling cadence violating the
    //   `cardanoLiaisonPollingPeriodSafetyFactor` invariant on `TxTiming`.
    private def panicOnMismatchWithExpectedBrief(
        expectedBrief: Option[BlockBrief],
        actualBrief: BlockBrief
    ): IO[Unit] =
        IO.unlessA(expectedBrief.fold(true)(_ == actualBrief))(
          panic(
            "Reference block brief didn't match actual block brief; consensus is broken.\n" +
                s"actual block brief: $actualBrief\n" +
                s"expected block brief: $expectedBrief"
          ) >> context.self.stop
        )

    // Sends a panic to the multisig regime manager, indicating that the node cannot proceed any more
    // TODO: Implement better, it should be typed and the multisig regime manager should be able to pattern match
    private def panic(msg: String): IO[Unit] = throw new RuntimeException(msg)
}

/** ==Hydrozoa's joint ledger on Cardano in the multisig regime==
  *
  * Hydrozoa's joint ledger connects its dapp ledger to its virtual ledger. It dispatches some state
  * transitions to them individually, but it also periodically reconciles state transitions across
  * them to keep them aligned.
  */
object JointLedger {

    type Handle = ActorRef[IO, Requests.Request]

    type Config = HeadConfig.Section & OwnHeadPeerPrivate.Section

    final case class Connections(
        fastConsensusActor: FastConsensusActor.Handle,
        stackComposer: StackComposer.Handle,
        peerLiaisons: List[PeerLiaison.Handle]
    )

    enum UserRequestError extends Throwable:
        // Inherits Throwable.toString = "<className>: <getMessage>"; we override getMessage so
        // the rejection log shows all three timestamps and can be diagnosed at a glance.
        override def getMessage: String = this match
            case e: BlockOutOfRequestValidityInterval =>
                s"blockCreationStartTime=${e.blockCreationStartTime.convert}, " +
                    s"requestValidityStart=${e.requestValidityStart.convert}, " +
                    s"requestValidityEnd=${e.requestValidityEnd.convert}"

        case BlockOutOfRequestValidityInterval(
            blockCreationStartTime: BlockCreationStartTime,
            requestValidityStart: RequestValidityStartTime,
            requestValidityEnd: RequestValidityEndTime
        ) extends UserRequestError

    /** Failure registering a deposit into the fast-side L1 deposits map — either the deposit tx
      * fails to parse, or its submission deadline doesn't match the one expected from its
      * validity-end. Rejected via `rejectEvent` (stringified into the L2 proxy error), never
      * raised, so it need not extend `Throwable`.
      */
    sealed trait DepositLedgerError
    object DepositLedgerError {
        final case class ParseError(wrapped: DepositRefundTxSeq.Parse.Error)
            extends DepositLedgerError {
            override def toString: String = s"ParseError: $wrapped"
        }

        final case class DepositTxInvalidTTL(
            expectedSubmissionDeadline: QuantizedInstant,
            actualSubmissionDeadline: QuantizedInstant
        ) extends DepositLedgerError {
            override def toString: String =
                "DepositTxInvalidTTL: expected submission deadline " +
                    s"$expectedSubmissionDeadline, but got $actualSubmissionDeadline"
        }
    }

    object Requests {
        type Request =
            PreStart.type | UserRequestWithId | StartBlock | CompleteBlockRegular |
                CompleteBlockFinal | GetState.Sync | Block.SoftConfirmed.Next

        case object PreStart

        case class StartBlock(
            blockNum: BlockNumber,
            blockCreationStartTime: BlockCreationStartTime
        )

        /** @param referenceBlockBrief
          *   provided by the BlockWeaver when it is in follower mode. When the joint ledger is
          *   finished reproducing the block, it compares against this reference block to determine
          *   whether the leader properly constructed the original block.
          * @param pollResults
          *   there are two reasons to have it here:
          *   - pollResults are absent upon weaver's start time. Passing it here may improve things.
          *   - pollResults are needed only when we are finishing a regular (non-final) block.
          * @param finalizationLocallyTriggered
          *   this flag indicates that head finalization request was received LOCALLY and the next
          *   block should be the final block which is indicated by setting the flag
          *   `finalizationRequested` in the block acknowledgement
          */
        case class CompleteBlockRegular(
            referenceBlockBrief: Option[BlockBrief.Intermediate],
            pollResults: PollResults,
            finalizationLocallyTriggered: LocalFinalizationTrigger,
            blockCreationEndTime: BlockCreationEndTime
        )

        case class CompleteBlockFinal(
            referenceBlockBrief: Option[BlockBrief.Final],
            blockCreationEndTime: BlockCreationEndTime
        )

        case object GetState extends SyncRequest[IO, GetState.type, State] {
            type Sync = SyncRequest.Envelope[IO, GetState.type, State]

            def ?: : this.Send = SyncRequest.send(_, this)
        }

    }

    sealed trait State {
        def previousBlockHeader: BlockHeader

        /** The fast side's L1 deposits map. */
        def deposits: DepositsMap
    }

    object State {
        def initialize(config: Config): Done = Done(
          previousBlockHeader = config.initialBlock.blockBrief.header,
          deposits = DepositsMap.empty,
        )
    }

    final case class Done private[JointLedger] (
        override val previousBlockHeader: BlockHeader,
        override val deposits: DepositsMap,
    ) extends State {
        def setDeposits(newDeposits: DepositsMap): Done =
            this.focus(_.deposits).replace(newDeposits)

        def producing(
            l2LedgerState: L2LedgerState,
            startTime: BlockCreationStartTime,
            userRequestState: UserRequestState
        ): Producing = previousBlockHeader match {
            case b: BlockHeader.NonFinal =>
                Producing(
                  b,
                  deposits,
                  l2LedgerState,
                  startTime,
                  userRequestState
                )
            case _ =>
                throw new RuntimeException(
                  "Impossible: tried to produce next block after final block."
                )
        }

    }

    final case class Producing private[JointLedger] (
        override val previousBlockHeader: BlockHeader.NonFinal,
        override val deposits: DepositsMap,
        l2LedgerState: L2LedgerState,
        BlockCreationStartTime: BlockCreationStartTime,
        userRequestState: UserRequestState
    ) extends State {
        val nextBlockNumber: BlockNumber.BlockNumber = previousBlockHeader.blockNum.increment

        transparent inline def competingFallbackTxTime: FallbackTxStartTime =
            previousBlockHeader.fallbackTxStartTime

        def runL2CommandReal[F[_], T](
            l2Ledger: L2Ledger[F],
            command: L2LedgerCommand.Real
        ): F[Either[L2LedgerError, L2LedgerState]] = {
            val action = l2Ledger.L2LedgerAction.fromL2LedgerCommandReal(command)
            action.run(l2LedgerState)
        }

        def setDeposits(newDeposits: DepositsMap): Producing =
            this.focus(_.deposits).replace(newDeposits)

        def setL2LedgerState(newL2State: L2LedgerState): Producing =
            this.focus(_.l2LedgerState).replace(newL2State)

        def done(newBlockHeader: BlockHeader): Done =
            Done(newBlockHeader, deposits)
    }
}
