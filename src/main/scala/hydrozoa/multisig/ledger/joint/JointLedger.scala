package hydrozoa.multisig.ledger.joint

import cats.effect.{IO, Ref}
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.{ConsensusActor, PeerLiaison}
import hydrozoa.multisig.ledger.block.*
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag.{Invalid, Valid}
import hydrozoa.multisig.ledger.joint.EvacuationMap.applyDiffs
import hydrozoa.multisig.ledger.joint.JointLedger.*
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.*
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.l1.L1LedgerM
import hydrozoa.multisig.ledger.l1.L1LedgerM.*
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.l1.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import hydrozoa.multisig.ledger.l2.{L2Ledger, L2LedgerCommand, L2LedgerError, L2LedgerState}
import hydrozoa.multisig.server.UserRequestWithId
import monocle.Focus.focus
import scala.collection.immutable.Queue
import scala.math.Ordered.orderingToOrdered
import scalus.cardano.ledger.TransactionInput
import scalus.uplc.builtin.ByteString

// Fields of a work-in-progress block pertaining to user requests, with an additional field for dealing with withdrawn utxos
private case class UserRequestState(
    requests: List[(RequestId, ValidityFlag)],
    postDatedRefundTxs: Vector[RefundTx.PostDated]
)

// NOTE: Joint ledger is created by the MultisigManager.
// NOTE: As of 2025-11-16, George says BlockWeaver should be the ONLY actor calling the joint ledger
final case class JointLedger(
    config: JointLedger.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | JointLedger.Connections,
    l2Ledger: L2Ledger[IO],
    tracer: hydrozoa.lib.tracing.ProtocolTracer
) extends Actor[IO, Requests.Request] {
    import config.*

    private val logger = Logging.loggerIO("JointLedger")
    private val logger_ = Logging.logger("JointLedger1")

    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    val state: Ref[IO, JointLedger.State] =
        Ref.unsafe[IO, JointLedger.State](
          Done(
            producedBlock = config.initialBlock,
            lastFallbackValidityStart = config.initialFallbackTx.validityStart,
            l1LedgerState =
                L1LedgerM.State(config.initializationTx.treasuryProduced, DepositsMap.empty),
            evacuationMap = config.initialEvacuationMap
          )
        )

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
                      consensusActor = _connections.consensusActor,
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
                throw new RuntimeException(
                  "Expected a `Producing` State, but got `Done`. This indicates" +
                      " that a request was issued to the JointLedger that is only valid when the hydrozoa node is producing" +
                      " a block."
                )

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

    override def preStart: IO[Unit] = context.self ! Requests.PreStart

    // TODO: PartialFunction.fromFunction is a noop here
    override def receive: Receive[IO, Requests.Request] = PartialFunction.fromFunction {
        case Requests.PreStart       => preStartLocal
        case e: UserRequestWithId    => applyUserRequestWithId(e)
        case s: StartBlock           => startBlock(s)
        case c: CompleteBlockRegular => completeBlockRegular(c)
        case f: CompleteBlockFinal   => completeBlockFinal(f)
        case req: SyncRequest.Any =>
            req.request match {
                case r: GetState.type => r.handleSync(req, _ => state.get)
            }
        case p: Block.MultiSigned.Next => proxyConfirmation(p)
    }

    // QUESTION: This gets sent from the consensus actor, but the consensus actor has the full ability to send it
    // itself. Should we move this into the consensus actor?
    private def proxyConfirmation(next: Block.MultiSigned.Next): IO[Unit] =
        val l2Command = L2LedgerCommand.ProxyBlockConfirmation(
          next.blockNum,
          Vector.from(
            next.postDatedRefundTxs.map(refund =>
                (refund.requestId, ByteString.fromArray(refund.tx.toCbor))
            )
          )
        )
        for {
            // FIXME: Should we retry? Throw an exception?
            _ <- l2Ledger.L2LedgerAction
                .fromL2LedgerCommand(l2Command)
                // bit of a hack, but we don't care about the actual l2 state here.
                .run(L2LedgerState.empty)
        } yield ()

    private def preStartLocal: IO[Unit] =
        for {
            _ <- initializeConnections
            l2Command = L2LedgerCommand.Initialize(
              headId = config.headId,
              initialL2Value = config.initialL2Value,
              // TODO: This should ostensibly not be empty in the future
              l2Payload = ByteString.empty
            )
            // FIXME: should we retry?
            _ <- l2Ledger.L2LedgerAction
                .fromL2LedgerCommand(l2Command)
                .run(L2LedgerState.empty)
        } yield ()

    private def applyUserRequestWithId(e: UserRequestWithId): IO[Unit] = {
        // TODO: check that blockStartTime is within the request's validity bounds
        e match {
            case req: UserRequestWithId.DepositRequest     => registerDepositRequest(req)
            case req: UserRequestWithId.TransactionRequest => applyTransactionRequest(req)
        }
    }

    private def checkRequestValidityInterval(
        req: UserRequestWithId,
        blockCreationStartTime: QuantizedInstant
    ): Boolean = {
        val header = req.request.header
        val bt = blockCreationStartTime.toPosixTime
        header.validityStart <= bt && bt < header.validityEnd
    }

    private def rejectEvent(
        requestId: RequestId,
        e: JointLedger.UserRequestError | L1LedgerM.Error | L2LedgerError
    ): IO[Unit] =
        for {
            oldState <- unsafeGetProducing
            currentBlockNum = oldState.nextBlockNumber
            newState = oldState
                .focus(_.userRequestState.requests)
                .modify(_.appended((requestId, Invalid)))
            _ <- state.set(newState)
            _ <- logger.debug(s"registerDepositRequest failure: $e")
            _ <- tracer.eventProcessed(
              s"${requestId.peerNum: Int}:${requestId.requestNum: Int}",
              currentBlockNum: Int,
              false
            )
            l2Command = L2LedgerCommand.ProxyRequestError(
              requestId = requestId,
              message = e.toString
            )
            // FIXME: Should we retry?
            _ <- l2Ledger.L2LedgerAction
                .fromL2LedgerCommand(l2Command)
                .run(newState.l2LedgerState)
        } yield ()

    /** Update the JointLedger's state -- the work-in-progress block -- to accept or reject deposits
      * depending on whether the [[dappLedger]] Actor can successfully register the deposit,
      */
    private def registerDepositRequest(req: UserRequestWithId.DepositRequest): IO[Unit] = {
        import req.*
        import request.*
        import body.*

        for {
            p <- unsafeGetProducing
            blockStartTime = p.startTime
            currentBlockNum = p.nextBlockNumber

            _ <-
                if !checkRequestValidityInterval(req, blockStartTime) then
                    rejectEvent(
                      requestId,
                      JointLedger.UserRequestError.BlockOutOfRequestValidityInterval(blockStartTime)
                    )
                else {
                    L1LedgerM.registerDeposit(req).run(config, p.l1LedgerState) match {
                        case Left(error) => rejectEvent(requestId, error)
                        case Right(newL1State, (depositProduced, refundTx)) => {
                            val l2Command = L2LedgerCommand.RegisterDeposit(
                              requestId = requestId,
                              userVKey = req.request.userVk,
                              blockNumber = currentBlockNum,
                              blockCreationStartTime = p.startTime.toPosixTime,
                              depositId = depositProduced.utxoId,
                              depositFee = depositProduced.depositFee,
                              depositL2Value = depositProduced.l2Value,
                              refundDestination = refundTx.refundDestination,
                              l2Payload = l2Payload
                            )
                            for {
                                res <- l2Ledger.L2LedgerAction
                                    .fromL2LedgerCommand(l2Command)
                                    .run(p.l2LedgerState)
                                _ <- res match {
                                    // FIXME: Should we distinguish between genuine L2 failures and things like
                                    // network errors?
                                    case Left(e) => rejectEvent(requestId, e)
                                    case Right(newL2State) =>
                                        for {
                                            _ <- state.set(
                                              p.setL1LedgerState(newL1State)
                                                  .setL2LedgerState(newL2State)
                                                  .focus(_.userRequestState.requests)
                                                  .modify(_.appended((requestId, Valid)))
                                                  .focus(_.userRequestState.postDatedRefundTxs)
                                                  .modify(_.appended(refundTx))
                                            )
                                            _ <- tracer.eventProcessed(
                                              s"${requestId.peerNum: Int}:${requestId.requestNum: Int}",
                                              currentBlockNum: Int,
                                              true
                                            )
                                        } yield ()
                                }
                            } yield ()
                        }
                    }
                }
        } yield ()
    }

    /** Update the current block with the result of passing the tx to the virtual ledger, as well as
      * updating ledgerEventsRequired
      */
    private def applyTransactionRequest(
        req: UserRequestWithId.TransactionRequest
    ): IO[Unit] = {
        import req.*
        import request.*
        import body.*

        for {
            p <- unsafeGetProducing
            blockStartTime = p.startTime
            currentBlockNum = p.nextBlockNumber

            _ <-
                if !checkRequestValidityInterval(req, blockStartTime) then
                    rejectEvent(
                      requestId,
                      JointLedger.UserRequestError.BlockOutOfRequestValidityInterval(blockStartTime)
                    )
                else {
                    val l2Command: L2LedgerCommand.ApplyTransaction = L2LedgerCommand
                        .ApplyTransaction(
                          requestId = req.requestId,
                          userVKey = req.request.userVk,
                          blockNumber = p.nextBlockNumber,
                          blockCreationStartTime = p.startTime,
                          l2Payload = l2Payload
                        )

                    for {
                        res <- this.l2Ledger.L2LedgerAction
                            .fromL2LedgerCommand(l2Command)
                            .run(p.l2LedgerState)
                        _ <- res match {
                            case Left(e) => rejectEvent(requestId, e)
                            case Right(newL2State) =>
                                for {
                                    _ <- state.set(
                                      p.setL2LedgerState(newL2State)
                                          .focus(_.userRequestState.requests)
                                          .modify(_.appended((requestId, Valid)))
                                    )
                                    _ <- tracer.eventProcessed(
                                      s"${requestId.peerNum: Int}:${requestId.requestNum: Int}",
                                      currentBlockNum: Int,
                                      true
                                    )
                                } yield ()
                        }
                    } yield ()
                }
        } yield ()
    }

    /** Moves the state of the JointLedger from "Done" to "Producing", setting the time and
      * ledgerEventsRequired appropriately, while initializing all other fields.
      * @return
      */
    private def startBlock(args: StartBlock): IO[Unit] = {
        import args.*
        for {
            _ <- logger.info(s"start block: ${args.blockNum}...")
            d <- unsafeGetDone
            _ <- state.set(
              Producing(
                previousBlock = d.producedBlock,
                competingFallbackValidityStart = d.lastFallbackValidityStart,
                startTime = blockCreationTime,
                userRequestState = UserRequestState(
                  requests = List.empty,
                  postDatedRefundTxs = Vector.empty
                ),
                l1LedgerState = d.l1LedgerState,
                l2LedgerState = L2LedgerState.empty,
                evacuationMap = d.evacuationMap
              )
            )
        } yield ()
    }

    /** Complete a Minor or Major block If
      * @return
      */
    private def completeBlockRegular(args: CompleteBlockRegular): IO[Unit] = {
        import args.*

        /** @return
          *   Queue order:
          *   - eligible for absorption
          *   - ineligible for absorption - (immature + mature but non-existent)
          *   - rejected
          */

        def partitionDeposits(
            depositsMap: DepositsMap,
            blockStartTime: QuantizedInstant,
            settlementValidityEnd: QuantizedInstant
        ): IO[
          (
              DepositsMap,
              DepositsMap,
              DepositsMap
          )
        ] = for {
            _ <- IO.pure(())

            _ <- logger.trace(
              s"partitionDeposits: deposits: ${depositsMap.treeMap.values.map(_.map(_._1))}, " +
                  s"blockStartTime=$blockStartTime, " +
                  s"settlementValidityEnd=$settlementValidityEnd"
            )

            ret = depositsMap.foldLeft(
              (
                DepositsMap.empty,
                DepositsMap.empty,
                DepositsMap.empty
              )
            ) { case (acc0, (_absorptionStartTime, depositQueue)) =>
                depositQueue.foldLeft(acc0) { case (acc, event @ (_eventId, deposit)) =>
                    val depositValidityEnd = deposit.submissionDeadline
                    val depositAbsorptionStart =
                        txTiming.depositAbsorptionStartTime(depositValidityEnd)
                    val depositAbsorptionEnd =
                        txTiming.depositAbsorptionEndTime(depositValidityEnd)

                    // Maturity. The deposit is mature if its absorption start time is no later
                    // than the block brief’s creation end time.
                    val isMature = depositAbsorptionStart <= blockCreationEndTime

                    // Non-expiry. The deposit is non-expired if its absorption end time is
                    // no earlier than the competing fallback’s validity start time.

                    // NB: Here, we can't use submission deadline, which is currently the same thing as deposit tx
                    // ttl to decide whether we should reject a deposit - even if the submission time is over,
                    // the state we observe may fall behind for any reason (network congestion, fork).
                    // So we can only use absorption end time to decide it's time to reject a deposit.
                    val isExpired = settlementValidityEnd > depositAbsorptionEnd

                    // TODO: The fast-consensus leader sees the deposit on the L1 blockchain when he ends his leadership term.
                    // If a follower is reproducing the block brief, replace this condition with:
                    // the follower sees the deposit on the L1 blockchain when he finishes verifying
                    // the block brief’s events.
                    val isExistent = pollResults.contains(deposit.toUtxo.input)

                    val isEligible = !isExpired && isMature && isExistent

                    // A deposit is ineligible and must be rejected if
                    // - the deposit is expired, or
                    // - if it’s mature and non-expired but doesn’t exist on L1.
                    val isRejected = isExpired || (isMature && !isExistent)

                    val isIneligible = !isExpired && (!isMature || !isExistent)

                    logger_.trace(
                      s"deposit: ${deposit._1}, " +
                          s"depositValidityEnd=$depositValidityEnd, " +
                          s"depositAbsorptionStart=$depositAbsorptionStart, " +
                          s"depositAbsorptionEnd=$depositAbsorptionEnd, " +
                          s"isMature=$isMature, " +
                          s"isExpired=$isExpired, " +
                          s"isExistent=$isExistent, " +
                          s"isEligible=$isEligible, " +
                          s"isRejected=$isRejected, " +
                          s"isIneligible=$isIneligible"
                    )

                    if isEligible
                    then acc.focus(_._1).modify(_.appended(event, config.txTiming))
                    else if isIneligible
                    then acc.focus(_._2).modify(_.appended(event, config.txTiming))
                    else if isExpired
                    then acc.focus(_._3).modify(_.appended(event, config.txTiming))
                    else throw RuntimeException("Deposit decision function is not total")
                }
            }
        } yield (ret._1, ret._2, ret._3)

        /** KZG commitment + block brief (which is a bit strange)
          */
        def mkBlockBrief(
            absorbedDeposits: Queue[(RequestId, DepositUtxo)],
            rejectedDeposits: Queue[(RequestId, DepositUtxo)],
        ): IO[BlockBrief.Intermediate] = for {

            p <- unsafeGetProducing

            previousHeader = p.previousBlock.header
            blockWithdrawnUtxos = p.l2LedgerState.payouts
            blockStartTime = p.startTime
            competingFallbackValidityStart = p.competingFallbackValidityStart
            events = p.userRequestState.requests

            _ <- logger.trace(
              s"mkBlockBrief: previousHeader=$previousHeader\n" +
                  s"mkBlockBrief: blockWithdrawnUtxos=$blockWithdrawnUtxos\n" +
                  s"mkBlockBrief: blockStartTime=$blockStartTime\n" +
                  s"mkBlockBrief: competingFallbackValidityStart=$competingFallbackValidityStart\n" +
                  s"mkBlockBrief: events=$events"
            )

            // Block header
            headerIntermediate: BlockHeader.Intermediate <-
                if absorbedDeposits.isEmpty && blockWithdrawnUtxos.isEmpty
                then
                    IO.pure(
                      previousHeader.nextHeaderIntermediate(
                        txTiming,
                        blockStartTime,
                        args.blockCreationEndTime,
                        competingFallbackValidityStart,
                        // TODO: We want this to be done in a separate actor in the future
                        // this doesn't include genesis
                        applyDiffs(p.evacuationMap, p.l2LedgerState.diffs).kzgCommitment
                      )
                    )
                else {
                    val depositEventDecisions: L2LedgerCommand.ApplyDepositDecisions =
                        L2LedgerCommand.ApplyDepositDecisions(
                          blockNumber = p.nextBlockNumber,
                          blockCreationEndTime = blockCreationEndTime.toPosixTime,
                          // Why vector and not Queue?
                          absorbedDeposits = Vector.from(absorbedDeposits.map(_._1)),
                          rejectedDeposits = Vector.from(rejectedDeposits.map(_._1))
                        )
                    for {
                        res <- this.l2Ledger.L2LedgerAction
                            .fromL2LedgerCommand(depositEventDecisions)
                            .run(p.l2LedgerState)
                        newL2State <- IO.fromEither(res)
                        newEvacuationMap = applyDiffs(p.evacuationMap, newL2State.diffs)
                        newJLState = p
                            .setL2LedgerState(newL2State)
                            .focus(_.evacuationMap)
                            .replace(newEvacuationMap)
                        _ <- state.set(newJLState)

                        // TODO: We want this to be done in a separate actor in the future
                        kzgCommitment = newEvacuationMap.kzgCommitment
                        headerIntermediate = previousHeader.nextHeaderMajor(
                          blockStartTime,
                          args.blockCreationEndTime,
                          kzgCommitment
                        )
                    } yield headerIntermediate
                }

            // Block brief
            blockBrief: BlockBrief.Intermediate = headerIntermediate match {
                case header: BlockHeader.Minor =>
                    val blockBody = BlockBody.Minor(events, rejectedDeposits.map(_._1).toList)
                    BlockBrief.Minor(header, blockBody)
                case header: BlockHeader.Major =>
                    val blockBody = BlockBody.Major(
                      events,
                      absorbedDeposits.map(_._1).toList,
                      rejectedDeposits.map(_._1).toList
                    )
                    BlockBrief.Major(header, blockBody)
            }
        } yield blockBrief

        def mkBlockEffects(
            next: BlockBrief.Intermediate,
            absorbedDeposits: Queue[(RequestId, DepositUtxo)],
            postDatedRefundTxs: List[RefundTx.PostDated]
        ): IO[Block.Unsigned.Next] = for {
            ret <- next match {
                case blockBrief @ BlockBrief.Minor(header, _) =>
                    val blockEffects = BlockEffects.Unsigned.Minor(
                      headerSerialized = header.onchainMsg,
                      postDatedRefundTxs = postDatedRefundTxs
                    )
                    IO.pure(Block.Unsigned.Minor(blockBrief, blockEffects))
                case blockBrief @ BlockBrief.Major(header, _) =>
                    for {
                        p <- unsafeGetProducing

                        payoutObligations = p.l2LedgerState.payouts

                        res <- IO.fromEither(
                          L1LedgerM
                              .mkSettlementTxSeq(
                                nextKzg = header.kzgCommitment,
                                absorbedDeposits = absorbedDeposits,
                                payoutObligations = payoutObligations,
                                blockCreationEndTime = header.endTime,
                                competingFallbackValidityStart = p.competingFallbackValidityStart
                              )
                              .run(config, p.l1LedgerState)
                        )

                        (newL1State, settlementTxSeq) = res
                        _ <- state.set(p.setL1LedgerState(newL1State))

                        blockEffects = BlockEffects.Unsigned.Major(
                          settlementTx = settlementTxSeq.settlementTx,
                          fallbackTx = settlementTxSeq.fallbackTx,
                          rolloutTxs = settlementTxSeq.rolloutTxs,
                          postDatedRefundTxs = postDatedRefundTxs
                        )
                    } yield Block.Unsigned.Major(blockBrief, blockEffects)
            }
        } yield ret

        // ===================================
        // Finally, the body of completeBlockRegular
        // ===================================

        import config.txTiming

        for {
            _ <- logger.info(s"complete block ${args.referenceBlockBrief}")
            producing <- unsafeGetProducing

            ret <- partitionDeposits(
              depositsMap = producing.l1LedgerState.deposits,
              blockStartTime = producing.startTime,
              settlementValidityEnd =
                  txTiming.newSettlementEndTime(producing.competingFallbackValidityStart)
            )

            (eligible, ineligible, rejected) = ret

            (absorbedDeposits, unabsorbedDeposits) = eligible.splitAt(maxDepositsAbsorbedPerBlock)

            _ <- logger.trace(
              s"joint ledger: absorbed=$absorbedDeposits" + "\n" +
                  s"joint ledger: unabsorbed=$unabsorbedDeposits" + "\n" +
                  s"joint ledger: ineligible=$ineligible" + "\n" +
                  s"joint ledger: rejected=$rejected" + "\n"
            )

            blockBrief <- mkBlockBrief(
              absorbedDeposits = Queue.from(absorbedDeposits.flatValues),
              rejectedDeposits = Queue.from(rejected.flatValues)
            )

            block <- mkBlockEffects(
              blockBrief,
              Queue.from(absorbedDeposits.flatValues),
              producing.userRequestState.postDatedRefundTxs.toList
            )

            _ <- checkReferenceBlock(referenceBlockBrief, block)

            // Block is done

            res <- IO.fromEither(
              L1LedgerM
                  .handleBlockBrief(unabsorbedDeposits ++ ineligible)
                  .run(config, producing.l1LedgerState)
            )
            (newL1State, ()) = res
            _ <- state.set(producing.setL1LedgerState(newL1State))

            // Tell others about the block
            _ <- handleBlock(block, finalizationLocallyTriggered)
        } yield ()
    }

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
        import config.txTiming

        for {
            p <- unsafeGetProducing

            res <- IO.fromEither(
              L1LedgerM
                  .finalizeLedger(
                    payoutObligationsRemaining = Vector.from(
                      p.evacuationMap.evacuationMap.map((_, o) => Payout.Obligation(o))
                    ),
                    competingFallbackValidityStart = p.competingFallbackValidityStart
                  )
                  .run(config, p.l1LedgerState)
            )
            (newL1State, finalizationTxSeq) = res
            _ <- state.set(p.setL1LedgerState(newL1State))

            block: Block.Unsigned.Final = {
                import p.userRequestState.*
                val blockHeader =
                    p.previousBlock.header.nextHeaderFinal(p.startTime, args.blockCreationEndTime)

                val blockBody = BlockBody.Final(
                  events = requests,
                  // Final block should reject all the deposits known.
                  depositsRefunded = p.l1LedgerState.deposits.flatEvents.toList
                )

                val blockBrief = BlockBrief.Final(blockHeader, blockBody)

                val blockEffects = BlockEffects.Unsigned.Final(
                  finalizationTx = finalizationTxSeq.finalizationTx,
                  rolloutTxs = finalizationTxSeq.rolloutTxs
                )

                Block.Unsigned.Final(blockBrief, blockEffects)
            }

            _ <- checkReferenceBlock(referenceBlockBrief, block)
            _ <- handleBlock(block, false)

        } yield ()
    }

    /** Extract trace metadata from a block for the tracer.
      *
      * @param block
      *   the block to extract metadata from
      * @return
      *   tuple of (blockType, versionMajor, versionMinor, eventCount)
      */
    private def extractBlockTraceMetadata(
        block: Block.Unsigned.Next
    ): (String, Int, Int, Int) = block match {
        case b: Block.Unsigned.Minor =>
            (
              "minor",
              b.header.blockVersion.major: Int,
              b.header.blockVersion.minor: Int,
              b.body.events.size
            )
        case b: Block.Unsigned.Major =>
            (
              "major",
              b.header.blockVersion.major: Int,
              b.header.blockVersion.minor: Int,
              b.body.events.size
            )
        case b: Block.Unsigned.Final =>
            (
              "final",
              b.header.blockVersion.major: Int,
              b.header.blockVersion.minor: Int,
              b.body.events.size
            )
    }

    /** When a block is finished, we handle it by:
      *   - sending the pure (with no effects) block to peer liaisons for circulation
      *   - sending the block brief to the peer liaisons
      *   - sending the block to the consensus actor
      *   - signing block's effects and producing our own set of acks
      *   - sending block's ack(s) to the consensus actor
      */
    private def handleBlock(
        block: Block.Unsigned.Next,
        localFinalization: Boolean
    ): IO[Unit] =
        for {
            conn <- getConnections
            (bt, vMaj, vMin, evtCnt) = extractBlockTraceMetadata(block)
            _ <- tracer.briefProduced(
              block.blockNum: Int,
              config.ownHeadPeerNum: Int,
              bt,
              vMaj,
              vMin,
              evtCnt
            )
            acks = ownHeadWallet.mkAcks(block, localFinalization)
            _ <- (conn.peerLiaisons ! block.blockBriefNext).parallel
            _ <- conn.consensusActor ! block
            _ <- IO.traverse_(acks)(ack => conn.consensusActor ! ack)
        } yield ()

    private def checkReferenceBlock(
        expectedBlockBrief: Option[BlockBrief],
        actualBlock: Block
    ): IO[Unit] = for {
        p <- unsafeGetProducing
        fallbackValidityStart: QuantizedInstant =
            actualBlock match {
                case major: Block.Unsigned.Major =>
                    major.effects.fallbackTx.validityStart
                case _ => p.competingFallbackValidityStart
            }

        _ <- expectedBlockBrief match {
            case Some(refBlock) if refBlock == actualBlock =>
                state.set(
                  Done(
                    actualBlock.block,
                    fallbackValidityStart,
                    p.l1LedgerState,
                    p.evacuationMap
                  )
                )
            case Some(_) =>
                panic(
                  "Reference block didn't match actual block; consensus is broken."
                ) >> context.self.stop
            case None =>
                state.set(
                  Done(
                    actualBlock.block,
                    fallbackValidityStart,
                    p.l1LedgerState,
                    p.evacuationMap
                  )
                )
        }
    } yield ()

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
        consensusActor: ConsensusActor.Handle,
        peerLiaisons: List[PeerLiaison.Handle]
    )

    enum UserRequestError extends Throwable:
        case BlockOutOfRequestValidityInterval(blockCreationStartTime: QuantizedInstant)
            extends UserRequestError

    object Requests {
        type Request =
            PreStart.type | UserRequestWithId | StartBlock | CompleteBlockRegular |
                CompleteBlockFinal | GetState.Sync | Block.MultiSigned.Next

        case object PreStart

        case class StartBlock(
            blockNum: BlockNumber,
            blockCreationTime: QuantizedInstant
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
            pollResults: Set[TransactionInput],
            finalizationLocallyTriggered: Boolean,
            blockCreationEndTime: QuantizedInstant
        )

        case class CompleteBlockFinal(
            referenceBlockBrief: Option[BlockBrief.Final],
            blockCreationEndTime: QuantizedInstant
        )

        case object GetState extends SyncRequest[IO, GetState.type, State] {
            type Sync = SyncRequest.Envelope[IO, GetState.type, State]

            def ?: : this.Send = SyncRequest.send(_, this)
        }

    }

    sealed trait State {
        def l1LedgerState: L1LedgerM.State
        def evacuationMap: EvacuationMap
    }

    final case class Done(
        producedBlock: Block,
        // None for the first block
        lastFallbackValidityStart: QuantizedInstant,
        override val l1LedgerState: L1LedgerM.State,
        override val evacuationMap: EvacuationMap
    ) extends State {
        def setL1LedgerState(newL1State: L1LedgerM.State): Done =
            this.focus(_.l1LedgerState).replace(newL1State)
    }

    final case class Producing(
        override val l1LedgerState: L1LedgerM.State,
        override val evacuationMap: EvacuationMap,
        l2LedgerState: L2LedgerState,
        previousBlock: Block,
        // None for the first block
        competingFallbackValidityStart: QuantizedInstant,
        startTime: QuantizedInstant,
        userRequestState: UserRequestState
    ) extends State {
        val nextBlockNumber: BlockNumber.BlockNumber = previousBlock.blockNum.increment

        def setL1LedgerState(newL1State: L1LedgerM.State): Producing =
            this.focus(_.l1LedgerState).replace(newL1State)

        def setL2LedgerState(newL2State: L2LedgerState): Producing =
            this.focus(_.l2LedgerState).replace(newL2State)
    }
}
