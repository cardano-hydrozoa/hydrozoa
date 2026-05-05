package hydrozoa.multisig.ledger.joint

import cats.effect.{IO, IOLocal, Ref}
import com.bloxbean.cardano.client.util.HexUtil
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import com.suprnation.typelevel.actors.syntax.BroadcastOps
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.multisig.timing.TxTiming.BlockTimes.{BlockCreationEndTime, BlockCreationStartTime, FallbackTxStartTime}
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.actor.*
import hydrozoa.lib.logging.{Tracer, logWith}
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.BlockWeaver.LocalFinalizationTrigger
import hydrozoa.multisig.consensus.BlockWeaver.LocalFinalizationTrigger.NotTriggered
import hydrozoa.multisig.consensus.pollresults.PollResults
import hydrozoa.multisig.consensus.{ConsensusActor, PeerLiaison, UserRequestWithId, pollresults}
import hydrozoa.multisig.ledger.block.*
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag.{Invalid, Valid}
import hydrozoa.multisig.ledger.joint.EvacuationMap.applyDiffs
import hydrozoa.multisig.ledger.joint.JointLedger.*
import hydrozoa.multisig.ledger.joint.JointLedger.Requests.*
import hydrozoa.multisig.ledger.l1.L1LedgerM
import hydrozoa.multisig.ledger.l1.L1LedgerM.*
import hydrozoa.multisig.ledger.l1.deposits.map.DepositsMap
import hydrozoa.multisig.ledger.l1.tx.RefundTx
import hydrozoa.multisig.ledger.l1.txseq.{FinalizationTxSeq, SettlementTxSeq}
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import hydrozoa.multisig.ledger.l2.{L2Ledger, L2LedgerCommand, L2LedgerError, L2LedgerState}
import monocle.Focus.focus
import scalus.uplc.builtin.ByteString

private case class UserRequestState(
    requests: List[(RequestId, ValidityFlag)],
    postDatedRefundTxs: Vector[RefundTx.PostDated]
)

final case class JointLedger(
    config: JointLedger.Config,
    pendingConnections: MultisigRegimeManager.PendingConnections | JointLedger.Connections,
    l2Ledger: L2Ledger[IO],
    tracer: hydrozoa.lib.tracing.ProtocolTracer,
    tracerLocal: IOLocal[Tracer]
) extends Actor[IO, Requests.Request] {
    import config.*

    given IOLocal[Tracer] = tracerLocal

    private val connections = Ref.unsafe[IO, Option[Connections]](None)

    val state: Ref[IO, JointLedger.State] =
        Ref.unsafe[IO, JointLedger.State](JointLedger.State.initialize(config))

    private def executeL1Action[T](
        state: JointLedger.Producing,
        action: L1LedgerM[T]
    ): IO[(L1LedgerM.State, T)] = for {
        either <- IO.pure(runL1Action[T](state, action))
        ret <- either match {
            case Left(err) =>
                Tracer.error(s"L1 action failed: $err") *> IO.raiseError(err)
            case Right(ret) => IO.pure(ret)
        }
    } yield ret

    private def executeL2Command(
        state: JointLedger.Producing,
        command: L2LedgerCommand.Real
    ): IO[L2LedgerState] = for {
        either <- runL2Command(state, command)
        ret <- either match {
            case Left(err) =>
                Tracer.error(s"L2 command failed: $err") *> IO.raiseError(err)
            case Right(ret) => IO.pure(ret)
        }
    } yield ret

    private def executeL2ProxyCommand(
        command: L2LedgerCommand.Proxy
    ): IO[Unit] = L2LedgerState
        .executeProxyCommand(l2Ledger, command)
        .handleErrorWith { err =>
            Tracer.error(s"L2 proxy command failed: $err") *> IO.raiseError(err)
        }

    private def runL1Action[T](
        state: JointLedger.Producing,
        action: L1LedgerM[T]
    ): Either[L1LedgerM.Error, (L1LedgerM.State, T)] =
        state.runL1Action[T](config, action)

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
                val msg = "Expected a `Producing` State, but got `Done`. This indicates" +
                    " that a request was issued to the JointLedger that is only valid when the hydrozoa node is producing" +
                    " a block."
                Tracer.error(msg) >> IO.raiseError(RuntimeException(msg))
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

    override def receive: Receive[IO, Requests.Request] = PartialFunction.fromFunction(receiveTotal)

    private def receiveTotal(req: Requests.Request): IO[Unit] = {
        Tracer.scoped(_.copy(logger = "JointLedger")) {
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
                case p: Block.MultiSigned.Next => proxyConfirmation(p)
            }
        }
    }

    // QUESTION: This gets sent from the consensus actor, but the consensus actor has the full ability to send it
    // itself. Should we move this into the consensus actor?
    private def proxyConfirmation(next: Block.MultiSigned.Next): IO[Unit] = {
        val l2Command = L2LedgerCommand.ProxyBlockConfirmation(
          next.blockNum,
          Vector.from(
            next.postDatedRefundTxs.map(refund =>
                (refund.requestId, ByteString.fromArray(refund.tx.toCbor))
            )
          )
        )
        executeL2ProxyCommand(l2Command)
    }

    private def preStartLocal: IO[Unit] =
        for {
            _ <- initializeConnections
        } yield ()

    private def applyUserRequestWithId(e: UserRequestWithId): IO[Unit] = {
        Tracer.scopedCtx("requestId" -> e.requestId.toString)(
          e match {
              case req: UserRequestWithId.DepositRequest     => registerDeposit(req)
              case req: UserRequestWithId.TransactionRequest => applyTransaction(req)
          }
        )
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
        e: JointLedger.UserRequestError | L1LedgerM.Error | L2LedgerError
    ): IO[Unit] =
        for {
            oldState <- unsafeGetProducing
            currentBlockNum = oldState.nextBlockNumber
            newState = oldState
                .focus(_.userRequestState.requests)
                .modify(_.appended((requestId, Invalid)))
            _ <- state.set(newState)
            _ <- Tracer.warn(s"Request rejected ($requestId): $e")
            _ <- tracer.eventProcessed(
              s"${requestId.peerNum}:${requestId.requestNum}",
              currentBlockNum.toLong,
              false
            )
            l2Command = L2LedgerCommand.ProxyRequestError(
              requestId = requestId,
              message = e.toString
            )
            // FIXME: Should we retry?
            _ <- executeL2ProxyCommand(l2Command)
        } yield ()

    /** Update the JointLedger's state -- the work-in-progress block -- to accept or reject deposits
      * depending on whether the [[dappLedger]] Actor can successfully register the deposit,
      */
    private def registerDeposit(req: UserRequestWithId.DepositRequest): IO[Unit] = {
        import req.*
        import request.*
        import body.*

        for {
            _ <- Tracer.info(s"register new deposit, request id: $requestId)")

            p <- unsafeGetProducing
            blockStartTime = p.BlockCreationStartTime
            currentBlockNum = p.nextBlockNumber

            _ <-
                if !checkRequestValidityInterval(req, blockStartTime) then
                    rejectEvent(
                      requestId,
                      JointLedger.UserRequestError.BlockOutOfRequestValidityInterval(blockStartTime)
                    )
                else {
                    val l1Res = L1LedgerM.registerDeposit(req).run(config, p.l1LedgerState)
                    l1Res match {
                        case Left(error) => rejectEvent(requestId, error)
                        case Right(newL1State, (depositProduced, refundTx)) => {
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
                                              p.setL1LedgerState(newL1State)
                                                  .setL2LedgerState(newL2State)
                                                  .focus(_.userRequestState.requests)
                                                  .modify(_.appended((requestId, Valid)))
                                                  .focus(_.userRequestState.postDatedRefundTxs)
                                                  .modify(_.appended(refundTx))
                                            )
                                            _ <- Tracer.debug(s"Request processed ($requestId)")
                                            _ <- tracer.eventProcessed(
                                              s"${requestId.peerNum}:${requestId.requestNum}",
                                              currentBlockNum.toLong,
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
    private def applyTransaction(
        req: UserRequestWithId.TransactionRequest
    ): IO[Unit] = {
        import req.*
        import request.*
        import body.*

        for {
            _ <- Tracer.info(s"applying transaction, request id: $requestId)")

            p <- unsafeGetProducing
            blockStartTime = p.BlockCreationStartTime
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
                                    _ <- tracer.eventProcessed(
                                      s"${requestId.peerNum}:${requestId.requestNum}",
                                      currentBlockNum.toLong,
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
        Tracer.scopedCtx("blockNum" -> s"${args.blockNum: Int}") {
            for {
                _ <- Tracer.info(s"start block: ${args.blockNum}...")
                _ <- Tracer.trace(s"blockCreationStartTime=$blockCreationStartTime")
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
    }

    /** Complete a Minor or Major block If
      * @return
      */
    private def completeBlockRegular(
        args: CompleteBlockRegular
    ): IO[Unit] = {
        import args.*
        unsafeGetProducing.flatMap { p =>
            Tracer.scopedCtx("blockNum" -> s"${p.nextBlockNumber: Int}") {
                for {
                    _ <- Tracer.info(s"completing block ${p.nextBlockNumber}")
                    _ <- Tracer.trace(s"blockCreationEndTime=$blockCreationEndTime")
                    _ <- Tracer.trace(s"competingFallbackTxTime=${p.competingFallbackTxTime}")
                    _ <- Tracer.trace(
                      s"settlementTxEndTime=${config.txTiming.newSettlementEndTime(p.competingFallbackTxTime)}"
                    )

                    partition = p.l1LedgerState.deposits.partition(
                      blockCreationEndTime = blockCreationEndTime,
                      settlementTxEndTime =
                          config.txTiming.newSettlementEndTime(p.competingFallbackTxTime),
                      pollResults = pollResults
                    )

                    split = partition.split(maxDepositsAbsorbedPerBlock)

                    // We don't need to trace this if we're tracing the `split`
                    // Because `split` is a refinement of `partition`.
                    // _ <- Tracer.trace(partition.toString)

                    _ <- Tracer.trace(split.toString)

                    blockBriefRes <- mkBlockBriefIntermediate(
                      p,
                      blockCreationEndTime,
                      split.decisions
                    )
                    (pBlockBrief, blockBrief) = blockBriefRes

                    blockRes <- mkBlockEffectsIntermediate(
                      pBlockBrief,
                      blockBrief,
                      split.absorbed.unzip,
                      pBlockBrief.userRequestState.postDatedRefundTxs.toList
                    )
                    (pBlock, block) = blockRes

                    // Verify the block against the reference block
                    _ <- panicOnMismatchWithExpectedBlock(referenceBlockBrief, block)

                    // Block is done
                    res <- executeL1Action(pBlock, L1LedgerM.handleBlockBrief(split.surviving))
                    (newL1State, ()) = res

                    newJlState = pBlock.setL1LedgerState(newL1State)

                    _ <- state.set(newJlState.done(block.header))

                    // Tell others about the block
                    _ <- handleBlock(block, finalizationLocallyTriggered)
                } yield ()
            }
        }
    }

    /** KZG commitment + block brief (which is a bit strange)
      */
    def mkBlockBriefIntermediate(
        p: JointLedger.Producing,
        blockCreationEndTime: BlockCreationEndTime,
        decisions: DepositsMap.Decisions
    ): IO[(JointLedger.Producing, BlockBrief.Intermediate)] = {
        val blockCreationStartTime = p.BlockCreationStartTime
        val previousHeader = p.previousBlockHeader
        val blockWithdrawnUtxos = p.l2LedgerState.payouts
        val events = p.userRequestState.requests
        for {
            _ <- Tracer.trace(
              s"mkBlockBrief: previousHeader=$previousHeader\n" +
                  s"mkBlockBrief: blockWithdrawnUtxos=$blockWithdrawnUtxos\n" +
                  s"mkBlockBrief: blockStartTime=$blockCreationStartTime\n" +
                  s"mkBlockBrief: competingFallbackValidityStart=${p.competingFallbackTxTime}\n" +
                  s"mkBlockBrief: events=$events\n" +
                  s"mkBlockBrief: decisions.absorbed=${decisions.absorbed.requestIds}\n" +
                  s"mkBlockBrief: decisions.refunded=${decisions.refunded.requestIds}"
            )

            depositEventDecisions: L2LedgerCommand.ApplyDepositDecisions =
                L2LedgerCommand.ApplyDepositDecisions(
                  blockNumber = p.nextBlockNumber,
                  blockCreationEndTime = blockCreationEndTime.toPosixTime,
                  absorbedDeposits = decisions.absorbed.requestIds,
                  refundedDeposits = decisions.refunded.requestIds
                )

            // Block header
            headerRes <-
                if decisions.absorbed.isEmpty && blockWithdrawnUtxos.isEmpty
                then
                    val newEvacuationMap = applyDiffs(p.evacuationMap, p.l2LedgerState.diffs)
                    for {
                        newL2State <-
                            if decisions.refunded.isEmpty then IO.pure(p.l2LedgerState)
                            else executeL2Command(p, depositEventDecisions)
                        _ <- Tracer.trace(s"New evacuation map: ${newEvacuationMap.evacuationMap}")

                        // Update the state with the new evacuation map
                        newJLState = p
                            .setL2LedgerState(newL2State)
                            .focus(_.evacuationMap)
                            .replace(newEvacuationMap)

                        // TODO: We want this to be done in a separate actor in the future
                        // this doesn't include genesis
                        headerIntermediate <- previousHeader
                            .nextHeaderIntermediate(
                              txTiming,
                              blockCreationStartTime,
                              blockCreationEndTime,
                              decisions.mNextAbsorptionStartTime,
                              newEvacuationMap.kzgCommitment
                            )
                            .logWith
                    } yield (newJLState, headerIntermediate)
                else {
                    for {
                        newL2State <- executeL2Command(p, depositEventDecisions)
                        newEvacuationMap = applyDiffs(p.evacuationMap, newL2State.diffs)
                        _ <- Tracer.trace(s"New evacuation map: ${newEvacuationMap.evacuationMap}")
                        newJLState = p
                            .setL2LedgerState(newL2State)
                            .focus(_.evacuationMap)
                            .replace(newEvacuationMap)

                        // TODO: We want this to be done in a separate actor in the future
                        kzgCommitment = newEvacuationMap.kzgCommitment
                        headerIntermediate <- previousHeader
                            .nextHeaderMajor(
                              txTiming,
                              blockCreationStartTime,
                              blockCreationEndTime,
                              decisions.mNextAbsorptionStartTime,
                              kzgCommitment
                            )
                            .logWith
                    } yield (newJLState, headerIntermediate)
                }
            (newJlState, headerIntermediate) = headerRes

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

            _ <- Tracer.trace(
              "mkBlockBriefIntermediate result:\n" +
                  s"  Block type: ${blockBrief match {
                          case _: BlockBrief.Minor => "Minor"; case _: BlockBrief.Major => "Major"
                      }}\n" +
                  s"  Block number: ${headerIntermediate.blockNum}\n" +
                  s"  Block brief: $blockBrief"
            )
        } yield (newJlState, blockBrief)
    }

    def mkBlockEffectsIntermediate(
        p: JointLedger.Producing,
        next: BlockBrief.Intermediate,
        absorbedDeposits: DepositsMap.Unzip,
        postDatedRefundTxs: List[RefundTx.PostDated]
    ): IO[(JointLedger.Producing, Block.Unsigned.Intermediate)] = for {
        _ <- Tracer.trace(
          "mkBlockEffectsIntermediate:\n" +
              s"  Block type: ${next match {
                      case _: BlockBrief.Minor => "Minor"; case _: BlockBrief.Major => "Major"
                  }}\n" +
              s"  Block number: ${next.header.blockNum}\n" +
              s"  Absorbed deposits: ${absorbedDeposits.requestIds}\n" +
              s"  Post-dated refund txs: ${postDatedRefundTxs.size}\n" +
              s"  L2 payouts: ${p.l2LedgerState.payouts.size}"
        )

        result <- next match {
            case blockBrief @ BlockBrief.Minor(header, _) =>
                val blockEffects = BlockEffects.Unsigned.Minor(
                  headerSerialized = header.onchainMsg,
                  postDatedRefundTxs = postDatedRefundTxs
                )
                for {
                    _ <- Tracer.trace(
                      s"Building effects for minor block ${next.blockNum} with version ${next.blockVersion}." + "\n" +
                          s"Previous block (${p.previousBlockHeader.blockNum}) had version ${p.previousBlockHeader.blockVersion}."
                    )
                } yield (p, Block.Unsigned.Minor(blockBrief, blockEffects))
            case blockBrief @ BlockBrief.Major(header, _) =>
                for {
                    // TODO: pass in args: should not access the state directly
                    _ <- Tracer.trace(
                      s"Building effects for major block ${next.blockNum} with version ${next.blockVersion}." + "\n" +
                          s"Previous block (${p.previousBlockHeader.blockNum}) had version ${p.previousBlockHeader.blockVersion}."
                    )
                    payoutObligations <- IO.pure(p.l2LedgerState.payouts)
                    _ <- Tracer.trace(s"Remitting payouts: ${payoutObligations
                            .map(x => (x.utxo.value.address, x.utxo.value.value))}")

                    res <- executeL1Action(
                      p,
                      L1LedgerM.mkSettlementTxSeq(
                        nextKzg = header.kzgCommitment,
                        absorbedDeposits = absorbedDeposits.depositUtxos,
                        payoutObligations = payoutObligations,
                        blockCreationEndTime = header.endTime,
                        competingFallbackValidityStart = p.competingFallbackTxTime
                      )
                    )
                    (newL1State, settlementTxSeq) = res
                    newJlState = p.setL1LedgerState(newL1State)

                    blockEffects = BlockEffects.Unsigned.Major(
                      settlementTx = settlementTxSeq.settlementTx,
                      fallbackTx = settlementTxSeq.fallbackTx,
                      rolloutTxs = settlementTxSeq.rolloutTxs,
                      postDatedRefundTxs = postDatedRefundTxs
                    )

                    _ <- Tracer.trace("mkBlockEffectsIntermediate: Major block effects created")

                    _ <- Tracer.trace(
                      s"Settlement tx (${blockEffects.settlementTx.tx.id}): ${HexUtil.encodeHexString(blockEffects.settlementTx.tx.toCbor)}"
                    )
                    _ <- Tracer.trace(
                      s"Fallback tx (${blockEffects.fallbackTx.tx.id}): ${HexUtil.encodeHexString(blockEffects.fallbackTx.tx.toCbor)}"
                    )
                    _ <- IO.traverse_(blockEffects.rolloutTxs)(rolloutTx =>
                        Tracer.trace(
                          s"Rollout tx (${rolloutTx.tx.id}): ${HexUtil.encodeHexString(rolloutTx.tx.toCbor)}"
                        )
                    )
                    _ <- IO.traverse_(blockEffects.postDatedRefundTxs)(refundTx =>
                        Tracer.trace(
                          s"Post-dated refund tx (${refundTx.tx.id}): ${HexUtil.encodeHexString(refundTx.tx.toCbor)}"
                        )
                    )
                } yield (newJlState, Block.Unsigned.Major(blockBrief, blockEffects))
        }
    } yield result

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
            Tracer.scopedCtx("blockNum" -> s"${p.nextBlockNumber: Int}") {
                for {
                    res <- executeL1Action(
                      p,
                      L1LedgerM.finalizeLedger(
                        payoutObligationsRemaining = Vector.from(
                          p.evacuationMap.evacuationMap.values
                        ),
                        competingFallbackValidityStart = p.competingFallbackTxTime
                      )
                    )
                    (newL1State, finalizationTxSeq) = res

                    newJlState = p.setL1LedgerState(newL1State)

                    _ <- state.set(newJlState)

                    block: Block.Unsigned.Final = {
                        import newJlState.userRequestState.*
                        val blockHeader =
                            newJlState.previousBlockHeader
                                .nextHeaderFinal(
                                  newJlState.BlockCreationStartTime,
                                  args.blockCreationEndTime
                                )

                        val blockBody = BlockBody.Final(
                          events = requests,
                          // Final block should reject all the deposits known.
                          depositsRefunded = newJlState.l1LedgerState.deposits.requestIds
                        )

                        val blockBrief = BlockBrief.Final(blockHeader, blockBody)

                        val blockEffects = BlockEffects.Unsigned.Final(
                          finalizationTx = finalizationTxSeq.finalizationTx,
                          rolloutTxs = finalizationTxSeq.rolloutTxs
                        )

                        Block.Unsigned.Final(blockBrief, blockEffects)
                    }

                    _ <- panicOnMismatchWithExpectedBlock(referenceBlockBrief, block)

                    _ <- state.set(newJlState.done(block.header))

                    _ <- handleBlock(block, NotTriggered)
                } yield ()
            }
        }
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
      *   1. Sending the block brief to the peer liaisons - only when leading a block
      *   2. Signing block's effects and producing our own set of acks
      *   3. Sending the block to the consensus actor
      *   4. Sending own set of block ack(s) to the consensus actor
      */
    private def handleBlock(
        block: Block.Unsigned.Next,
        localFinalization: LocalFinalizationTrigger
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
            // 1. Sending the block brief to the peer liaisons - only when leading a block
            _ <- IO.whenA(config.ownHeadPeerId.isLeader(block.blockNum))(
              (conn.peerLiaisons ! block.blockBriefNext).parallel
            )
            //  2. Signing block's effects and producing our own set of acks
            acks = ownHeadWallet.mkAcks(block, localFinalization.asBoolean)
            // 3. Sending the block to the consensus actor
            _ <- conn.consensusActor ! block
            // 4. Sending own set of block ack(s) to the consensus actor
            _ <- IO.traverse_(acks)(ack => conn.consensusActor ! ack)
        } yield ()

    private def panicOnMismatchWithExpectedBlock(
        expectedBlockBrief: Option[BlockBrief],
        actualBlock: Block
    ): IO[Unit] =
        IO.unlessA(expectedBlockBrief.fold(true)(_ == actualBlock.blockBrief))(
          panic(
            "Reference block brief didn't match actual block brief; consensus is broken.\n" +
                s"actual block brief: ${actualBlock.blockBrief}\n" +
                s"expected block brief: $expectedBlockBrief"
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
        consensusActor: ConsensusActor.Handle,
        peerLiaisons: List[PeerLiaison.Handle]
    )

    enum UserRequestError extends Throwable:
        case BlockOutOfRequestValidityInterval(blockCreationStartTime: BlockCreationStartTime)
            extends UserRequestError

    object Requests {
        type Request =
            PreStart.type | UserRequestWithId | StartBlock | CompleteBlockRegular |
                CompleteBlockFinal | GetState.Sync | Block.MultiSigned.Next

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
        def l1LedgerState: L1LedgerM.State
        def evacuationMap: EvacuationMap
    }

    object State {
        def initialize(config: Config): Done = Done(
          previousBlockHeader = config.initialBlock.header,
          l1LedgerState =
              L1LedgerM.State(config.initializationTx.treasuryProduced, DepositsMap.empty),
          evacuationMap = config.initialEvacuationMap
        )
    }

    final case class Done private[JointLedger] (
        override val previousBlockHeader: BlockHeader,
        override val l1LedgerState: L1LedgerM.State,
        override val evacuationMap: EvacuationMap
    ) extends State {
        def setL1LedgerState(newL1State: L1LedgerM.State): Done =
            this.focus(_.l1LedgerState).replace(newL1State)

        def producing(
            l2LedgerState: L2LedgerState,
            startTime: BlockCreationStartTime,
            userRequestState: UserRequestState
        ): Producing = previousBlockHeader match {
            case b: BlockHeader.NonFinal =>
                Producing(
                  b,
                  l1LedgerState,
                  evacuationMap,
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
        override val l1LedgerState: L1LedgerM.State,
        override val evacuationMap: EvacuationMap,
        l2LedgerState: L2LedgerState,
        BlockCreationStartTime: BlockCreationStartTime,
        userRequestState: UserRequestState
    ) extends State {
        val nextBlockNumber: BlockNumber.BlockNumber = previousBlockHeader.blockNum.increment

        transparent inline def competingFallbackTxTime: FallbackTxStartTime =
            previousBlockHeader.fallbackTxStartTime

        def runL1Action[T](
            config: Config,
            action: L1LedgerM[T]
        ): Either[L1LedgerM.Error, (L1LedgerM.State, T)] =
            action.run(config, l1LedgerState)

        def runL2CommandReal[F[_], T](
            l2Ledger: L2Ledger[F],
            command: L2LedgerCommand.Real
        ): F[Either[L2LedgerError, L2LedgerState]] = {
            val action = l2Ledger.L2LedgerAction.fromL2LedgerCommandReal(command)
            action.run(l2LedgerState)
        }

        def setL1LedgerState(newL1State: L1LedgerM.State): Producing =
            this.focus(_.l1LedgerState).replace(newL1State)

        def setL2LedgerState(newL2State: L2LedgerState): Producing =
            this.focus(_.l2LedgerState).replace(newL2State)

        def done(newBlockHeader: BlockHeader): Done =
            Done(newBlockHeader, l1LedgerState, evacuationMap)
    }
}
