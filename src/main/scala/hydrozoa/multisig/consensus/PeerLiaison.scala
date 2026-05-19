package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.PeerLiaison.*
import hydrozoa.multisig.consensus.PeerLiaison.Request.*
import hydrozoa.multisig.consensus.ack.{AckId, AckNumber, HardAck, HardAckNumber, SoftAck}
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber, BlockStatus, BlockType}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import scala.collection.immutable.Queue

trait PeerLiaison(
    config: Config,
    remotePeerId: HeadPeerId,
    pendingConnections: MultisigRegimeManager.PendingConnections | PeerLiaison.Connections,
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[Connections]](None)
    private val state = State()

    private val logger =
        Logging.loggerIO(s"PeerLiaison.${config.ownHeadPeerNum}->${remotePeerId.peerNum}")

    private val loggerMmd =
        Logging.loggerIO("Mermaid.PeerLiaison")

    private def mermaid(s: String): IO[Unit] =
        loggerMmd.info(s"\t${config.ownHeadPeerNum}->>${remotePeerId.peerNum}: ${s}")

    private def getConnections: IO[Connections] = for {
        mConn <- this.connections.get
        conn <- mConn.fold(
          IO.raiseError(
            java.lang.Error(
              "Peer liaison is missing its connections to other actors."
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
                      blockWeaver = _connections.blockWeaver,
                      consensusActor = _connections.consensusActor,
                      stackComposer = _connections.stackComposer,
                      slowConsensusActor = _connections.slowConsensusActor,
                      remotePeerLiaison = _connections.remotePeerLiaisons(remotePeerId)
                    )
                  )
                )
            } yield ()
        case x: PeerLiaison.Connections => connections.set(Some(x))
    }

    override def preStart: IO[Unit] = context.self ! PeerLiaison.PreStart

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction {
            case PeerLiaison.PreStart      => preStartLocal
            case PeerLiaison.ResendCurrent => getConnections.flatMap(resendCurrentTo)
            case req                       => getConnections.flatMap(receiveTotal(req, _))
        }

    private def resendCurrentTo(conn: Connections): IO[Unit] =
        for {
            current <- state.getCurrentlyRequesting
            _ <- logger.debug(
              s"resend tick: GetMsgBatch batch=${current.batchNum}, ack=${current.ackNum}, " +
                  s"block=${current.blockNum}, stackBrief=${current.stackBriefNum}, " +
                  s"hardAck=${current.hardAckNum}, req=${current.requestNum}"
            )
            _ <- conn.remotePeerLiaison ! current
        } yield ()

    private def receiveTotal(req: Request, conn: Connections): IO[Unit] =
        req match {

            case PeerLiaison.PreStart =>
                // Should never reach here since PreStart is handled in receive
                IO.raiseError(new IllegalStateException("PreStart handled in receive"))

            case PeerLiaison.ResendCurrent =>
                // Should never reach here since ResendCurrent is handled in receive
                IO.raiseError(new IllegalStateException("ResendCurrent handled in receive"))

            case x: RemoteBroadcast =>
                for {
                    _ <- x match {
                        case y: UserRequestWithId =>
                            logger.debug(
                              s"outbox: request (${y.requestId.peerNum}:${y.requestId.requestNum})"
                            )
                        case y: SoftAck =>
                            logger.debug(s"outbox: soft ack block=${y.blockNum} peer=${y.peerNum}")
                        case y: BlockBrief.Next =>
                            logger.debug(s"outbox: block block=${y.blockNum}")
                        case y: StackBrief =>
                            logger.debug(s"outbox: stack brief stack=${y.stackNum}")
                        case y: HardAck =>
                            logger.debug(
                              s"outbox: hard ack stack=${y.stackNum} peer=${y.peerNum} " +
                                  s"round=${y.payload.round}"
                            )
                    }
                    // Append the event to the corresponding queue in the state
                    _ <- state.appendToOutbox(x)
                    // Check whether the next batch must be sent immediately, and then turn off the flag regardless.
                    mbBatchReq <- state.dischargeSendNextBatchImmediately
                    // Pretend we just received the cached batch request that we need to send immediately (if any)
                    _ <- mbBatchReq.fold(IO.unit)(batchReq => receiveTotal(batchReq, conn))
                } yield ()

            case x: GetMsgBatch =>
                for {
                    _ <- logger.debug(
                      s"Got GetMsgBatch: batch=${x.batchNum}, ack=${x.ackNum}, " +
                          s"block=${x.blockNum}, stackBrief=${x.stackBriefNum}, " +
                          s"hardAck=${x.hardAckNum}, req=${x.requestNum}"
                    )
                    eNewBatch <- state.buildNewMsgBatch(x, config.peerLiaisonMaxRequestsPerBatch)
                    _ <- eNewBatch match {
                        case Right(newBatch) =>
                            for {
                                msg <- IO.pure(
                                  s"NewMsgBatch: batch=${newBatch.batchNum}, " +
                                      s"ack=${newBatch.ack.map(_.ackNum)}, " +
                                      s"block=${newBatch.blockBrief.map(_.blockNum)}, " +
                                      s"stackBrief=${newBatch.stackBrief.map(_.stackNum)}, " +
                                      s"hardAck=${newBatch.hardAck
                                              .map(h => s"${h.stackNum}/${h.payload.round}")}, " +
                                      s"reqs=${newBatch.requests.map(_.requestId)}"
                                )
                                _ <- logger.debug(s"sending $msg")
                                _ <- mermaid(msg)
                                _ <- conn.remotePeerLiaison ! newBatch
                            } yield ()
                        case Left(state.EmptyNewMsgBatch) =>
                            state.sendNextBatchImmediatelyUponNewMsg(x)
                        case Left(state.OutOfBoundsGetMsgBatch) =>
                            IO.raiseError(Error("Incorrect bounds in GetMsgBatch."))
                    }
                } yield ()

            case x: NewMsgBatch =>
                for {
                    _ <- logger.debug(
                      s"got NewMsgBatch: batch=${x.batchNum}, " +
                          s"ack=${x.ack.map(_.ackNum)}, " +
                          s"block=${x.blockBrief.map(_.blockNum)}, " +
                          s"stackBrief=${x.stackBrief.map(_.stackNum)}, " +
                          s"hardAck=${x.hardAck.map(h => s"${h.stackNum}/${h.payload.round}")}, " +
                          s"reqs=${x.requests.size}"
                    )
                    outcome <- state.verify(x)
                    _ <- outcome match {
                        case VerifyOutcome.Advance(next) =>
                            for {
                                _ <- state.advanceTo(next)
                                msg <- IO.pure(
                                  s"GetMsgBatch: batch=${next.batchNum}, ack=${next.ackNum}, " +
                                      s"block=${next.blockNum}, " +
                                      s"stackBrief=${next.stackBriefNum}, " +
                                      s"hardAck=${next.hardAckNum}, req=${next.requestNum}"
                                )
                                _ <- mermaid(msg)
                                _ <- conn.remotePeerLiaison ! next
                                _ <- x.ack.traverse_(conn.consensusActor ! _)
                                _ <- x.blockBrief.traverse_(conn.blockWeaver ! _)
                                _ <- x.stackBrief.traverse_(conn.stackComposer ! _)
                                _ <- x.hardAck.traverse_(conn.slowConsensusActor ! _)
                                _ <- x.requests.traverse_(conn.blockWeaver ! _)
                            } yield ()
                        case VerifyOutcome.Reject(reason) =>
                            // Rejected reply: either a stale duplicate caused by the
                            // retransmit timer or a real verification mismatch. Either
                            // way, don't bounce a GetMsgBatch back (the timer keeps the
                            // chain alive) and don't re-dispatch payloads. WARN so the
                            // specific failing predicate shows up in normal logs.
                            logger.warn(
                              s"dropping NewMsgBatch batch=${x.batchNum}: $reason"
                            )
                    }
                } yield ()

            case x: (BlockConfirmed & BlockType.NonFinal) =>
                logger.debug(s"Got BlockConfirmed (non-final): block=${x.blockNum}") >>
                    state.dequeueConfirmed(x)

            // TODO: when the final block is confirmed, inform the counterpart that
            //   we no longer need to receive any blocks, acks, or events from them.
            //   When both sides have received the final confirmed block, the connection can be closed and
            //   the peer liaison can terminate.
            case x: (BlockConfirmed & BlockType.Final) =>
                logger.debug(s"Got BlockConfirmed (final): block=${x.blockNum}") >>
                    state.dequeueConfirmed(x)
        }

    private def preStartLocal: IO[Unit] =
        for
            _ <- logger.info(s"starting, remote peer: ${remotePeerId.peerNum}")
            _ <- initializeConnections
            conn <- getConnections
            _ <- mermaid("GetMsgBatch.initial")
            _ <- conn.remotePeerLiaison ! GetMsgBatch.initial
            _ <- startResendTimer
        yield ()

    /** Fire a periodic self-message so the request-response chain self-heals after wire-level
      * losses, see the design note on [[ResendCurrent]] for the rationale.
      *
      * The tick is fire-and-forget: it lives as long as the actor system. We don't try to cancel it
      * on actor shutdown because the actor terminates with the system in our deployment, and an
      * orphaned `IO.sleep` is cheap.
      */
    private def startResendTimer: IO[Unit] = {
        val interval = config.peerLiaisonResendInterval
        val tick: IO[Unit] =
            IO.sleep(interval) >> (context.self ! PeerLiaison.ResendCurrent)
        tick.foreverM.start.void
    }

    private final class State {
        private val currentlyRequesting: Ref[IO, GetMsgBatch] =
            Ref.unsafe[IO, GetMsgBatch](GetMsgBatch.initial)

        /** Read-only access to the outstanding [[GetMsgBatch]]. Used by the retransmit timer (see
          * [[startResendTimer]]) and by the [[NewMsgBatch]] handler to detect duplicates.
          */
        def getCurrentlyRequesting: IO[GetMsgBatch] = this.currentlyRequesting.get
        private val nAck = Ref.unsafe[IO, AckNumber](AckNumber.zero)
        private val nBlock = Ref.unsafe[IO, BlockNumber](BlockNumber.zero)
        private val nRequest = Ref.unsafe[IO, RequestNumber](RequestNumber.zero)
        private val nStackBrief = Ref.unsafe[IO, StackNumber](StackNumber.zero)
        private val nHardAck = Ref.unsafe[IO, HardAckNumber](HardAckNumber.zero)
        private val qAck = Ref.unsafe[IO, Queue[SoftAck]](Queue())
        private val qBlock = Ref.unsafe[IO, Queue[BlockBrief.Next]](Queue())
        private val qRequest = Ref.unsafe[IO, Queue[UserRequestWithId]](Queue())
        private val qStackBrief = Ref.unsafe[IO, Queue[StackBrief]](Queue())
        private val qHardAck = Ref.unsafe[IO, Queue[HardAck]](Queue())
        private val sendBatchImmediately = Ref.unsafe[IO, Option[GetMsgBatch]](None)

        /** Check whether there are no acks, blocks, events, stack briefs, or hard-acks queued-up to
          * be sent out.
          */
        private def queuesAreEmpty: IO[Boolean] =
            for {
                ack <- this.qAck.get.map(_.isEmpty)
                block <- this.qBlock.get.map(_.isEmpty)
                event <- this.qRequest.get.map(_.isEmpty)
                stackBrief <- this.qStackBrief.get.map(_.isEmpty)
                hardAck <- this.qHardAck.get.map(_.isEmpty)
            } yield ack && block && event && stackBrief && hardAck

        infix def appendToOutbox(x: RemoteBroadcast): IO[Unit] =
            x match {
                case y: UserRequestWithId =>
                    for {
                        nEvent <- this.nRequest.get
                        nY = y.requestId.requestNum
                        _ <- IO.raiseWhen(nY.convert != 0L && nEvent.increment != nY)(
                          Error(s"Bad LedgerEvent increment: last-seen: $nEvent, attempted: $nY")
                        )
                        _ <- this.nRequest.set(nY)
                        _ <- this.qRequest.update(_ :+ y)
                    } yield ()
                case y: SoftAck =>
                    for {
                        nAck <- this.nAck.get
                        nY = y.ackNum
                        _ <- IO.raiseWhen(nY.convert != 0L && nAck.increment != nY)(
                          Error("Bad SoftAck increment: last-seen: $nRequest, attempted: $nY")
                        )
                        _ <- this.nAck.set(nY)
                        _ <- this.qAck.update(_ :+ y)
                    } yield ()
                case y: BlockBrief.Next =>
                    for {
                        nBlock <- this.nBlock.get
                        nY = y.blockNum
                        nextOwnBlock = config.ownHeadPeerId.nextLeaderBlock(nBlock)
                        _ <- IO.raiseWhen(nextOwnBlock != nY)(
                          Error(
                            s"Bad BlockBrief.Next increment: last-seen: ${nBlock}, " +
                                s"expected next block: ${nextOwnBlock}, attempted: ${nY}"
                          )
                        )
                        _ <- this.nBlock.set(nY)
                        _ <- this.qBlock.update(_ :+ y)
                    } yield ()
                case y: StackBrief =>
                    // Stack briefs are sparse per-link, mirroring BlockBrief.Next: only the
                    // SLOW-LEADER of a stack broadcasts its brief, and slow-leadership is
                    // round-robin, so this outbox carries only the subset of stackNums that
                    // OUR peer slow-leads. The next legitimate stackNum on this lane is
                    // therefore `nextSlowLeaderStack(last)`, not `last.increment`.
                    for {
                        nStack <- this.nStackBrief.get
                        nY = y.stackNum
                        nextOwnStack = config.ownHeadPeerId.nextSlowLeaderStack(nStack)
                        _ <- IO.raiseWhen(nextOwnStack != nY)(
                          Error(
                            s"Bad StackBrief increment: last-seen: $nStack, " +
                                s"expected next stack: $nextOwnStack, attempted: $nY"
                          )
                        )
                        _ <- this.nStackBrief.set(nY)
                        _ <- this.qStackBrief.update(_ :+ y)
                    } yield ()
                case y: HardAck =>
                    // Hard-acks: monotonically increasing per-peer by hardAckNum, regardless of
                    // stackNum/round. Wire ordering invariant (round-1 precedes round-2 for the
                    // same stack) is upheld by the producer (StackComposer / SlowConsensusActor).
                    for {
                        nHard <- this.nHardAck.get
                        nY = y.hardAckNum
                        _ <- IO.raiseWhen(nY.convert != 0 && nHard.increment != nY)(
                          Error(
                            s"Bad HardAck increment: last-seen: $nHard, attempted: $nY"
                          )
                        )
                        _ <- this.nHardAck.set(nY)
                        _ <- this.qHardAck.update(_ :+ y)
                    } yield ()
            }

        /** Make sure a batch is sent to the counterparty as soon as another local ack/block/event
          * arrives.
          */
        def sendNextBatchImmediatelyUponNewMsg(batchReq: GetMsgBatch): IO[Unit] =
            this.sendBatchImmediately.set(Some(batchReq))

        /** Check whether a new batch must be immediately sent, deactivating the flag in the
          * process.
          */
        def dischargeSendNextBatchImmediately: IO[Option[GetMsgBatch]] =
            this.sendBatchImmediately.getAndSet(None)

        sealed trait ExtractNewMsgBatchError extends Throwable

        case object EmptyNewMsgBatch extends ExtractNewMsgBatchError

        case object OutOfBoundsGetMsgBatch extends ExtractNewMsgBatchError

        /** Construct a [[NewMsgBatch]] containing acks, blocks, and requests starting '''from'''
          * (inclusive) the numbers indicated in a [[GetMsgBatch]] request, up to the configured
          * limits. The [[GetMsgBatch]] numbers use "next expected" semantics: requestNum=N means
          * "send me events with requestNum >= N".
          *
          * @param batchReq
          *   the [[GetMsgBatch]] request
          * @param maxRequests
          *   the maximum number of user requests to include in the [[NewMsgBatch]].
          */

        def buildNewMsgBatch(
            batchReq: GetMsgBatch,
            maxRequests: Int
        ): IO[Either[ExtractNewMsgBatchError, NewMsgBatch]] =
            (for {
                nAck <- this.nAck.get
                nBlock <- this.nBlock.get
                nStack <- this.nStackBrief.get
                nHard <- this.nHardAck.get
                nRequest <- this.nRequest.get

                // OutOfBounds sanity guard, one per lane: the remote's cursor can
                // never legitimately exceed what we've produced (`nX` = highest
                // number ever appended to this outbox; monotonic, prune-independent).
                //   - LAST-SEEN lanes (block, stackBrief — both sparse round-robin):
                //     cursor = "highest seen", so cursor must be `<= nX`
                //                            ⇒ error iff `nX < cursor`.
                //   - NEXT-EXPECTED lanes (ack/hardAck/request): cursor = "next
                //     number wanted", which may legitimately reach `nX + 1` (remote
                //     got the latest, wants the next), so cursor must be `<= nX + 1`
                //                            ⇒ error iff `nX.increment < cursor`.
                // Never false-fires, including at startup where each cursor equals
                // its initial value and `nX == 0` (e.g. ack: `1 <= 0.increment`).
                _ <- IO.raiseWhen(
                  nAck.increment < batchReq.ackNum ||
                      nBlock < batchReq.blockNum ||
                      nStack < batchReq.stackBriefNum ||
                      nHard.increment < batchReq.hardAckNum ||
                      nRequest.increment < batchReq.requestNum
                )(OutOfBoundsGetMsgBatch)

                _ <- this.queuesAreEmpty.ifM(IO.raiseError(EmptyNewMsgBatch), IO.unit)

                // Prune queues in place (read-and-shrink in one `modify`) per the
                // GetMsgBatch cursor protocol (see the note on `GetMsgBatch`):
                //   - ack / hardAck / request lanes are NEXT-EXPECTED: the cursor is
                //     the next number the remote wants ⇒ drop `< cursor`, keep the
                //     head (`>= cursor`). The head is retained until the remote's
                //     cursor moves PAST it, so a dropped batch is simply re-sent on
                //     the next request (retransmit-safe).
                //   - block / stackBrief lanes are LAST-SEEN (sparse round-robin;
                //     "I have N, send me the next one I lead after N") ⇒ drop
                //     `<= cursor`.
                //
                // Pruning per-remote on each incoming `GetMsgBatch` — NOT on local
                // `BlockConfirmed` — is essential. Local consensus for block N
                // confirms when WE receive enough acks for N from other peers; it
                // does NOT imply every remote peer has polled OUR outgoing ack/
                // brief/request for N. If we prune on local confirmation, a slow
                // remote can miss content we own and the chain stalls. The remote's
                // GetMsgBatch positions are the authoritative per-remote receipt
                // signal.
                mAck <- this.qAck.modify { q =>
                    val pruned = q.dropWhile(_.ackNum < batchReq.ackNum)
                    (pruned, pruned.headOption)
                }
                mBlock <- this.qBlock.modify { q =>
                    val pruned = q.dropWhile(_.blockNum <= batchReq.blockNum)
                    (pruned, pruned.headOption)
                }
                mStackBrief <- this.qStackBrief.modify { q =>
                    val pruned = q.dropWhile(_.stackNum <= batchReq.stackBriefNum)
                    (pruned, pruned.headOption)
                }
                mHardAck <- this.qHardAck.modify { q =>
                    val pruned = q.dropWhile(_.hardAckNum < batchReq.hardAckNum)
                    (pruned, pruned.headOption)
                }
                events <- this.qRequest.modify { q =>
                    val pruned = q.dropWhile(_.requestId._2 < batchReq.requestNum)
                    (pruned, pruned.take(maxRequests))
                }

                _ <- IO.raiseWhen(
                  mAck.isEmpty && mBlock.isEmpty && events.isEmpty &&
                      mStackBrief.isEmpty && mHardAck.isEmpty
                )(EmptyNewMsgBatch)
            } yield NewMsgBatch(
              batchNum = batchReq.batchNum,
              ack = mAck,
              blockBrief = mBlock,
              stackBrief = mStackBrief,
              hardAck = mHardAck,
              requests = events.toList
            )).attemptNarrow

        /** No-op placeholder. All three outbox queues (`qAck`, `qBlock`, `qRequest`) are now pruned
          * per-remote-peer in [[buildNewMsgBatch]] based on the incoming `GetMsgBatch` positions,
          * which are the authoritative per-remote receipt signal. Pruning on local `BlockConfirmed`
          * was incorrect for `qAck` (a slow remote may not yet have polled our outgoing ack for
          * block N when we confirm it locally) and unifying the trigger keeps the protocol
          * symmetric.
          *
          * Kept as a method (and the `BlockConfirmed` receive case) for the Final- block
          * close-connection TODO in the receive handler.
          */
        def dequeueConfirmed(block: BlockConfirmed): IO[Unit] = IO.unit

        /** Check whether the received [[NewMsgBatch]] matches what we're currently expecting on
          * this link. Pure inspection — does not mutate `currentlyRequesting`. Call [[advanceTo]]
          * with `Advance.next` to commit the new state when (and only when) the result is
          * [[VerifyOutcome.Advance]].
          */
        def verify(receivedBatch: NewMsgBatch): IO[VerifyOutcome] =
            this.currentlyRequesting.get.map(verifyAgainst(_, receivedBatch))

        /** Commit a successful verification by advancing the outstanding request to `next`. Should
          * be called only after [[verify]] returned [[VerifyOutcome.Advance]] carrying this same
          * `next` — calling it otherwise would skip past valid state.
          */
        def advanceTo(next: GetMsgBatch): IO[Unit] =
            this.currentlyRequesting.set(next)

        /** Pure verification kernel. Compares the received batch against the expected next-batch
          * contract and either:
          *   - returns [[VerifyOutcome.Advance]] with the new [[GetMsgBatch]] for the caller to
          *     advance to, or
          *   - returns [[VerifyOutcome.Reject]] tagged with the first failing predicate.
          *
          * Predicates are checked in a fixed order; the first failure short-circuits, so the
          * reported [[VerifyOutcome.Rejection]] is the most specific one available.
          */
        private def verifyAgainst(
            current: GetMsgBatch,
            received: NewMsgBatch
        ): VerifyOutcome = {
            import VerifyOutcome.{Advance, Reject, Rejection}

            // 1. BatchNum must match the currently outstanding request exactly. Any
            //    mismatch means the reply is for a different batch — stale duplicate
            //    (received < current) or, indicating a logic bug, ahead of state.
            if current.batchNum != received.batchNum then
                return Reject(Rejection.BatchNumMismatch(current.batchNum, received.batchNum))

            // 2. If an ack is present, its peer must be the remote peer and its
            //    number must equal the next-expected `current.ackNum` (next-expected
            //    cursor — same pattern as requestNum below).
            received.ack match
                case Some(a) if a.ackId.peerNum != remotePeerId.peerNum =>
                    return Reject(Rejection.AckPeerMismatch(remotePeerId.peerNum, a.ackId.peerNum))
                case Some(a) if a.ackNum != current.ackNum =>
                    return Reject(Rejection.AckNumMismatch(current.ackNum, a.ackNum))
                case _ => ()

            // 3. If a block brief is present, its blockNum must be the next leader-block
            //    of the remote peer after `current.blockNum`. The block lane stays
            //    last-seen: per-peer block numbers are sparse (round-robin leadership,
            //    `nextLeaderBlock`, not contiguous `+1`) and block 0 is distributed
            //    out-of-band via the head config, so it cannot join the next-expected
            //    scheme.
            val nextBlockNum = remotePeerId.nextLeaderBlock(current.blockNum)
            received.blockBrief match
                case Some(b) if b.blockNum != nextBlockNum =>
                    return Reject(Rejection.BlockNumMismatch(nextBlockNum, b.blockNum))
                case _ => ()

            // 3a. If a stack brief is present, its stackNum must be the next stack
            //     this remote slow-leads after `current.stackBriefNum`. The
            //     stackBrief lane mirrors the block lane: sparse per-link
            //     (round-robin slow-leadership, `nextSlowLeaderStack`, not contiguous
            //     `+1`) and stack 0 is the bootstrap stack distributed out-of-band,
            //     so it stays last-seen.
            val nextStackBriefNum = remotePeerId.nextSlowLeaderStack(current.stackBriefNum)
            received.stackBrief match
                case Some(sb) if sb.stackNum != nextStackBriefNum =>
                    return Reject(
                      Rejection.StackBriefNumMismatch(nextStackBriefNum, sb.stackNum)
                    )
                case _ => ()

            // 3b. If a hard-ack is present, its peer must be the remote peer and its
            //     hardAckNum must equal the next-expected `current.hardAckNum`.
            received.hardAck match
                case Some(h) if h.ackId.peerNum != remotePeerId.peerNum =>
                    return Reject(
                      Rejection.HardAckPeerMismatch(remotePeerId.peerNum, h.ackId.peerNum)
                    )
                case Some(h) if h.hardAckNum != current.hardAckNum =>
                    return Reject(
                      Rejection.HardAckNumMismatch(current.hardAckNum, h.hardAckNum)
                    )
                case _ => ()

            // 4. If requests are present, the first must equal `current.requestNum` (the
            //    next-expected event number) and subsequent requests must be consecutive.
            received.requests.headOption match
                case Some(r) if r.requestId.requestNum != current.requestNum =>
                    return Reject(
                      Rejection.RequestsHeadMismatch(current.requestNum, r.requestId.requestNum)
                    )
                case _ => ()
            received.requests.iterator
                .map(_.requestId)
                .sliding(2)
                .withPartial(false)
                .collectFirst { case Seq(a, b) if !a.precedes(b) => (a, b) } match
                case Some((a, b)) =>
                    return Reject(Rejection.RequestsNotConsecutive(a, b))
                case None => ()

            // All checks passed: compute the next outstanding [[GetMsgBatch]].
            Advance(
              GetMsgBatch(
                batchNum = current.batchNum.increment,
                ackNum = received.ack.fold(current.ackNum)(_.ackNum.increment),
                blockNum = received.blockBrief.fold(current.blockNum)(_.blockNum),
                stackBriefNum = received.stackBrief.fold(current.stackBriefNum)(_.stackNum),
                hardAckNum = received.hardAck.fold(current.hardAckNum)(_.hardAckNum.increment),
                requestNum = received.requests.lastOption.fold(current.requestNum)(
                  _.requestId.requestNum.increment
                )
              )
            )
        }
    }
}

/** A communication actor that is connected to its counterpart at another peer:
  *
  *   - Requests communication batches from the counterpart.
  *   - Responds to the counterpart's requests for communication batches.
  */
object PeerLiaison {
    def apply(
        config: Config,
        remotePeerId: HeadPeerId,
        pendingLocalConnections: MultisigRegimeManager.PendingConnections,
    ): IO[PeerLiaison] =
        IO(new PeerLiaison(config, remotePeerId, pendingLocalConnections) {})

    type Config = OwnHeadPeerPublic.Section & NodeOperationMultisigConfig.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        consensusActor: ConsensusActor.Handle,
        stackComposer: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        remotePeerLiaison: PeerLiaison.Handle
    )

    type Handle = ActorRef[IO, Request]

    type BlockConfirmed = BlockBrief.Section & BlockType.Next & BlockStatus.SoftConfirmed

    object BlockConfirmed {

        /** For testing purposes */
        object Minimal {
            def apply(x: BlockBrief.Next): BlockConfirmed = x.asSoftConfirmed
        }
    }

    type Request =
        PreStart.type | ResendCurrent.type | RemoteBroadcast | GetMsgBatch | NewMsgBatch |
            BlockConfirmed

    /** Outcome of verifying a [[NewMsgBatch]] against the receiver's expected [[GetMsgBatch]]
      * state.
      *
      * The two-arm split decouples verification (a pure check) from state mutation: only
      * [[VerifyOutcome.Advance]] should trigger a write of `currentlyRequesting` and the downstream
      * dispatch of the batch's payload.
      */
    enum VerifyOutcome:
        /** The received batch matches the expected next-batch contract. `next` is the new value the
          * caller should write to `currentlyRequesting` and use as the outgoing [[GetMsgBatch]].
          */
        case Advance(next: GetMsgBatch)

        /** The received batch was rejected. The protocol stays at the current [[GetMsgBatch]].
          * `reason` carries the specific verification predicate that failed, for logging and
          * triage.
          */
        case Reject(reason: VerifyOutcome.Rejection)

    object VerifyOutcome:
        /** Why a [[NewMsgBatch]] was rejected during verification. Each case carries the expected
          * vs. received pair so the rejection can be diagnosed from a single log line.
          */
        enum Rejection:
            case BatchNumMismatch(expected: Batch.Number, received: Batch.Number)
            case AckPeerMismatch(expected: HeadPeerNumber, received: HeadPeerNumber)
            case AckNumMismatch(expected: AckNumber, received: AckNumber)
            case BlockNumMismatch(expected: BlockNumber, received: BlockNumber)
            case StackBriefNumMismatch(expected: StackNumber, received: StackNumber)
            case HardAckPeerMismatch(expected: HeadPeerNumber, received: HeadPeerNumber)
            case HardAckNumMismatch(expected: HardAckNumber, received: HardAckNumber)
            case RequestsHeadMismatch(expected: RequestNumber, received: RequestNumber)
            case RequestsNotConsecutive(prev: RequestId, next: RequestId)

            override def toString: String = this match
                case BatchNumMismatch(e, r) =>
                    s"BatchNumMismatch(expected=$e, received=$r)"
                case AckPeerMismatch(e, r) =>
                    s"AckPeerMismatch(expected=$e, received=$r)"
                case AckNumMismatch(e, r) =>
                    s"AckNumMismatch(expected=$e, received=$r)"
                case BlockNumMismatch(e, r) =>
                    s"BlockNumMismatch(expected=$e, received=$r)"
                case StackBriefNumMismatch(e, r) =>
                    s"StackBriefNumMismatch(expected=$e, received=$r)"
                case HardAckPeerMismatch(e, r) =>
                    s"HardAckPeerMismatch(expected=$e, received=$r)"
                case HardAckNumMismatch(e, r) =>
                    s"HardAckNumMismatch(expected=$e, received=$r)"
                case RequestsHeadMismatch(e, r) =>
                    s"RequestsHeadMismatch(expected=$e, received=$r)"
                case RequestsNotConsecutive(p, n) =>
                    s"RequestsNotConsecutive(prev=(${p.peerNum}:${p.requestNum}), " +
                        s"next=(${n.peerNum}:${n.requestNum}))"

    case object PreStart

    /** Local-only self-tick that asks the liaison to re-send its currently outstanding
      * [[GetMsgBatch]] to the remote peer. Never put on the wire — filtered out by
      * [[hydrozoa.multisig.consensus.transport.Frame.fromWire]].
      *
      * ## Design note: keeping the chain alive at the protocol layer
      *
      * The `GetMsgBatch` / `NewMsgBatch` exchange is a single-outstanding-request chain: each side
      * only sends the next `GetMsgBatch` after processing the previous `NewMsgBatch` reply. Message
      * content itself is idempotent — sender queues are only drained on `BlockConfirmed`, and
      * `buildNewMsgBatch` reads via `dropWhile` rather than dequeueing — so no data is lost when a
      * frame goes missing. What is at risk is the chain's "next pull": if a `GetMsgBatch` or its
      * reply does not land, neither side fires again on its own.
      *
      * Two places this responsibility could live:
      *
      *   - '''(A) The transport.''' A "link came back up" callback would let `PeerLiaison` re-emit
      *     `currentlyRequesting` at the moment a reconnect completes. Couples the liaison to a
      *     specific transport's notion of "connection event" and covers only the failures that
      *     transport can observe.
      *   - '''(B) The protocol itself.''' `PeerLiaison` owns a slow periodic resend of its
      *     outstanding `GetMsgBatch`. Transport-agnostic. Equally applicable to clean reconnects,
      *     half-open TCP, paused GC on the peer, frame-corrupting NAT timeouts, or any other path
      *     that leaves the chain stalled without a visible "reconnect" signal.
      *
      * We chose '''(B)'''. The protocol's liveness is a protocol-level concern, not the
      * transport's. It is also cheap to keep healthy: when the chain is already moving, a resend
      * either harmlessly overwrites the remote's stashed `sendBatchImmediately` value, or it
      * triggers a duplicate `NewMsgBatch` that the `advanced` check in the `NewMsgBatch` handler
      * discards without bouncing a `GetMsgBatch` back. Worst-case steady-state cost is one extra
      * round-trip per peer per `peerLiaisonResendInterval`.
      */
    case object ResendCurrent

    object Request {
        type RemoteBroadcast = SoftAck | BlockBrief.Next | UserRequestWithId | StackBrief | HardAck

        /** Request by a comm actor to its remote comm-actor counterpart for a batch of acks,
          * blocks, stack briefs, hard-acks, and user requests originating from the remote peer.
          *
          * ====Cursor protocol====
          *
          * (See the per-lane summary table just above the `final case class GetMsgBatch` below for
          * a one-glance reference of all five lanes.)
          *
          * The five lanes split by per-link sequence shape:
          *
          *   - Three are CONTIGUOUS per-peer (every peer produces every element): `ackNum`,
          *     `hardAckNum`, `requestNum`. They share one '''next-expected''' contract.
          *   - Two are SPARSE per-link (only the round-robin leader produces the element):
          *     `blockNum` and `stackBriefNum`. They share one '''last-seen''' contract keyed on the
          *     remote's leader schedule.
          *
          * Next-expected lanes (`ackNum`, `hardAckNum`, `requestNum`):
          *
          *   - The cursor names the NEXT number the requester wants. The responder
          *     (`buildNewMsgBatch`) prunes each queue with `dropWhile(_ < cursor)` and sends the
          *     head. It RETAINS the head until the requester's cursor moves PAST it, so a lost
          *     batch is just re-sent on the next request (retransmit-safe).
          *   - The verifier (`verifyAgainst`) requires the received element's number to equal the
          *     cursor exactly (`received == current`), then advances the cursor to
          *     `received.increment` (requests: to `last.increment`).
          *   - The initial cursor is each lane's FIRST emitted number, so the very first element
          *     validates against `GetMsgBatch.initial`:
          *     - `requestNum` = 0 — user requests are 0-indexed per peer.
          *     - `hardAckNum` = 0 — the per-peer hard-ack counter is 0-based; it tracks the slow
          *       cycle from stack 0, so the initial stack (once injected) takes 0 = round-1, 1 =
          *       round-2.
          *     - `ackNum` = 1 — `SoftAck.ackNum == blockNum`; block 0 is the config-bootstrapped
          *       block and is never acked over the wire, so the first soft-ack is for block 1.
          *   - The producer side (`appendToOutbox`) independently enforces gap-free monotonic
          *     numbering on these three lanes, so the wire stream is always contiguous from the
          *     initial cursor — the precondition for `received == current`.
          *
          * Last-seen sparse lanes (`blockNum`, `stackBriefNum`):
          *
          *   - "I have N, send me the next one I lead after N": prune `<= cursor`; verify
          *     `received == remotePeerId.nextLeader…(current)` (where `nextLeader…` is
          *     `nextLeaderBlock` for blocks and `nextSlowLeaderStack` for stack briefs); advance
          *     `cursor = received` (last-seen).
          *   - Why not next-expected: per-link sequences are SPARSE. A peer only broadcasts the
          *     blocks it (fast-)leads and the stack briefs it slow-leads, both round-robin. So the
          *     per-link stream is not contiguous `+1`; the "next-expected" number depends on the
          *     remote's leader schedule, not a successor function in the cursor itself.
          *   - Item 0 (block 0 / stack 0) is the config / bootstrap artefact distributed
          *     out-of-band and never sent here; initial cursor = `0` is the "nothing seen yet"
          *     sentinel, and `nextLeader…(0)` returns the remote's first wire-eligible item
          *     (strictly-after semantics).
          *
          * Every lane additionally carries an `OutOfBounds` sanity guard in `buildNewMsgBatch`: the
          * remote's cursor may never exceed what we've produced (last-seen ⇒ `<= produced`;
          * next-expected ⇒ `<= produced + 1`). It never false-fires in correct operation; tripping
          * it means protocol desync/corruption and raises rather than silently stalling.
          *
          * @param batchNum
          *   Batch number that increases by one for every consecutive batch.
          * @param ackNum
          *   Next-expected soft-ack number (== block number) from the remote peer.
          * @param blockNum
          *   Last-seen block from the remote peer (round-robin; see above).
          * @param stackBriefNum
          *   Last-seen stack-brief number (== stack number) from the remote peer (sparse
          *   round-robin, see above).
          * @param hardAckNum
          *   Next-expected hard-ack number from the remote peer.
          * @param requestNum
          *   Next-expected user-request number from the remote peer (inclusive).
          */
        // format: off
        // ===== Per-lane batch-protocol summary =====
        //
        // Symbols: nX  = highest number ever appended to this outbox (per-lane).
        //          cur = the remote's `GetMsgBatch.X` for this lane.
        //          NE  = next-expected contract.   LS  = last-seen contract.
        //
        //   lane           family  producer          first emitted #          init cur
        //   ackNum         NE      every peer        1                        1
        //   blockNum       LS      only fast-leader  nextLeaderBlock(0)       0
        //   stackBriefNum  LS      only slow-leader  nextSlowLeaderStack(0)   0
        //   hardAckNum     NE      every peer        0                        0
        //   requestNum     NE      every peer        0                        0
        //
        //   family  append (raise iff)   prune      verify (reject iff)       advance  OOB iff
        //   NE      nY != nX + 1 *       < cur      recv != cur               recv+1   nX+1 < cur
        //   LS      nY != ownId.f(nX)    <= cur     recv != remote.f(cur)     recv     nX < cur
        //
        // * Bootstrap escape on NE: the very first append may carry the lane's initial
        //   number unconditionally (`nY.convert != 0 && …`).
        // f = the lane's leader-schedule function: `nextLeaderBlock` for `blockNum`;
        //     `nextSlowLeaderStack` for `stackBriefNum`.
        // format: on
        final case class GetMsgBatch(
            batchNum: Batch.Number,
            ackNum: AckNumber,
            blockNum: BlockNumber,
            stackBriefNum: StackNumber,
            hardAckNum: HardAckNumber,
            requestNum: RequestNumber
        )

        object GetMsgBatch {
            // Initial cursor values (see the cursor-protocol note above):
            //   - next-expected lanes: each lane's FIRST emitted number — requests
            //     and hard-acks are 0-based; soft-acks (== blockNum) are 1-based
            //     because block 0 is the config-bootstrap block never acked.
            //   - last-seen sparse lanes (block, stackBrief): 0 = "nothing seen
            //     yet"; `nextLeader…(0)` resolves to the remote's first wire-eligible
            //     item (block 0 / stack 0 are out-of-band).
            def initial: GetMsgBatch = GetMsgBatch(
              batchNum = Batch.Number(0),
              ackNum = AckNumber.zero.increment,
              blockNum = BlockNumber.zero,
              stackBriefNum = StackNumber.zero,
              hardAckNum = HardAckNumber.zero,
              requestNum = RequestNumber.zero
            )
        }

        /** Comm actor provides a batch in response to its remote comm-actor counterpart's request.
          *
          * @param batchNum
          *   Batch number matching the one from the request.
          * @param ack
          *   If provided, a block acknowledgment originating from the responder after the requested
          *   [[AckNum]].
          * @param blockBrief
          *   If provided, a block originating from the responder after the requested [[Number]].
          *   The initial block is never sent in a message batch because it's already pre-confirmed
          *   in the head config, which every peer already has locally.
          * @param requests
          *   A possibly empty list of events originating from the responder after the requested
          *   [[LedgerEventNum]].
          */
        final case class NewMsgBatch(
            batchNum: Batch.Number,
            ack: Option[SoftAck],
            blockBrief: Option[BlockBrief.Next],
            stackBrief: Option[StackBrief],
            hardAck: Option[HardAck],
            requests: List[UserRequestWithId]
        )
    }

    object Batch {
        type Number = Number.Number

        object Number {
            opaque type Number = Int

            def apply(i: Int): Number = i

            given Conversion[Number, Int] = identity

            given Ordering[Number] with {
                override def compare(x: Number, y: Number): Int =
                    x.compare(y)
            }

            extension (self: Number) def increment: Number = Number(self + 1)
        }
    }
}
