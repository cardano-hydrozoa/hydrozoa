package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.operation.multisig.NodeOperationMultisigConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.MultisigRegimeManager
import hydrozoa.multisig.consensus.PeerLiaison.*
import hydrozoa.multisig.consensus.PeerLiaison.Request.*
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, SoftAck, SoftAckId, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{HeadPeerId, HeadPeerNumber}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber, BlockStatus, BlockType}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import hydrozoa.multisig.persistence.recovery.LaneScan
import hydrozoa.multisig.persistence.{LaneKey, LaneValue, Persistence, WriteBatch}
import scala.collection.immutable.Queue

trait PeerLiaison(
    config: Config,
    remotePeerId: HeadPeerId,
    pendingConnections: MultisigRegimeManager.PendingConnections | PeerLiaison.Connections,
    persistence: Persistence[IO]
) extends Actor[IO, Request] {
    private val connections = Ref.unsafe[IO, Option[Connections]](None)
    private val state = State()

    /** `config` is a `CardanoNetwork.Section`; expose it as a given so the inbound-lane
      * `WriteBatch.put`s in [[persistInbound]] pick it up.
      */
    private given CardanoNetwork.Section = config

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
              s"resend tick: GetMsgBatch batch=${current.batchNum}, ack=${current.softAckNumber}, " +
                  s"block=${current.blockNum}, stackBrief=${current.stackNum}, " +
                  s"hardAck=${current.hardAckNum}, req=${current.requestNum}"
            )
            _ <- conn.remotePeerLiaison ! current
        } yield ()

    /** Persist the inbound remote lane entries carried by a [[NewMsgBatch]] before the receive
      * cursor advances past them (CR8 write-before-advance, §4). The one place this peer persists
      * data it did not author. Each entry is **receipt**-stamped and keyed by its author: spine
      * entries (`Block` / `Stack`) by their index; satellites (`SoftAck` / `HardAck` / `Request`)
      * by the remote author + index. An empty batch is a no-op.
      */
    private def persistInbound(batch: NewMsgBatch): IO[Unit] =
        persistence.arrivalStamp.flatMap { stamp =>
            def lv[P](payload: P): LaneValue[P] = LaneValue(stamp, payload)
            // One independent put-thunk per present entry, then a single fold — no running `wbN`
            // chain to thread by hand (where a wrong index would silently drop an entry).
            val puts: List[WriteBatch => WriteBatch] =
                List(
                  batch.blockBrief.map(b =>
                      (wb: WriteBatch) => wb.put(LaneKey.Block(b.blockNum))(lv(b))
                  ),
                  batch.stackBrief.map(b =>
                      (wb: WriteBatch) => wb.put(LaneKey.Stack(b.stackNum))(lv(b))
                  ),
                  batch.softAck.map(a =>
                      (wb: WriteBatch) => wb.put(LaneKey.SoftAck(a.peerNum, a.ackNum))(lv(a))
                  ),
                  batch.hardAck.map(a =>
                      (wb: WriteBatch) => wb.put(LaneKey.HardAck(a.peerNum, a.hardAckNum))(lv(a))
                  )
                ).flatten ++ batch.requests.map(r =>
                    (wb: WriteBatch) =>
                        wb.put(LaneKey.Request(r.requestId.peerNum, r.requestId.requestNum))(lv(r))
                )
            val full = puts.foldLeft(WriteBatch.start)((wb, put) => put(wb))
            IO.whenA(full.size > 0)(persistence.write(full))
        }

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
                      s"Got GetMsgBatch: batch=${x.batchNum}, ack=${x.softAckNumber}, " +
                          s"block=${x.blockNum}, stackBrief=${x.stackNum}, " +
                          s"hardAck=${x.hardAckNum}, req=${x.requestNum}"
                    )
                    eNewBatch <- state.buildNewMsgBatch(x, config.peerLiaisonMaxRequestsPerBatch)
                    _ <- eNewBatch match {
                        case Right(newBatch) =>
                            for {
                                msg <- IO.pure(
                                  s"NewMsgBatch: batch=${newBatch.batchNum}, " +
                                      s"ack=${newBatch.softAck.map(_.ackNum)}, " +
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
                          s"ack=${x.softAck.map(_.ackNum)}, " +
                          s"block=${x.blockBrief.map(_.blockNum)}, " +
                          s"stackBrief=${x.stackBrief.map(_.stackNum)}, " +
                          s"hardAck=${x.hardAck.map(h => s"${h.stackNum}/${h.payload.round}")}, " +
                          s"reqs=${x.requests.size}"
                    )
                    outcome <- state.verify(x)
                    _ <- outcome match {
                        case VerifyOutcome.Advance(next) =>
                            for {
                                // CR8: persist the inbound remote lane entries BEFORE the receive
                                // cursor advances past them (write-before-advance, §4).
                                _ <- persistInbound(x)
                                _ <- state.advanceTo(next)
                                msg <- IO.pure(
                                  s"GetMsgBatch: batch=${next.batchNum}, ack=${next.softAckNumber}, " +
                                      s"block=${next.blockNum}, " +
                                      s"stackBrief=${next.stackNum}, " +
                                      s"hardAck=${next.hardAckNum}, req=${next.requestNum}"
                                )
                                _ <- mermaid(msg)
                                _ <- conn.remotePeerLiaison ! next
                                _ <- x.softAck.traverse_(conn.consensusActor ! _)
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
            _ <- conn.remotePeerLiaison ! GetMsgBatch.initial(remotePeerId)
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
            Ref.unsafe[IO, GetMsgBatch](GetMsgBatch.initial(remotePeerId))

        /** Read-only access to the outstanding [[GetMsgBatch]]. Used by the retransmit timer (see
          * [[startResendTimer]]) and by the [[NewMsgBatch]] handler to detect duplicates.
          */
        def getCurrentlyRequesting: IO[GetMsgBatch] = this.currentlyRequesting.get
        private val nAck = Ref.unsafe[IO, SoftAckNumber](SoftAckNumber.zero)
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
                          Error(
                            s"Bad LedgerEvent increment: last-appended: $nEvent, attempted: $nY"
                          )
                        )
                        _ <- this.nRequest.set(nY)
                        _ <- this.qRequest.update(_ :+ y)
                    } yield ()
                case y: SoftAck =>
                    for {
                        nAck <- this.nAck.get
                        nY = y.ackNum
                        _ <- IO.raiseWhen(nY.convert != 0L && nAck.increment != nY)(
                          Error(s"Bad SoftAck increment: last-appended: $nAck, attempted: $nY")
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
                            s"Bad BlockBrief.Next increment: last-appended: ${nBlock}, " +
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
                            s"Bad StackBrief increment: last-appended: $nStack, " +
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
                            s"Bad HardAck increment: last-appended: $nHard, attempted: $nY"
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

                // OutOfBounds sanity guard, one per lane. The remote's cursor can
                // never legitimately exceed the next-expected number after what
                // we've produced (`nX` = highest number ever appended to this
                // outbox; monotonic, prune-independent). All lanes are
                // next-expected; the "+1" successor function differs by lane:
                //   - Contiguous lanes (ack, hardAck, request): successor =
                //     `nX.increment` ⇒ error iff `nX.increment < cursor`.
                //   - Sparse lanes (block, stackBrief): successor =
                //     `ownId.nextLeader…(nX)` (our own leader schedule —
                //     OUR outbox carries the items WE lead) ⇒
                //     error iff `ownId.nextLeader…(nX) < cursor`.
                // Never false-fires, including at startup where each cursor
                // equals its initial value: contiguous lanes start at the lane's
                // first emitted number; sparse lanes start at
                // `remoteId.nextLeader…(0)` (the same value our `nextLeader…(nX)`
                // returns when `nX == 0` — the leader-schedule formula is
                // symmetric in `(peer, n) ↦ first leader ≥ next`).
                _ <- IO.raiseWhen(
                  nAck.increment < batchReq.softAckNumber ||
                      config.ownHeadPeerId.nextLeaderBlock(nBlock) < batchReq.blockNum ||
                      config.ownHeadPeerId.nextSlowLeaderStack(nStack) <
                      batchReq.stackNum ||
                      nHard.increment < batchReq.hardAckNum ||
                      nRequest.increment < batchReq.requestNum
                )(OutOfBoundsGetMsgBatch)

                _ <- this.queuesAreEmpty.ifM(IO.raiseError(EmptyNewMsgBatch), IO.unit)

                // Prune queues in place (read-and-shrink in one `modify`). All five
                // lanes are next-expected: cursor is the next number the remote
                // wants ⇒ drop `< cursor`, keep the head (`>= cursor`). The head is
                // retained until the remote's cursor moves PAST it, so a dropped
                // batch is simply re-sent on the next request (retransmit-safe).
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
                    val pruned = q.dropWhile(_.ackNum < batchReq.softAckNumber)
                    (pruned, pruned.headOption)
                }
                mBlock <- this.qBlock.modify { q =>
                    val pruned = q.dropWhile(_.blockNum < batchReq.blockNum)
                    (pruned, pruned.headOption)
                }
                mStackBrief <- this.qStackBrief.modify { q =>
                    val pruned = q.dropWhile(_.stackNum < batchReq.stackNum)
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
              softAck = mAck,
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
            received.softAck match
                case Some(a) if a.ackId.peerNum != remotePeerId.peerNum =>
                    return Reject(Rejection.AckPeerMismatch(remotePeerId.peerNum, a.ackId.peerNum))
                case Some(a) if a.ackNum != current.softAckNumber =>
                    return Reject(Rejection.AckNumMismatch(current.softAckNumber, a.ackNum))
                case _ => ()

            // 3. If a block brief is present, its blockNum must equal the
            //    next-expected `current.blockNum`. Block/stack lanes are sparse but
            //    still next-expected: the cursor is precomputed to the next block
            //    this remote leads, so the verify check is the same exact-match
            //    shape as ack/hardAck/request. Block 0 is the bootstrap block
            //    distributed out-of-band via the head config — `nextLeaderBlock(0)`
            //    is what advances the cursor past it (for the remote peer that
            //    would otherwise lead it).
            received.blockBrief match
                case Some(b) if b.blockNum != current.blockNum =>
                    return Reject(Rejection.BlockNumMismatch(current.blockNum, b.blockNum))
                case _ => ()

            // 3a. If a stack brief is present, its stackNum must equal
            //     `current.stackBriefNum` — same next-expected contract as the
            //     block lane. Stack 0 is the bootstrap stack distributed
            //     out-of-band and excluded by `nextSlowLeaderStack(0)`.
            received.stackBrief match
                case Some(sb) if sb.stackNum != current.stackNum =>
                    return Reject(
                      Rejection.StackBriefNumMismatch(current.stackNum, sb.stackNum)
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
            // Every lane advances to its next-expected successor: `+1` for the
            // contiguous lanes (ack, hardAck, request); the remote's leader
            // schedule for the sparse lanes (block, stackBrief).
            Advance(
              GetMsgBatch(
                batchNum = current.batchNum.increment,
                softAckNumber = received.softAck.fold(current.softAckNumber)(_.ackNum.increment),
                blockNum = received.blockBrief.fold(current.blockNum)(b =>
                    remotePeerId.nextLeaderBlock(b.blockNum)
                ),
                stackNum = received.stackBrief.fold(current.stackNum)(sb =>
                    remotePeerId.nextSlowLeaderStack(sb.stackNum)
                ),
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
        persistence: Persistence[IO]
    ): IO[PeerLiaison] =
        IO(new PeerLiaison(config, remotePeerId, pendingLocalConnections, persistence) {})

    // `& CardanoNetwork.Section`: the inbound lane codecs are Section-dependent; the full configs
    // passed in satisfy it.
    type Config =
        OwnHeadPeerPublic.Section & NodeOperationMultisigConfig.Section & CardanoNetwork.Section

    final case class Connections(
        blockWeaver: BlockWeaver.Handle,
        consensusActor: FastConsensusActor.Handle,
        stackComposer: StackComposer.Handle,
        slowConsensusActor: SlowConsensusActor.Handle,
        remotePeerLiaison: PeerLiaison.Handle
    )

    type Handle = ActorRef[IO, Request]

    /** This peer's own-produced outbox restored from the store on boot — the five lanes' own
      * entries in ascending number order. `recover` rebuilds it; R3 seeds [[PeerLiaison]]'s `State`
      * from it (each queue's last entry's number is the corresponding cursor `nX`; the cold value
      * is all empty, with `currentlyRequesting` left at `GetMsgBatch.initial(remote)` so the remote
      * re-polls).
      */
    final case class OutboxSeed(
        qAck: List[SoftAck],
        qBlock: List[BlockBrief.Next],
        qRequest: List[UserRequestWithId],
        qStackBrief: List[StackBrief],
        qHardAck: List[HardAck]
    )

    /** Reconstruct this peer's own-produced outbox after a crash (R2-bnd) — the entries the remote
      * polls from us via `GetMsgBatch`. Pure over the store; the actor wiring (seeding `State`) is
      * R3. Each lane is read ascending: the satellites (`SoftAck` / `HardAck` / `Request`) are
      * own-keyed, so a per-peer scan yields exactly our entries; the spines (`Block` / `Stack`)
      * carry every leader's brief, so we keep only the ones THIS peer leads (`isLeader` /
      * `isSlowLeader`). **Inbound is not restored** — `persistInbound` forwards each received entry
      * rather than holding it, so inbound re-delivery is the `ReplayActor`'s job in R3 (restoring
      * it here would double-deliver).
      */
    def recover(persistence: Persistence[IO], own: HeadPeerId)(using
        CardanoNetwork.Section
    ): IO[OutboxSeed] =
        val backend = persistence.backend
        val ownNum = own.peerNum
        val kAck = LaneKey.SoftAck(ownNum, SoftAckNumber.zero)
        val kRequest = LaneKey.Request(ownNum, RequestNumber.zero)
        val kHardAck = LaneKey.HardAck(ownNum, HardAckNumber.zero)
        val kBlock = LaneKey.Block(BlockNumber.zero)
        val kStack = LaneKey.Stack(StackNumber.zero)
        for {
            acks <- LaneScan.scan(backend, kAck).map(_.map(e => kAck.decodeValue(e.framed).payload))
            requests <- LaneScan
                .scan(backend, kRequest)
                .map(_.map(e => kRequest.decodeValue(e.framed).payload))
            hardAcks <- LaneScan
                .scan(backend, kHardAck)
                .map(_.map(e => kHardAck.decodeValue(e.framed).payload))
            blocks <- LaneScan
                .scan(backend, kBlock)
                .map(
                  _.map(e => kBlock.decodeValue(e.framed).payload)
                      .filter(b => own.isLeader(b.blockNum))
                )
            stacks <- LaneScan
                .scan(backend, kStack)
                .map(
                  _.map(e => kStack.decodeValue(e.framed).payload)
                      .filter(s => own.isSlowLeader(s.stackNum))
                )
        } yield OutboxSeed(
          qAck = acks,
          qBlock = blocks,
          qRequest = requests,
          qStackBrief = stacks,
          qHardAck = hardAcks
        )

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
            case AckNumMismatch(expected: SoftAckNumber, received: SoftAckNumber)
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
          * All five lanes share ONE '''next-expected''' cursor contract. They differ only in
          * per-link sequence SHAPE, which changes the successor function — not the contract:
          *
          *   - Three are CONTIGUOUS per-peer (every peer produces every element): `ackNum`,
          *     `hardAckNum`, `requestNum`. Successor is `+1`.
          *   - Two are SPARSE per-link (only the round-robin leader produces the element):
          *     `blockNum` and `stackBriefNum`. Successor is the remote's leader schedule
          *     (`nextLeaderBlock` / `nextSlowLeaderStack`); the cursor is precomputed to the next
          *     leader item, so its verify check is the same exact match as the contiguous lanes.
          *
          * The shared next-expected contract:
          *
          *   - The cursor names the NEXT number the requester wants. The responder
          *     (`buildNewMsgBatch`) prunes each queue with `dropWhile(_ < cursor)` and sends the
          *     head. It RETAINS the head until the requester's cursor moves PAST it, so a lost
          *     batch is just re-sent on the next request (retransmit-safe).
          *   - The verifier (`verifyAgainst`) requires the received element's number to equal the
          *     cursor exactly (`received == current`), then advances the cursor to its successor
          *     (`+1` for contiguous lanes; the leader-schedule successor for sparse lanes).
          *   - The initial cursor is each lane's FIRST emitted number, so the very first element
          *     validates against `GetMsgBatch.initial`:
          *     - `requestNum` = 0 — user requests are 0-indexed per peer.
          *     - `hardAckNum` = 0 — the per-peer hard-ack counter is 0-based; it tracks the slow
          *       cycle from stack 0, so the initial stack (once injected) takes 0 = round-1, 1 =
          *       round-2.
          *     - `ackNum` = 1 — `SoftAck.ackNum == blockNum`; block 0 is the config-bootstrapped
          *       block and is never acked over the wire, so the first soft-ack is for block 1.
          *     - `blockNum` = `remotePeerId.nextLeaderBlock(0)` — the first block this remote
          *       leads, skipping block 0 (config-bootstrap, never sent on the wire).
          *     - `stackBriefNum` = `remotePeerId.nextSlowLeaderStack(0)` — the first stack this
          *       remote slow-leads, skipping stack 0 (bootstrap stack, never sent on the wire).
          *   - The producer side (`appendToOutbox`) independently enforces gap-free monotonic
          *     numbering: contiguous lanes use `+1` per emission; sparse lanes use
          *     `ownHeadPeerId.nextLeader…(last)` (this peer only emits items it leads). The wire
          *     stream is therefore always at the cursor's next-expected value — the precondition
          *     for `received == current`.
          *
          * Every lane additionally carries an `OutOfBounds` sanity guard in `buildNewMsgBatch`: the
          * remote's cursor may never exceed the next-expected number after what we've produced
          * (contiguous ⇒ `<= produced + 1`; sparse ⇒ `<= ownId.nextLeader…(produced)`). It never
          * false-fires in correct operation; tripping it means protocol desync/corruption and
          * raises rather than silently stalling.
          *
          * @param batchNum
          *   Batch number that increases by one for every consecutive batch.
          * @param softAckNumber
          *   Next-expected soft-ack number (== block number) from the remote peer.
          * @param blockNum
          *   Next-expected block number from the remote peer — the next leader block of this
          *   remote, computed via `nextLeaderBlock`.
          * @param stackNum
          *   Next-expected stack-brief number from the remote peer — the next slow-leader stack of
          *   this remote, computed via `nextSlowLeaderStack`.
          * @param hardAckNum
          *   Next-expected hard-ack number from the remote peer.
          * @param requestNum
          *   Next-expected user-request number from the remote peer (inclusive).
          */
        // format: off
        // ===== Per-lane batch-protocol summary =====
        //
        // All five lanes are NEXT-EXPECTED. The differences are only in:
        //   1. who produces (every peer vs only the lane's leader), and
        //   2. the successor function (contiguous `+1` vs leader-schedule `f`).
        //
        // Symbols: nX  = highest number ever appended to this outbox (per-lane).
        //          cur = the remote's `GetMsgBatch.X` for this lane.
        //          f   = the lane's leader-schedule function: `nextLeaderBlock` for `blockNum`,
        //                `nextSlowLeaderStack` for `stackBriefNum`.
        //
        //   lane           producer          first emitted #               init cur (per-remote)
        //   ackNum         every peer        1                             1
        //   blockNum       only fast-leader  ownId.nextLeaderBlock(0)      remote.nextLeaderBlock(0)
        //   stackBriefNum  only slow-leader  ownId.nextSlowLeaderStack(0)  remote.nextSlowLeaderStack(0)
        //   hardAckNum     every peer        0                             0
        //   requestNum     every peer        0                             0
        //
        //   family       append (raise iff)   prune    verify (reject iff)  advance        OOB iff
        //   contiguous   nY != nX + 1 *       < cur    recv != cur          recv+1         nX+1 < cur
        //   sparse       nY != ownId.f(nX)    < cur    recv != cur          remote.f(recv) ownId.f(nX) < cur
        //
        // * Bootstrap escape on contiguous lanes: the very first append may carry the lane's
        //   initial number unconditionally (`nY.convert != 0 && …`).
        // format: on
        final case class GetMsgBatch(
            batchNum: Batch.Number,
            softAckNumber: SoftAckNumber,
            blockNum: BlockNumber,
            stackNum: StackNumber,
            hardAckNum: HardAckNumber,
            requestNum: RequestNumber
        )

        object GetMsgBatch {
            // Initial cursor values (see the cursor-protocol note above). All lanes are
            // next-expected:
            //   - Contiguous lanes (ack, hardAck, request): each lane's FIRST emitted number —
            //     requests and hard-acks are 0-based; soft-acks (== blockNum) are 1-based
            //     because block 0 is the config-bootstrap block never acked.
            //   - Sparse lanes (block, stackBrief): the remote's first wire-eligible item per its
            //     leader schedule, since `remotePeerId.nextLeader…(0)` skips the out-of-band
            //     bootstrap item 0.
            //
            // Per-remote because the sparse-lane initial cursor depends on which remote peer the
            // liaison is talking to. Tests can use any [[HeadPeerId]] fixture.
            def initial(remotePeerId: HeadPeerId): GetMsgBatch = GetMsgBatch(
              batchNum = Batch.Number(0),
              softAckNumber = SoftAckNumber.zero.increment,
              blockNum = remotePeerId.nextLeaderBlock(BlockNumber.zero),
              stackNum = remotePeerId.nextSlowLeaderStack(StackNumber.zero),
              hardAckNum = HardAckNumber.zero,
              requestNum = RequestNumber.zero
            )
        }

        /** Comm actor provides a batch in response to its remote comm-actor counterpart's request.
          *
          * @param batchNum
          *   Batch number matching the one from the request.
          * @param softAck
          *   If provided, a soft block acknowledgment originating from the responder after the
          *   requested [[SoftAckNumber]].
          * @param blockBrief
          *   If provided, a block originating from the responder after the requested
          *   [[BlockNumber]]. The initial block is never sent in a message batch, instead it's
          *   derived from the head config locally.
          * @stackBrief
          *   If provided, the next stack after requested [[StackNumber]]. The initial stack (#0) is
          *   never sent in a message batch, instead it's derived f rom the head config locally.
          * @param hardAck
          *   If provided, a hard stack acknowledgment originating from the responder after the
          *   requested * [[HardAckNum]].
          * @param requests
          *   A possibly empty list of events originating from the responder after the requested
          *   [[RequestNumber]].
          */
        final case class NewMsgBatch(
            batchNum: Batch.Number,
            softAck: Option[SoftAck],
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
