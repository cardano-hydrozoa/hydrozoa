package hydrozoa.multisig.consensus

import cats.effect.{IO, Ref}
import cats.implicits.*
import com.suprnation.actor.Actor.{Actor, Receive}
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.consensus.PeerLiaison.*
import hydrozoa.multisig.consensus.PeerLiaison.Request.*
import hydrozoa.multisig.consensus.ack.{HardAck, HardAckNumber, HardAckWithId, HubHardAckNumber, RelayedMsg, RelayedMsgNumber, SoftAck, SoftAckNumber}
import hydrozoa.multisig.consensus.peer.{PeerId, RemotePeer}
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber, BlockType}
import hydrozoa.multisig.ledger.event.RequestNumber
import hydrozoa.multisig.ledger.stack.{StackBrief, StackNumber}
import scala.collection.immutable.Queue

/** The shared `GetMsgBatch` / `NewMsgBatch` cursor protocol that every liaison shape speaks (§8 of
  * `design/coil-network.md`). It owns the per-lane outbox queues, cursor bookkeeping, batch
  * build/verify/advance machinery, and the retransmit timer — everything that does NOT depend on
  * which local actors a given liaison wires to.
  *
  * Concrete liaisons supply three seams:
  *   - [[initializeConnections]] — resolve and cache this liaison's connections at start.
  *   - [[sendToRemoteLiaison]] — how to reach the counterpart liaison on the other end of the link.
  *   - [[dispatchVerifiedBatch]] — where to route a verified inbound batch's payloads. This is the
  *     one behavioral axis on which the three shapes differ (head mesh routes everything; a
  *     hub→coil link routes only the coil's own hard-acks; a coil→hub link routes the full relayed
  *     stream).
  */
trait BatchProtocolLiaison(
    config: Config,
    remotePeer: RemotePeer,
) extends Actor[IO, Request] {
    private val state = State()

    private val logger =
        Logging.loggerIO(s"PeerLiaison.${config.ownPeerLabel}->${remotePeer.label}")

    private val loggerMmd =
        Logging.loggerIO("Mermaid.PeerLiaison")

    private def mermaid(s: String): IO[Unit] =
        loggerMmd.info(s"\t${config.ownPeerLabel}->>${remotePeer.label}: ${s}")

    /** Resolve and cache this liaison's connections to local actors. Called once at [[preStart]].
      */
    protected def initializeConnections: IO[Unit]

    /** Send a message to the counterpart liaison on the other end of this link. */
    protected def sendToRemoteLiaison(msg: Request): IO[Unit]

    /** Route the payloads of a verified inbound [[NewMsgBatch]] to the local actors. The lanes a
      * given liaison shape expects to be populated are exactly the ones it routes here; the rest
      * are `None` in correct operation.
      */
    protected def dispatchVerifiedBatch(batch: NewMsgBatch): IO[Unit]

    // ---- Brief-lane shape (§8) ----
    // The block/stack-brief lanes are SPARSE on the head mesh (only the round-robin leader emits an
    // item, so the successor is that peer's leader schedule) but CONTIGUOUS on the hub→coil relay
    // (the hub forwards EVERY block/stack in order, so the successor is `+1`). These seams let a
    // concrete liaison pick the shape; the head mesh keeps the sparse default.

    /** Successor on THIS liaison's outbound block-brief lane (used by append + the OOB guard). */
    protected def nextOwnBriefBlock(after: BlockNumber): Option[BlockNumber] =
        config.nextOwnLeaderBlock(after)

    /** Successor on THIS liaison's outbound stack-brief lane. */
    protected def nextOwnBriefStack(after: StackNumber): Option[StackNumber] =
        config.nextOwnSlowLeaderStack(after)

    /** Successor on the INBOUND block-brief lane (used by verify to advance the cursor). */
    protected def nextRemoteBriefBlock(after: BlockNumber): Option[BlockNumber] =
        remotePeer.nextLeaderBlock(after)

    /** Successor on the INBOUND stack-brief lane. */
    protected def nextRemoteBriefStack(after: StackNumber): Option[StackNumber] =
        remotePeer.nextSlowLeaderStack(after)

    /** The initial outstanding request — its brief cursors depend on the lane shape above. */
    protected def initialRequest: GetMsgBatch = GetMsgBatch.initial(remotePeer)

    override def preStart: IO[Unit] = context.self ! PeerLiaison.PreStart

    override def receive: Receive[IO, Request] =
        PartialFunction.fromFunction {
            case PeerLiaison.PreStart      => preStartLocal
            case PeerLiaison.ResendCurrent => resendCurrent
            case req                       => receiveTotal(req)
        }

    private def resendCurrent: IO[Unit] =
        for {
            current <- state.getCurrentlyRequesting
            _ <- logger.debug(
              s"resend tick: GetMsgBatch batch=${current.batchNum}, ack=${current.softAckNumber}, " +
                  s"block=${current.blockNum}, stackBrief=${current.stackNum}, " +
                  s"hardAck=${current.hardAckNum}, req=${current.requestNum}"
            )
            _ <- sendToRemoteLiaison(current)
        } yield ()

    private def receiveTotal(req: Request): IO[Unit] =
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
                              s"outbox: hard ack stack=${y.stackNum} peer=${y.peerId} " +
                                  s"round=${y.payload.round}"
                            )
                        case y: HardAckWithId =>
                            logger.debug(
                              s"outbox: hub-coil ack seq=${y.seqNum} coil=${y.ack.peerId}"
                            )
                        case y: RelayedMsg =>
                            logger.debug(s"outbox: relayed ack seq=${y.seqNum}")
                    }
                    // Append the event to the corresponding queue in the state
                    _ <- state.appendToOutbox(x)
                    // Check whether the next batch must be sent immediately, and then turn off the flag regardless.
                    mbBatchReq <- state.dischargeSendNextBatchImmediately
                    // Pretend we just received the cached batch request that we need to send immediately (if any)
                    _ <- mbBatchReq.fold(IO.unit)(batchReq => receiveTotal(batchReq))
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
                                _ <- sendToRemoteLiaison(newBatch)
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
                                _ <- state.advanceTo(next)
                                msg <- IO.pure(
                                  s"GetMsgBatch: batch=${next.batchNum}, ack=${next.softAckNumber}, " +
                                      s"block=${next.blockNum}, " +
                                      s"stackBrief=${next.stackNum}, " +
                                      s"hardAck=${next.hardAckNum}, req=${next.requestNum}"
                                )
                                _ <- mermaid(msg)
                                _ <- sendToRemoteLiaison(next)
                                _ <- dispatchVerifiedBatch(x)
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
            _ <- logger.info(s"starting, remote peer: ${remotePeer.label}")
            _ <- initializeConnections
            _ <- mermaid("GetMsgBatch.initial")
            _ <- sendToRemoteLiaison(initialRequest)
            _ <- startResendTimer
        yield ()

    /** Fire a periodic self-message so the request-response chain self-heals after wire-level
      * losses, see the design note on [[PeerLiaison.ResendCurrent]] for the rationale.
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
            Ref.unsafe[IO, GetMsgBatch](initialRequest)

        /** Read-only access to the outstanding [[GetMsgBatch]]. Used by the retransmit timer (see
          * [[startResendTimer]]) and by the [[NewMsgBatch]] handler to detect duplicates.
          */
        def getCurrentlyRequesting: IO[GetMsgBatch] = this.currentlyRequesting.get
        private val nAck = Ref.unsafe[IO, SoftAckNumber](SoftAckNumber.zero)
        private val nBlock = Ref.unsafe[IO, BlockNumber](BlockNumber.zero)
        private val nRequest = Ref.unsafe[IO, RequestNumber](RequestNumber.zero)
        private val nStackBrief = Ref.unsafe[IO, StackNumber](StackNumber.zero)
        private val nHardAck = Ref.unsafe[IO, HardAckNumber](HardAckNumber.zero)
        private val nHubHardAck = Ref.unsafe[IO, HubHardAckNumber](HubHardAckNumber.zero)
        private val nRelayedMsg = Ref.unsafe[IO, RelayedMsgNumber](RelayedMsgNumber.zero)
        private val qAck = Ref.unsafe[IO, Queue[SoftAck]](Queue())
        private val qBlock = Ref.unsafe[IO, Queue[BlockBrief.Next]](Queue())
        private val qRequest = Ref.unsafe[IO, Queue[UserRequestWithId]](Queue())
        private val qStackBrief = Ref.unsafe[IO, Queue[StackBrief]](Queue())
        private val qHardAck = Ref.unsafe[IO, Queue[HardAck]](Queue())
        private val qHubHardAck = Ref.unsafe[IO, Queue[HardAckWithId]](Queue())
        private val qRelayedMsg = Ref.unsafe[IO, Queue[RelayedMsg]](Queue())
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
                hubHardAck <- this.qHubHardAck.get.map(_.isEmpty)
                relayedMsg <- this.qRelayedMsg.get.map(_.isEmpty)
            } yield ack && block && event && stackBrief && hardAck && hubHardAck && relayedMsg

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
                        nextOwnBlock = nextOwnBriefBlock(nBlock)
                        _ <- IO.raiseWhen(!nextOwnBlock.contains(nY))(
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
                        nextOwnStack = nextOwnBriefStack(nStack)
                        _ <- IO.raiseWhen(!nextOwnStack.contains(nY))(
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
                case y: HardAckWithId =>
                    // Relayed coil hard-acks, hub-sequenced: contiguous per-link by `seqNum`
                    // (mirrors the hard-ack lane). The hub's CoilAckSequencer assigns the seqNums.
                    for {
                        nHub <- this.nHubHardAck.get
                        nY = y.seqNum
                        _ <- IO.raiseWhen(nY.convert != 0 && nHub.increment != nY)(
                          Error(
                            s"Bad HardAckWithId increment: last-appended: $nHub, attempted: $nY"
                          )
                        )
                        _ <- this.nHubHardAck.set(nY)
                        _ <- this.qHubHardAck.update(_ :+ y)
                    } yield ()
                case y: RelayedMsg =>
                    // Relayed acks (head soft/hard + coil hard), hub-sequenced: contiguous per-link
                    // by `seqNum`. The hub's CoilLinkRelay assigns the seqNums.
                    for {
                        nRelayed <- this.nRelayedMsg.get
                        nY = y.seqNum
                        _ <- IO.raiseWhen(nY.convert != 0 && nRelayed.increment != nY)(
                          Error(
                            s"Bad RelayedMsg increment: last-appended: $nRelayed, attempted: $nY"
                          )
                        )
                        _ <- this.nRelayedMsg.set(nY)
                        _ <- this.qRelayedMsg.update(_ :+ y)
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
                nHub <- this.nHubHardAck.get
                nRelayed <- this.nRelayedMsg.get
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
                      nextOwnBriefBlock(nBlock).exists(_ < batchReq.blockNum) ||
                      nextOwnBriefStack(nStack).exists(_ < batchReq.stackNum) ||
                      nHard.increment < batchReq.hardAckNum ||
                      nHub.increment < batchReq.hubHardAckNum ||
                      nRelayed.increment < batchReq.relayedMsgNum ||
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
                mHubHardAck <- this.qHubHardAck.modify { q =>
                    val pruned = q.dropWhile(_.seqNum < batchReq.hubHardAckNum)
                    (pruned, pruned.headOption)
                }
                mRelayedMsg <- this.qRelayedMsg.modify { q =>
                    val pruned = q.dropWhile(_.seqNum < batchReq.relayedMsgNum)
                    (pruned, pruned.headOption)
                }
                events <- this.qRequest.modify { q =>
                    val pruned = q.dropWhile(_.requestId._2 < batchReq.requestNum)
                    (pruned, pruned.take(maxRequests))
                }

                _ <- IO.raiseWhen(
                  mAck.isEmpty && mBlock.isEmpty && events.isEmpty &&
                      mStackBrief.isEmpty && mHardAck.isEmpty && mHubHardAck.isEmpty &&
                      mRelayedMsg.isEmpty
                )(EmptyNewMsgBatch)
            } yield NewMsgBatch(
              batchNum = batchReq.batchNum,
              softAck = mAck,
              blockBrief = mBlock,
              stackBrief = mStackBrief,
              hardAck = mHardAck,
              hubHardAck = mHubHardAck,
              relayedMsg = mRelayedMsg,
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
          *
          * TODO(Pc4): for a coil→hub link carrying the relayed multi-head stream, the soft-ack and
          * hard-ack author checks must relax from `author == remote` to `author ∈ population,
          * verified by signature` (§8.3) — the hub relays other heads' artifacts, so their author
          * is not the remote hub. Sound at one head (sole author == remote); revisit when
          * multi-head.
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
                case Some(a) if remotePeer.peerId != PeerId.Head(a.ackId.peerNum) =>
                    return Reject(Rejection.AckPeerMismatch(remotePeer.peerId, a.ackId.peerNum))
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
                case Some(h) if h.ackId.peerId != remotePeer.peerId =>
                    return Reject(
                      Rejection.HardAckPeerMismatch(remotePeer.peerId, h.ackId.peerId)
                    )
                case Some(h) if h.hardAckNum != current.hardAckNum =>
                    return Reject(
                      Rejection.HardAckNumMismatch(current.hardAckNum, h.hardAckNum)
                    )
                case _ => ()

            // 3c. If a relayed coil hard-ack is present, its seqNum must equal the next-expected
            //     `current.hubHardAckNum`. No author check — the embedded ack is a coil's, verified
            //     end-to-end by SlowConsensusActor; the seqNum is the remote hub's relay ordering.
            received.hubHardAck match
                case Some(hc) if hc.seqNum != current.hubHardAckNum =>
                    return Reject(
                      Rejection.HubHardAckNumMismatch(current.hubHardAckNum, hc.seqNum)
                    )
                case _ => ()

            // 3d. If a relayed ack is present, its seqNum must equal the next-expected
            //     `current.relayedMsgNum`. No author check — the embedded soft/hard-ack is signed
            //     and verified end-to-end (and aggregated per author) by the coil's FCA/SCA; the
            //     seqNum is the hub's relay ordering.
            received.relayedMsg match
                case Some(ra) if ra.seqNum != current.relayedMsgNum =>
                    return Reject(
                      Rejection.RelayedMsgNumMismatch(current.relayedMsgNum, ra.seqNum)
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
                    nextRemoteBriefBlock(b.blockNum).getOrElse(current.blockNum)
                ),
                stackNum = received.stackBrief.fold(current.stackNum)(sb =>
                    nextRemoteBriefStack(sb.stackNum).getOrElse(current.stackNum)
                ),
                hardAckNum = received.hardAck.fold(current.hardAckNum)(_.hardAckNum.increment),
                hubHardAckNum = received.hubHardAck.fold(current.hubHardAckNum)(_.seqNum.increment),
                relayedMsgNum = received.relayedMsg.fold(current.relayedMsgNum)(_.seqNum.increment),
                requestNum = received.requests.lastOption.fold(current.requestNum)(
                  _.requestId.requestNum.increment
                )
              )
            )
        }
    }
}
