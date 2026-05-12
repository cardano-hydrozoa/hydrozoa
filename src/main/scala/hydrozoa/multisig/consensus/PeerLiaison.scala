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
import hydrozoa.multisig.consensus.ack.{AckBlock, AckId, AckNumber}
import hydrozoa.multisig.consensus.peer.HeadPeerId
import hydrozoa.multisig.ledger.block.{BlockBrief, BlockNumber, BlockStatus, BlockType}
import hydrozoa.multisig.ledger.event.{RequestId, RequestNumber}
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
                  s"block=${current.blockNum}, req=${current.requestNum}"
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
                        case y: AckBlock =>
                            logger.debug(s"outbox: ack block=${y.blockNum} peer=${y.peerNum}")
                        case y: BlockBrief.Next =>
                            logger.debug(s"outbox: block block=${y.blockNum}")
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
                      s"Got GetMsgBatch: batch=${x.batchNum}, ack=${x.ackNum}, block=${x.blockNum}, req=${x.requestNum}"
                    )
                    eNewBatch <- state.buildNewMsgBatch(x, config.peerLiaisonMaxRequestsPerBatch)
                    _ <- eNewBatch match {
                        case Right(newBatch) =>
                            for {
                                msg <- IO.pure(
                                  s"NewMsgBatch: batch=${newBatch.batchNum}, " +
                                      s"ack=${newBatch.ack.map(_.ackNum)}, " +
                                      s"block=${newBatch.blockBrief.map(_.blockNum)}, " +
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
                      s"got NewMsgBatch: batch=${x.batchNum}, ack=${x.ack.map(_.ackNum)}, block=${x.blockBrief.map(_.blockNum)}, reqs=${x.requests.size}"
                    )
                    prev <- state.getCurrentlyRequesting
                    next: GetMsgBatch <- state.verifyBatchAndGetNext(x)
                    advanced = next.batchNum != prev.batchNum
                    _ <-
                        if advanced then
                            for {
                                msg <- IO.pure(
                                  s"GetMsgBatch: batch=${next.batchNum}, ack=${next.ackNum}, " +
                                      s"block=${next.blockNum}, req=${next.requestNum}"
                                )
                                // _ <- mermaid(msg)
                                _ <- conn.remotePeerLiaison ! next
                                _ <- x.ack.traverse_(conn.consensusActor ! _)
                                _ <- x.blockBrief.traverse_(conn.blockWeaver ! _)
                                _ <- x.requests.traverse_(conn.blockWeaver ! _)
                            } yield ()
                        else
                            // Stale/duplicate reply: either a verification mismatch or a
                            // late reply for an already-advanced batch (the retransmit timer
                            // can cause this). Don't bounce a GetMsgBatch back — the timer
                            // will keep the chain alive — and don't re-dispatch payloads
                            // (we've already seen them).
                            logger.debug(
                              s"dropping stale NewMsgBatch batch=${x.batchNum} " +
                                  s"(current=${prev.batchNum})"
                            )
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

    /** Fire a periodic [[ResendCurrent]] self-message so the request-response chain self-heals
      * after wire-level losses. See the design note on [[PeerLiaison]] for the rationale.
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
        private val nAck = Ref.unsafe[IO, AckNumber](AckNumber(0))
        private val nBlock = Ref.unsafe[IO, BlockNumber](BlockNumber(0))
        private val nEvent = Ref.unsafe[IO, RequestNumber](RequestNumber(0))
        private val qAck = Ref.unsafe[IO, Queue[AckBlock]](Queue())
        private val qBlock = Ref.unsafe[IO, Queue[BlockBrief.Next]](Queue())
        private val qEvent = Ref.unsafe[IO, Queue[UserRequestWithId]](Queue())
        private val sendBatchImmediately = Ref.unsafe[IO, Option[GetMsgBatch]](None)

        /** Check whether there are no acks, blocks, or events queued-up to be sent out. */
        private def queuesAreEmpty: IO[Boolean] =
            for {
                ack <- this.qAck.get.map(_.isEmpty)
                block <- this.qBlock.get.map(_.isEmpty)
                event <- this.qEvent.get.map(_.isEmpty)
            } yield ack && block && event

        infix def appendToOutbox(x: RemoteBroadcast): IO[Unit] =
            x match {
                case y: UserRequestWithId =>
                    for {
                        nEvent <- this.nEvent.get
                        nY = y.requestId.requestNum
                        _ <- IO.raiseWhen(nY.convert != 0L && nEvent.increment != nY)(
                          Error(s"Bad LedgerEvent increment: last-seen: $nEvent, attempted: $nY")
                        )
                        _ <- this.nEvent.set(nY)
                        _ <- this.qEvent.update(_ :+ y)
                    } yield ()
                case y: AckBlock =>
                    for {
                        nAck <- this.nAck.get
                        nY = y.ackNum
                        _ <- IO.raiseWhen(nY.convert != 0L && nAck.increment != nY)(
                          Error("Bad AckBlock increment: last-seen: $nEvent, attempted: $nY")
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

                _ <- IO.raiseWhen(
                  nAck < batchReq.ackNum ||
                      nBlock < batchReq.blockNum
                )(OutOfBoundsGetMsgBatch)

                _ <- this.queuesAreEmpty.ifM(IO.raiseError(EmptyNewMsgBatch), IO.unit)

                // TODO: once ackNum/blockNum switch to next-expected semantics, change `<=` to `<`
                mAck <- this.qAck.get.map(_.dropWhile(_.ackNum <= batchReq.ackNum).headOption)
                mBlock <- this.qBlock.get.map(
                  _.dropWhile(_.blockNum <= batchReq.blockNum).headOption
                )
                events <- this.qEvent.get.map(
                  _.dropWhile(_.requestId._2 < batchReq.requestNum).take(maxRequests)
                )

                _ <- IO.raiseWhen(mAck.isEmpty && mBlock.isEmpty && events.isEmpty)(
                  EmptyNewMsgBatch
                )
            } yield NewMsgBatch(
              batchNum = batchReq.batchNum,
              ack = mAck,
              blockBrief = mBlock,
              requests = events.toList
            )).attemptNarrow

        def dequeueConfirmed(block: BlockConfirmed): IO[Unit] = {
            val ackNum: AckNumber = AckNumber.neededToConfirm(block.header)
            val ownConfirmedRequests: List[RequestNumber] = block.events.collect {
                case x if x._1.peerNum == config.ownHeadPeerId.peerNum => x._1.requestNum
            }
            for {
                _ <- this.qAck.update(q => q.dropWhile(_.ackId._2 <= ackNum))
                _ <- this.qBlock.update(q => q.dropWhile(_.blockNum <= block.blockNum))
                _ <- IO.whenA(ownConfirmedRequests.nonEmpty)(
                  this.qEvent.update(q =>
                      q.dropWhile(_.requestId.requestNum <= ownConfirmedRequests.max)
                  )
                )
            } yield ()
        }

        def verifyBatchAndGetNext(receivedBatch: NewMsgBatch): IO[GetMsgBatch] = {
            import receivedBatch.{ack, blockBrief, requests}
            for {
                current <- this.currentlyRequesting.get
                nextBatchNum = current.batchNum.increment
                nextAckNum = current.ackNum.increment
                nextBlockNum = remotePeerId.nextLeaderBlock(current.blockNum)

                correctBatchNum = current.batchNum == receivedBatch.batchNum

                // Received ack num (if any) is the increment of the requested ack num.
                // TODO: once ackNum switches to next-expected semantics, check head == current.ackNum
                // (same pattern as correctReceivedRequestIds below).
                correctAckNum = ack.forall(x =>
                    x.ackId.peerNum == remotePeerId.peerNum &&
                        x.ackNum == nextAckNum
                )

                // Received block num (if any) is the next leader block from the remote peer
                // after the requested block num.
                // TODO: once blockNum switches to next-expected semantics, check head == current.blockNum.
                correctBlockNum = blockBrief.forall(_.blockNum == nextBlockNum)

                // requestNum uses "next expected" semantics: current.requestNum is the first
                // expected event number. The first received event (if any) must equal it, and
                // subsequent events must be consecutive.
                correctReceivedRequestIds =
                    requests.headOption.forall(_.requestId.requestNum == current.requestNum) &&
                        requests
                            .map(_.requestId)
                            .iterator
                            .sliding(2)
                            .withPartial(false)
                            .forall(x => x.head.precedes(x(1)))

                nextBatch =
                    if correctBatchNum && correctAckNum && correctBlockNum && correctReceivedRequestIds
                    then
                        GetMsgBatch(
                          batchNum = nextBatchNum,
                          ackNum = ack.fold(current.ackNum)(_.ackNum),
                          blockNum = blockBrief.fold(current.blockNum)(_.blockNum),
                          requestNum = requests.lastOption.fold(current.requestNum)(
                            _.requestId.requestNum.increment
                          )
                        )
                    else current
                _ <- this.currentlyRequesting.set(nextBatch)
            } yield nextBatch
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
        remotePeerLiaison: PeerLiaison.Handle
    )

    type Handle = ActorRef[IO, Request]

    type BlockConfirmed = BlockBrief.Section & BlockType.Next & BlockStatus.MultiSigned

    object BlockConfirmed {

        /** For testing purposes */
        object Minimal {
            def apply(x: BlockBrief.Next): BlockConfirmed = x.asMultiSigned
        }
    }

    type Request =
        PreStart.type | ResendCurrent.type | RemoteBroadcast | GetMsgBatch | NewMsgBatch |
            BlockConfirmed

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
        type RemoteBroadcast = AckBlock | BlockBrief.Next | UserRequestWithId

        /** Request by a comm actor to its remote comm-actor counterpart for a batch of events,
          * blocks, or block acknowledgements originating from the remote peer.
          *
          * @param batchNum
          *   Batch number that increases by one for every consecutive batch.
          * @param ackNum
          *   The requester's last seen block acknowledgement from the remote peer. TODO: switch to
          *   next-expected semantics (like requestNum) once block 0 is distributed via the batch
          *   channel instead of out-of-band via the head config.
          * @param blockNum
          *   The requester's last seen block from the remote peer. TODO: switch to next-expected
          *   semantics (like requestNum) once block 0 is distributed via the batch channel instead
          *   of out-of-band via the head config.
          * @param requestNum
          *   Next-expected event number from the remote peer (inclusive): the responder sends all
          *   events with requestNum >= this value.
          */
        final case class GetMsgBatch(
            batchNum: Batch.Number,
            ackNum: AckNumber,
            blockNum: BlockNumber,
            requestNum: RequestNumber
        )

        object GetMsgBatch {
            def initial: GetMsgBatch = GetMsgBatch(
              batchNum = Batch.Number(0),
              ackNum = AckNumber(0),
              blockNum = BlockNumber(0),
              requestNum = RequestNumber(0)
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
            ack: Option[AckBlock],
            blockBrief: Option[BlockBrief.Next],
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
