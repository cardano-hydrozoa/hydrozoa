package hydrozoa.integration.stage4

import cats.effect.{Fiber, IO, IOLocal, Ref}
import cats.syntax.all.*
import com.suprnation.actor.ActorSystem
import hydrozoa.integration.stage4.Commands.*
import hydrozoa.lib.logging.Slf4jTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.*
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.joint.JointLedger
import hydrozoa.multisig.ledger.stack.Stack
import hydrozoa.multisig.persistence.{BackendStore, Persistence}
import org.scalacheck.commands.SutCommand

// ===================================
// Per-peer actor stack (local to startupSut)
// ===================================

private[stage4] case class PeerStack(
    blockWeaver: BlockWeaver.Handle,
    cardanoLiaison: CardanoLiaison.Handle,
    eventSequencer: EventSequencer.Handle,
    jointLedger: JointLedger.Handle,
    consensusActor: FastConsensusActor.Handle,
    stackComposer: StackComposer.Handle,
    slowConsensusActor: SlowConsensusActor.Handle,
    /** Per-peer in-memory persistence backend — exposed for post-scenario verification that
      * SC's stack-close writes (Treasury + EvacuationMap) and SCA's hard-confirmation writes
      * (HardConfirmation) landed in the store. See `analyzePersistence` in `Suite.scala`.
      */
    backendStore: BackendStore[IO],
    /** Per-peer typed persistence over [[backendStore]] — shared by all of this peer's actors
      * (the ones here + the peer's `PeerLiaison`s, built later). One per peer so the arrival-stamp
      * generation is bumped exactly once per peer/process.
      */
    persistence: Persistence[IO],
)

// ===================================
// Stage 4 SUT
// ===================================

/** Per-peer actor handles exposed to SUT commands. */
case class Stage4PeerHandle(
    eventSequencer: EventSequencer.Handle,
)

case class Stage4Sut(
    system: ActorSystem[IO],
    cardanoBackend: CardanoBackend[IO],
    peers: Map[HeadPeerNumber, Stage4PeerHandle],
    sutErrors: Ref[IO, List[String]],
    errorDrainer: Fiber[IO, Throwable, Nothing],
    // Per-peer fibers that periodically poke each `CardanoLiaison` with
    // `CardanoLiaison.Timeout`. Replaces the broken `setReceiveTimeout`-based polling
    // (cats-actors `setReceiveTimeout` checks via a hardcoded 1s ping AND uses
    // `System.currentTimeMillis()` rather than the F-effect clock — both unusable under
    // TestControl). Each fiber sleeps `cardanoLiaisonPollingPeriod` of virtual time, then
    // sends `Timeout` directly to the liaison's mailbox, which triggers a normal `runEffects`
    // (poll L1 + push `PollResults` to BlockWeaver).
    liaisonTickFibers: List[Fiber[IO, Throwable, Nothing]],
    blockBriefs: Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]],
    // Per-peer hard-confirmed stacks captured by the SCA ContraTracer sink (the slow cycle's
    // terminal artifact), parallel to `blockBriefs` on the fast side. Used by
    // `analyzeBlockBriefs` to assert the slow side covered every observed block.
    stacks: Map[HeadPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
    /** Per-peer persistence backend store — used by `analyzePersistence` to assert SC + SCA
      * writes (Treasury + EvacuationMap + HardConfirmation) landed during the scenario.
      */
    backendStores: Map[HeadPeerNumber, BackendStore[IO]],
    submittedRequestIds: Ref[IO, Vector[RequestId]],
    tracerLocal: IOLocal[Slf4jTracer],
)

/** Selects how a [[Stage4Sut]] wires its peer liaisons to remote peers.
  *
  *   - [[Direct]] (default) — every peer's [[PeerLiaison]] gets the actual remote handle from the
  *     corresponding peer's actor system. In-process, no network. Compatible with
  *     [[ModelBasedSuite#useTestControl]] = `true`.
  *   - [[WebSocket]] — every peer runs its own [[PeerWsTransport]] bound to localhost on a distinct
  *     port. Cross-peer communication happens over real WebSocket connections. Forces
  *     [[ModelBasedSuite#useTestControl]] = `false` since real sockets don't speak virtual time.
  *     Ports start at [[basePort]] and increase by `peerNum`.
  */
enum TransportMode:
    case Direct
    case WebSocket(basePort: Int = 31000)

// ===================================
// SUT command instances (direct submission)
// ===================================

object Stage4SutCommands:

    // Delay: the framework's IO.sleep(cmd.delay) already advances simulated time under
    // TestControl. No actor message needed — delays exist only to pace model clock and
    // give the BlockWeaver time to fire its wakeup timeouts.
    given SutCommand[DelayCommand, Unit, Stage4Sut] with {
        override def run(cmd: DelayCommand, sut: Stage4Sut): IO[Unit] = IO.unit
    }

    // L2 tx: submit directly to the peer's EventSequencer.
    // Result is always Valid (trivial); oracle check in shutdownSut compares model
    // predictions against actual block-brief outcomes.
    given SutCommand[L2TxCommand, ValidityFlag, Stage4Sut] with {
        override def run(cmd: L2TxCommand, sut: Stage4Sut): IO[ValidityFlag] = {
            given IOLocal[Slf4jTracer] = sut.tracerLocal
            for {
                reqId <- sut.peers(cmd.peerNum).eventSequencer ?: cmd.request.asUserRequest
                _ <- Slf4jTracer.trace(s"reqId=$reqId, cmd.request.requestId=${cmd.request.requestId}")
                _ <- sut.submittedRequestIds.update(_ :+ cmd.request.requestId)
            } yield ValidityFlag.Valid
        }
    }

    // Deposit: register with EventSequencer AND submit the signed deposit tx to the shared
    // mock L1 backend so CardanoLiaison can observe it on-chain at the correct time.
    given SutCommand[RegisterAndSubmitDepositCommand, ValidityFlag, Stage4Sut] with {
        override def run(cmd: RegisterAndSubmitDepositCommand, sut: Stage4Sut): IO[ValidityFlag] = {
            given IOLocal[Slf4jTracer] = sut.tracerLocal
            for {
                reqId <- sut.peers(cmd.peerNum).eventSequencer ?: cmd.request.asUserRequest
                _ <- Slf4jTracer.trace(s"reqId=$reqId, cmd.request.requestId=${cmd.request.requestId}")
                _ <- sut.submittedRequestIds.update(_ :+ cmd.request.requestId)
                _ <- sut.cardanoBackend.submitTx(cmd.depositTxBytesSigned)
            } yield ValidityFlag.Valid
        }
    }

extension (self: UserRequestWithId)

    /** One-way loosing conversion */
    def asUserRequest: UserRequest = self match {
        case UserRequestWithId.DepositRequest(_, r) =>
            UserRequest.DepositRequest(
              header = r.header,
              body = r.body,
              userVk = r.userVk
            )
        case UserRequestWithId.TransactionRequest(_, r) =>
            UserRequest.TransactionRequest(
              header = r.header,
              body = r.body,
              userVk = r.userVk
            )
    }
