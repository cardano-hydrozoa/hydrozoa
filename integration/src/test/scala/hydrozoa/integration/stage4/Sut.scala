package hydrozoa.integration.stage4

import cats.effect.{Deferred, Fiber, IO, Ref}
import cats.syntax.all.*
import com.suprnation.actor.ActorSystem
import hydrozoa.integration.stage4.Commands.*
import hydrozoa.integration.stage4.EffectsLanded.BlockExpectation
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, trace}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import hydrozoa.multisig.consensus.{RequestSequencer, UserRequest, UserRequestWithId}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.stack.Stack
import hydrozoa.multisig.persistence.BackendStore
import org.scalacheck.commands.SutCommand
import scalus.cardano.ledger.TransactionHash

// ===================================
// Stage 4 SUT
// ===================================

/** Per-peer actor handles exposed to SUT commands. */
case class Stage4PeerHandle(
    requestSequencer: RequestSequencer.Handle,
)

/** Immutable, set-once resources of a running stage4 SUT. Allocated during `sutResource`,
  * referenced from anywhere thereafter — no field on this case class ever changes.
  */
case class Stage4SutStatic(
    system: ActorSystem[IO],
    cardanoBackend: CardanoBackend[IO],
    peers: Map[HeadPeerNumber, Stage4PeerHandle],
    errorDrainer: Fiber[IO, Throwable, Nothing],
    // Per-peer fibers that periodically poke each `CardanoLiaison` with
    // `CardanoLiaison.Timeout`. Replaces the broken `setReceiveTimeout`-based polling
    // (cats-actors `setReceiveTimeout` checks via a hardcoded 1s ping AND uses
    // `System.currentTimeMillis()` rather than the F-effect clock — both unusable under
    // TestControl). Each fiber sleeps `cardanoLiaisonPollingPeriod` of virtual time, then
    // sends `Timeout` directly to the liaison's mailbox, which triggers a normal `runEffects`
    // (poll L1 + push `PollResults` to BlockWeaver).
    liaisonTickFibers: List[Fiber[IO, Throwable, Nothing]],
    /** Per-peer persistence backend store — used by `analyzePersistence` to assert SC + SCA
      * writes (Treasury + EvacuationMap + HardConfirmation) landed during the scenario.
      */
    backendStores: Map[HeadPeerNumber, BackendStore[IO]],
    log: ContraTracer[IO, Slf4jMsg],
)

/** Mutable state of a running stage4 SUT. `Ref`s are updated by capture observers (and a few
  * SUT command-side accumulators); `Deferred`s are completed exactly once during shutdown
  * coordination in `beforeFinalize`.
  */
case class Stage4SutMutable(
    sutErrors: Ref[IO, List[String]],
    blockBriefs: Map[HeadPeerNumber, Ref[IO, Vector[BlockBrief.Intermediate]]],
    // Per-peer hard-confirmed stacks captured by the SCA ContraTracer sink (the slow cycle's
    // terminal artifact), parallel to `blockBriefs` on the fast side. Used by
    // `analyzeBlockBriefs` to assert the slow side covered every observed block.
    stacks: Map[HeadPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
    // Per-coil hard-confirmed stacks, captured by each coil peer follower's SCA ContraTracer sink
    // (same mechanism as `stacks` on the head side). Empty for a pure-head run. Used to assert
    // the coil peer participates in the slow cycle.
    coilStacks: Map[CoilPeerNumber, Ref[IO, Vector[Stack.HardConfirmed]]],
    submittedRequestIds: Ref[IO, Vector[RequestId]],
    fastSettlementSignal: Deferred[IO, Unit],
    slowCoverageSignal: Deferred[IO, Unit],
    /** Set by [[beforeFinalize]] with the final submitted ID set; the JL capture sink reads it
      * via `tryGet` and only fires [[fastSettlementSignal]] once the target is populated.
      * Prevents the signal from firing mid-run against a partial [[submittedRequestIds]] snapshot.
      */
    fastSettlementTarget: Deferred[IO, Set[RequestId]],
    /** Set by [[beforeFinalize]] (after fast drain) with the block numbers that must be covered;
      * the SCA capture sink reads it via `tryGet` and only fires [[slowCoverageSignal]] once set.
      */
    slowCoverageTarget: Deferred[IO, Set[Int]],
    /** Cross-peer set of L1 tx hashes observed via `CardanoLiaisonEvent.TxSubmitting`. All head
      * peers submit the same backbone txs in parallel; the `Set` collapses duplicates.
      */
    effectsLanded: Ref[IO, Set[TransactionHash]],
    /** Fires when the same condition `EffectsLanded.propEffectsLanded` checks is satisfied —
      * i.e. every backbone expectation completed via happy path or competing fallback. Anchored
      * on `CardanoLiaisonEvent.TxSubmitting` (the enactment event), distinct from
      * [[slowCoverageSignal]] which fires on consensus reach. The gap between the two is the
      * `StackComposer` rate-limit delay; observing both lets us distinguish "stalled at
      * consensus" from "stalled before enactment".
      */
    effectsLandedSignal: Deferred[IO, Unit],
    /** Set by [[beforeFinalize]] (after slow drain) with the backbone expectations derived from
      * the canonical hard-confirmed stacks. The TxSubmitting sink reads via `tryGet` and only
      * fires [[effectsLandedSignal]] once this is populated.
      */
    effectsLandedTarget: Deferred[IO, List[BlockExpectation]],
    /** Fires on the first successful `FallbackToRuleBased` dispatch; `beforeFinalize` races it
      * against the happy-path drain. See the package docstring for the contract.
      */
    fallbackEnteredSignal: Deferred[IO, TransactionHash],
)

case class Stage4Sut(
    static: Stage4SutStatic,
    mutable: Stage4SutMutable,
)

/** Selects how a [[Stage4Sut]] wires its peer liaisons to remote peers.
  *
  *   - [[Direct]] (default) — every peer's liaison gets the actual remote handle from the
  *     corresponding peer's actor system (head↔head and hub↔coil alike). In-process, no network.
  *     Compatible with [[ModelBasedSuite#useTestControl]] = `true`.
  *   - [[WebSocket]] — every head peer runs one shared WS server (`NodeWsServer`) bound to
  *     `127.0.0.1` on an OS-assigned ephemeral port, mounting `/head` for the head mesh and (on a
  *     hub) `/hub` for its coil peers; each coil dials its hub's `/hub`. Ports are discovered
  *     post-bind so two WS-mode test instances in the same JVM never collide. Forces
  *     [[ModelBasedSuite#useTestControl]] = `false` since real sockets don't speak virtual time.
  */
enum TransportMode:
    case Direct
    case WebSocket

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

    // L2 tx: submit directly to the peer's RequestSequencer.
    // Result is always Valid (trivial); oracle check in shutdownSut compares model
    // predictions against actual block-brief outcomes.
    given SutCommand[L2TxCommand, ValidityFlag, Stage4Sut] with {
        override def run(cmd: L2TxCommand, sut: Stage4Sut): IO[ValidityFlag] = {
            for {
                reqId <- sut.static.peers(cmd.peerNum).requestSequencer ?: cmd.request.asUserRequest
                _ <- sut.static.log.trace(s"reqId=$reqId, cmd.request.requestId=${cmd.request.requestId}")
                _ <- sut.mutable.submittedRequestIds.update(_ :+ cmd.request.requestId)
            } yield ValidityFlag.Valid
        }
    }

    // Deposit: register with RequestSequencer AND submit the signed deposit tx to the shared
    // mock L1 backend so CardanoLiaison can observe it on-chain at the correct time.
    given SutCommand[RegisterAndSubmitDepositCommand, ValidityFlag, Stage4Sut] with {
        override def run(cmd: RegisterAndSubmitDepositCommand, sut: Stage4Sut): IO[ValidityFlag] = {
            for {
                reqId <- sut.static.peers(cmd.peerNum).requestSequencer ?: cmd.request.asUserRequest
                _ <- sut.static.log.trace(s"reqId=$reqId, cmd.request.requestId=${cmd.request.requestId}")
                _ <- sut.mutable.submittedRequestIds.update(_ :+ cmd.request.requestId)
                _ <- sut.static.cardanoBackend.submitTx(cmd.depositTxBytesSigned)
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
