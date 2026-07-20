package hydrozoa.integration.stage4

import cats.effect.{Deferred, IO, Ref}
import cats.implicits.*
import com.suprnation.actor.ActorSystem
import hydrozoa.integration.harness.{Capture, Signal}
import hydrozoa.integration.stage4.Commands.*
import hydrozoa.integration.stage4.EffectsLanded.BlockExpectation
import hydrozoa.lib.logging.{ContraTracer, Slf4jMsg, trace}
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import hydrozoa.multisig.consensus.{UserRequest, UserRequestWithId}
import hydrozoa.multisig.ledger.block.BlockBrief
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.event.RequestId.ValidityFlag
import hydrozoa.multisig.ledger.l1.tx.RawTx
import hydrozoa.multisig.ledger.stack.Stack
import hydrozoa.multisig.persistence.BackendStore
import hydrozoa.multisig.server.SubmissionClient
import org.scalacheck.commands.SutCommand
import scalus.cardano.ledger.TransactionHash

// ===================================
// Stage 4 SUT
// ===================================

/** Immutable, set-once resources of a running stage4 SUT. Allocated during `sutResource`,
  * referenced from anywhere thereafter — no field on this case class ever changes.
  *
  * `peers` holds each head peer's [[SubmissionClient]] — built by the harness against that peer's
  * in-process [[hydrozoa.multisig.server.HydrozoaRoutes]] via `Client.fromHttpApp`, so submissions
  * exercise the real HTTP/JSON codec surface without binding a socket.
  */
case class Stage4SutStatic(
    system: ActorSystem[IO],
    cardanoBackend: CardanoBackend[IO],
    peers: Map[HeadPeerNumber, SubmissionClient],
    /** Per-peer persistence backend store — used by `analyzePersistence` to assert SC + SCA
      * writes (Treasury + EvacuationMap + HardConfirmation) landed during the scenario.
      */
    backendStores: Map[HeadPeerNumber, BackendStore[IO]],
    log: ContraTracer[IO, Slf4jMsg],
)

/** Per-head-peer mutable Refs populated by the [[Stage4Plugins.perPeerCaptures]] writer arm.
  * `blockBriefs` holds every `BlockBrief.Intermediate` JL emits on the fast side; `stacks` holds
  * every `Stack.HardConfirmed` SCA emits on the slow side. Bundled here so the SUT carries one
  * `Map[HeadPeerNumber, PerPeerCaptures]` instead of two parallel maps.
  */
case class PerPeerCaptures(
    blockBriefs: Ref[IO, Vector[BlockBrief.Intermediate]],
    stacks: Ref[IO, Vector[Stack.HardConfirmed]],
)

object PerPeerCaptures:
    def make: IO[PerPeerCaptures] =
        for
            briefs <- Ref[IO].of(Vector.empty[BlockBrief.Intermediate])
            stacks <- Ref[IO].of(Vector.empty[Stack.HardConfirmed])
        yield PerPeerCaptures(briefs, stacks)

    def makeMap(peers: Seq[HeadPeerNumber]): IO[Map[HeadPeerNumber, PerPeerCaptures]] =
        peers.toList.traverse(p => make.map(p -> _)).map(_.toMap)

/** Per-coil-peer mutable Refs populated by the [[Stage4Plugins.perCoilCaptures]] writer arm.
  * Today only the hard-confirmed stacks stream is captured; bundled here so a future coil-side
  * signal (e.g. coil `TxSubmitting`) lands alongside it without churning the
  * [[Stage4SutMutable]] field set.
  */
case class PerCoilCaptures(
    stacks: Ref[IO, Vector[Stack.HardConfirmed]],
)

object PerCoilCaptures:
    def make: IO[PerCoilCaptures] =
        Ref[IO].of(Vector.empty[Stack.HardConfirmed]).map(PerCoilCaptures(_))

    def makeMap(coils: Seq[CoilPeerNumber]): IO[Map[CoilPeerNumber, PerCoilCaptures]] =
        coils.toList.traverse(c => make.map(c -> _)).map(_.toMap)

/** Mutable state of a running stage4 SUT. [[Capture]]s wrap Refs populated by writer arms;
  * [[Signal]]s wrap one-shot Deferreds completed by predicate arms; `*Target` Deferreds are
  * armed by [[beforeFinalize]] to gate when the matching signal predicate may fire.
  */
case class Stage4SutMutable(
    sutErrors: Ref[IO, List[String]],
    submittedRequestIds: Ref[IO, Vector[RequestId]],

    perPeer: Capture[Map[HeadPeerNumber, PerPeerCaptures]],
    // Per-coil hard-confirmed stacks, captured by each coil peer follower's SCA arm
    // (same mechanism as `perPeer.state(_).stacks` on the head side). Empty for a pure-head run.
    // Used to assert the coil peer participates in the slow cycle.
    perCoil: Capture[Map[CoilPeerNumber, PerCoilCaptures]],
    /** Cross-peer set of L1 tx hashes observed via `CardanoLiaisonEvent.TxSubmitting`. All head
     * peers submit the same backbone txs in parallel; the `Set` collapses duplicates.
     */
    landedTxs: Capture[Ref[IO, Set[TransactionHash]]],

    fastSettlementSignal: Signal[Unit],
    /** Set by [[beforeFinalize]] with the final submitted ID set; the JL predicate arm reads it
     * via `tryGet` and only fires [[fastSettlementSignal]] once the target is populated.
     * Prevents the signal from firing mid-run against a partial [[submittedRequestIds]] snapshot.
     */
    fastSettlementTarget: Deferred[IO, Set[RequestId]],

    slowCoverageSignal: Signal[Unit],
    /** Set by [[beforeFinalize]] (after fast drain) with the block numbers that must be covered;
      * the SCA predicate arm reads it via `tryGet` and only fires [[slowCoverageSignal]] once set.
      */
    slowCoverageTarget: Deferred[IO, Set[Int]],

    /** Fires when the same condit2ion `EffectsLanded.propEffectsLanded` checks is satisfied —
      * i.e. every backbone expectation completed via happy path or competing fallback. Anchored
      * on `CardanoLiaisonEvent.TxSubmitting` (the enactment event), distinct from
      * [[slowCoverageSignal]] which fires on consensus reach. The gap between the two is the
      * `StackComposer` rate-limit delay; observing both lets us distinguish "stalled at
      * consensus" from "stalled before enactment".
      */
    effectsLandedSignal: Signal[Unit],
    /** Set by [[beforeFinalize]] (after slow drain) with the backbone expectations derived from
      * the canonical hard-confirmed stacks. The TxSubmitting predicate arm reads via `tryGet`
      * and only fires [[effectsLandedSignal]] once this is populated.
      */
    effectsLandedTarget: Deferred[IO, List[BlockExpectation]],

    /** Fires on the first successful `FallbackToRuleBased` dispatch; `beforeFinalize` races it
      * against the happy-path drain. See the package docstring for the contract.
      */
    fallbackEnteredSignal: Signal[TransactionHash],
)

case class Stage4Sut(
    static: Stage4SutStatic,
    mutable: Stage4SutMutable,
)

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

    // L2 tx: submit via the peer's SubmissionClient (in-memory http4s round-trip through the
    // harness's HydrozoaRoutes). Result is always Valid (trivial); oracle check in shutdownSut
    // compares model predictions against actual block-brief outcomes.
    given SutCommand[L2TxCommand, ValidityFlag, Stage4Sut] with {
        override def run(cmd: L2TxCommand, sut: Stage4Sut): IO[ValidityFlag] = {
            for {
                reqId <- sut.static.peers(cmd.peerNum).submit(cmd.request.asUserRequest)
                _ <- sut.static.log.trace(s"reqId=$reqId, cmd.request.requestId=${cmd.request.requestId}")
                _ <- sut.mutable.submittedRequestIds.update(_ :+ cmd.request.requestId)
            } yield ValidityFlag.Valid
        }
    }

    // Deposit: submit via the peer's SubmissionClient (in-memory http4s round-trip) AND submit
    // the signed deposit tx to the shared mock L1 backend so CardanoLiaison can observe it
    // on-chain at the correct time.
    given SutCommand[RegisterAndSubmitDepositCommand, ValidityFlag, Stage4Sut] with {
        override def run(cmd: RegisterAndSubmitDepositCommand, sut: Stage4Sut): IO[ValidityFlag] = {
            for {
                reqId <- sut.static.peers(cmd.peerNum).submit(cmd.request.asUserRequest)
                _ <- sut.static.log.trace(s"reqId=$reqId, cmd.request.requestId=${cmd.request.requestId}")
                _ <- sut.mutable.submittedRequestIds.update(_ :+ cmd.request.requestId)
                _ <- sut.static.cardanoBackend.submitTx(RawTx(cmd.depositTxBytesSigned))
            } yield ValidityFlag.Valid
        }
    }

extension (self: UserRequestWithId)

    /** One-way loosing conversion */
    def asUserRequest: UserRequest = self match {
        case UserRequestWithId.DepositRequest(_, r) =>
            UserRequest.DepositRequest(body = r.body)
        case UserRequestWithId.TransactionRequest(_, r) =>
            UserRequest.TransactionRequest(body = r.body)
    }
