package hydrozoa.rulebased

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.*
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.NodePrivateConfig
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.owninfo.OwnPeerPublic
import hydrozoa.config.{HydrozoaBlueprint, ScriptReferenceUtxos}
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.{pubKeyHash, shelleyAddress}
import hydrozoa.lib.cardano.scalus.ledger.{CollateralOutput, CollateralUtxo}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.commitment.Membership
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap}
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.tx.{EnrichedTx, FallbackTx, TxFamily}
import hydrozoa.multisig.ledger.stack.{PartitionEffects, StackEffects, StackNumber, StandaloneEvacuationCommitment}
import hydrozoa.multisig.persistence.{Markers, Persistence, StoreKey}
import hydrozoa.rulebased.RuleBasedActor.Error.NoSuitableCollateralUtxosFound
import hydrozoa.rulebased.RuleBasedActor.Requests.Tick
import hydrozoa.rulebased.RuleBasedActor.{Error, *}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.{Abstain, AwaitingVote, Voted}
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteStatus, secFromData}
import hydrozoa.rulebased.ledger.l1.tx.*
import hydrozoa.rulebased.ledger.l1.utxo.*
import scala.util.{Failure, Success, Try}
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.{Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxo, Utxos}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.fromData

// TODO: relocate
extension (tx: Transaction) {

    /** Sign with the rule-based actor's own wallet (distinct from the MRM peer wallet). */
    def selfSigned(using config: Config): Transaction = config.ruleBasedWallet.signTx(tx)
}

extension [T <: EnrichedTx[T]](etx: T) {

    /** Sign the enriched tx with the rule-based actor's own wallet, preserving family info so
      * downstream wrappers (e.g. the firewall) can dispatch statically on it.
      */
    def selfSigned(using config: Config): T = config.ruleBasedWallet.signTx(etx)
}

/** Single actor that drives the rule-based regime end to end: while the treasury is Unresolved it
  * runs the dispute branch (vote/abstain → tally → resolve); once Resolved it runs the evacuation
  * branch (chained `lastContinuingTxs` reads + evacuation tx submissions). Each tick parses the
  * treasury once and dispatches on its datum to exactly one branch, so only that branch's backend
  * queries run.
  *
  * The actor holds no per-iteration state: every tick re-queries the chain and re-reads the two
  * persistence-backed inputs it needs — the peer's SEC + signatures (for `Vote`), and the candidate
  * evacuation maps + fallback anchor (for `Evacuate`). Both are recovered by reading `Persistence`
  * at the current markers; idempotent by construction.
  *
  * Erroring semantics:
  *   - Cardano backend query/submit failures are swallowed and retried.
  *   - Parsing failures behind a token guard throw — the on-chain state has diverged from the spec.
  *   - Tx-builder failures throw — all builder inputs are validated upstream.
  *   - Multiple treasury utxos with the treasury token throw.
  */
final case class RuleBasedActor(
    persistence: Persistence[IO],
    cardanoBackend: CardanoBackend[IO],
    tracer: ContraTracer[IO, RuleBasedActorEvent]
)(using config: Config)
    extends Actor[IO, RuleBasedActor.Requests.Request] {

    /** Rule-based backend queries used by this actor. Each helper bakes in tracing of its specific
      * backend-error event and lifts recoverable errors into `Error.RecoverableErrors`.
      */
    private object Backend {
        def utxosAtDispute: EitherT[IO, Error.RecoverableErrors, Utxos] =
            traced(
              before = RuleBasedActorEvent.Dispute.Querying,
              action = cardanoBackend.utxosAt(
                address = HydrozoaBlueprint.mkDisputeAddress(config.cardanoInfo.network),
                asset = (config.headMultisigScript.policyId, config.headTokenNames.voteTokenName)
              ),
              onError = RuleBasedActorEvent.Backend.ErrorDisputeUtxos(_)
            )

        def utxosAtTreasury: EitherT[IO, Error.RecoverableErrors, Utxos] =
            traced(
              before = RuleBasedActorEvent.Treasury.Querying,
              action = cardanoBackend.utxosAt(
                address = HydrozoaBlueprint.mkTreasuryAddress(config.cardanoInfo.network),
                asset =
                    (config.headMultisigScript.policyId, config.headTokenNames.treasuryTokenName)
              ),
              onError = RuleBasedActorEvent.Backend.ErrorTreasuryUtxos(_)
            )

        def utxosAtPeer(addr: ShelleyAddress): EitherT[IO, Error.RecoverableErrors, Utxos] =
            traced(
              before = RuleBasedActorEvent.Collateral.Querying(addr),
              action = cardanoBackend.utxosAt(addr),
              onError = RuleBasedActorEvent.Backend.ErrorPeerUtxos(_)
            )

        def utxosAtFee(addr: ShelleyAddress): EitherT[IO, Error.RecoverableErrors, Utxos] =
            run(cardanoBackend.utxosAt(addr), RuleBasedActorEvent.Backend.ErrorFeeUtxos(_))

        def continuingTreasuryTxsAfter(
            after: TransactionHash
        ): EitherT[IO, Error.RecoverableErrors, List[(TransactionHash, Data, Data)]] =
            run(
              cardanoBackend.lastContinuingTxs(
                asset =
                    (config.headMultisigScript.policyId, config.headTokenNames.treasuryTokenName),
                after = after
              ),
              RuleBasedActorEvent.Backend.ErrorContinuingTxs(_)
            )

        def submit(etx: EnrichedTx[?]): EitherT[IO, Error.RecoverableErrors, Unit] =
            run(cardanoBackend.submitTx(etx), RuleBasedActorEvent.Backend.ErrorSubmittingTx(_))

        /** Emit `before` (a "Querying" event), then run + wrap error per [[run]]. */
        private def traced[A](
            before: RuleBasedActorEvent,
            action: IO[Either[CardanoBackend.Error, A]],
            onError: CardanoBackend.Error => RuleBasedActorEvent
        ): EitherT[IO, Error.RecoverableErrors, A] =
            EitherT.right[Error.RecoverableErrors](tracer.traceWith(before)) >>
                run(action, onError)

        private def run[A](
            action: IO[Either[CardanoBackend.Error, A]],
            errorEvent: CardanoBackend.Error => RuleBasedActorEvent
        ): EitherT[IO, Error.RecoverableErrors, A] =
            EitherT(action)
                .leftSemiflatTap(e => tracer.traceWith(errorEvent(e)))
                .leftMap(Error.RecoverableCardanoBackendError(_): Error.RecoverableErrors)
    }

    /** Tracing helpers that fold the tracer's emit into the EitherT pipeline. */
    private object Trace {
        def traceRight(event: RuleBasedActorEvent): EitherT[IO, Error.RecoverableErrors, Unit] =
            EitherT.right(tracer.traceWith(event))

        /** Emit `event`, then short-circuit the pipeline with the recoverable `err`. */
        def traceLeft[A](
            event: RuleBasedActorEvent,
            err: Error.RecoverableErrors
        ): EitherT[IO, Error.RecoverableErrors, A] =
            EitherT.left[A](tracer.traceWith(event) >> IO.pure(err))
    }
    import Trace.*

    private def pure[A](a: A): EitherT[IO, Error.RecoverableErrors, A] =
        EitherT.right(IO.pure(a))

    private def raiseError[A](t: Throwable): EitherT[IO, Error.RecoverableErrors, A] =
        EitherT.liftF(IO.raiseError(t))

    /** Build, sign, and submit one dispute-flow tx.
      */
    private def buildAndSubmit[T <: EnrichedTx[T]: TxFamily, E <: Throwable](
        result: => Either[E, T],
        wrapError: E => Throwable,
    ): EitherT[IO, Error.RecoverableErrors, Unit] =
        for {
            _ <- traceRight(RuleBasedActorEvent.Tx.Building(TxFamily[T].name))
            tx <- result match {
                case Left(e)   => raiseError(wrapError(e))
                case Right(tx) => pure(tx)
            }
            _ <- signAndSubmitTx[T](tx)
        } yield ()

    private def signAndSubmitTx[TxType <: EnrichedTx[TxType]](
        tx: TxType,
    ): EitherT[IO, Error.RecoverableErrors, Unit] = {
        for {
            _ <- traceRight(RuleBasedActorEvent.Tx.Submitting(tx))
            res <- Backend.submit(tx.selfSigned)
            _ <- traceRight(RuleBasedActorEvent.Tx.SubmitSuccess(tx))
        } yield res
    }

    /** Read the treasury utxo (by treasury token) once per tick and parse it. The datum subtype
      * dispatches the caller to dispute (Unresolved) vs evacuation (Resolved). Missing is
      * recoverable (we retry); other parse failures throw (the on-chain state has diverged from the
      * spec).
      */
    private def getTreasury: EitherT[IO, Error.RecoverableErrors, RuleBasedTreasuryUtxo] =
        for {
            utxos <- Backend.utxosAtTreasury
            treasuryUtxo <- RuleBasedTreasuryUtxo.parseOrMissing(utxos) match {
                case Right(u) => pure(u)
                case Left(RuleBasedTreasuryUtxo.LookupError.Missing) =>
                    traceRight(RuleBasedActorEvent.Treasury.NotFound) >>
                        EitherT.leftT[IO, RuleBasedTreasuryUtxo](
                          Error.ParseError.Treasury.TreasuryMissing: Error.RecoverableErrors
                        )
                case Left(e) => raiseError(e)
            }
            _ <- traceRight(
              RuleBasedActorEvent.Treasury.Found(treasuryUtxo.treasuryOutput.value.toString)
            )
            _ <- traceRight(RuleBasedActorEvent.Treasury.Parsing)
            _ <- treasuryUtxo.treasuryOutput.datum match {
                case _: RuleBasedTreasuryDatum.Unresolved =>
                    traceRight(RuleBasedActorEvent.Treasury.ParsedUnresolved)
                case _: RuleBasedTreasuryDatum.Resolved =>
                    traceRight(RuleBasedActorEvent.Treasury.ParsedResolved)
            }
        } yield treasuryUtxo

    /** Determine this peer's dispute action, scoped to the on-chain treasury's `versionMajor`. The
      * dispute-resolution script pins `sec.versionMajor` to the treasury reference input's, so a
      * vote whose SEC came from a stack ahead of the on-chain settlement fails Plutus validation.
      *
      * Walks backward from `markers.hardConfirmed` down to `StackNumber.first`, at each stack
      * picking the last SEC (partition reverse order) whose `blockVersion.major` matches
      * `treasuryVersionMajor`. Stops at the first match. Returns `Abstain` if none exists — the
      * dispute is tallied/resolved from there.
      */
    private def loadAction(treasuryVersionMajor: BigInt): IO[DisputeAction] =
        for {
            markers <- Markers.derive(persistence.backend, config.ownPeerId)
            latest <- markers.hardConfirmed.liftTo[IO](
              MissingState("no hard-confirmed stack on disk")
            )
            action <- Monad[IO].tailRecM[StackNumber, DisputeAction](latest) { stack =>
                persistence.get(StoreKey.HardConfirmation(stack)).flatMap {
                    case None =>
                        IO.raiseError(MissingState(s"HardConfirmation($stack) missing"))
                    case Some(_: StackEffects.HardConfirmed.Initial) =>
                        // Stack 0 is Initial by construction: no SEC-bearing partition, no
                        // deeper walk. Even if the treasury's `versionMajor` were 0 here,
                        // Initial stacks don't carry standalone evacuation commitments.
                        IO.pure(Right(DisputeAction.Abstain))
                    case Some(r: StackEffects.HardConfirmed.Regular) =>
                        RuleBasedActor
                            .lastSecMatchingVersion(r.partitions, treasuryVersionMajor) match {
                            case Some(multiSec) =>
                                // `HardAckAggregator.collectSecSignatures` stores sigs in
                                // `secSigners` order = `allHeadPeers.sorted ++
                                // coilPeers.sorted.take(coilQuorum)`. Head sigs occupy the
                                // first `nHeadPeers` positions; the tail is the coil quorum,
                                // dense (only signers appear — no `None` slots).
                                val (head, coil) = multiSec.headerMultiSigned
                                    .splitAt(config.nHeadPeers.convert)
                                IO.pure(
                                  Right(
                                    DisputeAction.Vote(
                                      sec = RuleBasedActor.toOnchain(multiSec.commitment),
                                      signatures = head,
                                      coilSignatures = coil.map(Some(_))
                                    )
                                  )
                                )
                            case None if stack == StackNumber.first =>
                                IO.pure(Right(DisputeAction.Abstain))
                            case None =>
                                IO.pure(Left(stack.decrement))
                        }
                }
            }
        } yield action

    /** Re-read the evacuation-side rule-based inputs from persistence. Called on each tick that
      * runs the evacuation branch. Walks backward through hard-confirmed stacks (like
      * [[loadAction]]) until it finds one with a Major partition — its fallback tx is the anchor,
      * and all its SECs / the default map come from the same stack. Minor-only stacks accumulated
      * after the last Major are skipped; the initial stack terminates the walk with
      * `config.initialEvacuationMap` + the initial fallback.
      */
    private def loadEvacuationInputs: IO[EvacuationInputs] =
        for {
            markers <- Markers.derive(persistence.backend, config.ownPeerId)
            latest <- markers.hardConfirmed.liftTo[IO](
              MissingState("no hard-confirmed stack on disk")
            )
            res <- Monad[IO].tailRecM[StackNumber, EvacuationInputs](latest) { stack =>
                persistence.get(StoreKey.HardConfirmation(stack)).flatMap {
                    case None =>
                        IO.raiseError(MissingState(s"HardConfirmation($stack) missing"))
                    case Some(i: StackEffects.HardConfirmed.Initial) =>
                        IO.pure(
                          Right(
                            EvacuationInputs(
                              candidateEvacMaps = Map(
                                config.initialEvacuationMap.kzgCommitment ->
                                    config.initialEvacuationMap
                              ),
                              fallbackTxHash = i.fallbackTx.tx.id
                            )
                          )
                        )
                    case Some(r: StackEffects.HardConfirmed.Regular) =>
                        RuleBasedActor.lastFallback(r.partitions) match {
                            case None =>
                                // Minor-only stack: no fallback to anchor from. Walk back.
                                if stack == StackNumber.first then
                                    IO.raiseError(
                                      MissingState(
                                        s"walked back to $stack with no fallback found"
                                      )
                                    )
                                else IO.pure(Left(stack.decrement))
                            case Some(fallbackTx) =>
                                loadRegularEvacuationInputs(stack, r, fallbackTx).map(Right(_))
                        }
                }
            }
        } yield res

    /** Collect the evacuation inputs for a specific `Regular` stack that carries a fallback: the
      * default evacuation map (keyed by the stack's `lastBlockNum`) and one entry per SEC carried
      * by its partitions, keyed by kzg commitment. Extracted so the tailRec walk stays thin.
      */
    private def loadRegularEvacuationInputs(
        stackNum: StackNumber,
        r: StackEffects.HardConfirmed.Regular,
        fallbackTx: FallbackTx,
    ): IO[EvacuationInputs] =
        for {
            // Default-vote map — what the multisig treasury was committed to at fallback
            // time. The default vote utxo carries this kzg, so if peers never tally onto a
            // newer SEC the resolution will land here. The closing stack's `lastBlockNum`
            // comes from the `UnsignedStack` every peer persists on every close (atomic
            // with the hard-ack), so it is present for any hard-confirmed stack — unlike
            // the StackLane brief (leader-authored only).
            unsignedStack <- persistence
                .get(StoreKey.UnsignedStack(stackNum))
                .flatMap(
                  _.liftTo[IO](MissingState(s"UnsignedStack($stackNum) missing"))
                )
            lastBlockNum = unsignedStack.brief.lastBlockNum
            defaultMap <- persistence
                .get(StoreKey.EvacuationMap(lastBlockNum))
                .flatMap(
                  _.liftTo[IO](MissingState(s"EvacuationMap($lastBlockNum) missing"))
                )
            // SEC maps — every candidate SEC peers could vote for, keyed by its kzg
            // commitment. The dispute resolution writes whichever wins into the treasury's
            // Resolved.evacuationActive, so the EvacuationActor looks it up here at runtime.
            secMaps <- RuleBasedActor
                .allSecs(r.partitions)
                .traverse { multiSec =>
                    persistence
                        .get(StoreKey.EvacuationMap(multiSec.commitment.blockNum))
                        .flatMap(
                          _.liftTo[IO](
                            MissingState(
                              s"EvacuationMap(${multiSec.commitment.blockNum})" +
                                  " missing for candidate SEC"
                            )
                          )
                        )
                        .map(map => multiSec.commitment.kzgCommitment -> map)
                }
        } yield EvacuationInputs(
          candidateEvacMaps = ((defaultMap.kzgCommitment -> defaultMap) +: secMaps).toMap,
          fallbackTxHash = fallbackTx.tx.id
        )

    private object Dispute {

        /** Dispute branch — treasury is Unresolved. Head peers cast their reserved AwaitingVote
          * box; coil peers ratchet a public (Voted/Abstain) box forward with a multisigned SEC (see
          * [[handleCoil]]). Both peer types fall through to Tally / Resolve / EmptyVotes
          * classification when their peer-specific vote path is inapplicable.
          */
        def handle(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            config.ownPeerId match {
                case PeerId.Head(_) => handleHead(treasuryUtxo)
                case PeerId.Coil(_) => handleCoil(treasuryUtxo)
            }

        /** Head-peer dispute flow. Classifies the dispute address's utxos and dispatches to
          * [[castVote]] / [[tally]] / [[resolve]].
          */
        private def handleHead(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            for {
                disputeUtxos <- getDisputeUtxos
                collateralUtxo <- getCollateral
                _ <- disputeUtxos match {
                    case DisputeUtxos.CastVote(ownBallotBox) =>
                        castVote(ownBallotBox, treasuryUtxo, collateralUtxo)
                    case DisputeUtxos.Tally(otherUtxos) =>
                        tally(otherUtxos, treasuryUtxo, collateralUtxo)
                    case DisputeUtxos.Resolve(finalVoteUtxo) =>
                        resolve(finalVoteUtxo, treasuryUtxo, collateralUtxo)
                    // Treasury was Unresolved but the dispute address holds no vote utxos. Per
                    // the spec this state is unreachable, so escalate.
                    case DisputeUtxos.EmptyVotes =>
                        raiseError(Error.TreasuryUnresolvedButNoVotes)
                }
            } yield ()

        /** Coil-peer dispute flow. Loads the target SEC upfront (same SEC-selection logic as head
          * peers), then decides between:
          *   - if any Open-phase box is already at `(sec.commitment, sec.versionMinor)`, trace +
          *     noop (nothing to add this tick);
          *   - otherwise pick the lowest-versionMinor Open box below the target and submit a
          *     [[RatchetVoteTx]];
          *   - if no SEC (Abstain) or no ratchet-able box, fall through to residual
          *     Tally/Resolve/EmptyVotes classification so the coil peer still helps finalize.
          */
        private def handleCoil(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            for {
                versionMajor <- extractTreasuryVersionMajor(treasuryUtxo)
                action <- EitherT.liftF[IO, Error.RecoverableErrors, DisputeAction](
                  loadAction(versionMajor)
                )
                parsed <- parseDisputeUtxos
                collateralUtxo <- getCollateral
                _ <- action match {
                    case v: DisputeAction.Vote =>
                        val alreadyAtTarget = parsed.exists { bb =>
                            bb.ballotBoxOutput.status match {
                                case Voted(c, m) =>
                                    c == v.sec.commitment && m == v.sec.versionMinor
                                case _ => false
                            }
                        }
                        if alreadyAtTarget then
                            traceRight(RuleBasedActorEvent.Dispute.Coil.AlreadyAtTarget)
                        else
                            pickRatchetTarget(parsed, v.sec.versionMinor) match {
                                case Some(openBox) =>
                                    traceRight(RuleBasedActorEvent.Dispute.Coil.ParsingRatchet) >>
                                        buildAndSubmit(
                                          result = RatchetVoteTx
                                              .Build(
                                                openBallotBox = openBox,
                                                treasuryUtxo = treasuryUtxo,
                                                collateralUtxo = collateralUtxo,
                                                sec = v.sec,
                                                signatures = v.signatures,
                                                coilSignatures = v.coilSignatures
                                              )
                                              .result,
                                          wrapError = Error.BuildError.RatchetVote(_)
                                        )
                                case None =>
                                    traceRight(
                                      RuleBasedActorEvent.Dispute.Coil.NoRatchetTarget
                                    ) >> dispatchResidual(parsed, treasuryUtxo, collateralUtxo)
                            }
                    case DisputeAction.Abstain =>
                        traceRight(RuleBasedActorEvent.Dispute.Coil.NoRatchetTarget) >>
                            dispatchResidual(parsed, treasuryUtxo, collateralUtxo)
                }
            } yield ()

        /** Pick the lowest-versionMinor Open-phase box strictly below `targetVersionMinor`. Abstain
          * is treated as `versionMinor = 0`. Deterministic ordering so multiple coil peers converge
          * on the same target instead of racing to distinct boxes.
          */
        private def pickRatchetTarget(
            parsed: List[BallotBox[VoteStatus]],
            targetVersionMinor: BigInt
        ): Option[BallotBox[Voted | Abstain.type]] = {
            val openBoxes: List[(BallotBox[Voted | Abstain.type], BigInt)] = parsed.collect {
                case bb if bb.ballotBoxOutput.status.isInstanceOf[Voted] =>
                    val v = bb.ballotBoxOutput.status.asInstanceOf[Voted]
                    (bb.asInstanceOf[BallotBox[Voted | Abstain.type]], v.versionMinor)
                case bb if bb.ballotBoxOutput.status == Abstain =>
                    (bb.asInstanceOf[BallotBox[Voted | Abstain.type]], BigInt(0))
            }
            openBoxes.filter(_._2 < targetVersionMinor).sortBy(_._2).headOption.map(_._1)
        }

        /** Residual dispatch shared by head fallthrough (own box absent) and coil fallthrough (no
          * ratchet target or no SEC): Tally / Resolve / EmptyVotes.
          */
        private def dispatchResidual(
            parsed: List[BallotBox[VoteStatus]],
            treasuryUtxo: RuleBasedTreasuryUtxo,
            collateralUtxo: CollateralUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            parsed match {
                case Nil =>
                    traceRight(RuleBasedActorEvent.Dispute.ParsingEmptyVotes) >>
                        raiseError(Error.TreasuryUnresolvedButNoVotes)
                case x :: Nil =>
                    x.ballotBoxOutput.status match {
                        case _: Voted =>
                            traceRight(RuleBasedActorEvent.Dispute.ParsingResolve) >>
                                resolve(
                                  x.asInstanceOf[BallotBox[Voted]],
                                  treasuryUtxo,
                                  collateralUtxo
                                )
                        case _ => raiseError(Error.NonVotedUtxoAtResolve(x))
                    }
                case xs =>
                    traceRight(RuleBasedActorEvent.Dispute.ParsingTally) >>
                        tally(xs, treasuryUtxo, collateralUtxo)
            }

        /** Parse the treasury's `versionMajor` from an Unresolved datum. `Dispute.handle` is only
          * invoked when the treasury is Unresolved (see [[handleTick]]), so the Resolved branch is
          * unreachable — surfaced as an [[IllegalStateException]] rather than silently swallowed so
          * any future dispatcher regression is loud.
          */
        private def extractTreasuryVersionMajor(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): EitherT[IO, Error.RecoverableErrors, BigInt] =
            treasuryUtxo.treasuryOutput.datum match {
                case RuleBasedTreasuryDatum.Unresolved(_, v, _) => pure(v)
                case _: RuleBasedTreasuryDatum.Resolved =>
                    raiseError(
                      new IllegalStateException(
                        "Dispute.handle reached with a Resolved treasury datum; " +
                            "handleTick dispatches Resolved to Evacuation.handle"
                      )
                    )
            }

        /** Find an ada-only collateral utxo at the own peer's address. */
        def getCollateral: EitherT[IO, Error.RecoverableErrors, CollateralUtxo] = {
            val peerAddr = config.ownWallet.exportVerificationKey.shelleyAddress()
            for {
                collateralCandidates <- Backend.utxosAtPeer(peerAddr)
                collateralUtxoTuple <- collateralCandidates.filter((_, to) =>
                    to.value.isOnlyAda
                ) match {
                    case x if x.nonEmpty =>
                        pure(x.toList.maxBy(_._2.value.coin.value))
                    case _ =>
                        traceLeft[(TransactionInput, TransactionOutput)](
                          RuleBasedActorEvent.Collateral.NotFound(peerAddr),
                          NoSuitableCollateralUtxosFound(peerAddr)
                        )
                }
                collateralOutput <- collateralUtxoTuple._2 match {
                    case TransactionOutput.Babbage(
                          ShelleyAddress(_, key: ShelleyPaymentPart.Key, delegation),
                          value,
                          datum,
                          scriptRef
                        ) =>
                        pure(
                          CollateralOutput(
                            addrKeyHash = key.hash,
                            delegationPart = delegation,
                            coin = value.coin,
                            datumOption = datum,
                            scriptRef = scriptRef
                          )
                        )
                    case _ =>
                        traceLeft[CollateralOutput](
                          RuleBasedActorEvent.Collateral.NotFound(peerAddr),
                          NoSuitableCollateralUtxosFound(peerAddr)
                        )
                }
                _ <- traceRight(RuleBasedActorEvent.Collateral.Found)
            } yield CollateralUtxo(collateralUtxoTuple._1, collateralOutput)
        }

        /** Branch: own ballot box still `AwaitingVote`. Load this peer's action (Vote/Abstain) and
          * submit the corresponding tx. `loadAction` is scoped to the on-chain treasury's
          * `versionMajor` — the dispute script matches SEC.versionMajor against the treasury
          * reference input, so voting with the newest off-chain SEC when the treasury's settlement
          * is lagging is guaranteed to fail Plutus validation.
          */
        def castVote(
            ownBallotBox: BallotBox[AwaitingVote],
            treasuryUtxo: RuleBasedTreasuryUtxo,
            collateralUtxo: CollateralUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            for {
                treasuryVersionMajor <- treasuryUtxo.treasuryOutput.datum match {
                    case RuleBasedTreasuryDatum.Unresolved(_, v, _) => pure(v)
                    case _: RuleBasedTreasuryDatum.Resolved =>
                        raiseError(
                          new IllegalStateException(
                            "castVote reached with a Resolved treasury datum; " +
                                "handleTick dispatches Resolved to Evacuation.handle"
                          )
                        )
                }
                action <- EitherT.liftF[
                  IO,
                  Error.RecoverableErrors,
                  DisputeAction
                ](loadAction(treasuryVersionMajor))
                _ <- action match {
                    case DisputeAction.Vote(
                          sec,
                          signatures,
                          coilSignatures
                        ) =>
                        buildAndSubmit(
                          result = VoteTx
                              .Build(
                                uncastBallotBox = ownBallotBox,
                                treasuryUtxo = treasuryUtxo,
                                collateralUtxo = collateralUtxo,
                                sec = sec,
                                signatures = signatures,
                                coilSignatures = coilSignatures,
                              )
                              .result,
                          wrapError = Error.BuildError.Vote(_)
                        )
                    case DisputeAction.Abstain =>
                        buildAndSubmit(
                          result = AbstainTx
                              .Build(
                                uncastBallotBox = ownBallotBox,
                                collateralUtxo = collateralUtxo
                              )
                              .result,
                          wrapError = Error.BuildError.Abstain(_)
                        )
                }
            } yield ()

        /** Branch: deadline passed; merge two ballot boxes via TallyTx. The continuing input must
          * have a key strictly less than the removed input, so we sort.
          */
        def tally(
            otherUtxos: Seq[BallotBox[VoteStatus]],
            treasuryUtxo: RuleBasedTreasuryUtxo,
            collateralUtxo: CollateralUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] = {
            val keySorted = otherUtxos.sortBy(_.ballotBoxOutput.key)
            val continuing = keySorted.head
            for {
                _ <- traceRight(RuleBasedActorEvent.Tx.Tallying)
                removed <- keySorted.tail.find(ballotBox =>
                    ballotBox.ballotBoxOutput.key == continuing.ballotBoxOutput.link
                ) match {
                    case None    => raiseError(Error.NoCompatibleVoteForTallyingFound(otherUtxos))
                    case Some(x) => pure(x)
                }
                _ <- buildAndSubmit(
                  result = TallyTx
                      .Build(
                        continuingBallotBox = continuing,
                        removedBallotBox = removed,
                        treasuryUtxo = treasuryUtxo,
                        collateralUtxo = collateralUtxo
                      )
                      .result,
                  wrapError = Error.BuildError.Tally(_)
                )
            } yield ()
        }

        /** Branch: only a single Voted ballot box remains. Submit ResolutionTx to flip the treasury
          * datum to `Resolved`.
          */
        def resolve(
            finalVoteUtxo: BallotBox[Voted],
            treasuryUtxo: RuleBasedTreasuryUtxo,
            collateralUtxo: CollateralUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            buildAndSubmit(
              result = ResolutionTx
                  .Build(
                    talliedBallotBox = finalVoteUtxo,
                    treasuryUtxo = treasuryUtxo,
                    collateralUtxo = collateralUtxo
                  )
                  .result,
              wrapError = Error.BuildError.Resolution(_)
            )
    }

    private object Evacuation {

        /** Evacuation branch — treasury is Resolved. Determines the remaining evacuation map (the
          * full map at resolution time, minus already-evacuated keys), then builds + submits an
          * [[EvacuationTx]] for whatever is left.
          */
        def handle(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            for {
                toEvacuate <- getEvacuationMap
                _ <-
                    if toEvacuate.isEmpty
                    then
                        traceLeft[Unit](
                          RuleBasedActorEvent.Evacuation.NoMore,
                          Error.QueryError.NoEvacuateesRemaining
                        )
                    else traceRight(RuleBasedActorEvent.Evacuation.PayoutsLeft(toEvacuate.size))

                feeAndCollateral <- getFeeAndCollateral
                evacBuilder = EvacuationTx.Build(
                  inputTreasuryUtxo = treasuryUtxo,
                  evacuateesToTryNext = toEvacuate,
                  allRemainingEvacuatees = toEvacuate,
                  feeUtxos = feeAndCollateral._1,
                  collateralUtxo = feeAndCollateral._2
                )
                // NoEvacuatees is recoverable (poll-then-retry), not a fatal build failure — peel
                // it off before delegating to the generic buildAndSubmit which raises on Left.
                _ <- evacBuilder.result match {
                    case Left(EvacuationTx.Build.Error.NoEvacuatees) =>
                        EitherT.leftT[IO, Unit](
                          Error.QueryError.NoEvacuateesRemaining: Error.RecoverableErrors
                        )
                    case other =>
                        buildAndSubmit(other, Error.BuildError.Evacuate(_))
                }
            } yield ()

        /** Query continuing treasury txs after the fallback and derive the current evacuation map
          * = (map at resolution time) − (keys already evacuated by past withdrawal txs).
          */
        def getEvacuationMap: EitherT[IO, Error.RecoverableErrors, EvacuationMap] =
            for {
                inputs <- EitherT.liftF[
                  IO,
                  Error.RecoverableErrors,
                  RuleBasedActor.EvacuationInputs
                ](loadEvacuationInputs)

                // The resolution tx is the last (oldest) tx after the fallback that spends the
                // treasury; the preceding entries are withdrawal transactions.
                treasuryTxs <- Backend.continuingTreasuryTxsAfter(inputs.fallbackTxHash)
                pastEvacuateRedeemers <- parsePastRedeemers(treasuryTxs)

                // The treasury's current `evacuationActive` commits to the REMAINING map after
                // past withdrawals; the original resolution-time map is committed by the oldest
                // treasury tx's output datum, which is what `candidateEvacMaps` is keyed by.
                resolutionKzg <- parseResolutionKzg(treasuryTxs)
                evacuationMapAtResolution <- lookupMap(resolutionKzg, inputs.candidateEvacMaps)

                previouslyEvacuated: Set[EvacuationKey] = pastEvacuateRedeemers.foldLeft(
                  Set.empty[EvacuationKey]
                )((acc, redeemer) => acc ++ redeemer.evacuationKeys.toScalaList)
            } yield evacuationMapAtResolution.removedAll(previouslyEvacuated)

        /** Parse the redeemers of past withdrawal transactions. The list's last entry is the
          * resolution tx (no evacuate redeemer); everything before is a withdrawal whose redeemer
          * names the keys evacuated by that tx.
          */
        def parsePastRedeemers(
            treasuryTxs: List[(TransactionHash, Data, Data)]
        ): EitherT[IO, Error.RecoverableErrors, List[EvacuateRedeemer]] =
            treasuryTxs.length match {
                // Backend hasn't surfaced the resolution tx yet — race vs the resolved datum we
                // already parsed, or a rollback. The recoverable Left propagates; no extra trace.
                case 0 =>
                    EitherT.leftT[IO, List[EvacuateRedeemer]](
                      Error.QueryError.NoTreasuryFound: Error.RecoverableErrors
                    )
                // Resolution only, no withdrawals processed yet.
                case 1 => pure(List.empty[EvacuateRedeemer])
                case _ =>
                    treasuryTxs.init.traverse { case (_, redeemerData, _) =>
                        Try(fromData[TreasuryRedeemer](redeemerData)) match {
                            case Failure(t) =>
                                raiseError(
                                  Error.ParseError.TreasuryEvacuationRedeemerParseError(t)
                                )
                            case Success(TreasuryRedeemer.Evacuate(r)) => pure(r)
                            case Success(_) =>
                                EitherT.leftT[IO, EvacuateRedeemer](
                                  Error.ParseError.TreasuryDeinitialized: Error.RecoverableErrors
                                )
                        }
                    }
            }

        /** Parse the resolution-time evacuation kzg from the oldest entry of `treasuryTxs` (the
          * resolution tx's output datum). This is the kzg the candidate maps are keyed by — it
          * differs from the current treasury's `evacuationActive` once any withdrawals have
          * occurred.
          */
        def parseResolutionKzg(
            treasuryTxs: List[(TransactionHash, Data, Data)]
        ): EitherT[IO, Error.RecoverableErrors, KzgCommitment] = {
            val resolutionTreasuryOutputDatum = treasuryTxs.last._3
            Try(
              fromData[TreasuryState.RuleBasedTreasuryDatumOnchain](
                resolutionTreasuryOutputDatum
              )
            ) match {
                case Failure(t) =>
                    raiseError(Error.ParseError.TreasuryResolveRedeemerParseError(t))
                case Success(r: TreasuryState.RuleBasedTreasuryDatumOnchain.ResolvedOnchain) =>
                    pure(r.evacuationActive)
                case Success(_) =>
                    raiseError(Error.ParseError.TreasuryNotResolved)
            }
        }

        /** Look up the evacuation-map preimage that the resolution committed to. */
        def lookupMap(
            kzg: KzgCommitment,
            candidateMaps: Map[KzgCommitment, EvacuationMap]
        ): EitherT[IO, Error.RecoverableErrors, EvacuationMap] =
            candidateMaps.get(kzg) match {
                case None       => raiseError(Error.UnknownResolvedKzg(kzg))
                case Some(eMap) => pure(eMap)
            }

        /** Query the evacuation wallet's utxos and derive collateral from the first one. */
        def getFeeAndCollateral: EitherT[IO, Error.RecoverableErrors, (Utxos, CollateralUtxo)] = {
            val walletAddress = config.ruleBasedWallet.exportVerificationKey.shelleyAddress()
            for {
                feeUtxos <- Backend.utxosAtFee(walletAddress)
                collateralUtxo <- feeUtxos.headOption match {
                    case None =>
                        traceLeft[CollateralUtxo](
                          RuleBasedActorEvent.Collateral.NotFound(walletAddress),
                          NoSuitableCollateralUtxosFound(walletAddress)
                        )
                    case Some((input, output)) =>
                        CollateralUtxo.parse(Utxo(input, output)) match {
                            case Right(c) => pure(c)
                            case Left(_) =>
                                traceLeft[CollateralUtxo](
                                  RuleBasedActorEvent.Collateral.NotFound(walletAddress),
                                  NoSuitableCollateralUtxosFound(walletAddress)
                                )
                        }
                }
            } yield (feeUtxos, collateralUtxo)
        }
    }

    /** Single tick: parse the treasury, dispatch on its datum subtype to the appropriate branch.
      * Returns the recoverable-Left vs Right for test introspection; the receive handler discards.
      */
    def handleTick: IO[Either[Error.RecoverableErrors, Unit]] = {
        val et: EitherT[IO, Error.RecoverableErrors, Unit] = for {
            treasuryUtxo <- getTreasury
            _ <- treasuryUtxo.treasuryOutput.datum match {
                case _: RuleBasedTreasuryDatum.Unresolved => Dispute.handle(treasuryUtxo)
                case _: RuleBasedTreasuryDatum.Resolved   => Evacuation.handle(treasuryUtxo)
            }
        } yield ()
        et.value
    }

    override def preStart: IO[Unit] =
        context.self ! Requests.PreStart

    private def preStartLocal: IO[Unit] =
        context.setReceiveTimeout(config.evacuationBotPollingPeriod, Tick)

    override def receive: Receive[IO, Requests.Request] = {
        case _: Requests.PreStart.type => preStartLocal
        case _: Requests.Tick.type     => handleTick.void
    }

    /** Queries the cardano backend for all utxos at the dispute resolution address, and then parses
      * them.
      *
      * Assumptions
      *   - We don't have any extra vote utxos that will validly parse, i.e., we don't check the
      *     number of utxos given to this function. This is an invariant of the system and needs to
      *     be established elsewhere
      *     - We assume the CardanoBackend is correctly implemented such that
      *       - We receive all the vote utxos that exist at the time of the query; we're not missing
      *         any.
      *       - Each utxo has a correct transaction input according to the results of the query.
      *       - Each utxo has the correct vote token in it and sits at the correct adddress.
      */
    /** Query the dispute address utxos, parse each into a [[BallotBox]], and classify the set into
      * the appropriate [[DisputeUtxos]] case. Traces each phase (Querying / Parsing / Parsed*).
      */
    /** Query and parse the dispute address's utxos into a flat list of `BallotBox[VoteStatus]`,
      * emitting the shared `Dispute.Parsing` trace. Head and coil paths classify on top.
      */
    private def parseDisputeUtxos
        : EitherT[IO, Error.RecoverableErrors, List[BallotBox[VoteStatus]]] =
        for {
            utxos <- Backend.utxosAtDispute
            _ <- traceRight(RuleBasedActorEvent.Dispute.Parsing)
            // TODO: Collect parsing errors
            parsed <- EitherT.liftF[IO, Error.RecoverableErrors, List[BallotBox[VoteStatus]]](
              IO.fromEither(utxos.toList.traverse((i, o) => BallotBox.parse(Utxo(i, o))))
            )
        } yield parsed

    private def getDisputeUtxos: EitherT[IO, Error.RecoverableErrors, DisputeUtxos] = {
        val ownPkhHash = config.ownWallet.exportVerificationKey.pubKeyHash.hash
        for {
            parsed <- parseDisputeUtxos
            ownAwaiting = parsed.collectFirst {
                case v if v.ballotBoxOutput.status match {
                        case AwaitingVote(peer) => peer.hash == ownPkhHash
                        case _                  => false
                    } =>
                    v.asInstanceOf[BallotBox[AwaitingVote]]
            }
            result <- ownAwaiting match {
                case Some(own) =>
                    traceRight(RuleBasedActorEvent.Dispute.ParsingCastVote)
                        .as(DisputeUtxos.CastVote(own): DisputeUtxos)
                case None =>
                    parsed match {
                        case Nil =>
                            traceRight(RuleBasedActorEvent.Dispute.ParsingEmptyVotes)
                                .as(DisputeUtxos.EmptyVotes: DisputeUtxos)
                        case x :: Nil =>
                            x.ballotBoxOutput.status match {
                                case _: Voted =>
                                    traceRight(RuleBasedActorEvent.Dispute.ParsingResolve)
                                        .as(
                                          DisputeUtxos.Resolve(
                                            x.asInstanceOf[BallotBox[Voted]]
                                          ): DisputeUtxos
                                        )
                                case _ => raiseError(Error.NonVotedUtxoAtResolve(x))
                            }
                        case xs =>
                            traceRight(RuleBasedActorEvent.Dispute.ParsingTally)
                                .as(DisputeUtxos.Tally(xs): DisputeUtxos)
                    }
            }
        } yield result
    }

}

object RuleBasedActor {

    /** The possible cases for (valid, parsed) utxos at the dispute resolution address.
      *
      * [[EmptyVotes]] should not be observed while the treasury is still unresolved (the spec
      * guarantees at least one vote utxo in that state). It is reachable in practice when the
      * dispute has already been resolved and the deinit transaction has consumed every vote utxo;
      * the caller is expected to detect that situation via the treasury parse and short-circuit
      * before matching here.
      */
    enum DisputeUtxos:
        case CastVote(ownVoteUtxo: BallotBox[AwaitingVote])
        case Tally(utxos: Seq[BallotBox[VoteStatus]])
        case Resolve(finalVoteUtxo: BallotBox[Voted])
        case EmptyVotes

    /** Per-tick rule-based inputs that cannot be recovered from chain queries:
      *   - `candidateEvacMaps` enumerates the evacuation maps peers may tally onto; their kzg
      *     commitment is on chain but the preimage (the actual L2 utxo map) is not.
      *   - `fallbackTxHash` anchors the `lastContinuingTxs` query that surfaces the resolution and
      *     subsequent withdrawal txs.
      */
    final case class EvacuationInputs(
        candidateEvacMaps: Map[KzgCommitment, EvacuationMap],
        fallbackTxHash: TransactionHash,
    )

    type Config = NodePrivateConfig.Section & HeadConfig.Section &
        NodeOperationEvacuationConfig.Section & CardanoNetwork.Section & HeadPeers.Section &
        HasTokenNames & ScriptReferenceUtxos.Section & OwnPeerPublic.Section &
        HeadConfig.Bootstrap.Section

    /** What action this peer's [[RuleBasedActor]] should take when it observes its own
      * `AwaitingVote` vote utxo on L1.
      *
      *   - [[Vote]]: a hard-confirmed stack carries a SEC whose `versionMajor` matches the on-chain
      *     treasury's — we have a signed SEC + peer header signatures, so the actor builds and
      *     submits a `VoteTx` that flips the datum to `Voted`.
      *   - [[Abstain]]: no such SEC exists (the walk backward through hard-confirmed stacks found
      *     nothing) — the actor publicly abstains via the on-chain Abstain branch and the dispute
      *     is tallied/resolved from there.
      */
    enum DisputeAction:
        case Vote(
            sec: StandaloneEvacuationCommitment.Onchain,
            signatures: List[BlockHeader.Minor.HeaderSignature],
            coilSignatures: List[Option[BlockHeader.Minor.HeaderSignature]]
        )
        case Abstain

    /** Raised when a loader can't reconstruct the rule-based state from persistence — e.g. no
      * hard-confirmed stack on disk, a stack's `HardConfirmation` / `UnsignedStack` /
      * `EvacuationMap` entry is absent, or a fallback tx is absent from a Regular stack.
      */
    final case class MissingState(message: String) extends RuntimeException(message)

    /** Walk the partitions in reverse and return the latest SEC whose `blockVersion.major` matches
      * `versionMajor`. Major's SEC is optional (only present when the partition has trailing
      * minors); Minor's SEC is mandatory. Both are candidates.
      */
    private def lastSecMatchingVersion(
        partitions: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]],
        versionMajor: BigInt
    ): Option[StandaloneEvacuationCommitment.MultiSigned] =
        partitions.toList.reverseIterator.collectFirst {
            case PartitionEffects.Minor(sec, _)
                if BigInt(sec.commitment.blockVersion.major: Int) == versionMajor =>
                sec
            case PartitionEffects.Major(_, _, _, _, Some(sec))
                if BigInt(sec.commitment.blockVersion.major: Int) == versionMajor =>
                sec
        }

    /** Every SEC carried by the partitions, in partition order — these are the candidates peers may
      * tally onto and the dispute resolution may settle on.
      */
    // TODO: Truncate this to the actual votable SECs -- anything lower than the default vote won't work
    private def allSecs(
        partitions: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]]
    ): List[StandaloneEvacuationCommitment.MultiSigned] =
        partitions.toList.flatMap {
            case PartitionEffects.Minor(sec, _)                => List(sec)
            case PartitionEffects.Major(_, _, _, _, Some(sec)) => List(sec)
            case _                                             => Nil
        }

    /** Walk the partitions in reverse and return the latest fallback tx. Only Major partitions
      * carry one (Treasury rotation happens via Major.settlement or Final.finalization).
      */
    private def lastFallback(
        partitions: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]]
    ): Option[FallbackTx] =
        partitions.toList.reverseIterator.collectFirst {
            case PartitionEffects.Major(_, fallback, _, _, _) => fallback
        }

    /** Decode the SEC's `Serialized` header bytes back into the on-chain datum form the dispute
      * resolution script consumes. The bytes are `serialiseData(Onchain.toData)` per
      * [[StandaloneEvacuationCommitment.Onchain.Serialized.apply]], so we round-trip through
      * `Data.fromCbor` + `fromData`.
      */
    private def toOnchain(
        commitment: StandaloneEvacuationCommitment
    ): StandaloneEvacuationCommitment.Onchain = {
        val bytes: Array[Byte] = commitment.header
        fromData[StandaloneEvacuationCommitment.Onchain](Data.fromCbor(bytes))
    }

    type Handle = ActorRef[IO, Requests.Request]

    object Requests {
        case object PreStart
        case object Tick
        type Request = Tick.type | PreStart.type
    }

    object Error {
        type RecoverableErrors = Recoverable
        sealed trait Recoverable
        case class RecoverableCardanoBackendError(
            wrapped: CardanoBackend.Error
        ) extends Recoverable

        type UnrecoverableErrors = Unrecoverable | Membership.MembershipCheckError
        sealed trait Unrecoverable extends Exception

        // Dispute side
        case object TreasuryUnresolvedButNoVotes extends Unrecoverable
        case class NoCompatibleVoteForTallyingFound(voteUtxos: Seq[BallotBox[VoteStatus]])
            extends Unrecoverable {
            override def getMessage: String =
                s"No compatible ballot box with key ${voteUtxos.head._2.link} found. Datums found: " +
                    s"${voteUtxos.map { _._2 }}"
        }
        // Treasury is unresolved, exactly one vote utxo remains, but its status isn't Voted.
        // The spec says the final tally output is always Voted — Abstain/AwaitingVote here
        // means the on-chain state has diverged from what we expect, so we escalate.
        case class NonVotedUtxoAtResolve(voteUtxo: BallotBox[VoteStatus]) extends Unrecoverable {
            override def getMessage: String =
                s"Single remaining vote utxo at resolve time is not Voted: $voteUtxo"
        }
        case class UnrecoverableCardanoBackendError(
            wrapped: CardanoBackend.Error
        ) extends Unrecoverable {
            override val getMessage: String = wrapped.getMessage
            override def toString: String = getMessage
        }
        final case class NoSuitableCollateralUtxosFound(address: ShelleyAddress) extends Recoverable

        // Evacuation side
        final case class UnknownResolvedKzg(resolvedKzg: KzgCommitment) extends Unrecoverable {
            override def getMessage: String =
                s"Resolved treasury commits to kzg $resolvedKzg, which is not in the candidate" +
                    " evacuation map set loaded at boot."
        }

        object QueryError {
            case object NoTreasuryFound extends Recoverable
            // Nothing left for us to evacuate. The actor stays alive to handle potential rollbacks
            // that would re-introduce work — so we keep polling rather than terminate.
            case object NoEvacuateesRemaining extends Recoverable
        }

        sealed trait BuildError extends Throwable
        object BuildError {
            case class Vote(wrapped: VoteTx.Build.Error) extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }

            case class RatchetVote(wrapped: RatchetVoteTx.Build.Error) extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }

            case class Tally(wrapped: TallyTx.Build.Error) extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }

            case class Resolution(wrapped: ResolutionTx.Build.Error) extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }

            case class Abstain(wrapped: AbstainTx.Build.Error) extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }

            case class Evacuate(wrapped: EvacuationTx.Build.Error) extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }
        }

        sealed trait ParseError
        object ParseError {
            object Treasury {
                case class WrappedTreasuryParseError(wrapped: RuleBasedTreasuryOutput.ParseError)
                    extends Unrecoverable

                /** This either means something is very wrong, or simply that the dispute resolution
                  * is over and the deinit transaction completed successfully.
                  */
                case object TreasuryMissing extends Recoverable
                case object TreasuryResolved extends Recoverable
            }

            case object TreasuryDeinitialized extends Recoverable

            case object TreasuryNotResolved extends Unrecoverable

            case class TreasuryEvacuationRedeemerParseError(wrapped: Throwable)
                extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }

            case class TreasuryResolveRedeemerParseError(wrapped: Throwable) extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }
        }
    }

}
