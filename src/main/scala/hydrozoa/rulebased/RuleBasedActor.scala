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
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant.realTimeQuantizedInstant
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.{pubKeyHash, shelleyAddress}
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.PeerId
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.commitment.Membership
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap}
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.tx.{EnrichedTx, FallbackTx, SettlementTx, TxFamily}
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
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{DatumOption, TransactionHash, TransactionInput, TransactionOutput, Utxo, Utxos}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.fromData

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

    // Handle to the self-tick fiber ([[startTickTimer]]); cancelled in [[postStop]] so it doesn't
    // outlive the actor.
    private val tickFiber = Ref.unsafe[IO, Option[Fiber[IO, Throwable, Nothing]]](None)

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

        def utxosAtRegime: EitherT[IO, Error.RecoverableErrors, Utxos] =
            traced(
              before = RuleBasedActorEvent.Regime.Querying,
              action = cardanoBackend.utxosAt(
                address = config.headMultisigAddress,
                asset = (
                  config.headMultisigScript.policyId,
                  config.headTokenNames.regimeWitnessTokenName
                )
              ),
              onError = RuleBasedActorEvent.Backend.ErrorRegimeUtxos(_)
            )

        def utxosAtPeer(addr: ShelleyAddress): EitherT[IO, Error.RecoverableErrors, Utxos] =
            traced(
              before = RuleBasedActorEvent.Collateral.Querying(addr),
              action = cardanoBackend.utxosAt(addr),
              onError = RuleBasedActorEvent.Backend.ErrorPeerUtxos(_)
            )

        def utxosAtFee(addr: ShelleyAddress): EitherT[IO, Error.RecoverableErrors, Utxos] =
            traced(
              before = RuleBasedActorEvent.Fee.Querying(addr),
              action = cardanoBackend.utxosAt(addr),
              onError = RuleBasedActorEvent.Backend.ErrorFeeUtxos(_)
            )

        def continuingTreasuryTxsAfter(
            after: TransactionHash
        ): EitherT[IO, Error.RecoverableErrors, List[CardanoBackend.ContinuingTx]] =
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

    /** Build, sign, and submit one rule-based tx.
      *
      * All rule-based txs are signed by `ownWallet` — the peer's consensus key. The dispute ballot
      * boxes are locked to it by the fallback tx, and both the dispute and evacuation collateral
      * sit at its address; signing with any other key yields `MissingVKeyWitness` at submission.
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
            res <- Backend.submit(config.ownWallet.signTx(tx))
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
              RuleBasedActorEvent.Treasury.Found(treasuryUtxo.treasuryOutput.value)
            )
            _ <- traceRight(RuleBasedActorEvent.Treasury.Parsing)
            _ <- treasuryUtxo.treasuryOutput.datum match {
                case _: RuleBasedTreasuryDatum.Unresolved =>
                    traceRight(RuleBasedActorEvent.Treasury.ParsedUnresolved)
                case _: RuleBasedTreasuryDatum.Resolved =>
                    traceRight(RuleBasedActorEvent.Treasury.ParsedResolved)
            }
        } yield treasuryUtxo

    /** Read the regime utxo (by HRWT beacon at the head multisig address) and parse it. The
      * rule-based txs reference it for the immutable head-identity fields. Missing or datum-less is
      * recoverable — the HRWT still sits in the datum-less multisig regime utxo until the fallback
      * tx lands (or it was rolled back); other parse failures throw.
      */
    private def getRegime: EitherT[IO, Error.RecoverableErrors, RuleBasedRegimeUtxo] = {
        val regimeMissing: EitherT[IO, Error.RecoverableErrors, RuleBasedRegimeUtxo] =
            traceRight(RuleBasedActorEvent.Regime.NotFound) >>
                EitherT.leftT[IO, RuleBasedRegimeUtxo](
                  Error.ParseError.Regime.RegimeMissing: Error.RecoverableErrors
                )
        for {
            utxos <- Backend.utxosAtRegime
            regimeUtxo <- utxos.toList match {
                case (i, o) :: Nil =>
                    RuleBasedRegimeUtxo.parse(Utxo(i, o)) match {
                        case Right(u) => pure(u)
                        case Left(_: RuleBasedRegimeOutput.ParseError.RegimeDatumMissing) =>
                            regimeMissing
                        case Left(e) =>
                            raiseError(Error.ParseError.Regime.WrappedRegimeParseError(e))
                    }
                case Nil => regimeMissing
                case _   => raiseError(Error.ParseError.Regime.MultipleRegimeUtxos(utxos))
            }
            _ <- traceRight(RuleBasedActorEvent.Regime.Found)
        } yield regimeUtxo
    }

    /** Determine this peer's dispute action, scoped to the on-chain treasury's `versionMajor`. The
      * dispute-resolution script pins `sec.versionMajor` to the treasury reference input's, so a
      * vote whose SEC came from a stack ahead of the on-chain settlement fails Plutus validation.
      *
      * Votes the latest votable SEC (the head of [[votableSecs]], which is latest-first). Returns
      * `Abstain` if there is none — the dispute is tallied/resolved from there.
      */
    private def loadAction(treasuryVersionMajor: BigInt): IO[DisputeAction] =
        votableSecs(treasuryVersionMajor).map {
            case Nil =>
                DisputeAction.Abstain
            case multiSec :: _ =>
                // `headerMultiSigned` is peer-position-aligned over
                // `allHeadPeers.sorted ++ allCoilPeers.sorted` (Some/None per peer). The first
                // `nHeadPeers` slots are the head peers (AllOf, always Some); the rest are the coil
                // peers in sorted order — exactly the sparse `coilMultisig` the dispute-resolution
                // script verifies position for position, so pass the coil tail through unchanged.
                val (head, coil) = multiSec.headerMultiSigned.splitAt(config.nHeadPeers.convert)
                DisputeAction.Vote(
                  sec = RuleBasedActor.toOnchain(multiSec.commitment),
                  signatures = head.flatten,
                  coilSignatures = coil
                )
        }

    /** The votable SEC set for `versionMajor`, latest-first: walk the hard-confirmed stacks
      * backward from `markers.hardConfirmed`, collecting every SEC whose `blockVersion.major`
      * matches, and stop as soon as a stack also carries an earlier-major SEC (the previous major
      * block — see [[secsAtVersion]]) or the Initial stack is reached. This is exactly the set
      * peers can tally onto and the dispute can resolve to, so both [[loadAction]] (which votes the
      * head) and [[loadEvacuationInputs]] (which must hold a preimage for every one) draw from it.
      */
    private def votableSecs(
        versionMajor: BigInt
    ): IO[List[StandaloneEvacuationCommitment.MultiSigned]] =
        for {
            markers <- Markers.derive(persistence.backend, config.ownPeerId)
            latest <- markers.hardConfirmed.liftTo[IO](
              MissingState("no hard-confirmed stack on disk")
            )
            secs <- Monad[IO].tailRecM[
              (StackNumber, List[StandaloneEvacuationCommitment.MultiSigned]),
              List[StandaloneEvacuationCommitment.MultiSigned]
            ]((latest, Nil)) { case (stack, acc) =>
                persistence.get(StoreKey.HardConfirmation(stack)).map(_.map(_.payload)).flatMap {
                    case None =>
                        IO.raiseError(MissingState(s"HardConfirmation($stack) missing"))
                    // Stack 0 is Initial by construction: no SEC-bearing partition. Terminate.
                    case Some(_: StackEffects.HardConfirmed.Initial) => IO.pure(Right(acc))
                    case Some(r: StackEffects.HardConfirmed.Regular) =>
                        val (matched, reachedPreviousMajor) =
                            RuleBasedActor.secsAtVersion(r.partitions, versionMajor)
                        val acc2 = acc ++ matched
                        // A Regular stack is always >= 1, so `decrement` never underflows.
                        if reachedPreviousMajor || stack == StackNumber.first then
                            IO.pure(Right(acc2))
                        else IO.pure(Left((stack.decrement, acc2)))
                }
            }
        } yield secs

    /** Re-read the evacuation-side rule-based inputs from persistence. Called on each tick that
      * runs the evacuation branch.
      *
      * The candidate map set ([[loadCandidateEvacMaps]]) and the fallback anchor
      * ([[loadFallbackAnchor]]) are derived independently. The maps cover exactly what the dispute
      * can resolve to — the base `(versionMajor, 0)` map plus every votable minor SEC map, the same
      * set voting ([[votableSecs]]/[[loadAction]]) draws from — so a fallback that fires before any
      * major settles (all Regular stacks minor-only back to the Initial stack) still holds a
      * preimage for a resolved minor SEC. The anchor walk only locates the fallback tx.
      */
    private[rulebased] def loadEvacuationInputs(versionMajor: BigInt): IO[EvacuationInputs] =
        for {
            markers <- Markers.derive(persistence.backend, config.ownPeerId)
            latest <- markers.hardConfirmed.liftTo[IO](
              MissingState("no hard-confirmed stack on disk")
            )
            candidateEvacMaps <- loadCandidateEvacMaps(versionMajor)
            fallbackTxHash <- loadFallbackAnchor(latest)
            _ <- tracer.traceWith(
              RuleBasedActorEvent.Evacuation.CandidateMaps(
                s"$latest",
                candidateEvacMaps.keySet
              )
            )
        } yield EvacuationInputs(candidateEvacMaps, fallbackTxHash)

    /** The full candidate evacuation-map set for `versionMajor` — the base `(versionMajor, 0)` map
      * the dispute resolves to on an all-abstain ([[defaultVoteMap]]), plus one entry per
      * **votable** minor SEC peers could ratchet onto ([[votableSecs]]), keyed by kzg. The
      * resolution writes whichever wins into the treasury's `Resolved.evacuationActive`, so this
      * set must cover the base plus the full votable set for no resolved commitment to be missing a
      * preimage.
      *
      * Derived the same way regardless of which stack carries the fallback anchor: [[votableSecs]]
      * walks back through the hard-confirmed stacks itself, so minor SECs are covered even when the
      * fallback fired from the Initial stack (before any major settled).
      */
    private def loadCandidateEvacMaps(
        versionMajor: BigInt
    ): IO[Map[KzgCommitment, EvacuationMap]] =
        for {
            base <- defaultVoteMap(versionMajor)
            (defaultKzg, defaultMap) = base
            votable <- votableSecs(versionMajor)
            secMaps <- votable.traverse { multiSec =>
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
            _ <- tracer.traceWith(
              RuleBasedActorEvent.Evacuation.CandidateMapSources(
                defaultMapBlock = s"base of settled major $versionMajor",
                defaultKzg = s"$defaultKzg",
                secs = votable.map(ms =>
                    s"block=${ms.commitment.blockNum} kzg=${ms.commitment.kzgCommitment}"
                )
              )
            )
        } yield ((defaultKzg -> defaultMap) +: secMaps).toMap

    /** The fallback tx that anchors the `continuingTreasuryTxsAfter` query. Walk backward through
      * hard-confirmed stacks until one carries a fallback (a Major partition); minor-only Regular
      * stacks accumulated after the last Major are skipped, and the Initial stack terminates the
      * walk with the initial fallback. A Regular stack is always >= 1, so `decrement` never
      * underflows. Traces [[RuleBasedActorEvent.Evacuation.EvacuationAnchor]] with the stack that
      * actually supplied the anchor.
      */
    private def loadFallbackAnchor(latest: StackNumber): IO[TransactionHash] =
        Monad[IO].tailRecM[StackNumber, TransactionHash](latest) { stack =>
            def anchoredAt(
                txId: TransactionHash,
                label: String
            ): IO[Either[StackNumber, TransactionHash]] =
                tracer
                    .traceWith(
                      RuleBasedActorEvent.Evacuation.EvacuationAnchor(label, txId)
                    )
                    .as(Right(txId))
            persistence.get(StoreKey.HardConfirmation(stack)).map(_.map(_.payload)).flatMap {
                case None =>
                    IO.raiseError(MissingState(s"HardConfirmation($stack) missing"))
                case Some(i: StackEffects.HardConfirmed.Initial) =>
                    anchoredAt(i.fallbackTx.tx.id, s"$stack (initial)")
                case Some(r: StackEffects.HardConfirmed.Regular) =>
                    RuleBasedActor.lastFallback(r.partitions) match {
                        case None             => IO.pure(Left(stack.decrement))
                        case Some(fallbackTx) => anchoredAt(fallbackTx.tx.id, s"$stack")
                    }
            }
        }

    /** The default-vote map for `versionMajor`: the base `(versionMajor, 0)` commitment the dispute
      * resolves to when every peer abstains (deposits, with no minor to ratchet onto). It is the
      * on-chain commitment of the LAST SETTLED major — carried by that major's settlement, not a
      * SEC and not the fallback stack (whose own settlement never reached L1, which is why its
      * fallback executed). Walk back to the stack whose settlement produced `versionMajor` and read
      * its base commitment (the settlement's kzg) with its map.
      *
      * Major 0 is the exception: it is the bootstrap (Initial) stack, whose base commitment is the
      * initial evacuation map itself — no `Major` partition ever produces major 0. Fallback that
      * fires before any Regular major settles (the first settlement dropped) resolves to this base.
      */
    private def defaultVoteMap(versionMajor: BigInt): IO[(KzgCommitment, EvacuationMap)] =
        if versionMajor == 0 then
            IO.pure(config.initialEvacuationMap.kzgCommitment -> config.initialEvacuationMap)
        else
            for {
                markers <- Markers.derive(persistence.backend, config.ownPeerId)
                latest <- markers.hardConfirmed.liftTo[IO](
                  MissingState("no hard-confirmed stack on disk")
                )
                result <- Monad[IO].tailRecM[StackNumber, (KzgCommitment, EvacuationMap)](latest) {
                    stack =>
                        persistence
                            .get(StoreKey.HardConfirmation(stack))
                            .map(_.map(_.payload))
                            .flatMap {
                                case None =>
                                    IO.raiseError(MissingState(s"HardConfirmation($stack) missing"))
                                case Some(_: StackEffects.HardConfirmed.Initial) =>
                                    IO.raiseError(
                                      MissingState(
                                        s"no settled major $versionMajor among hard-confirmed stacks"
                                      )
                                    )
                                case Some(r: StackEffects.HardConfirmed.Regular) =>
                                    RuleBasedActor.majorSettlementAt(
                                      r.partitions,
                                      versionMajor
                                    ) match {
                                        case None             => IO.pure(Left(stack.decrement))
                                        case Some(settlement) =>
                                            // TODO: `lastBlockNum` is the base `(major, 0)` block only when
                                            // the major has no trailing minors (the deposits-only case).
                                            // With trailing minors the base map lives at the earlier major
                                            // block; load that block's map (still keyed by the settlement's
                                            // kzg) instead.
                                            persistence
                                                .get(StoreKey.UnsignedStack(stack))
                                                .flatMap(
                                                  _.liftTo[IO](
                                                    MissingState(s"UnsignedStack($stack) missing")
                                                  )
                                                )
                                                .flatMap { unsignedStack =>
                                                    val blockNum = unsignedStack.brief.lastBlockNum
                                                    persistence
                                                        .get(StoreKey.EvacuationMap(blockNum))
                                                        .flatMap(
                                                          _.liftTo[IO](
                                                            MissingState(
                                                              s"EvacuationMap($blockNum) missing"
                                                            )
                                                          )
                                                        )
                                                        .map(map =>
                                                            Right(settlement.kzgCommitment -> map)
                                                        )
                                                }
                                    }
                            }
                }
            } yield result

    private object Dispute {

        /** Dispute branch — treasury is Unresolved. Head peers cast their reserved AwaitingVote
          * box; coil peers ratchet a public (Voted/Abstain) box forward with a multisigned SEC (see
          * [[handleCoil]]). Both peer types fall through to Tally / Resolve / EmptyVotes
          * classification when their peer-specific vote path is inapplicable.
          */
        def handle(
            treasuryUtxo: RuleBasedTreasuryUtxo,
            regimeUtxo: RuleBasedRegimeUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            config.ownPeerId match {
                case PeerId.Head(_) => handleHead(treasuryUtxo, regimeUtxo)
                case PeerId.Coil(_) => handleCoil(treasuryUtxo, regimeUtxo)
            }

        /** Head-peer dispute flow. Classifies the dispute address's utxos and dispatches to
          * [[castVote]] / [[tally]] / [[resolve]].
          */
        private def handleHead(
            treasuryUtxo: RuleBasedTreasuryUtxo,
            regimeUtxo: RuleBasedRegimeUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            for {
                disputeUtxos <- getDisputeUtxos
                _ <- disputeUtxos match {
                    // Treasury is Unresolved but the dispute address holds no vote utxos.
                    // This is a two-query race: the resolution tx (which spends both the Unresolved
                    // treasury and the ballot boxes) can land between utxosAtTreasury and
                    // utxosAtDispute. The next tick sees the post-resolution state consistently.
                    case DisputeUtxos.EmptyVotes =>
                        EitherT.leftT[IO, Unit](
                          Error.TreasuryUnresolvedButNoVotes: Error.RecoverableErrors
                        )
                    case DisputeUtxos.CastVote(ownBallotBox) =>
                        getCollateral.flatMap { collateralUtxo =>
                            hasDeadlineElapsed(treasuryUtxo).flatMap {
                                case true =>
                                    // Own VoteTx/AbstainTx can no longer land — dispatch to
                                    // residual tally/resolve with the full parsed set (own
                                    // AwaitingVote box included; tally handles it via
                                    // `gateOnVotingDeadline`).
                                    traceRight(RuleBasedActorEvent.Dispute.VotingDeadlineElapsed) >>
                                        parseDisputeUtxos.flatMap(
                                          dispatchResidual(
                                            _,
                                            treasuryUtxo,
                                            regimeUtxo,
                                            collateralUtxo
                                          )
                                        )
                                case false =>
                                    castVote(
                                      ownBallotBox,
                                      treasuryUtxo,
                                      regimeUtxo,
                                      collateralUtxo
                                    )
                            }
                        }
                    case DisputeUtxos.Tally(otherUtxos) =>
                        getCollateral.flatMap(tally(otherUtxos, treasuryUtxo, regimeUtxo, _))
                    case DisputeUtxos.Resolve(finalVoteUtxo) =>
                        getCollateral.flatMap(resolve(finalVoteUtxo, treasuryUtxo, regimeUtxo, _))
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
            treasuryUtxo: RuleBasedTreasuryUtxo,
            regimeUtxo: RuleBasedRegimeUtxo
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
                            hasDeadlineElapsed(treasuryUtxo).flatMap {
                                case true =>
                                    // RatchetVoteTx's validity range ends at the deadline —
                                    // skip and let the residual tally path drive resolution.
                                    traceRight(
                                      RuleBasedActorEvent.Dispute.VotingDeadlineElapsed
                                    ) >> dispatchResidual(
                                      parsed,
                                      treasuryUtxo,
                                      regimeUtxo,
                                      collateralUtxo
                                    )
                                case false =>
                                    pickRatchetTarget(parsed, v.sec.versionMinor) match {
                                        case Some(openBox) =>
                                            traceRight(
                                              RuleBasedActorEvent.Dispute.Coil.ParsingRatchet
                                            ) >> buildAndSubmit(
                                              result = RatchetVoteTx
                                                  .Build(
                                                    openBallotBox = openBox,
                                                    treasuryUtxo = treasuryUtxo,
                                                    regimeUtxo = regimeUtxo,
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
                                            ) >> dispatchResidual(
                                              parsed,
                                              treasuryUtxo,
                                              regimeUtxo,
                                              collateralUtxo
                                            )
                                    }
                            }
                    case DisputeAction.Abstain =>
                        traceRight(RuleBasedActorEvent.Dispute.Coil.NoRatchetTarget) >>
                            dispatchResidual(parsed, treasuryUtxo, regimeUtxo, collateralUtxo)
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
            // BallotBox is invariant in its Status parameter, so the narrowed type has to be
            // recovered by cast — the match itself is the check that keeps it sound.
            val openBoxes: List[(BallotBox[Voted | Abstain.type], BigInt)] = parsed.flatMap { bb =>
                bb.ballotBoxOutput.status match {
                    case v: Voted =>
                        List((bb.asInstanceOf[BallotBox[Voted | Abstain.type]], v.versionMinor))
                    case Abstain =>
                        List((bb.asInstanceOf[BallotBox[Voted | Abstain.type]], BigInt(0)))
                    case _: AwaitingVote => Nil
                }
            }
            openBoxes.filter(_._2 < targetVersionMinor).minByOption(_._2).map(_._1)
        }

        /** Residual dispatch shared by head fallthrough (own box absent) and coil fallthrough (no
          * ratchet target or no SEC): Tally / Resolve / EmptyVotes.
          */
        private def dispatchResidual(
            parsed: List[BallotBox[VoteStatus]],
            treasuryUtxo: RuleBasedTreasuryUtxo,
            regimeUtxo: RuleBasedRegimeUtxo,
            collateralUtxo: CollateralUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            classifyResidual(parsed).flatMap {
                case DisputeUtxos.EmptyVotes =>
                    EitherT.leftT[IO, Unit](
                      Error.TreasuryUnresolvedButNoVotes: Error.RecoverableErrors
                    )
                case DisputeUtxos.Resolve(finalVoteUtxo) =>
                    resolve(finalVoteUtxo, treasuryUtxo, regimeUtxo, collateralUtxo)
                case DisputeUtxos.Tally(utxos) =>
                    tally(utxos, treasuryUtxo, regimeUtxo, collateralUtxo)
                case DisputeUtxos.CastVote(_) =>
                    raiseError(
                      new IllegalStateException("classifyResidual never returns CastVote")
                    )
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
                case RuleBasedTreasuryDatum.Unresolved(_, _, v) => pure(v)
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
            collateralAt(Backend.utxosAtPeer(peerAddr), peerAddr)
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
            regimeUtxo: RuleBasedRegimeUtxo,
            collateralUtxo: CollateralUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            for {
                treasuryVersionMajor <- extractTreasuryVersionMajor(treasuryUtxo)
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
                                regimeUtxo = regimeUtxo,
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

        /** Merge two ballot boxes via TallyTx. The continuing input must have a key strictly less
          * than the removed input, so we sort. We always defer until the voting deadline elapses: a
          * public `Voted` box can be ratcheted by anyone until then, so no ballot is final
          * pre-deadline, and the TallyTx's validity range starts at the deadline — submitting
          * earlier just parks doomed txs in the mempool and spams the backend.
          */
        def tally(
            otherUtxos: NonEmptyList[BallotBox[VoteStatus]],
            treasuryUtxo: RuleBasedTreasuryUtxo,
            regimeUtxo: RuleBasedRegimeUtxo,
            collateralUtxo: CollateralUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] = {
            val keySorted = otherUtxos.sortBy(_.ballotBoxOutput.key)
            val continuing = keySorted.head
            for {
                _ <- gateOnVotingDeadline(treasuryUtxo)
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
                        regimeUtxo = regimeUtxo,
                        collateralUtxo = collateralUtxo
                      )
                      .result,
                  wrapError = Error.BuildError.Tally(_)
                )
            } yield ()
        }

        /** Wall-clock predicate: has the treasury's voting deadline elapsed? Used by [[handleHead]]
          * / [[handleCoil]] to short-circuit the own-vote / ratchet path once the corresponding tx
          * can no longer land on-chain (VoteTx / RatchetVoteTx / AbstainTx have validity ranges
          * bounded by the deadline).
          */
        private def hasDeadlineElapsed(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): EitherT[IO, Error.RecoverableErrors, Boolean] =
            for {
                deadline <- treasuryUtxo.parseVotingDeadline match {
                    case Right(s) => pure(s)
                    case Left(e) =>
                        raiseError(Error.ParseError.Treasury.WrappedTreasuryParseError(e))
                }
                now <- EitherT.right[Error.RecoverableErrors](
                  realTimeQuantizedInstant(config.slotConfig)
                )
            } yield now.toSlot.slot > deadline.slot

        /** Recoverable Left when wall-clock time hasn't crossed the treasury's voting deadline;
          * Right otherwise. `parseVotingDeadline` failure escalates as an unrecoverable spec
          * violation (we only reach here on an Unresolved treasury).
          */
        private def gateOnVotingDeadline(
            treasuryUtxo: RuleBasedTreasuryUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            hasDeadlineElapsed(treasuryUtxo).flatMap {
                case true => pure(())
                case false =>
                    traceRight(RuleBasedActorEvent.Dispute.WaitingForVotingDeadline) >>
                        EitherT.leftT[IO, Unit](
                          Error.TallyBlockedBeforeDeadline: Error.RecoverableErrors
                        )
            }

        /** Branch: only a single Voted ballot box remains. Submit ResolutionTx to flip the treasury
          * datum to `Resolved`.
          */
        def resolve(
            finalVoteUtxo: BallotBox[Voted],
            treasuryUtxo: RuleBasedTreasuryUtxo,
            regimeUtxo: RuleBasedRegimeUtxo,
            collateralUtxo: CollateralUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            buildAndSubmit(
              result = ResolutionTx
                  .Build(
                    talliedBallotBox = finalVoteUtxo,
                    treasuryUtxo = treasuryUtxo,
                    regimeUtxo = regimeUtxo,
                    collateralUtxo = collateralUtxo
                  )
                  .result,
              wrapError = Error.BuildError.Resolution(_)
            )
    }

    private object Evacuation {

        /** Evacuation branch — treasury is Resolved. From a single `continuingTreasuryTxsAfter`
          * read it derives both the current treasury utxo and the remaining evacuation map (see
          * [[loadEvacuationState]]), then builds + submits an [[EvacuationTx]] for whatever is
          * left.
          */
        def handle(
            regimeUtxo: RuleBasedRegimeUtxo,
            versionMajor: BigInt
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            for {
                state <- loadEvacuationState(versionMajor)
                (treasuryUtxo, toEvacuate) = state
                _ <-
                    if toEvacuate.isEmpty
                    then
                        traceLeft[Unit](
                          RuleBasedActorEvent.Evacuation.NoMore,
                          Error.QueryError.NoEvacuateesRemaining
                        )
                    else traceRight(RuleBasedActorEvent.Evacuation.PayoutsLeft(toEvacuate.size))

                collateralUtxo <- getEvacuationCollateral
                evacBuilder = EvacuationTx.Build(
                  inputTreasuryUtxo = treasuryUtxo,
                  regimeUtxo = regimeUtxo,
                  evacuateesToTryNext = toEvacuate,
                  allRemainingEvacuatees = toEvacuate,
                  collateralUtxo = collateralUtxo
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

        /** From a single `continuingTreasuryTxsAfter` read, derive both the current treasury utxo
          * and the remaining evacuation map = (map at resolution time) − (keys already evacuated by
          * past withdrawal txs).
          *
          * Deriving both from the *same* read is deliberate. The current treasury utxo is the
          * continuing output of the newest tx in the chain, so reading it from the utxo set
          * separately (as [[getTreasury]] does for the dispatch) races the chain history on an
          * eventually-consistent backend: the two projections lag independently, and a treasury
          * utxo that disagrees with the map/history produces an [[EvacuationTx]] the ledger rejects
          * at script evaluation.
          */
        def loadEvacuationState(
            versionMajor: BigInt
        ): EitherT[IO, Error.RecoverableErrors, (RuleBasedTreasuryUtxo, EvacuationMap)] =
            for {
                inputs <- EitherT.liftF[
                  IO,
                  Error.RecoverableErrors,
                  RuleBasedActor.EvacuationInputs
                ](loadEvacuationInputs(versionMajor))

                // The resolution tx is the last (oldest) tx after the fallback that spends the
                // treasury; the preceding entries are withdrawal transactions, and the first
                // (newest) entry's continuing output is the current treasury utxo.
                // `parsePastRedeemers` gates non-empty, parses each withdrawal's redeemer, and
                // extracts the resolution-time kzg from the oldest datum in one pass.
                treasuryTxs <- Backend.continuingTreasuryTxsAfter(inputs.fallbackTxHash)
                // Trace the surfaced chain length: `currentTreasury`/`parsePastRedeemers` turn an
                // empty list into a silent `NoTreasuryFound` recoverable, so without this an
                // evacuation stalled waiting for the resolution tx to surface is invisible.
                _ <- EitherT.right[Error.RecoverableErrors](
                  tracer.traceWith(
                    RuleBasedActorEvent.Evacuation.ContinuingTreasuryTxs(treasuryTxs.size)
                  )
                )
                treasuryUtxo <- currentTreasury(treasuryTxs)
                parsed <- parsePastRedeemers(treasuryTxs)
                (pastEvacuateRedeemers, resolutionKzg) = parsed

                // The treasury's current `evacuationActive` commits to the REMAINING map after
                // past withdrawals; the original resolution-time map is committed by the oldest
                // treasury tx's output datum, which is what `candidateEvacMaps` is keyed by.
                _ <- EitherT.right[Error.RecoverableErrors](
                  tracer.traceWith(RuleBasedActorEvent.Evacuation.ResolvedKzg(resolutionKzg))
                )
                evacuationMapAtResolution <- lookupMap(resolutionKzg, inputs.candidateEvacMaps)

                previouslyEvacuated: Set[EvacuationKey] = pastEvacuateRedeemers.foldLeft(
                  Set.empty[EvacuationKey]
                )((acc, redeemer) => acc ++ redeemer.evacuationKeys.toScalaList)
            } yield (treasuryUtxo, evacuationMapAtResolution.removedAll(previouslyEvacuated))

        /** The current treasury utxo = the continuing output of the newest tx in the chain.
          * Deriving it here (rather than a separate utxo-set read) keeps it coherent with the
          * evacuation map derived from the same list. An empty list means the backend hasn't
          * surfaced the resolution tx yet (race vs the resolved datum we dispatched on, or a
          * rollback) — recoverable. A parse failure means the on-chain continuing output diverged
          * from the treasury spec — fatal, matching [[getTreasury]].
          */
        private def currentTreasury(
            treasuryTxs: List[CardanoBackend.ContinuingTx]
        ): EitherT[IO, Error.RecoverableErrors, RuleBasedTreasuryUtxo] =
            treasuryTxs.headOption match {
                case None =>
                    EitherT.leftT[IO, RuleBasedTreasuryUtxo](
                      Error.QueryError.NoTreasuryFound: Error.RecoverableErrors
                    )
                case Some(newest) =>
                    RuleBasedTreasuryUtxo.parse(newest.continuingOutput) match {
                        case Right(u) => pure(u)
                        case Left(e)  => raiseError(e)
                    }
            }

        /** Parse the past withdrawal redeemers and the resolution-time evacuation kzg from
          * `treasuryTxs`. The list's last (oldest) entry is the resolution tx: no evacuate
          * redeemer, and its output datum commits to the kzg the candidate maps are keyed by.
          * Everything before is a withdrawal whose redeemer names the keys evacuated by that tx. An
          * empty list means the backend hasn't surfaced the resolution tx yet (race vs the resolved
          * datum we already parsed, or a rollback) — recoverable.
          */
        def parsePastRedeemers(
            treasuryTxs: List[CardanoBackend.ContinuingTx]
        ): EitherT[IO, Error.RecoverableErrors, (List[EvacuateRedeemer], KzgCommitment)] =
            NonEmptyList.fromList(treasuryTxs) match {
                case None =>
                    EitherT.leftT[IO, (List[EvacuateRedeemer], KzgCommitment)](
                      Error.QueryError.NoTreasuryFound: Error.RecoverableErrors
                    )
                case Some(nel) =>
                    val withdrawalTxs = nel.init
                    for {
                        resolutionDatum <- inlineDatumOf(nel.last.continuingOutput.output)
                        redeemers <- withdrawalTxs.traverse { ct =>
                            Try(fromData[TreasuryRedeemer](ct.spendingRedeemer)) match {
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
                        kzg <- Try(
                          fromData[TreasuryState.RuleBasedTreasuryDatum](resolutionDatum)
                        ) match {
                            case Failure(t) =>
                                raiseError(Error.ParseError.TreasuryResolveRedeemerParseError(t))
                            case Success(
                                  r: TreasuryState.RuleBasedTreasuryDatum.Resolved
                                ) =>
                                pure(r.evacuationActive)
                            case Success(_) =>
                                raiseError(Error.ParseError.TreasuryNotResolved)
                        }
                    } yield (redeemers, kzg)
            }

        /** Extract the inline datum from a continuing output. A non-inline or absent datum means
          * the backend surfaced a malformed continuing output — recoverable (retry).
          */
        private def inlineDatumOf(
            output: TransactionOutput
        ): EitherT[IO, Error.RecoverableErrors, Data] =
            output.datumOption match {
                case Some(DatumOption.Inline(d)) => pure(d)
                case _ =>
                    EitherT.leftT[IO, Data](
                      Error.QueryError.NoTreasuryFound: Error.RecoverableErrors
                    )
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

        /** Find an ada-only collateral utxo at the own wallet's address (the rule-based bot funds
          * and signs every tx with the peer's consensus key).
          */
        def getEvacuationCollateral: EitherT[IO, Error.RecoverableErrors, CollateralUtxo] = {
            val walletAddress = config.ownWallet.exportVerificationKey.shelleyAddress()
            collateralAt(Backend.utxosAtFee(walletAddress), walletAddress)
        }
    }

    /** Query `addr` for candidate collateral utxos, pick the ada-only one with the most coin, and
      * parse it as a [[CollateralUtxo]]. `query` carries the address-specific tracing and
      * error-wrapping (peer vs fee); `addr` is only used for the `NotFound` trace.
      */
    private def collateralAt(
        query: EitherT[IO, Error.RecoverableErrors, Utxos],
        addr: ShelleyAddress
    ): EitherT[IO, Error.RecoverableErrors, CollateralUtxo] =
        for {
            candidates <- query
            best <- candidates.filter((_, o) => o.value.isOnlyAda) match {
                case adaOnly if adaOnly.nonEmpty =>
                    pure(adaOnly.toList.maxBy(_._2.value.coin.value))
                case _ =>
                    traceLeft[(TransactionInput, TransactionOutput)](
                      RuleBasedActorEvent.Collateral.NotFound(addr),
                      NoSuitableCollateralUtxosFound(addr)
                    )
            }
            collateralUtxo <- CollateralUtxo.parse(Utxo(best._1, best._2)) match {
                case Right(c) => pure(c)
                case Left(_) =>
                    traceLeft[CollateralUtxo](
                      RuleBasedActorEvent.Collateral.NotFound(addr),
                      NoSuitableCollateralUtxosFound(addr)
                    )
            }
            _ <- traceRight(RuleBasedActorEvent.Collateral.Found)
        } yield collateralUtxo

    /** Single tick: parse the treasury, dispatch on its datum subtype to the appropriate branch.
      * Returns the recoverable-Left vs Right for test introspection; the receive handler discards.
      */
    def handleTick: IO[Either[Error.RecoverableErrors, Unit]] = {
        val et: EitherT[IO, Error.RecoverableErrors, Unit] = for {
            treasuryUtxo <- getTreasury
            _ <- treasuryUtxo.treasuryOutput.datum match {
                case _: RuleBasedTreasuryDatum.Unresolved =>
                    getRegime.flatMap(Dispute.handle(treasuryUtxo, _))
                case resolved: RuleBasedTreasuryDatum.Resolved =>
                    // The dispute pins `sec.versionMajor` to the treasury's major, so evacuation's
                    // votable candidate set is scoped to `resolved.version._1`.
                    getRegime.flatMap(Evacuation.handle(_, resolved.version._1))
            }
        } yield ()
        // The tick swallows recoverable errors and retries next tick; trace the Left (at debug) so
        // that retry is never fully silent. Backend errors already self-trace via `Backend.run`;
        // this catches the logical recoverables (e.g. `NoTreasuryFound`) that don't.
        et.value.flatTap {
            case Left(e)  => tracer.traceWith(RuleBasedActorEvent.Tick.RecoverableRetry(e.toString))
            case Right(_) => IO.unit
        }
    }

    // preStart runs in the actor's lifecycle callback, outside the receive loop; posting a
    // PreStart message defers initialization (`preStartLocal`) into the receive loop, matching
    // the codebase-wide idiom used across the multisig actors.
    override def preStart: IO[Unit] =
        context.self ! Requests.PreStart

    private def preStartLocal: IO[Unit] =
        startTickTimer

    // Drive the poll loop from a side fiber rather than `context.setReceiveTimeout`: the latter is
    // implemented by cats-actors with a hardcoded ~1s internal ping keyed off
    // `System.currentTimeMillis`, and under CPU starvation the `Tick` can silently stop firing — the
    // evacuation regime then spawns but never polls, so funds never come back (custody-critical). A
    // fixed-cadence self-tick fires every `evacuationBotPollingPeriod` regardless of mailbox
    // activity, which is correct here because `handleTick` is a poll-then-retry safe to re-run.
    private def startTickTimer: IO[Unit] =
        val pollLoop =
            (IO.sleep(config.evacuationBotPollingPeriod) >> (context.self ! Tick)).foreverM
        for
            fib <- pollLoop.start
            previous <- tickFiber.getAndSet(Some(fib))
            _ <- previous.fold(IO.unit)(_.cancel)
        yield ()

    /** Cancel the self-tick fiber so it stops pinging `self` once the actor has stopped, instead of
      * leaking a fiber that keeps delivering `Tick` to a dead actor (dead letters).
      */
    override def postStop: IO[Unit] =
        tickFiber.getAndSet(None).flatMap(_.fold(IO.unit)(_.cancel))

    override def receive: Receive[IO, Requests.Request] = {
        case _: Requests.PreStart.type => preStartLocal
        case _: Requests.Tick.type     => handleTick.void
    }

    /** Query and parse the dispute address's utxos into a flat list of `BallotBox[VoteStatus]`,
      * emitting the shared `Dispute.Parsing` trace. Head and coil paths classify on top.
      *
      * Assumes the backend returns every vote utxo that exists at query time (each with the correct
      * input, vote token, and address); parse failures raise via `IO.fromEither`.
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
                case None => classifyResidual(parsed)
            }
        } yield result
    }

    /** Classify the residual vote utxos (no own `AwaitingVote` box in play) into the terminal
      * dispute states, emitting the matching `Parsing*` trace. Shared by [[getDisputeUtxos]] (head
      * pre-vote classification) and [[Dispute.dispatchResidual]] (head/coil fallthrough); never
      * returns [[DisputeUtxos.CastVote]].
      */
    private def classifyResidual(
        parsed: List[BallotBox[VoteStatus]]
    ): EitherT[IO, Error.RecoverableErrors, DisputeUtxos] =
        parsed match {
            case Nil =>
                traceRight(RuleBasedActorEvent.Dispute.ParsingEmptyVotes)
                    .as(DisputeUtxos.EmptyVotes: DisputeUtxos)
            case x :: Nil =>
                x.ballotBoxOutput.status match {
                    case _: Voted =>
                        traceRight(RuleBasedActorEvent.Dispute.ParsingResolve)
                            .as(
                              DisputeUtxos.Resolve(x.asInstanceOf[BallotBox[Voted]]): DisputeUtxos
                            )
                    case _ =>
                        EitherT.leftT[IO, DisputeUtxos](
                          Error.NonVotedUtxoAtResolve(x): Error.RecoverableErrors
                        )
                }
            case x :: y :: rest =>
                traceRight(RuleBasedActorEvent.Dispute.ParsingTally)
                    .as(DisputeUtxos.Tally(NonEmptyList(x, y :: rest)): DisputeUtxos)
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
        case Tally(utxos: NonEmptyList[BallotBox[VoteStatus]])
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

    /** The SECs a stack's partitions carry for `versionMajor` (latest-first), and whether the stack
      * also carries an **earlier**-major SEC — the signal that the votable range ends at this stack
      * (the previous major block). A Major's SEC is optional (present only with trailing minors); a
      * Minor's is mandatory; both count. Shared by [[votableSecs]], which both voting
      * ([[loadAction]]) and evacuation ([[loadEvacuationInputs]]) draw from, so their SEC selection
      * can never diverge.
      */
    private def secsAtVersion(
        partitions: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]],
        versionMajor: BigInt
    ): (List[StandaloneEvacuationCommitment.MultiSigned], Boolean) =
        def major(sec: StandaloneEvacuationCommitment.MultiSigned): BigInt =
            BigInt(sec.commitment.blockVersion.major: Int)
        val secs = partitions.toList.reverse.flatMap {
            case PartitionEffects.Minor(sec, _)                => List(sec)
            case PartitionEffects.Major(_, _, _, _, Some(sec)) => List(sec)
            case _                                             => Nil
        }
        (secs.filter(major(_) == versionMajor), secs.exists(major(_) < versionMajor))

    /** The settlement of the Major partition that produced `versionMajor`, if this stack carries
      * it. Its `kzgCommitment` is the base `(versionMajor, 0)` commitment (the on-chain default
      * vote) — see [[defaultVoteMap]].
      */
    private def majorSettlementAt(
        partitions: NonEmptyList[PartitionEffects[StandaloneEvacuationCommitment.MultiSigned]],
        versionMajor: BigInt
    ): Option[SettlementTx] =
        partitions.toList.collectFirst {
            case PartitionEffects.Major(settlement, _, _, _, _)
                if BigInt(settlement.majorVersionProduced: Int) == versionMajor =>
                settlement
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

        // Tally deferred: the voting deadline hasn't elapsed. A public Voted box can still be
        // ratcheted by anyone until the deadline, and the TallyTx's validity range starts there,
        // so we wait rather than submit future-validity txs that just park in the mempool.
        case object TallyBlockedBeforeDeadline extends Recoverable

        type UnrecoverableErrors = Unrecoverable | Membership.MembershipCheckError
        sealed trait Unrecoverable extends Exception

        // Dispute side
        case object TreasuryUnresolvedButNoVotes extends Recoverable
        case class NoCompatibleVoteForTallyingFound(
            voteUtxos: NonEmptyList[BallotBox[VoteStatus]]
        ) extends Unrecoverable {
            override def getMessage: String =
                s"No compatible ballot box with key ${voteUtxos.head.ballotBoxOutput.link} " +
                    s"found. Datums found: ${voteUtxos.map(_.ballotBoxOutput).toList}"
        }
        // Treasury is unresolved, exactly one vote utxo remains, but its status isn't Voted.
        // On a coil peer running before the deadline this is legitimate — the head peer just
        // hasn't voted yet. Poll and retry.
        case class NonVotedUtxoAtResolve(voteUtxo: BallotBox[VoteStatus]) extends Recoverable
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

        /** Namespace for the per-tx build failures. Each case is an [[Unrecoverable]] in its own
          * right; there is deliberately no `BuildError` supertype, because nothing dispatches on
          * "was a build failure" as a category.
          */
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
            }

            object Regime {
                case class WrappedRegimeParseError(wrapped: RuleBasedRegimeOutput.ParseError)
                    extends Unrecoverable

                /** Recoverable: the fallback tx may not be visible yet, or a rollback removed it.
                  */
                case object RegimeMissing extends Recoverable
                case class MultipleRegimeUtxos(utxos: Utxos) extends Unrecoverable
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
