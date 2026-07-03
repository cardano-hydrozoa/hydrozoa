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
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.commitment.Membership
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap}
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.multisig.ledger.l1.tx.{EnrichedTx, TxFamily}
import hydrozoa.rulebased.RuleBasedActor.Error.NoSuitableCollateralUtxosFound
import hydrozoa.rulebased.RuleBasedActor.Requests.Tick
import hydrozoa.rulebased.RuleBasedActor.{Error, *}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.{AwaitingVote, Voted}
import hydrozoa.rulebased.ledger.l1.tx.*
import hydrozoa.rulebased.ledger.l1.utxo.*
import scala.util.{Failure, Success, Try}
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.{Transaction, TransactionHash, TransactionOutput, Utxo, Utxos}
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
  * The actor holds no per-iteration state: every tick re-queries the chain and re-calls the loader
  * thunks ([[loadAction]], [[loadEvacuationInputs]]) for the pieces that can't be recovered from
  * chain (the peer's SEC + signatures, the candidate evacuation maps, the fallback anchor). Loaders
  * are expected to be idempotent (typically a persistence read).
  *
  * Erroring semantics:
  *   - Cardano backend query/submit failures are swallowed and retried.
  *   - Parsing failures behind a token guard throw — the on-chain state has diverged from the spec.
  *   - Tx-builder failures throw — all builder inputs are validated upstream.
  *   - Multiple treasury utxos with the treasury token throw.
  */
final case class RuleBasedActor(
    loadAction: IO[RuleBasedRegimeManager.DisputeAction],
    loadEvacuationInputs: IO[RuleBasedActor.EvacuationInputs],
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
              before = RuleBasedActorEvent.Collateral.Querying(addr.toString),
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

    private object Dispute {

        /** Dispute branch — treasury is Unresolved. Queries the dispute address, gets collateral,
          * dispatches to one of [[castVote]] / [[tally]] / [[resolve]].
          */
        def handle(
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
                    case _ => raiseError(NoSuitableCollateralUtxosFound)
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
                        EitherT.liftF(
                          tracer
                              .traceWith(
                                RuleBasedActorEvent.Collateral.NotFound(config.ownPeerLabel)
                              )
                              .flatMap(_ => IO.raiseError(NoSuitableCollateralUtxosFound))
                        )
                }
                _ <- traceRight(RuleBasedActorEvent.Collateral.Found)
            } yield CollateralUtxo(collateralUtxoTuple._1, collateralOutput)
        }

        /** Branch: own ballot box still `AwaitingVote`. Load this peer's action (Vote/Abstain) and
          * submit the corresponding tx.
          */
        def castVote(
            ownBallotBox: BallotBox[AwaitingVote],
            treasuryUtxo: RuleBasedTreasuryUtxo,
            collateralUtxo: CollateralUtxo
        ): EitherT[IO, Error.RecoverableErrors, Unit] =
            for {
                action <- EitherT.liftF[
                  IO,
                  Error.RecoverableErrors,
                  RuleBasedRegimeManager.DisputeAction
                ](loadAction)
                _ <- action match {
                    case RuleBasedRegimeManager.DisputeAction.Vote(
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
                    case RuleBasedRegimeManager.DisputeAction.Abstain =>
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
                        EitherT.left[Unit](
                          tracer.traceWith(RuleBasedActorEvent.Evacuation.NoMore) >>
                              IO.pure(
                                Error.QueryError.NoEvacuateesRemaining: Error.RecoverableErrors
                              )
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
                        EitherT.left[CollateralUtxo](
                          tracer.traceWith(
                            RuleBasedActorEvent.Collateral.NoFeeCollateralUtxo
                          ) >>
                              IO.pure(
                                Error.QueryError.NoTreasuryFound: Error.RecoverableErrors
                              )
                        )
                    case Some((input, output)) =>
                        EitherT.fromEither[IO](
                          CollateralUtxo
                              .parse(Utxo(input, output))
                              .left
                              .map(_ => Error.QueryError.NoTreasuryFound: Error.RecoverableErrors)
                        )
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
    private def getDisputeUtxos: EitherT[IO, Error.RecoverableErrors, DisputeUtxos] = {
        val ownPkhHash = config.ownWallet.exportVerificationKey.pubKeyHash.hash
        for {
            utxos <- Backend.utxosAtDispute
            _ <- traceRight(RuleBasedActorEvent.Dispute.Parsing)
            // TODO: Collect parsing errors
            parsed <- EitherT.liftF[IO, Error.RecoverableErrors, List[BallotBox[VoteStatus]]](
              IO.fromEither(utxos.toList.traverse((i, o) => BallotBox.parse(Utxo(i, o))))
            )
            ownAwaiting = parsed.collectFirst {
                case v if v.ballotBoxOutput.status match {
                        case AwaitingVote(peer) => peer.hash == ownPkhHash
                        case _                  => false
                    } =>
                    v.asInstanceOf[BallotBox[AwaitingVote]]
            }
            result <- ownAwaiting match {
                case Some(own) =>
                    traceRight(RuleBasedActorEvent.Dispute.ParsedCastVote)
                        .as(DisputeUtxos.CastVote(own): DisputeUtxos)
                case None =>
                    parsed match {
                        case Nil =>
                            traceRight(RuleBasedActorEvent.Dispute.ParsedEmptyVotes)
                                .as(DisputeUtxos.EmptyVotes: DisputeUtxos)
                        case x :: Nil =>
                            x.ballotBoxOutput.status match {
                                case _: Voted =>
                                    traceRight(RuleBasedActorEvent.Dispute.ParsedResolve)
                                        .as(
                                          DisputeUtxos.Resolve(
                                            x.asInstanceOf[BallotBox[Voted]]
                                          ): DisputeUtxos
                                        )
                                case _ => raiseError(Error.NonVotedUtxoAtResolve(x))
                            }
                        case xs =>
                            traceRight(RuleBasedActorEvent.Dispute.ParsedTally)
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
        case object NoSuitableCollateralUtxosFound extends Unrecoverable {
            override def getMessage: String =
                "Needed at least one ada-only utxo to use for plutus script collateral" +
                    " at the peer's head address, but found none."
        }

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
