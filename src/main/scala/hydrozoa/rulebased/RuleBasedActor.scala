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
import hydrozoa.rulebased.RuleBasedActor.Error.ParseError.Treasury.TreasuryResolved
import hydrozoa.rulebased.RuleBasedActor.Requests.Tick
import hydrozoa.rulebased.RuleBasedActor.{Error, *}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.{AwaitingVote, Voted}
import hydrozoa.rulebased.ledger.l1.tx.*
import hydrozoa.rulebased.ledger.l1.utxo.*
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.{Transaction, TransactionHash, TransactionOutput, Utxo, Utxos}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.fromData

// TODO: relocate
extension (tx: Transaction) {
    def selfSigned(using config: Config): Transaction = config.ownWallet.signTx(tx)
}

/** Single actor that drives the rule-based regime end to end: while the treasury is Unresolved it
  * runs the dispute branch (vote/abstain → tally → resolve); once Resolved it runs the evacuation
  * branch (chained `lastContinuingTxs` reads + evacuation tx submissions). Each tick runs both
  * branches in sequence; each branch short-circuits when the treasury is in the other state.
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

    private def handleCardanoBackendError[A](
        action: IO[Either[CardanoBackend.Error, A]]
    ): EitherT[IO, Error.RecoverableErrors, A] =
        for {
            res <- EitherT.liftF(action)
            a <- res match {
                // All backend errors (Timeout, InvalidTx, etc.) are recoverable: swallow and retry on
                // the next poll. We expect transient failures from UTxO contention, validity-interval
                // mismatches (e.g. tally attempted before deadline), and rollbacks.
                case Left(e) =>
                    EitherT.left(
                      tracer
                          .traceWith(RuleBasedActorEvent.Backend.Error(e))
                          .as(Error.RecoverableCardanoBackendError(e))
                    )
                case Right(a) => EitherT.right[Error.RecoverableErrors](IO.pure(a))
            }
        } yield a

    /** Build, sign, and submit one dispute-flow tx.
      */
    private def buildAndSubmit[T <: EnrichedTx[T]: TxFamily, E <: Throwable](
        result: => Either[E, T],
        wrapError: E => Throwable,
    ): EitherT[IO, Error.RecoverableErrors, Unit] =
        for {
            _ <- EitherT.liftF(
              tracer.traceWith(RuleBasedActorEvent.Tx.Building(TxFamily[T].name))
            )
            tx <- result match {
                case Left(e)   => EitherT.liftF(IO.raiseError(wrapError(e)))
                case Right(tx) => EitherT.right(IO.pure(tx))
            }
            _ <- signAndSubmitTx[T](tx)
        } yield ()

    private def signAndSubmitTx[TxType <: EnrichedTx[TxType]](
        tx: EnrichedTx[TxType],
    ): EitherT[IO, Error.RecoverableErrors, Unit] = {
        for {
            _ <- EitherT.right(
              tracer.traceWith(
                RuleBasedActorEvent.Tx.Submitting(tx)
              )
            )
            res <- handleCardanoBackendError(cardanoBackend.submitTx(tx.tx.selfSigned))
            _ <- EitherT.right(
              tracer.traceWith(
                RuleBasedActorEvent.Tx.SubmitSuccess(tx)
              )
            )
        } yield res
    }

    private def getDisputeCollateral: EitherT[IO, Error.RecoverableErrors, CollateralUtxo] = {
        val peerAddr = config.ownWallet.exportVerificationKey.shelleyAddress()
        for {
            _ <- EitherT.liftF(
              tracer.traceWith(RuleBasedActorEvent.Collateral.Looking(peerAddr.toString))
            )
            collateralCandidates <- handleCardanoBackendError(
              cardanoBackend.utxosAt(peerAddr)
            )
            collateralUtxoTuple <- collateralCandidates.filter((_, to) =>
                to.value.isOnlyAda
            ) match {
                case x if x.nonEmpty =>
                    EitherT.right(IO.pure(x.toList.maxBy(_._2.value.coin.value)))
                case _ => EitherT.liftF(IO.raiseError(NoSuitableCollateralUtxosFound))
            }
            collateralOutput <- collateralUtxoTuple._2 match {
                case TransactionOutput.Babbage(
                      ShelleyAddress(network, key: ShelleyPaymentPart.Key, delegation),
                      value,
                      datum,
                      scriptRef
                    ) =>
                    EitherT.right(
                      IO.pure(
                        CollateralOutput(
                          addrKeyHash = key.hash,
                          delegationPart = delegation,
                          coin = value.coin,
                          datumOption = datum,
                          scriptRef = scriptRef
                        )
                      )
                    )
                case _ =>
                    EitherT.liftF(
                      tracer
                          .traceWith(RuleBasedActorEvent.Collateral.NotFound(config.ownPeerLabel))
                          .flatMap(_ => IO.raiseError(NoSuitableCollateralUtxosFound))
                    )
            }
            _ <- EitherT.liftF(tracer.traceWith(RuleBasedActorEvent.Collateral.Found))
        } yield CollateralUtxo(collateralUtxoTuple._1, collateralOutput)
    }

    /** Dispute branch — runs only when the treasury is Unresolved. Short-circuits via
      * [[Error.ParseError.Treasury.TreasuryResolved]] (or `TreasuryMissing`) once the dispute is
      * over, leaving evacuation to take over.
      */
    def handleDispute: IO[Either[Error.RecoverableErrors, Unit]] = {
        val et: EitherT[IO, Error.RecoverableErrors, Unit] = for {
            unparsedDisputeUtxos <- handleCardanoBackendError(
              cardanoBackend.utxosAt(
                address = HydrozoaBlueprint.mkDisputeAddress(config.cardanoInfo.network),
                asset = (config.headMultisigScript.policyId, config.headTokenNames.voteTokenName)
              )
            )
            disputeUtxos <- EitherT.liftF(parseDisputeUtxos(unparsedDisputeUtxos))
            collateralUtxo <- getDisputeCollateral

            unparsedTreasuryUtxo <- handleCardanoBackendError(
              cardanoBackend.utxosAt(
                address = HydrozoaBlueprint.mkTreasuryAddress(config.cardanoInfo.network),
                asset =
                    (config.headMultisigScript.policyId, config.headTokenNames.treasuryTokenName)
              )
            )
            treasuryUtxo <- parseRBTreasuryUnresolved(unparsedTreasuryUtxo)(using config, tracer)

            _ <- disputeUtxos match {
                case DisputeUtxos.CastVote(ownBallotBox) =>
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

                case DisputeUtxos.Tally(otherUtxos) =>
                    // We must have that they key of the continuing input is less than the key of
                    // the removed input, so we sort here.
                    val keySorted = otherUtxos.sortBy(_.ballotBoxOutput.key)
                    val continuing = keySorted.head
                    for {
                        _ <- EitherT.liftF(tracer.traceWith(RuleBasedActorEvent.Tx.Tallying))

                        removed <- keySorted.tail.find(ballotBox =>
                            ballotBox.ballotBoxOutput.key == continuing.ballotBoxOutput.link
                        ) match {
                            case None =>
                                EitherT.liftF(
                                  IO.raiseError(
                                    Error.NoCompatibleVoteForTallyingFound(otherUtxos)
                                  )
                                )
                            case Some(x) => EitherT.right(IO.pure(x))
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

                case DisputeUtxos.Resolve(finalVoteUtxo) =>
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

                // Treasury was still unresolved when we read it above but the dispute address holds
                // no vote utxos. Per the spec this state is unreachable, so escalate.
                case DisputeUtxos.EmptyVotes =>
                    EitherT.liftF(IO.raiseError(Error.TreasuryUnresolvedButNoVotes))
            }

        } yield ()
        et.value
    }

    /** Evacuation branch — runs only once the treasury has flipped to Resolved (which happens via a
      * `ResolutionTx` submitted by the dispute branch). Short-circuits while the dispute is still
      * in progress.
      */
    def handleEvacuation: IO[Either[Error.RecoverableErrors, Unit]] = {
        val et: EitherT[IO, Error.RecoverableErrors, Unit] = {
            for {
                inputs <- EitherT
                    .liftF[IO, Error.RecoverableErrors, RuleBasedActor.EvacuationInputs](
                      loadEvacuationInputs
                    )

                // The resolution tx is the first tx after the fallback that spends the treasury.
                // Every subsequent transaction also spends the treasury for a withdrawal.
                treasuryTxRedeemersAndDatums: List[(TransactionHash, Data, Data)] <-
                    EitherT(
                      cardanoBackend.lastContinuingTxs(
                        asset = (
                          config.headMultisigScript.policyId,
                          config.headTokenNames.treasuryTokenName
                        ),
                        after = inputs.fallbackTxHash
                      )
                    ).leftSemiflatTap(e =>
                        tracer.traceWith(RuleBasedActorEvent.Backend.ErrorContinuingTxs(e))
                    ).leftMap(Error.RecoverableCardanoBackendError(_): Error.RecoverableErrors)

                // All parsed redeemers. If parsing fails, something is seriously wrong and we return Left
                evacuateRedeemers: List[EvacuateRedeemer] <-
                    treasuryTxRedeemersAndDatums.length match {
                        // Treasury not resolved, can't evacuate, return left and retry
                        case 0 =>
                            EitherT.left[List[EvacuateRedeemer]](
                              tracer.traceWith(RuleBasedActorEvent.Treasury.NotYetResolved) >>
                                  IO.pure(Error.QueryError.NoTreasuryFound)
                            )
                        // Treasury resolved but no withdrawals processed yet, active utxo set will be as it was at fallback
                        case 1 => EitherT.fromEither[IO](Right(List.empty[EvacuateRedeemer]))
                        // Treasury resolved, some withdrawals processed. We need to parse redeemers to determine active utxo set.
                        case _ =>
                            // The last transaction reported will be the dispute resolution tx (oldest).
                            // The preceding entries will be withdrawal transactions, up until the
                            // point that the deinit transaction occurs.
                            treasuryTxRedeemersAndDatums.init.traverse {
                                case (_, redeemerData, _) =>
                                    Try(fromData[TreasuryRedeemer](redeemerData)) match {
                                        // Can't deserialize the redeemer. Raise exception
                                        case Failure(t) =>
                                            EitherT(
                                              IO.raiseError(
                                                Error.ParseError
                                                    .TreasuryEvacuationRedeemerParseError(
                                                      t
                                                    )
                                              )
                                            )
                                        case Success(TreasuryRedeemer.Evacuate(r)) =>
                                            EitherT.right[Error.RecoverableErrors](IO.pure(r))
                                        case Success(_) =>
                                            EitherT.left(
                                              IO.pure(Error.ParseError.TreasuryDeinitialized)
                                            )
                                    }
                            }

                    }

                resolutionKzg <- {
                    val resolutionTreasuryOutputDatum = treasuryTxRedeemersAndDatums.last._3
                    Try(
                      fromData[TreasuryState.RuleBasedTreasuryDatumOnchain](
                        resolutionTreasuryOutputDatum
                      )
                    ) match {
                        case Failure(t) =>
                            EitherT[IO, Error.RecoverableErrors, KzgCommitment](
                              IO.raiseError(Error.ParseError.TreasuryResolveRedeemerParseError(t))
                            )
                        case Success(
                              r: TreasuryState.RuleBasedTreasuryDatumOnchain.ResolvedOnchain
                            ) =>
                            EitherT.right(IO.pure(r.evacuationActive))
                        case Success(_) =>
                            EitherT(IO.raiseError(Error.ParseError.TreasuryNotResolved))
                    }
                }

                evacuationMapAtResolution <- inputs.candidateEvacMaps.get(resolutionKzg) match {
                    case None =>
                        EitherT.right[Error.RecoverableErrors](
                          IO.raiseError[EvacuationMap](Error.UnknownResolvedKzg(resolutionKzg))
                        )
                    case Some(evacMap) => EitherT.right(IO.pure(evacMap))
                }

                unparsedTreasuryUtxos <- EitherT(
                  cardanoBackend.utxosAt(
                    address = HydrozoaBlueprint.mkTreasuryAddress(config.cardanoInfo.network),
                    asset = (
                      config.headMultisigScript.policyId,
                      config.headTokenNames.treasuryTokenName
                    )
                  )
                ).leftSemiflatTap(e =>
                    tracer.traceWith(RuleBasedActorEvent.Backend.ErrorTreasuryUtxos(e))
                ).leftMap(Error.RecoverableCardanoBackendError(_): Error.RecoverableErrors)

                treasuryUtxoAndDatum <- parseRBTreasuryResolved(unparsedTreasuryUtxos)

                // All L2 utxos that were withdrawn in previous withdrawal transactions.
                previouslyEvacuated: Set[EvacuationKey] = evacuateRedeemers.foldLeft(
                  Set.empty[EvacuationKey]
                )((acc, redeemer) =>
                    acc ++
                        redeemer.evacuationKeys.toScalaList
                )

                // The current on-chain state of the total evacuation map
                currentEvacuationMap: EvacuationMap = evacuationMapAtResolution.removedAll(
                  previouslyEvacuated
                )

                // Evacuations _we_ still have to attempt — under the current "evacuate
                // everything we voted for" policy this is the same as the on-chain remainder.
                toEvacuate: EvacuationMap = currentEvacuationMap

                _ <-
                    if toEvacuate.isEmpty
                    then
                        EitherT.left[Unit](
                          tracer.traceWith(RuleBasedActorEvent.Evacuation.NoMore) >>
                              IO.sleep(10.seconds) >>
                              IO.pure(
                                Error.QueryError.NoEvacuateesRemaining: Error.RecoverableErrors
                              )
                        )
                    else
                        EitherT.right[Error.RecoverableErrors](
                          tracer.traceWith(
                            RuleBasedActorEvent.Evacuation.PayoutsLeft(toEvacuate.size)
                          )
                        )

                walletAddress = config.evacuationWallet.exportVerificationKey.shelleyAddress()(using
                  config
                )

                // Note that if there are no fee utxos, we just try again.
                feeUtxos <- EitherT(
                  cardanoBackend.utxosAt(
                    address = walletAddress
                  )
                ).leftSemiflatTap(e =>
                    tracer.traceWith(RuleBasedActorEvent.Backend.ErrorFeeUtxos(e))
                ).leftMap(Error.RecoverableCardanoBackendError(_): Error.RecoverableErrors)

                // Derive collateral from the fee wallet UTxOs.
                collateralUtxo <- feeUtxos.headOption match {
                    case None =>
                        EitherT.left[CollateralUtxo](
                          tracer.traceWith(RuleBasedActorEvent.Collateral.NoFeeCollateralUtxo) >>
                              IO.pure(Error.QueryError.NoTreasuryFound: Error.RecoverableErrors)
                        )
                    case Some((input, output)) =>
                        EitherT.fromEither[IO](
                          CollateralUtxo
                              .parse(Utxo(input, output))
                              .left
                              .map(_ => Error.QueryError.NoTreasuryFound: Error.RecoverableErrors)
                        )
                }

                evacBuilder = EvacuationTx
                    .Build(
                      inputTreasuryUtxo = treasuryUtxoAndDatum._1,
                      evacuateesToTryNext = toEvacuate,
                      allRemainingEvacuatees = currentEvacuationMap,
                      feeUtxos = feeUtxos,
                      collateralUtxo = collateralUtxo
                    )

                _ <- EitherT.liftF[IO, Error.RecoverableErrors, Unit](
                  tracer.traceWith(
                    RuleBasedActorEvent.Tx.Building(TxFamily[EvacuationTx].name)
                  )
                )
                evacTx <- evacBuilder.result match {
                    case Left(EvacuationTx.Build.Error.NoEvacuatees) =>
                        EitherT.left[EvacuationTx](
                          IO.pure(Error.QueryError.NoEvacuateesRemaining: Error.RecoverableErrors)
                        )
                    case Left(e) =>
                        EitherT[IO, Error.RecoverableErrors, EvacuationTx](
                          IO.raiseError(Error.BuildError.EvacuationTxBuildError(e))
                        )
                    case Right(tx) => EitherT.pure[IO, Error.RecoverableErrors](tx)
                }

                _ <- (for {
                    _ <- EitherT.liftF[IO, CardanoBackend.Error, Unit](
                      tracer.traceWith(RuleBasedActorEvent.Tx.Submitting(evacTx))
                    )
                    _ <- EitherT(cardanoBackend.submitTx(config.evacuationWallet.signTx(evacTx.tx)))
                } yield ())
                    .leftSemiflatTap(e =>
                        tracer.traceWith(RuleBasedActorEvent.Backend.ErrorSubmittingEvacTx(e))
                    )
                    .leftMap(Error.RecoverableCardanoBackendError(_): Error.RecoverableErrors)

                _ <- EitherT.liftF[IO, Error.RecoverableErrors, Unit](
                  tracer.traceWith(RuleBasedActorEvent.Tx.SubmitSuccess(evacTx))
                )
            } yield ()
        }
        et.value
    }

    /** Single tick: run the dispute branch first, then the evacuation branch. Each branch
      * short-circuits when the treasury state belongs to the other branch, so running both in
      * sequence is correct and only adds a handful of cheap queries per tick.
      */
    def handleTick: IO[Unit] =
        handleDispute >> handleEvacuation.void

    override def preStart: IO[Unit] =
        context.self ! Requests.PreStart

    private def preStartLocal: IO[Unit] =
        context.setReceiveTimeout(config.evacuationBotPollingPeriod, Tick)

    override def receive: Receive[IO, Requests.Request] = {
        case _: Requests.PreStart.type => preStartLocal
        case _: Requests.Tick.type     => handleTick
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
    private def parseDisputeUtxos(utxos: Utxos)(using
        config: Config
    ): IO[DisputeUtxos] = {
        val ownPkhHash = config.ownWallet.exportVerificationKey.pubKeyHash.hash
        for {
            // TODO: Collect parsing errors
            parsed <- IO.fromEither(utxos.toList.traverse((i, o) => BallotBox.parse(Utxo(i, o))))
            ownAwaiting = parsed.collectFirst {
                case v if v.ballotBoxOutput.status match {
                        case AwaitingVote(peer) => peer.hash == ownPkhHash
                        case _                  => false
                    } =>
                    v.asInstanceOf[BallotBox[AwaitingVote]]
            }
            result <- ownAwaiting match {
                case Some(own) => IO.pure(DisputeUtxos.CastVote(own))
                case None =>
                    parsed match {
                        case Nil => IO.pure(DisputeUtxos.EmptyVotes)
                        case x :: Nil =>
                            x.ballotBoxOutput.status match {
                                case _: Voted =>
                                    IO.pure(
                                      DisputeUtxos.Resolve(x.asInstanceOf[BallotBox[Voted]])
                                    )
                                case _ =>
                                    IO.raiseError(
                                      Error.NonVotedUtxoAtResolve(x)
                                    )
                            }
                        case xs => IO.pure(DisputeUtxos.Tally(xs))
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

            case class EvacuationTxBuildError(wrapped: EvacuationTx.Build.Error)
                extends Unrecoverable {
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

    // Parsing. It is in EitherT over IO because we have some recoverable failures (Lefts) and some
    // unrecoverable failures thrown as exceptions.
    // - If we get more than one treasury token or a parsing failure, that's an exception.
    // - If we get zero treasury tokens, it may mean that the deinit has succeeded. But we keep
    //   trying, in case of rollbacks.
    def parseRBTreasuryUnresolved(
        utxos: Utxos
    )(using
        config: Config,
        tracer: ContraTracer[IO, RuleBasedActorEvent]
    ): EitherT[IO, Error.RecoverableErrors, RuleBasedTreasuryUtxo] =
        import RuleBasedTreasuryUtxo.LookupError
        for {
            _ <- EitherT.liftF(tracer.traceWith(RuleBasedActorEvent.Treasury.Parsing))
            treasuryUtxo <- EitherT(
              IO.pure(RuleBasedTreasuryUtxo.parseOrMissing(utxos)).map {
                  case Right(u) => Right(u)
                  case Left(LookupError.Missing) =>
                      Left(Error.ParseError.Treasury.TreasuryMissing: Error.RecoverableErrors)
                  case Left(e) => throw e
              }
            )
            _ <- treasuryUtxo.treasuryOutput.datum match {
                case _: RuleBasedTreasuryDatum.Unresolved =>
                    EitherT.liftF[IO, Error.RecoverableErrors, Unit](
                      tracer.traceWith(RuleBasedActorEvent.Treasury.Unresolved)
                    )
                case _: RuleBasedTreasuryDatum.Resolved =>
                    EitherT.left[Unit](
                      tracer
                          .traceWith(RuleBasedActorEvent.Treasury.Resolved)
                          .as(TreasuryResolved: Error.RecoverableErrors)
                    )
            }

            _ <- EitherT.liftF(
              tracer.traceWith(
                RuleBasedActorEvent.Treasury.Found(treasuryUtxo.treasuryOutput.value.toString)
              )
            )
        } yield treasuryUtxo

    def parseRBTreasuryResolved(
        utxos: Utxos
    )(using
        config: Config
    ): EitherT[IO, Error.RecoverableErrors, (RuleBasedTreasuryUtxo, Resolved)] =
        import RuleBasedTreasuryUtxo.LookupError
        for {
            treasuryUtxo <- EitherT(
              IO.pure(RuleBasedTreasuryUtxo.parseOrMissing(utxos)).map {
                  case Right(u) => Right(u)
                  case Left(LookupError.Missing) =>
                      Left(Error.ParseError.Treasury.TreasuryMissing: Error.RecoverableErrors)
                  case Left(e) => throw e
              }
            )
            resolvedDatum <- treasuryUtxo.treasuryOutput.datum match {
                case datum: RuleBasedTreasuryDatum.Resolved =>
                    EitherT.right(IO.pure(datum))
                case _ => EitherT.liftF(IO.raiseError(Error.ParseError.TreasuryNotResolved))
            }
        } yield (treasuryUtxo, resolvedDatum)
}
