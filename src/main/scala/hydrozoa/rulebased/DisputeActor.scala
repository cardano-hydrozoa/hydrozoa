package hydrozoa.rulebased

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import com.bloxbean.cardano.client.util.HexUtil
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.*
import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.node.NodePrivateConfig
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.{pubKeyHash, shelleyAddress}
import hydrozoa.lib.cardano.scalus.ledger.{CollateralOutput, CollateralUtxo}
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import hydrozoa.rulebased.DisputeActor.Error.NoSuitableCollateralUtxosFound
import hydrozoa.rulebased.DisputeActor.Error.ParseError.Treasury.TreasuryResolved
import hydrozoa.rulebased.DisputeActor.Requests.HandleDisputeRes
import hydrozoa.rulebased.DisputeActor.{Error, *}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.{AwaitingVote, Voted}
import hydrozoa.rulebased.ledger.l1.tx.*
import hydrozoa.rulebased.ledger.l1.utxo.*
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.{Transaction, TransactionOutput, Utxo, Utxos}
import scalus.utils.Pretty

// TODO: relocate
extension (tx: Transaction) {
    def selfSigned(using config: Config): Transaction = config.ownWallet.signTx(tx)
}

/** Pulls in vote/treasury utxo from cardano backend, and decides whether to submit a vote tx, tally
  * tx, or dispute resolution tx. If none of these need to be submitted, it tells the rule-based
  * regime manager to start evacuation.
  *
  * This actor calls itself in a loop via an overridden [[preStart]] method.
  *
  * Its erroring semantics are as follows:
  *   - It (currently) swallows query failures from the cardano backend and automatically retries.
  *     In the future, we may want this to notify the user after some number of failed attempts.
  *   - It swallows failures from tx submission. We expect there to be some number of failures due
  *     to utxo contention and rollbacks. In the future, we may try to use heuristics to determine
  *     when we should start to worry.
  *   - It throws exceptions on parsing failures. All parsing should be guarded by the presence of a
  *     specific token, and if a utxo carrying that token is not parseable, we can't proceed. This
  *     indicates something is severely wrong.
  *   - It throws exceptions on failures during tx building. All inputs reaching the tx builder
  *     should be valid, and thus the tx builder should not be able to fail. If it does, we can't
  *     proceed.
  *   - It throws an exception if multiple utxos with the treasury token are found.
  */
final case class DisputeActor(
    action: RuleBasedRegimeManager.DisputeAction,
    cardanoBackend: CardanoBackend[IO],
    tracer: ContraTracer[IO, DisputeActorEvent]
)(using config: Config)
    extends Actor[IO, DisputeActor.Requests.Request] {

    private def handleCardanoBackendError[A](
        action: IO[Either[CardanoBackend.Error, A]]
    ): EitherT[IO, DisputeActor.Error.RecoverableErrors, A] =
        for {
            res <- EitherT.liftF(action)
            a <- res match {
                // All backend errors (Timeout, InvalidTx, etc.) are recoverable: swallow and retry on
                // the next poll. We expect transient failures from UTxO contention, validity-interval
                // mismatches (e.g. tally attempted before deadline), and rollbacks.
                case Left(e) =>
                    EitherT.left(
                      tracer
                          .traceWith(DisputeActorEvent.CardanoBackendError(e))
                          .as(Error.RecoverableCardanoBackendError(e))
                    )
                case Right(a) => EitherT.right[DisputeActor.Error.RecoverableErrors](IO.pure(a))
            }
        } yield a

    /** Build, log, sign, and submit one dispute-flow tx. Each arm of `handleDisputeRes` differs
      * only in `label`, the lazy build result, and the per-tx-family error wrapper — this helper
      * collapses the shared pattern.
      */
    private def buildAndSubmit[T <: EnrichedTx[T], E <: Throwable](
        label: String,
        result: => Either[E, T],
        wrapError: E => Throwable,
    ): EitherT[IO, DisputeActor.Error.RecoverableErrors, Unit] =
        for {
            _ <- EitherT.liftF(tracer.traceWith(DisputeActorEvent.BuildingTx(label)))
            tx <- result match {
                case Left(e)   => EitherT.liftF(IO.raiseError(wrapError(e)))
                case Right(tx) => EitherT.right(IO.pure(tx))
            }
            _ <- EitherT.liftF(tracer.traceWith(DisputeActorEvent.SubmittingTxLabel(label)))
            _ <- signAndSubmitTx[T](tx)
        } yield ()

    private def signAndSubmitTx[TxType <: EnrichedTx[TxType]](
        tx: EnrichedTx[TxType],
    ): EitherT[IO, DisputeActor.Error.RecoverableErrors, Unit] = {
        for {
            _ <- EitherT.right(
              tracer.traceWith(
                DisputeActorEvent.SubmittingTxFamily(tx.transactionFamily, tx.tx.id.toString)
              )
            )
            _ <- EitherT.right(
              tracer.traceWith(
                DisputeActorEvent.TxCbor(
                  summon[Pretty[Transaction]].pretty(tx.tx).render(100),
                  HexUtil.encodeHexString(tx.tx.toCbor)
                )
              )
            )
            res <- handleCardanoBackendError(cardanoBackend.submitTx(tx.tx.selfSigned))
            _ <- EitherT.right(
              tracer.traceWith(
                DisputeActorEvent.TxSubmitSuccess(tx.transactionFamily, tx.tx.id.toString)
              )
            )
        } yield res
    }

    private def getDisputeCollateral
        : EitherT[IO, DisputeActor.Error.RecoverableErrors, CollateralUtxo] = {
        val peerAddr = config.ownWallet.exportVerificationKey.shelleyAddress()
        for {
            _ <- EitherT.liftF(
              tracer.traceWith(DisputeActorEvent.LookingForCollateral(peerAddr.toString))
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
                          .traceWith(DisputeActorEvent.NoCollateralFound(config.ownPeerLabel))
                          .flatMap(_ => IO.raiseError(NoSuitableCollateralUtxosFound))
                    )
            }
            _ <- EitherT.liftF(tracer.traceWith(DisputeActorEvent.CollateralFound))
        } yield CollateralUtxo(collateralUtxoTuple._1, collateralOutput)
    }

    def handleDisputeRes: IO[Either[DisputeActor.Error.RecoverableErrors, Unit]] = {
        val et: EitherT[IO, DisputeActor.Error.RecoverableErrors, Unit] = for {
            // Wrapped in EitherT because a Left doesn't signify an unrecoverable failure
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
            treasuryUtxo <- parseRBTreasury(unparsedTreasuryUtxo)(using config, tracer)

            _ <- disputeUtxos match {
                case DisputeUtxos.CastVote(ownBallotBox) =>
                    action match {
                        case RuleBasedRegimeManager.DisputeAction.Vote(
                              sec,
                              signatures,
                              coilSignatures
                            ) =>
                            buildAndSubmit(
                              label = "vote",
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
                              label = "abstain",
                              result = AbstainTx
                                  .Build(
                                    uncastBallotBox = ownBallotBox,
                                    collateralUtxo = collateralUtxo
                                  )
                                  .result,
                              wrapError = Error.BuildError.Abstain(_)
                            )
                    }

                case DisputeUtxos.Tally(otherUtxos) =>
                    // We must have that they key of the continuing input is less than the key of the removed input,
                    // so we sort here.
                    val keySorted = otherUtxos.sortBy(_.ballotBoxOutput.key)
                    val continuing = keySorted.head
                    for {
                        _ <- EitherT.liftF(tracer.traceWith(DisputeActorEvent.Tallying))

                        removed <- keySorted.tail.find(ballotBox =>
                            ballotBox.ballotBoxOutput.key == continuing.ballotBoxOutput.link
                        ) match {
                            case None =>
                                EitherT.liftF(
                                  IO.raiseError(
                                    DisputeActor.Error.NoCompatibleVoteForTallyingFound(otherUtxos)
                                  )
                                )
                            case Some(x) => EitherT.right(IO.pure(x))
                        }
                        // NOTE: it could potentially go faster (by reducing contention) if we:
                        // - Tx-Chained multiple of these resolutions
                        // - Processed multiple disjoint tallies in parallel
                        // - Randomized or otherwise came up with an algorithm for peers to optimistically not
                        //   submit non-disjoint tallying transactions
                        // But right now I'm just doing the simplest thing
                        _ <- buildAndSubmit(
                          label = "tally",
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
                      label = "resolve",
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

    override def preStart: IO[Unit] =
        context.self ! Requests.PreStart

    private def preStartLocal: IO[Unit] =
        context.setReceiveTimeout(config.evacuationBotPollingPeriod, HandleDisputeRes)

    override def receive: Receive[IO, Requests.Request] = {
        case _: Requests.PreStart.type => preStartLocal
        case _: Requests.HandleDisputeRes.type =>
            handleDisputeRes
    }

    /** Queries the cardano backend for all utxos at the dispute resolution address, and then parses
      * them. This will tell us whether the peer's empty [[OwnVoteUtxo]] is present, or whether it
      * is ready for tallying.
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
      *
      * @return
      *   A [[DisputeUtxos]] describing the current state at the dispute address:
      *   [[DisputeUtxos.CastVote]] when the peer's own empty vote utxo is still present,
      *   [[DisputeUtxos.Tally]] when more than one cast-vote utxo remains, or
      *   [[DisputeUtxos.Resolve]] when a single tallied vote utxo remains.
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
                                      DisputeActor.Error.NonVotedUtxoAtResolve(x)
                                    )
                            }
                        case xs => IO.pure(DisputeUtxos.Tally(xs))
                    }
            }
        } yield result
    }

}

object DisputeActor {

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

    type Config = NodePrivateConfig.Section & HeadConfig.Section

    type Handle = ActorRef[IO, Requests.Request]

    object Requests {

        case object PreStart

        type Request = HandleDisputeRes.type | PreStart.type

        // Placeholder, I'm not sure if we need any additional state here
        case object HandleDisputeRes
    }

    object Error {
        type RecoverableErrors = Recoverable
        sealed trait Recoverable
        case class RecoverableCardanoBackendError(
            wrapped: CardanoBackend.Error
        ) extends Recoverable

        type UnrecoverableErrors = Unrecoverable
        sealed trait Unrecoverable extends Exception
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
        }

        sealed trait ParseError
        object ParseError {
            object Treasury {
                case class WrappedTreasuryParseError(wrapped: RuleBasedTreasuryOutput.ParseError)
                    extends Unrecoverable

                /** This either means something is very wrong, or simply that the dispute resolution
                  * is over and the deinit transaction completed successfully
                  */
                case object TreasuryMissing extends Recoverable
                case object TreasuryResolved extends Recoverable

            }
        }

    }

    // Parsing. Its in EitherT over IO because we have some recoverable failures (Lefts) and some unrecoverable failures
    // thrown as exceptions.
    // - If we get more than one treasury token or a parsing failure, thats an exception
    // - if we get zero treasury tokens, it may mean that the deinit has succeeded. But we keep trying, in case
    //   of rollbacks.
    def parseRBTreasury(
        utxos: Utxos
    )(using
        config: Config,
        tracer: ContraTracer[IO, DisputeActorEvent]
    ): EitherT[IO, Error.RecoverableErrors, RuleBasedTreasuryUtxo] =
        import RuleBasedTreasuryUtxo.LookupError
        for {
            _ <- EitherT.liftF(tracer.traceWith(DisputeActorEvent.ParsingTreasury))
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
                      tracer.traceWith(DisputeActorEvent.TreasuryIsUnresolved)
                    )
                case _: RuleBasedTreasuryDatum.Resolved =>
                    EitherT.left[Unit](
                      tracer
                          .traceWith(DisputeActorEvent.TreasuryIsResolved)
                          .as(TreasuryResolved: Error.RecoverableErrors)
                    )
            }

            _ <- EitherT.liftF(
              tracer.traceWith(
                DisputeActorEvent.TreasuryFound(treasuryUtxo.treasuryOutput.value.toString)
              )
            )
        } yield treasuryUtxo
}
