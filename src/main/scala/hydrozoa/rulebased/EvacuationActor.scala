package hydrozoa.rulebased

import cats.*
import cats.data.*
import cats.effect.{IO, IOLocal}
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.operation.evacuation.NodeOperationEvacuationConfig
import hydrozoa.config.node.owninfo.OwnHeadPeerPublic
import hydrozoa.config.{HydrozoaBlueprint, ScriptReferenceUtxos}
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.shelleyAddress
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.lib.logging.Tracer
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.commitment.Membership
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap}
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased
import hydrozoa.rulebased.EvacuationActor.Error.ParseError.TreasuryDeinitialized
import hydrozoa.rulebased.EvacuationActor.Requests.Evacuate
import hydrozoa.rulebased.EvacuationActor.{Error, *}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import hydrozoa.rulebased.ledger.l1.utxo.RuleBasedTreasuryUtxo
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}
import scalus.builtin.ByteString
import scalus.cardano.ledger.{TransactionHash, Utxo, Utxos}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.fromData

/** @param candidateEvacMaps
  *   every evacuation map peers could end up tallying onto, keyed by its kzg commitment. The actor
  *   waits until the dispute resolution lands, reads the resolved treasury's `evacuationActive`
  *   (the winning kzg), and picks the matching map from here as the active one to evacuate against.
  * @param cardanoBackend
  * @param fallbackTxHash
  * @param tracerLocal
  * @param config
  */
case class EvacuationActor(
    candidateEvacMaps: Map[KzgCommitment, EvacuationMap],
    cardanoBackend: CardanoBackend[IO],
    fallbackTxHash: TransactionHash,
    tracerLocal: IOLocal[Tracer],
)(using config: Config)
    extends Actor[IO, Requests.Request] {

    given IOLocal[Tracer] = tracerLocal

    override def preStart: IO[Unit] =
        context.self ! Requests.PreStart

    private def preStartLocal: IO[Unit] =
        for {
            _ <- context.setReceiveTimeout(config.evacuationBotPollingPeriod, Evacuate)
            _ <- Tracer.routeLocal(s"EvacuationActor.${config.ownHeadPeerNum}")
        } yield ()

    override def receive: Receive[IO, Requests.Request] = {
        case _: Requests.PreStart.type => preStartLocal
        case _: Requests.Evacuate.type => handleEvacuation
    }

    //  TODOS:
    //
    //   We might have too many evacuations that dont fit in ex-units or tx size. We need a strategy to deal with
    //   this efficiently.
    //
    //   I'm working under the assumption that minting the proof for such a transaction is expensive, so that we
    //   should NOT just do the same thing we do for tx packing (try to extract all utxos, then all except 1,
    //   all except 2). Some thoughts:
    //
    //   - If we can come up with bounds (exact or approximate), we can use them as a heuristic.
    //   - Evacuation txs can probably be chained.
    //   - If we can't come up with bounds, we can probably make this logarithmic. Try evacuating all; if it fails
    //     try evacuating half in two separate transactions and chaining. Or just evacuate half and submit ASAP
    //   - I don't know if the crypto allows it, but maybe this could be parallelized? But we might need a new
    //     commitment first
    //   - I don't know if the crypto works like this, but if evacuating `N` is too big for one tx, but `N/2` isn't,
    //     then maybe we just cache the `N/2` utxos to "evacuate next" and use that as our starting point for the next
    //     tx? It wouldn't make sense to keep trying `N` at a time.
    //   - datum parsing could definitely be parallelized
    //
    //   But, as usual, this is a pessimistic case. Performance still matters, but happy-path performance matters more.

    /** Queries for a treasury utxo, checks which utxos are members, and submits a evacuation tx.
      *
      * Error semantics:
      *   - Query failures on the cardano backend are ignored and we retry
      *   - If the treasury utxo is not found, we ignore it and try again. This can happen if the
      *     deinit transaction has completed successfully, but we keep trying in case of rollbacks
      *   - other parsing failures on the treasury utxo lead to exceptions
      *   - build failures on the withdrawal tx are thrown as exceptions
      * @return
      */
    private def handleEvacuation: IO[Either[EvacuationActor.Error.RecoverableErrors, Unit]] = {
        val et: EitherT[IO, EvacuationActor.Error.RecoverableErrors, Unit] = {
            for {
                //////////////////////////////
                // Step 1: Figure out utxos we need to withdraw
                /////////////////////////////

                // The resolution tx is the first tx after the fallback that spends the treasury.
                // Every subsequent transaction also spends the treasury for a withdrawal
                treasuryTxRedeemersAndDatums: List[(TransactionHash, Data, Data)] <- EitherT(
                  cardanoBackend.lastContinuingTxs(
                    asset = (
                      config.headMultisigScript.policyId,
                      config.headTokenNames.treasuryTokenName
                    ),
                    after = fallbackTxHash
                  )
                ).leftSemiflatTap(e =>
                    Tracer.warn(
                      s"Backend error querying continuing txs. Will retry.\n\tError: $e"
                    )
                )

                // All parsed redeemers. If parsing fails, something is seriously wrong and we return Left
                evacuateRedeemers: List[EvacuateRedeemer] <-
                    treasuryTxRedeemersAndDatums.length match {
                        // Treasury not resolved, can't evacuate, return left and retry
                        case 0 =>
                            EitherT.left(
                              Tracer.debug("Treasury not yet resolved, retrying") >>
                                  IO.pure(Error.QueryError.NoTreasuryFound)
                            )
                        // Treasury resolved but no withdrawals processed yet, active utxo set will be as it was at fallback
                        case 1 => EitherT.fromEither[IO](Right(List.empty[EvacuateRedeemer]))
                        // Treasury resolved, some withdrawals processed. We need to parse redeemers to determine active utxo set.
                        case n =>
                            // The last transaction reported will be the dispute resolution tx (oldest).
                            // The preceding entries will be withdrawal transactions, up until the point that the deinit transaction
                            // occurs.
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
                                            EitherT.left(IO.pure(TreasuryDeinitialized))
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
                            EitherT(
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

                evacuationMapAtResolution <- candidateEvacMaps.get(resolutionKzg) match {
                    case None =>
                        EitherT.right(IO.raiseError(Error.UnknownResolvedKzg(resolutionKzg)))
                    case Some(evacMap) => EitherT.right(IO.pure(evacMap))
                }

                /////////////////////////////////
                // Step 2: Figure out the current treasury, pick the active evacuation map
                ////////////////////////////////

                unparsedTreasuryUtxos <- EitherT(
                  cardanoBackend.utxosAt(
                    address = HydrozoaBlueprint.mkTreasuryAddress(config.cardanoInfo.network),
                    asset = (
                      config.headMultisigScript.policyId,
                      config.headTokenNames.treasuryTokenName
                    )
                  )
                ).leftSemiflatTap(e =>
                    Tracer.warn(s"Backend error querying treasury UTxOs. Will retry.\n\tError: $e")
                )

                treasuryUtxoAndDatum <- parseRBTreasury(unparsedTreasuryUtxos)

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
                        EitherT.left(
                          Tracer.info(LogMessages.NoMoreEvacuations) >> IO.sleep(10.seconds) >>
                              IO.pure(
                                Error.QueryError.NoEvacuateesRemaining: Error.RecoverableErrors
                              )
                        )
                    else EitherT.right(Tracer.info(s"${toEvacuate.size} payout obligations left"))

                walletAddress = config.evacuationWallet.exportVerificationKey.shelleyAddress()(using
                  config
                )

                // Note that if there are no fee utxos, we just try again.
                feeUtxos <- EitherT(
                  cardanoBackend.utxosAt(
                    address = walletAddress
                  )
                ).leftSemiflatTap(e =>
                    Tracer.warn(s"Backend error querying fee UTxOs. Will retry.\n\tError: $e")
                )

                // Derive collateral from the fee wallet UTxOs.
                collateralUtxo <- feeUtxos.headOption match {
                    case None =>
                        EitherT.left(
                          Tracer.debug(
                            "No fee/collateral UTxO found at wallet address, retrying"
                          ) >>
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

                _ <- EitherT.liftF(
                  Tracer.debug(
                    "Building EvacuationTx with:" +
                        s"\n treasury value: ${treasuryUtxoAndDatum._1.treasuryOutput.value}" +
                        s"\n # evacuatees: ${toEvacuate.size}" +
                        s"\n total evacuation value ${toEvacuate.totalValue}"
                  )
                )
                evacTx <- evacBuilder.result match {
                    case Left(EvacuationTx.Build.Error.NoEvacuatees) =>
                        // TODO: This is actually gated above and should not be reachable. We can refactor
                        // EvacuationMap to be a NonEmptyMap underneath, perhaps, and then this duplicate check
                        // goes away. But then we'd need to pass Option[EvacuationMap] at certain points....
                        EitherT.left[EvacuationTx](
                          IO.pure(Error.QueryError.NoEvacuateesRemaining: Error.RecoverableErrors)
                        )
                    case Left(e) =>
                        EitherT(IO.raiseError(Error.BuildError.EvacuationTxBuildError(e)))
                    case Right(tx) => EitherT.pure[IO, Error.RecoverableErrors](tx)
                }

                _ <- (for {
                    _ <- EitherT.liftF(
                      Tracer.debug(
                        s"submitting evacTx with ${evacTx.evacuatedOutputs.size} evacuated outputs" +
                            s"\n cbor:\n\n${ByteString.fromArray(evacTx.tx.toCbor).toHex}\n\n"
                      )
                    )
                    _ <- EitherT(cardanoBackend.submitTx(config.evacuationWallet.signTx(evacTx.tx)))
                } yield ()).leftSemiflatTap(e =>
                    Tracer.warn(s"Backend error submitting evacuation tx. Will retry.\n\tError: $e")
                )

                _ <- EitherT.liftF(Tracer.info("Evacuation tx submitted"))
            } yield ()
        }
        et.value
    }

}

object EvacuationActor {
    type Config = NodeOperationEvacuationConfig.Section & CardanoNetwork.Section &
        HeadPeers.Section & HasTokenNames & ScriptReferenceUtxos.Section & OwnHeadPeerPublic.Section

    // TODO: replace brittle string sentinels with domain-specific typed log events
    object LogMessages:
        val NoMoreEvacuations = "No more evacuations to be done. Staying alive in case of rollbacks"

    type Handle = ActorRef[IO, Requests.Request]

    object Requests {
        case object PreStart

        case object Evacuate

        type Request = Evacuate.type | PreStart.type
    }

    object Error {
        // These errors get swallowed and we retry
        type RecoverableErrors = Recoverable | CardanoBackend.Error
        sealed trait Recoverable

        type UnrecoverableErrors = Unrecoverable | Membership.MembershipCheckError
        sealed trait Unrecoverable extends Exception

        // The dispute resolution wrote a kzg commitment into the treasury that we did not
        // pre-load as a candidate. The candidate set is supposed to be a superset of what peers
        // can vote for, so this means our boot snapshot diverged from on-chain — escalate.
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

        object ParseError {
            case object TreasuryMissing extends Recoverable

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

        object BuildError {
            case class EvacuationTxBuildError(
                wrapped: EvacuationTx.Build.Error
            ) extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }
        }

    }

    def parseRBTreasury(
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
                      Left(Error.ParseError.TreasuryMissing: Error.RecoverableErrors)
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
