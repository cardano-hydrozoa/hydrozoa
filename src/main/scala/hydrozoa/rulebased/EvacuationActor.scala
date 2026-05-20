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
import hydrozoa.multisig.ledger.commitment.Membership
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap}
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased
import hydrozoa.rulebased.EvacuationActor.*
import hydrozoa.rulebased.EvacuationActor.Error.ParseError.TreasuryDeinitialized
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.{EvacuateRedeemer, TreasuryRedeemer}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.l1.tx.EvacuationTx
import hydrozoa.rulebased.ledger.l1.utxo.{RuleBasedTreasuryOutput, RuleBasedTreasuryUtxo}
import scala.util.{Failure, Success, Try}
import scalus.cardano.ledger.{TransactionHash, Utxo, Utxos}
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.fromData

/** @param thisNodeEvacuates
  *   an EvacuationMap that _this_ node has been instructed to evacuate. This may be a _proper_
  *   subset of the evacuation map at fallback
  * @param cardanoBackend
  * @param evacuationMapAtFallback
  * @param fallbackTxHash
  * @param tracerLocal
  * @param config
  */
case class EvacuationActor(
    thisNodeEvacuates: EvacuationMap,
    cardanoBackend: CardanoBackend[IO],
    evacuationMapAtFallback: EvacuationMap,
    fallbackTxHash: TransactionHash,
    tracerLocal: IOLocal[Tracer],
)(using config: Config)
    extends Actor[IO, Requests.Request] {

    given IOLocal[Tracer] = tracerLocal

    override def preStart: IO[Unit] =
        context.self ! Requests.PreStart

    private def preStartLocal: IO[Unit] =
        for {
            _ <- context.setReceiveTimeout(config.evacuationBotPollingPeriod, ())
            _ <- Tracer.routeLocal(s"EvacuationActor.${config.ownHeadPeerNum}")
        } yield ()

    override def receive: Receive[IO, Requests.Request] = {
        case _: Requests.PreStart.type => preStartLocal
        case _: Requests.Evacuate      => handleEvacuation
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
                treasuryTxRedeemers: List[(TransactionHash, Data)] <- EitherT(
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
                redeemers: List[EvacuateRedeemer] <- treasuryTxRedeemers.length match {
                    // Treasury not resolved, can't liquidate, return left and retry
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
                        treasuryTxRedeemers.init.traverse { case (_, redeemerData) =>
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
                                case Success(_) => EitherT.left(IO.pure(TreasuryDeinitialized))
                            }
                        }

                }

                // All L2 utxos that were withdrawn in previous withdrawal transactions.
                previouslyEvacuated: Set[EvacuationKey] = redeemers.foldLeft(
                  Set.empty[EvacuationKey]
                )((acc, redeemer) =>
                    acc ++
                        redeemer.evacuationKeys.toScalaList
                )

                // The current on-chain state of the total evacuation map
                currentEvacuationMap: EvacuationMap = evacuationMapAtFallback.removedAll(
                  previouslyEvacuated
                )

                // Evacuations _we_ still have to attempt
                toEvacuate: EvacuationMap =
                    thisNodeEvacuates.removedAll(previouslyEvacuated)

                _ <- EitherT.liftF(
                  if toEvacuate.isEmpty
                  then
                      Tracer.info(
                        "No more evacuations to be done. Staying alive in case of rollbacks"
                      )
                  else Tracer.info(s"${toEvacuate.size} payout obligations left")
                )
                /////////////////////////////////
                // Step 2: Figure out the current treasury
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
                      treasuryUtxo = treasuryUtxoAndDatum._1,
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
                    case Left(e) =>
                        EitherT(IO.raiseError(Error.BuildError.EvacuationTxBuildError(e)))
                    case Right(tx) => EitherT.pure[IO, Error.RecoverableErrors](tx)
                }

                _ <- (for {
                    _ <- EitherT.liftF(
                      Tracer.debug(
                        s"submitting evacTx with ${evacTx.evacuatedOutputs.size} evacuated outputs"
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

    type Handle = ActorRef[IO, Requests.Request]

    object Requests {
        case object PreStart

        type Evacuate = Unit // Stub type, not sure if we'll need anything in the request

        type Request = Evacuate | PreStart.type
    }

    object Error {
        // These errors get swallowed and we retry
        type RecoverableErrors = Recoverable | CardanoBackend.Error
        sealed trait Recoverable

        type UnrecoverableErrors = Unrecoverable | Membership.MembershipCheckError
        sealed trait Unrecoverable extends Exception

        object QueryError {
            case object NoTreasuryFound extends Recoverable
        }

        object ParseError {
            case class MultipleTreasuryTokensFound(utxoSetL1: Utxos) extends Unrecoverable

            case object TreasuryMissing extends Recoverable

            case object TreasuryDeinitialized extends Recoverable

            case object TreasuryNotResolved extends Unrecoverable

            case class TreasuryEvacuationRedeemerParseError(wrapped: Throwable)
                extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }

            case class RulesBasedTreasuryParseError(wrapped: RuleBasedTreasuryOutput.ParseError)
                extends Unrecoverable
        }

        object BuildError {
            case class EvacuationTxBuildError(
                wrapped: EvacuationTx.Build.Error
            ) extends Unrecoverable {
                override def getMessage: String = wrapped.getMessage
            }
        }

    }

    // NOTE: some duplication with the same function in DisputeActor
    def parseRBTreasury(
        utxos: Utxos
    )(using
        config: Config
    ): EitherT[IO, Error.RecoverableErrors, (RuleBasedTreasuryUtxo, Resolved)] =
        for {
            utxo <- utxos.size match {
                // May happen due to rollback, ignore and try again
                case 0 => EitherT.left(IO.pure(Error.ParseError.TreasuryMissing))
                case 1 => EitherT.right(IO.pure(Utxo(utxos.head)))
                case _ =>
                    EitherT.liftF(
                      IO.raiseError(Error.ParseError.MultipleTreasuryTokensFound(utxos))
                    )
            }

            treasuryUtxo <- RuleBasedTreasuryUtxo
                .parse(utxo) match {
                case Left(e) =>
                    EitherT.liftF(
                      IO.raiseError(
                        EvacuationActor.Error.ParseError.RulesBasedTreasuryParseError(e)
                      )
                    )
                case Right(rbt) => EitherT.right(IO.pure(rbt))
            }
            resolvedDatum <- treasuryUtxo.treasuryOutput.datum match {
                case datum: RuleBasedTreasuryDatum.Resolved =>
                    EitherT.right(IO.pure(datum))
                case _ => EitherT.liftF(IO.raiseError(Error.ParseError.TreasuryNotResolved))
            }
        } yield (treasuryUtxo, resolvedDatum)

}
