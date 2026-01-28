package hydrozoa.rulebased

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.virtual.commitment.Membership
import hydrozoa.rulebased.LiquidationActor.{Error, Requests}
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryValidator.WithdrawRedeemer
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.dapp.tx.Withdrawal
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import hydrozoa.{L1, L2, Output, UtxoId, UtxoIdL2, UtxoSet, UtxoSetL1, UtxoSetL2, Wallet, rulebased}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}
import scalus.builtin.Data
import scalus.builtin.Data.fromData
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.{CardanoInfo, PlutusScriptEvaluator, TransactionHash, TransactionInput, TransactionOutput}
import scalus.cardano.txbuilder.SomeBuildError

case class LiquidationActor(
    allUtxosToWithdraw: UtxoSetL2,
    cardanoBackend: CardanoBackend[IO],
    l2SetAtFallback: UtxoSetL2,
    fallbackTxHash: TransactionHash,
    /** The PKH of the wallet that will pay for withdrawal TX fees and receive the change NOTE: The
      * is no coin selection algorithm right now. This means that:
      *   - This wallet MUST NOT contain anything but ADA-only utxos
      *   - Every utxo at this address will be spent at each withdrawal transaction
      *   - All ADA left-overs will be sent back to this address. Do NOT use this address for
      *     anything else. Do NOT set the delegation part for this address
      */
    config: LiquidationActor.Config,
) extends Actor[IO, LiquidationActor.Requests.Request] {
    override def preStart: IO[Unit] = context.setReceiveTimeout(config.receiveTimeout, ())

    override def receive: Receive[IO, Requests.Request] = { case _: Requests.HandleLiquidation =>
        handleLiquidation
    }

    //  TODOS:
    //
    //   We might have too many withdrawals that dont fit in ex-units or tx size. We need a strategy to deal with
    //   this efficiently.
    //
    //   I'm working under the assumption that minting the proof for such a transaction is expensive, so that we
    //   should NOT just do the same thing we do for tx packing (try to extract all utxos, then all except 1,
    //   all except 2). Some thoughts:
    //
    //   - If we can come up with bounds (exact or approximate), we can use them as a heuristic.
    //   - Withdrawal txs can probably be chained.
    //   - If we can't come up with bounds, we can probably make this logarithmic. Try withdrawals all; if it fails
    //     try withdrawing half in two separate transactions and chaining. Or just withdraw half and submit ASAP
    //   - I don't know if the crypto allows it, but maybe this could be parallelized? But we might need a new
    //     commitment first
    //   - I don't know if the crypto works like this, but if withdrawing `N` is too big for one tx, but `N/2` isn't,
    //     then maybe we just cache the `N/2` utxos to "withdraw next" and use that as our starting point for the next
    //     tx? It wouldn't make sense to keep trying `N` at a time.
    //   - datum parsing could definitely be parallelized
    //
    //   But, as usual, this is a pessimistic case. Performance still matters, but happy-path performance matters more.

    /** Queries for a treasury utxo, checks which utxos are members, and submits a withdrawal tx.
      *
      * Error semantics:
      *   - Query failures on the cardano backend are ignored and we retry
      *   - If the treasury utxo is not found, we ignore it and try again. This can happen if the
      *     deinit transaction has completed successfully, but we keep trying in case of rollbacks
      *   - other parsing failures on the treasury utxo lead to exceptions
      *   - build failures on the withdrawal tx are thrown as exceptions
      * @return
      */
    private def handleLiquidation: IO[Either[Error.RecoverableErrors, Unit]] = {
        val et: EitherT[IO, Error.RecoverableErrors, Unit] = {
            for {
                //////////////////////////////
                // Step 1: Figure out utxos we need to withdraw
                /////////////////////////////

                // The resolution tx is the first tx after the fallback that spends the treasury.
                // Every subsequent transaction also spends the treasury for a withdrawal
                treasuryTxRedeemers: List[(TransactionHash, Data)] <- EitherT(
                  cardanoBackend.lastContinuingTxs(
                    asset = (config.headMultisigScript.policyId, config.tokenNames.headTokenName),
                    after = fallbackTxHash
                  )
                )

                // All parsed redeemers. If parsing fails, something is seriously wrong and we return Left
                redeemers: List[WithdrawRedeemer] <- treasuryTxRedeemers.length match {
                    // Treasury not resolved, can't liquidate, return left and retry
                    case 0 => EitherT.fromEither[IO](Left(Error.QueryError.NoTreasuryFound))
                    // Treasury resolved but no withdrawals processed yet, active utxo set will be as it was at fallback
                    case 1 => EitherT.fromEither[IO](Right(List.empty[WithdrawRedeemer]))
                    // Treasury resolved, some withdrawals processed. We need to parse redeemers to determine active utxo set.
                    case n =>
                        // The first transaction reported will be the dispute resolution tx.
                        // The remaining will be withdrawal transactions, up until the point that the deinit transaction
                        // occurs.
                        treasuryTxRedeemers.tail.traverse(tuple =>
                            val (_, redeemerData) = tuple
                            Try(fromData[WithdrawRedeemer](redeemerData)) match {
                                // Can't deserialize the redeemer. Raise exception
                                case Failure(t) =>
                                    EitherT(
                                      IO.raiseError(
                                        Error.ParseError
                                            .TreasuryWithdrawRedeemerDataDeserializationFailure(t)
                                      )
                                    )
                                case Success(r) =>
                                    EitherT.right[Error.RecoverableErrors](IO.pure(r))
                            }
                        )
                }

                // All L2 utxos that were withdrawn in previous withdrawal transactions.
                previouslyWithdrawnUtxos: Set[UtxoIdL2] = redeemers.foldLeft(Set.empty[UtxoIdL2])(
                  (acc, redeemer) =>
                      acc ++
                          redeemer.utxoIds
                              .map(id =>
                                  UtxoIdL2(
                                    TransactionInput(
                                      TransactionHash.fromByteString(id.id.hash),
                                      id.idx.toInt
                                    )
                                  )
                              )
                              .toScalaList
                )

                // The set of utxos that should currently exist on L2
                currentActiveSet: Map[TransactionInput, TransactionOutput] =
                    l2SetAtFallback.asScalus.removedAll(previouslyWithdrawnUtxos.map(_.convert))

                /////////////////////////////////
                // Step 2: Figure out the current treasury
                ////////////////////////////////

                unparsedTreasuryUtxos <- EitherT(
                  cardanoBackend.utxosAt(
                    address = RuleBasedTreasuryScript.address(config.cardanoInfo.network),
                    asset = (config.headMultisigScript.policyId, config.tokenNames.headTokenName)
                  )
                )

                treasuryUtxoAndDatum <- LiquidationActor.parseRBTreasury(unparsedTreasuryUtxos)

                walletAddress = ShelleyAddress(
                  network = config.cardanoInfo.network,
                  payment = Key(config.withdrawalFeeWallet.exportVerificationKeyBytes.verKeyHash),
                  delegation = ShelleyDelegationPart.Null
                )

                // Note that if there are no fee utxos, we just try again.
                feeUtxos <- EitherT(
                  cardanoBackend.utxosAt(
                    address = walletAddress
                  )
                )

                wConfig = Withdrawal.Config(
                  config.cardanoInfo,
                  changeAddress = ShelleyAddress(
                    config.cardanoInfo.network,
                    ShelleyPaymentPart.Key(
                      config.withdrawalFeeWallet.exportVerificationKeyBytes.verKeyHash
                    ),
                    ShelleyDelegationPart.Null
                  )
                )

                recipe = Withdrawal.Builder.Recipe(
                  treasuryUtxo = treasuryUtxoAndDatum._1,
                  withdrawalsSubset = UtxoSet[L2](
                    currentActiveSet
                        .filter((k, _) => allUtxosToWithdraw.contains(UtxoIdL2(k)))
                        .map((k, v) => (UtxoId[L2](k), Output[L2](v)))
                  ),
                  activeSet =
                      UtxoSet[L2](currentActiveSet.map((k, v) => (UtxoId[L2](k), Output[L2](v)))),
                  feeUtxos = feeUtxos
                )

                withdrawTx <- Withdrawal(wConfig).build(recipe) match {
                    case Left(e) => EitherT(IO.raiseError(Error.BuildError.WithdrawTxBuildError(e)))
                    case Right(tx) => EitherT.pure[IO, LiquidationActor.Error.RecoverableErrors](tx)
                }

                _ <- EitherT(cardanoBackend.submitTx(withdrawTx.tx))
            } yield ()
        }
        et.value
    }

}

object LiquidationActor {
    case class Config(
        withdrawalFeeWallet: Wallet,
        receiveTimeout: FiniteDuration,
        headMultisigScript: HeadMultisigScript,
        tokenNames: TokenNames,
        cardanoInfo: CardanoInfo
    ) {
        def evaluator: PlutusScriptEvaluator =
            PlutusScriptEvaluator(cardanoInfo, EvaluateAndComputeCost)
    }

    type Handle = ActorRef[IO, Requests.Request]

    object Requests {
        type HandleLiquidation = Unit // Stub type, not sure if we'll need anything in the request

        type Request = HandleLiquidation
    }

    object Error {
        // These errors get swallowed and we retry
        type RecoverableErrors = Recoverable | CardanoBackend.Error
        sealed trait Recoverable

        type UnrecoverableErrors = Unrecoverable | Membership.MembershipCheckError
        sealed trait Unrecoverable extends Throwable

        object QueryError {
            case object NoTreasuryFound extends Recoverable
        }

        object ParseError {
            case class MultipleTreasuryTokensFound(utxoSetL1: UtxoSetL1) extends Unrecoverable

            case object TreasuryMissing extends Recoverable

            case object TreasuryNotResolved extends Unrecoverable

            case class TreasuryWithdrawRedeemerDataDeserializationFailure(wrapped: Throwable)
                extends Unrecoverable

            case class RulesBasedTreasuryParseError(wrapped: RuleBasedTreasuryUtxo.ParseError)
                extends Unrecoverable
        }

        object BuildError {
            case class WithdrawTxBuildError(
                wrapped: Withdrawal.Builder.Error | SomeBuildError | Membership.MembershipCheckError
            ) extends Unrecoverable
        }

    }

    // NOTE: some duplication with the same function in DisputeActor
    def parseRBTreasury(
        utxos: UtxoSetL1
    ): EitherT[IO, Error.RecoverableErrors, (RuleBasedTreasuryUtxo, Resolved)] =
        for {
            utxo <- utxos.size match {
                // May happen due to rollback, ignore and try again
                case 0 => EitherT.left(IO.pure(Error.ParseError.TreasuryMissing))
                case 1 => EitherT.right(IO.pure(hydrozoa.Utxo[L1](utxos.head)))
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
                        LiquidationActor.Error.ParseError.RulesBasedTreasuryParseError(e)
                      )
                    )
                case Right(rbt) => EitherT.right(IO.pure(rbt))
            }
            resolvedDatum <- treasuryUtxo match {
                case RuleBasedTreasuryUtxo(
                      _utxoId,
                      _address,
                      datum: RuleBasedTreasuryDatum.Resolved,
                      _value
                    ) =>
                    EitherT.right(IO.pure(datum))
                case _ => EitherT.liftF(IO.raiseError(Error.ParseError.TreasuryNotResolved))
            }
        } yield (treasuryUtxo, resolvedDatum)

}
