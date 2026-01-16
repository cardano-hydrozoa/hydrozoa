package hydrozoa.rulebased

import cats.data.EitherT
import cats.effect.IO
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.rulebased.LiquidationActor.Error.*
import hydrozoa.rulebased.LiquidationActor.Requests
import hydrozoa.rulebased.ledger.dapp.script.plutus.RuleBasedTreasuryScript
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum.Resolved
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.{MembershipProof, RuleBasedTreasuryDatum}
import hydrozoa.rulebased.ledger.dapp.tx.WithdrawTx
import hydrozoa.rulebased.ledger.dapp.utxo.RuleBasedTreasuryUtxo
import hydrozoa.{L1, L2, Utxo, UtxoSet, UtxoSetL1, UtxoSetL2, rulebased}
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.txbuilder.SomeBuildError

case class LiquidationActor(
    utxosToWithdraw: UtxoSetL2,
    receiveTimeout: FiniteDuration,
    cardanoBackend: CardanoBackend[IO],
    config: Tx.Builder.Config,
    headMultisigScript: HeadMultisigScript,
    tokenNames: TokenNames
) extends Actor[IO, LiquidationActor.Requests.Request] {
    override def preStart: IO[Unit] = context.setReceiveTimeout(receiveTimeout, ())

    override def receive: Receive[IO, Requests.Request] = { case _: Requests.HandleLiquidation =>
        handleLiquidation
    }

    // stub function
    def isMember(membershipProof: MembershipProof, utxo: Utxo[L2]): Boolean = ???

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
    private def handleLiquidation: IO[Either[rulebased.LiquidationActor.Error.Error, Unit]] = {
        val et: EitherT[IO, rulebased.LiquidationActor.Error.Error, Unit] =
            for {
                unparsedTreasuryUtxos <- EitherT(
                  cardanoBackend.utxosAt(
                    address = RuleBasedTreasuryScript.address(config.env.network),
                    asset = (headMultisigScript.policyId, tokenNames.headTokenName)
                  )
                )
                treasuryUtxoAndDatum <- LiquidationActor.parseRBTreasury(unparsedTreasuryUtxos)
                membershipProof = treasuryUtxoAndDatum._2.resolvedDatum.utxosActive
                // How fast in the membership checking? Should we use parTraverse?
                utxosNotWithdrawn =
                    Map.from(
                      utxosToWithdraw.toList
                          .filter((id, output) => isMember(membershipProof, Utxo[L2](id, output)))
                    )
                recipe = WithdrawTx.Recipe(
                  treasuryUtxo = treasuryUtxoAndDatum._1,
                  withdrawals = UtxoSet[L2](utxosNotWithdrawn),
                  membershipProof = IArray.from(membershipProof.bytes),
                  validityEndSlot = ???,
                  network = config.env.network,
                  protocolParams = config.env.protocolParams,
                  evaluator = config.evaluator,
                  validators = config.validators
                )

                withdrawTx <- WithdrawTx.build(recipe) match {
                    case Left(e)   => EitherT.liftF(IO.raiseError(WithdrawTxBuildError(e)))
                    case Right(tx) => EitherT.pure[IO, LiquidationActor.Error.Error](tx)
                }

                _ <- EitherT(cardanoBackend.submitTx(withdrawTx.tx))
            } yield ()
        et.value
    }

}

object LiquidationActor {
    type Handle = ActorRef[IO, Requests.Request]

    object Requests {
        type HandleLiquidation = Unit // Stub type, not sure if we'll need anything in the request

        type Request = HandleLiquidation
    }

    object Error {
        type Error = ParseError | CardanoBackend.Error | BuildError

        sealed trait ParseError extends Throwable
        case class MultipleTreasuryTokensFound(utxoSetL1: UtxoSetL1) extends ParseError
        case object TreasuryMissing extends ParseError
        case class TreasuryNotResolved(treasuryUtxo: RuleBasedTreasuryUtxo) extends ParseError

        sealed trait BuildError extends Throwable
        case class WithdrawTxBuildError(wrapped: WithdrawTx.WithdrawalTxError | SomeBuildError)
            extends BuildError

    }

    // NOTE: some duplication with the same function in DisputeActor
    def parseRBTreasury(
        utxos: UtxoSetL1
    ): EitherT[IO, LiquidationActor.Error.Error, (RuleBasedTreasuryUtxo, Resolved)] =
        for {
            utxo <-
                if utxos.size == 1
                then EitherT.pure[IO, Error](hydrozoa.Utxo[L1](utxos.head))
                else if utxos.size > 1
                then EitherT.right(IO.raiseError(MultipleTreasuryTokensFound(utxos)))
                else EitherT.fromEither[IO](Left(TreasuryMissing))

            treasuryUtxo <- EitherT.right(IO.fromEither(RuleBasedTreasuryUtxo.parse(utxo)))
            resolvedDatum <- treasuryUtxo match {
                case RuleBasedTreasuryUtxo(
                      _utxoId,
                      _address,
                      datum: RuleBasedTreasuryDatum.Resolved,
                      _value
                    ) =>
                    EitherT.pure[IO, LiquidationActor.Error.Error](datum)
                // Could happen if there's a rollback
                case _ => EitherT.fromEither[IO](Left(TreasuryNotResolved(treasuryUtxo)))
            }
        } yield (treasuryUtxo, resolvedDatum)

}
