package hydrozoa.rulebased

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.*
import hydrozoa.multisig.backend.cardano
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.protocol.types.AckBlock.HeaderSignature
import hydrozoa.rulebased.DisputeActor.*
import hydrozoa.rulebased.DisputeActor.Error.*
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.OnchainBlockHeader
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.tx.ResolutionTx.ResolutionTxError
import hydrozoa.rulebased.ledger.dapp.tx.TallyTx.TallyTxError
import hydrozoa.rulebased.ledger.dapp.tx.VoteTx.VoteTxError
import hydrozoa.rulebased.ledger.dapp.tx.{ResolutionTx, TallyTx, VoteTx}
import hydrozoa.rulebased.ledger.dapp.utxo.{OwnVoteUtxo, RuleBasedTreasuryUtxo, TallyVoteUtxo}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}
import scalus.builtin.Data.fromData
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.{CardanoInfo, DatumOption, PlutusScriptEvaluator}
import scalus.cardano.txbuilder.SomeBuildError

// QUESTION: The `OwnVoteUtxo` type is pretty sparse. Should I augment it directly, or did we want to keep
// it small for some reason?
type VoteUtxoWithDatum = (hydrozoa.Utxo[L1], VoteDatum)

/** Pulls in vote/treasury utxo from cardano backend, and decides whether to submit a vote tx, tally
  * tx, or dispute resolution tx. If none of these need to be submitted, it tells the rule-based
  * regime manager to start liquidation.
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
    config: DisputeActor.Config,
    collateralUtxo: hydrozoa.Utxo[L1],
    blockHeader: OnchainBlockHeader,
    cardanoBackend: CardanoBackend[IO],
    signatures: List[HeaderSignature],
) extends Actor[IO, DisputeActor.Requests.Request] {

    private val handleDisputeRes: IO[Either[DisputeActor.Error.Error, Unit]] = {
        val et: EitherT[IO, DisputeActor.Error.Error, Unit] = for {
            // Wrapped in EitherT because a Left doesn't signify an unrecoverable failure
            unparsedDisputeUtxos <- EitherT(
              cardanoBackend.utxosAt(
                address = DisputeResolutionScript.address(config.cardanoInfo.network),
                asset = (config.headMultisigScript.policyId, config.tokenNames.voteTokenName)
              )
            )
            disputeUtxos <- EitherT.liftF(parseDisputeUtxos(unparsedDisputeUtxos))

            unparsedTreasuryUtxo <- EitherT(
              cardanoBackend.utxosAt(
                address = RuleBasedTreasuryScript.address(config.cardanoInfo.network),
                asset = (config.headMultisigScript.policyId, config.tokenNames.headTokenName)
              )
            )
            treasuryUtxo <- parseRBTreasury(unparsedTreasuryUtxo)

            _ <- disputeUtxos match {
                // Cast Vote
                case (Some(ownVoteUtxo), _) =>
                    val recipe = VoteTx.Recipe(
                      voteUtxo = OwnVoteUtxo(config.ownPeerPkh.verKeyHash, ownVoteUtxo._1),
                      treasuryUtxo = treasuryUtxo,
                      collateralUtxo = collateralUtxo,
                      blockHeader = blockHeader,
                      signatures = signatures,
                      network = config.cardanoInfo.network,
                      protocolParams = config.cardanoInfo.protocolParams,
                      evaluator = config.evaluator,
                      validators = Tx.Validators.nonSigningValidators
                    )
                    for {
                        voteTx <- VoteTx.build(recipe) match {
                            case Left(e)   => EitherT.liftF(IO.raiseError(VoteTxBuildError(e)))
                            case Right(tx) => EitherT.pure[IO, cardano.CardanoBackend.Error](tx)
                        }
                        _ <- EitherT(cardanoBackend.submitTx(voteTx.tx))
                    } yield ()

                // Tally
                case (None, otherUtxos) if otherUtxos.length > 1 =>
                    // NOTE: it could potentially go faster (by reducing contention) if we:
                    // - Tx-Chained multiple of these resolutions
                    // - Processed multiple disjoint tallies in parallel
                    // - Randomized or otherwise came up with an algorithm for peers to optimistically not
                    //   submit non-disjoint tallying transactions
                    // But right now I'm just doing the simplest thing
                    val recipe = TallyTx.Recipe(
                      continuingVoteUtxo = TallyVoteUtxo(otherUtxos.head._1),
                      removedVoteUtxo = TallyVoteUtxo(otherUtxos.tail.head._1),
                      treasuryUtxo = treasuryUtxo,
                      collateralUtxo = collateralUtxo,
                      network = config.cardanoInfo.network,
                      protocolParams = config.cardanoInfo.protocolParams,
                      evaluator = config.evaluator,
                      // Since this tx isn't multisigned, I suppose this would be one of the few transactions
                      // where we could actually use all of the validators?
                      validators = Tx.Validators.nonSigningValidators
                    )
                    for {
                        tallyTx <- TallyTx.build(recipe) match {
                            case Left(e)   => EitherT.liftF(IO.raiseError(TallyTxBuildError(e)))
                            case Right(tx) => EitherT.pure[IO, cardano.CardanoBackend.Error](tx)
                        }
                        _ <- EitherT(cardanoBackend.submitTx(tallyTx.tx))
                    } yield ()

                // Resolve
                case (None, lastVoteUtxo) if lastVoteUtxo.length == 1 =>
                    val recipe = ResolutionTx.Recipe(
                      talliedVoteUtxo = TallyVoteUtxo(lastVoteUtxo.head._1),
                      treasuryUtxo = treasuryUtxo,
                      collateralUtxo = collateralUtxo,
                      network = config.cardanoInfo.network,
                      protocolParams = config.cardanoInfo.protocolParams,
                      evaluator = config.evaluator,
                      validators = Tx.Validators.nonSigningValidators
                    )
                    for {
                        resolutionTx <- ResolutionTx.build(recipe) match {
                            case Left(e) => EitherT.liftF(IO.raiseError(ResolutionTxBuildError(e)))
                            case Right(tx) => EitherT.pure[IO, cardano.CardanoBackend.Error](tx)
                        }
                        _ <- EitherT(cardanoBackend.submitTx(resolutionTx.tx))
                    } yield ()
            }

        } yield ()
        et.value
    }

    override def preStart: IO[Unit] = context.setReceiveTimeout(config.receiveTimeout, ())

    override def receive: Receive[IO, Requests.Request] = { case _: Requests.HandleDisputeRes =>
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
      *   A tuple of [[VoteUtxoWithDatum]]. The first element is wrapped in an option, and is only
      *   "Some" if the dispute actor still needs to cast a vote. The second element is all other
      *   vote utxos.
      */
    private def parseDisputeUtxos(utxos: hydrozoa.UtxoSetL1): IO[
      (
          Option[VoteUtxoWithDatum],
          Seq[VoteUtxoWithDatum]
      )
    ] =
        for {
            voteUtxos <- utxos.toList.traverse((i, o) => utxoToVoteUtxo(hydrozoa.Utxo[L1](i, o)))

            votePartition = voteUtxos.partition {
                case (_, VoteDatum(_, _, VoteStatus.AwaitingVote(peerPkh))) =>
                    peerPkh == config.ownPeerPkh
                case _ => false
            }
        } yield (votePartition._1.headOption, votePartition._2)

    private def utxoToVoteUtxo(
        utxo: hydrozoa.Utxo[L1]
    ): IO[VoteUtxoWithDatum] = {
        for {
            d1: DatumOption <- utxo._2.datumOption match {
                case None    => IO.raiseError(DisputeActor.Error.MissingVoteUtxoDatum(utxo))
                case Some(x) => IO.pure(x)
            }
            d2: Inline <- d1 match {
                case i: Inline => IO.pure(i)
                case _         => IO.raiseError(DisputeActor.Error.VoteDatumNotInline(utxo))
            }
            d3: VoteDatum <- Try(fromData[VoteDatum](d2.data)) match {
                case Success(d) => IO.pure(d)
                case Failure(e) => IO.raiseError(VoteDatumDeserializationError(utxo, e))
            }
        } yield (utxo, d3)
    }

}

object DisputeActor {
    case class Config(
        ownPeerPkh: VerificationKeyBytes,
        tokenNames: TokenNames,
        headMultisigScript: HeadMultisigScript,
        receiveTimeout: FiniteDuration,
        cardanoInfo: CardanoInfo
    ) {
        def evaluator: PlutusScriptEvaluator =
            PlutusScriptEvaluator(cardanoInfo, EvaluateAndComputeCost)
    }

    type Handle = ActorRef[IO, Requests.Request]

    object Requests {
        type Request = HandleDisputeRes

        // Placeholder, I'm not sure if we need any additional state here
        type HandleDisputeRes = Unit
    }

    object Error {

        /** Hierarchy:
          *   - ParseError
          *     - VoteParseError
          *     - TreasuryParseError
          *   - BuildError
          *     - VoteTxBuildError
          *     - TallyTxBuildError
          *     - ResolutionTxBuildError
          *   - CardanoBackendError
          */
        type Error = DisputeActor.Error.ParseError | cardano.CardanoBackend.Error |
            DisputeActor.Error.BuildError

        sealed trait BuildError extends Throwable
        case class VoteTxBuildError(wrapped: VoteTx.VoteTxError | SomeBuildError) extends BuildError
        case class TallyTxBuildError(wrapped: TallyTx.TallyTxError | SomeBuildError)
            extends BuildError
        case class ResolutionTxBuildError(wrapped: ResolutionTxError | SomeBuildError)
            extends BuildError

        sealed trait ParseError extends Throwable
        sealed trait VoteParseError extends ParseError
        sealed trait TreasuryParseError extends ParseError

        case class MissingVoteUtxoDatum(utxo: hydrozoa.Utxo[L1]) extends VoteParseError
        case class VoteDatumNotInline(utxo: hydrozoa.Utxo[L1]) extends VoteParseError
        case class VoteDatumDeserializationError(utxo: hydrozoa.Utxo[L1], e: Throwable)
            extends VoteParseError

        case class MultipleTreasuryTokensFound(utxos: UtxoSetL1) extends TreasuryParseError
        case class WrappedTreasuryParseError(wrapped: TreasuryParseError) extends TreasuryParseError

        /** This either means something is very wrong, or simply that the dispute resolution is over
          * and the deinit transaction completed successfully
          */
        case object TreasuryMissing extends TreasuryParseError
    }

    // Parsing. Its in EitherT over IO because we have some recoverable failures (Lefts) and some unrecoverable failures
    // thrown as exceptions.
    // - If we get more than one treasury token or a parsing failure, thats an exception
    // - if we get zero treasury tokens, it may mean that the deinit has succeeded. But we keep trying, in case
    //   of rollbacks.
    // TODO: Factor out. Its shared between this and the li
    // obtained from the parameters to this class
    def parseRBTreasury(utxos: UtxoSetL1): EitherT[IO, ParseError, RuleBasedTreasuryUtxo] =
        for {
            utxo <-
                if utxos.size == 1
                then EitherT.pure[IO, ParseError](hydrozoa.Utxo[L1](utxos.head))
                else if utxos.size > 1
                then EitherT.right(IO.raiseError(MultipleTreasuryTokensFound(utxos)))
                else EitherT.fromEither[IO](Left(TreasuryMissing))

            res <- EitherT.right(IO.fromEither(RuleBasedTreasuryUtxo.parse(utxo)))

        } yield res
}
