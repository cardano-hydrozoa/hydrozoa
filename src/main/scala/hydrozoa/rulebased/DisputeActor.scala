package hydrozoa.rulebased

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.*
import hydrozoa.config.ScriptReferenceUtxos
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.NodePrivateConfig
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error.*
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.DisputeActor.Error.ParseError.Treasury.TreasuryResolved
import hydrozoa.rulebased.DisputeActor.{Error, *}
import hydrozoa.rulebased.ledger.l1.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import hydrozoa.rulebased.ledger.l1.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.l1.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.l1.tx.*
import hydrozoa.rulebased.ledger.l1.utxo.*
import scala.util.{Failure, Success, Try}
import scalus.builtin.Data.fromData
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.EvaluatorMode.EvaluateAndComputeCost
import scalus.cardano.ledger.{AddrKeyHash, DatumOption, PlutusScriptEvaluator, Transaction, Utxo, Utxos}
import scalus.cardano.onchain.plutus.v3.PubKeyHash

// QUESTION: The `OwnVoteUtxo` type is pretty sparse. Should I augment it directly, or did we want to keep
// it small for some reason?<
type VoteUtxoWithDatum = (Utxo, VoteDatum)

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
final case class DisputeActor(config: DisputeActor.Config)(
    collateralUtxo: CollateralUtxo,
    blockHeader: BlockHeader.Minor.Onchain,
    _cardanoBackend: CardanoBackend[IO],
    signatures: List[BlockHeader.Minor.HeaderSignature],
) extends Actor[IO, DisputeActor.Requests.Request] {
    private def evaluator = PlutusScriptEvaluator(config.cardanoInfo, EvaluateAndComputeCost)

    final val cardanoBackend = _cardanoBackend

    private def handleCardanoBackendError[A](
        action: IO[Either[CardanoBackend.Error, A]]
    ): EitherT[IO, DisputeActor.Error.RecoverableErrors, A] =
        for {
            res <- EitherT.liftF(action)
            a <- res match {
                case Left(e: Timeout) =>
                    EitherT.left(IO.pure(Error.RecoverableCardanoBackendError(e)))
                case Left(e) =>
                    EitherT.liftF(IO.raiseError(Error.UnrecoverableCardanoBackendError(e)))
                case Right(a) => EitherT.right[DisputeActor.Error.RecoverableErrors](IO.pure(a))
            }
        } yield a

    private def signAndSubmitTx(
        tx: Transaction
    ): EitherT[IO, DisputeActor.Error.RecoverableErrors, Unit] =
        val signed = config.ownHeadWallet.signTx(tx)
        handleCardanoBackendError(cardanoBackend.submitTx(signed))

    // TODO: no transactions are currently getting signed
    val handleDisputeRes: IO[Either[DisputeActor.Error.RecoverableErrors, Unit]] = {
        val et: EitherT[IO, DisputeActor.Error.RecoverableErrors, Unit] = for {
            // Wrapped in EitherT because a Left doesn't signify an unrecoverable failure
            unparsedDisputeUtxos <- handleCardanoBackendError(
              cardanoBackend.utxosAt(
                address = DisputeResolutionScript.address(config.cardanoInfo.network),
                asset = (config.headMultisigScript.policyId, config.headTokenNames.voteTokenName)
              )
            )
            disputeUtxos <- EitherT.liftF(parseDisputeUtxos(unparsedDisputeUtxos))

            unparsedTreasuryUtxo <- handleCardanoBackendError(
              cardanoBackend.utxosAt(
                address = RuleBasedTreasuryScript.address(config.cardanoInfo.network),
                asset =
                    (config.headMultisigScript.policyId, config.headTokenNames.treasuryTokenName)
              )
            )
            treasuryUtxo <- parseRBTreasury(unparsedTreasuryUtxo)

            _ <- disputeUtxos match {
                // Cast Vote
                case (Some(ownVoteUtxo), _) =>
                    val builder = VoteTx.Build(config)(
                      _voteUtxo = OwnVoteUtxo(
                        AddrKeyHash(config.ownHeadWallet.pubKeyHash.hash),
                        ownVoteUtxo._1
                      ),
                      _treasuryUtxo = treasuryUtxo,
                      _collateralUtxo = collateralUtxo,
                      _blockHeader = blockHeader,
                      _signatures = signatures,
                    )
                    for {
                        voteTx <- builder.result match {
                            case Left(e)   => EitherT.liftF(IO.raiseError(Error.BuildError.Vote(e)))
                            case Right(tx) => EitherT.right(IO.pure(tx))
                        }
                        _ <- signAndSubmitTx(voteTx.tx)
                    } yield ()

                // Tally
                case (None, otherUtxos) if otherUtxos.length > 1 =>
                    // We must have that they key of the continuing input is less than the key of the removed input,
                    // so we sort here.
                    val keySorted = otherUtxos.sortBy(_._2.key)
                    val continuing = keySorted.head
                    for {
                        removed <- keySorted.tail.find((utxo, voteDatum) =>
                            voteDatum.key == continuing._2.link
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
                        builder = TallyTx.Build(config)(
                          _continuingVoteUtxo = TallyVoteUtxo(continuing._1),
                          _removedVoteUtxo = TallyVoteUtxo(removed._1),
                          _treasuryUtxo = treasuryUtxo,
                          _collateralUtxo = collateralUtxo
                        )
                        tallyTx <- builder.result match {
                            case Left(e) => EitherT.liftF(IO.raiseError(Error.BuildError.Tally(e)))
                            case Right(tx) => EitherT.right(IO.pure(tx))
                        }
                        _ <- signAndSubmitTx(tallyTx.tx)
                    } yield ()

                // Resolve
                case (None, lastVoteUtxo) if lastVoteUtxo.length == 1 =>
                    for {
                        resolutionTx <- ResolutionTx
                            .Build(config)(
                              _talliedVoteUtxo = TallyVoteUtxo(lastVoteUtxo.head._1),
                              _treasuryUtxo = treasuryUtxo,
                              _collateralUtxo = collateralUtxo
                            )
                            .result match {
                            case Left(e) =>
                                EitherT.liftF(IO.raiseError(Error.BuildError.Resolution(e)))
                            case Right(tx) => EitherT.right(IO.pure(tx))
                        }
                        _ <- signAndSubmitTx(resolutionTx.tx)
                    } yield ()

                // This should not be able to happen. If the treasury is unresolved,
                // then there must be at least one vote according to the spec.
                // If the treasury is resolved, we should short circuit with a left when the treasury
                // utxo is parsed.
                case (None, _) => EitherT.liftF(IO.raiseError(Error.TreasuryUnresolvedButNoVotes))
            }

        } yield ()
        et.value
    }

    override def preStart: IO[Unit] =
        context.setReceiveTimeout(config.evacuationBotPollingPeriod, ())

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
      *   vote utxos with cast votes.
      */
    private def parseDisputeUtxos(utxos: Utxos): IO[
      (
          Option[VoteUtxoWithDatum],
          Seq[VoteUtxoWithDatum]
      )
    ] =
        for {
            voteUtxos <- utxos.toList.traverse((i, o) => utxoToVoteUtxo(Utxo(i, o)))

            votePartition = voteUtxos.partition {
                case (_, VoteDatum(_, _, VoteStatus.AwaitingVote(peerPkh))) => {
                    val ownPkh: PubKeyHash = config.ownHeadWallet.pubKeyHash
                    peerPkh == ownPkh
                }
                case _ => false
            }
        } yield (votePartition._1.headOption, votePartition._2)

    private def utxoToVoteUtxo(
        utxo: Utxo
    ): IO[VoteUtxoWithDatum] = {
        for {
            d1: DatumOption <- utxo._2.datumOption match {
                case None    => IO.raiseError(DisputeActor.Error.ParseError.Vote.MissingDatum(utxo))
                case Some(x) => IO.pure(x)
            }
            d2: Inline <- d1 match {
                case i: Inline => IO.pure(i)
                case _ => IO.raiseError(DisputeActor.Error.ParseError.Vote.DatumNotInline(utxo))
            }
            d3: VoteDatum <- Try(fromData[VoteDatum](d2.data)) match {
                case Success(d) => IO.pure(d)
                case Failure(e) =>
                    IO.raiseError(
                      DisputeActor.Error.ParseError.Vote.DatumDeserializationError(utxo, e)
                    )
            }
        } yield (utxo, d3)
    }

}

object DisputeActor {
    type Config = NodePrivateConfig.Section & CardanoNetwork.Section & HeadPeers.Section &
        HasTokenNames & ScriptReferenceUtxos.Section

    type Handle = ActorRef[IO, Requests.Request]

    object Requests {
        type Request = HandleDisputeRes

        // Placeholder, I'm not sure if we need any additional state here
        type HandleDisputeRes = Unit
    }

    object Error {
        type RecoverableErrors = Recoverable
        sealed trait Recoverable
        case class RecoverableCardanoBackendError(
            wrapped: CardanoBackend.Error
        ) extends Recoverable

        type UnrecoverableErrors = Unrecoverable
        sealed trait Unrecoverable extends Throwable
        case object TreasuryUnresolvedButNoVotes extends Unrecoverable
        case class NoCompatibleVoteForTallyingFound(voteUtxos: Seq[(Utxo, VoteDatum)])
            extends Unrecoverable {
            override def getMessage: String =
                s"No compatible vote utxo with key ${voteUtxos.head._2.link} found. Datums found: " +
                    s"${voteUtxos.map { _._2 }}"
        }
        case class UnrecoverableCardanoBackendError(
            wrapped: CardanoBackend.Error
        ) extends Unrecoverable {
            override val getMessage: String = wrapped.getMessage
            override def toString: String = getMessage
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

        }

        sealed trait ParseError
        object ParseError {
            object Vote {
                case class MissingDatum(utxo: Utxo) extends Unrecoverable

                case class DatumNotInline(utxo: Utxo) extends Unrecoverable

                case class DatumDeserializationError(utxo: Utxo, e: Throwable) extends Unrecoverable
            }

            object Treasury {
                case class MultipleTreasuryTokensFound(utxos: Utxos) extends Unrecoverable

                case class WrappedTreasuryParseError(wrapped: RuleBasedTreasuryUtxo.ParseError)
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
    // TODO: Factor out. Its shared between this and the li
    // obtained from the parameters to this class
    def parseRBTreasury(
        utxos: Utxos
    ): EitherT[IO, Error.RecoverableErrors, RuleBasedTreasuryUtxo] =
        for {
            utxo <- utxos.size match {
                // May happen due to rollback, ignore and try again
                case 0 => EitherT.left(IO.pure(Error.ParseError.Treasury.TreasuryMissing))
                case 1 => EitherT.right(IO.pure(Utxo(utxos.head)))
                case _ =>
                    EitherT.liftF(
                      IO.raiseError(Error.ParseError.Treasury.MultipleTreasuryTokensFound(utxos))
                    )
            }

            rbTreasury <- EitherT.right(IO.fromEither(RuleBasedTreasuryUtxo.parse(utxo)))
            unresolvedRbt <- rbTreasury.datum match {
                case RuleBasedTreasuryDatum.Unresolved(_) => EitherT.right(IO.pure(rbTreasury))
                case RuleBasedTreasuryDatum.Resolved(_)   => EitherT.left(IO.pure(TreasuryResolved))
            }
        } yield unresolvedRbt
}
