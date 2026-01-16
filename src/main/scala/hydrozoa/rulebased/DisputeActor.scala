package hydrozoa.rulebased

import cats.*
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.{Actor, Receive}
import com.suprnation.actor.ActorRef.ActorRef
import hydrozoa.*
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.multisig.ledger.dapp.tx.Tx
import hydrozoa.multisig.protocol.types.AckBlock.HeaderSignature
import hydrozoa.rulebased.DisputeActor.*
import hydrozoa.rulebased.DisputeActor.Error.*
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionValidator.OnchainBlockHeader
import hydrozoa.rulebased.ledger.dapp.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import hydrozoa.rulebased.ledger.dapp.state.TreasuryState.RuleBasedTreasuryDatum
import hydrozoa.rulebased.ledger.dapp.state.VoteState.{VoteDatum, VoteStatus}
import hydrozoa.rulebased.ledger.dapp.tx.ResolutionTx.ResolutionTxError
import hydrozoa.rulebased.ledger.dapp.tx.TallyTx.TallyTxError
import hydrozoa.rulebased.ledger.dapp.tx.VoteTx.VoteTxError
import hydrozoa.rulebased.ledger.dapp.tx.{ResolutionTx, TallyTx, VoteTx}
import hydrozoa.rulebased.ledger.dapp.utxo.{OwnVoteUtxo, RuleBasedTreasuryUtxo, TallyVoteUtxo}
import scala.util.{Failure, Success, Try}
import scalus.builtin.Data.fromData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.{DatumOption, Slot}
import scalus.cardano.txbuilder.SomeBuildError

// QUESTION: The `OwnVoteUtxo` type is pretty sparse. Should I augment it directly, or did we want to keep
// it small for some reason?
type VoteUtxoWithDatum = (hydrozoa.Utxo[L1], VoteDatum)

/**
 * Returned from the DisputeActor when all vote utxos are processed, and we can proceed with liquidation
  */
case object StartLiquidation

final case class DisputeActor(
    collateralUtxo: hydrozoa.Utxo[L1],
    blockHeader: OnchainBlockHeader,
    signatures: List[HeaderSignature],
    validityEndSlot: Slot,
    ownPeerPkh: VerificationKeyBytes,
    config: Tx.Builder.Config,
    tokenNames: TokenNames,
    headMultisigScript: HeadMultisigScript,
    cardanoBackend: CardanoBackend[IO],
    utxosToWithdrawL2: UtxoSetL2,
) extends Actor[IO, DisputeActor.Requests.Request] {

    /** Throws if the cardano backend gives an error. We can't really recover from this. */
    private val getDisputeUtxos: IO[hydrozoa.UtxoSetL1] =
        for {
            eUtxos <- cardanoBackend.utxosAt(
              address = DisputeResolutionScript.address(config.env.network),
              asset = (headMultisigScript.policyId, tokenNames.voteTokenName)
            )
            utxos <- IO.fromEither(eUtxos)
        } yield utxos

    /** Throws if the cardano backend gives an error. We can't really recover from this. */
    private val getRulesBasedTreasuryUtxo: IO[hydrozoa.Utxo[L1]] =
        for {
            eUtxos <- cardanoBackend.utxosAt(
              address = RuleBasedTreasuryScript.address(config.env.network),
              asset = (headMultisigScript.policyId, tokenNames.headTokenName)
            )
            utxos <- IO.fromEither(eUtxos)
            utxo <-
                if utxos.toList.length == 1
                then IO.pure(hydrozoa.Utxo[L1](utxos.head))
                // FIXME: type this better
                else
                    IO.raiseError(
                      RuntimeException("Multiple rule-based treasury utxo tokens found")
                    )
        } yield utxo

    private val handleDisputeRes: IO[Unit | StartLiquidation.type] = {
        for {
            disputeUtxos <- getDisputeUtxos.flatMap(utxos =>
                IO.fromEither(parseDisputeUtxos(utxos))
            )
            treasuryUtxo <- getRulesBasedTreasuryUtxo.flatMap(utxo =>
                IO.fromEither(parseRBTreasury(utxo))
            )
            _ <- disputeUtxos match {
                // Cast Vote
                case (Some(ownVoteUtxo), _) =>
                    val recipe = VoteTx.Recipe(
                      voteUtxo = OwnVoteUtxo(ownPeerPkh.verKeyHash, ownVoteUtxo._1),
                      treasuryUtxo = treasuryUtxo,
                      collateralUtxo = collateralUtxo,
                      blockHeader = blockHeader,
                      signatures = signatures,
                      validityEndSlot = validityEndSlot,
                      network = config.env.network,
                      protocolParams = config.env.protocolParams,
                      evaluator = config.evaluator,
                      validators = config.validators
                    )
                    for {
                        // FIXME: if we make the error typesThrowable, we can call IO.fromEither
                        // instead
                        voteTx <- VoteTx.build(recipe) match {
                            case Left(e: SomeBuildError) => IO.raiseError(e.reason)
                            case Left(e: VoteTxError) =>
                                IO.raiseError(new RuntimeException(s"error building VoteTx: ${e}"))
                            case Right(tx) => IO.pure(tx)
                        }
                        eUnit <- cardanoBackend.submitTx(voteTx.tx)
                        _ <- IO.fromEither(eUnit)
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
                      validityEndSlot = validityEndSlot,
                      network = config.env.network,
                      protocolParams = config.env.protocolParams,
                      evaluator = config.evaluator,
                      validators = config.validators
                    )
                    for {
                        // FIXME: if we make the error typesThrowable, we can call IO.fromEither
                        // instead
                        tallyTx <- TallyTx.build(recipe) match {
                            case Left(e: SomeBuildError) => IO.raiseError(e.reason)
                            case Left(e: TallyTxError) =>
                                IO.raiseError(new RuntimeException(s"error building tallyTx: ${e}"))
                            case Right(tx) => IO.pure(tx)
                        }
                        eUnit <- cardanoBackend.submitTx(tallyTx.tx)
                        _ <- IO.fromEither(eUnit)
                    } yield ()

                // Resolve
                case (None, lastVoteUtxo) if lastVoteUtxo.length == 1 =>
                    val recipe = ResolutionTx.Recipe(
                      talliedVoteUtxo = TallyVoteUtxo(lastVoteUtxo.head._1),
                      treasuryUtxo = treasuryUtxo,
                      collateralUtxo = collateralUtxo,
                      validityEndSlot = validityEndSlot,
                      network = config.env.network,
                      protocolParams = config.env.protocolParams,
                      evaluator = config.evaluator,
                      validators = config.validators
                    )
                    for {
                        // FIXME: if we make the error typesThrowable, we can call IO.fromEither
                        // instead
                        resolutionTx <- ResolutionTx.build(recipe) match {
                            case Left(e: SomeBuildError) => IO.raiseError(e.reason)
                            case Left(e: ResolutionTxError) =>
                                IO.raiseError(
                                  new RuntimeException(s"error building ResolutionTx: ${e}")
                                )
                            case Right(tx) => IO.pure(tx)
                        }
                        eUnit <- cardanoBackend.submitTx(resolutionTx.tx)
                        _ <- IO.fromEither(eUnit)
                    } yield ()

                // This case only matches in the returned list of vote utxos is empty.
                // This means we're all done with dispute resolution; start liquidation
                case (None, _) => IO.pure(StartLiquidation)
            }
        } yield ()
    }

    override def receive: Receive[IO, Requests.Request] = { case _: Requests.HandleDisputeRes =>
        handleDisputeRes
    }

    /** Queries the cardano backend for all utxos at the dispute resolution address, and then parses
      * them. This will tell us whether the peer's empty [[OwnVoteUtxo]] is present, or whether it
      * is ready for tallying.
      *
      * Assumptions
      *   - We don't have any extra vote utxos that will validly parse. I.e., we don't check the
      *     number of utxos given to this function
      *   - We have all of the vote utxos that exist at the time of the query; we're not missing any
      *   - Each utxo has a correct transaction input according to the results of the querty
      *   - Each utxo has the correct vote token in it (I assume this is how the query layer
      *     identifies the utxos in the first place, so I skip that check here) and sits at the
      *     correct adddress.
      *
      * @return
      *   A tuple of [[VoteUtxoWithDatum]]. The first element is wrapped in an option, and is only
      *   "Some" if the dispute actor still needs to cast a vote. The second element is all other
      *   vote utxos.
      */
    private def parseDisputeUtxos(utxos: hydrozoa.UtxoSetL1): Either[
      Error.ParseError,
      (
          Option[VoteUtxoWithDatum] // Own vote utxo
          ,
          Seq[VoteUtxoWithDatum]
      ) // Other vote utxos
    ] =
        for {
            voteUtxos <- utxos.toList.traverse((i, o) => utxoToVoteUtxo(hydrozoa.Utxo[L1](i, o)))

            votePartition = voteUtxos.partition {
                case (_, VoteDatum(_, _, VoteStatus.AwaitingVote(peerPkh))) =>
                    peerPkh == ownPeerPkh
                case _ => false
            }
        } yield (votePartition._1.headOption, votePartition._2)

    private def utxoToVoteUtxo(
        utxo: hydrozoa.Utxo[L1]
    ): Either[Error.ParseError, VoteUtxoWithDatum] = {
        for {
            d1: DatumOption <- utxo._2.datumOption.toRight(
              DisputeActor.Error.MissingVoteUtxoDatum(utxo)
            )
            d2: Inline <- d1 match {
                case i: Inline => Right(i)
                case _         => Left(DisputeActor.Error.VoteDatumNotInline(utxo))
            }
            d3: VoteDatum <- Try(fromData[VoteDatum](d2.data)) match {
                case Success(d) => Right(d)
                case Failure(e) => Left(VoteDatumDeserializationError(utxo, e))
            }
        } yield (utxo, d3)
    }

    /** Assumptions:
      *   - The treasury exists at the correct address and has the correct token
      *
      * @param utxo
      * @return
      */
    // TODO: Maybe this should live in a companion object for RulesBasedTreasuryUtxo?
    private def parseRBTreasury(utxo: hydrozoa.Utxo[L1]): Either[Error.ParseError, RuleBasedTreasuryUtxo] =
        for {
            d1 <- utxo.output.datumOption.toRight(TreasuryDatumMissing(utxo))
            d2 <- d1 match {
                case i: Inline => Right(i)
                case _         => Left(TreasuryDatumNotInline(utxo))
            }
            datum <- Try(fromData[RuleBasedTreasuryDatum](d2.data)) match {
                case Success(d) => Right(d)
                case Failure(e) => Left(TreasuryDatumDeserializationError(utxo, e))
            }

            address <- utxo.output.address match {
                case sa: ShelleyAddress => Right(sa)
                case _                  => Left(TreasuryAddressNotShelley(utxo))
            }

        } yield RuleBasedTreasuryUtxo(
          treasuryTokenName = tokenNames.headTokenName,
          utxoId = utxo.input,
          address = address,
          datum = datum,
          value = utxo.output.value
        )
}

object DisputeActor {
    type Handle = ActorRef[IO, Requests.Request]

    object Requests {
        type Request = HandleDisputeRes

        // Placeholder, I'm not sure if we need any additional state here
        type HandleDisputeRes = Unit
    }

    object Error {
        sealed trait ParseError extends Throwable

        case class MissingVoteUtxoDatum(utxo: hydrozoa.Utxo[L1]) extends ParseError
        case class VoteDatumNotInline(utxo: hydrozoa.Utxo[L1]) extends ParseError
        case class VoteDatumDeserializationError(utxo: hydrozoa.Utxo[L1], e: Throwable)
            extends ParseError
        case class TreasuryDatumMissing(utxo: hydrozoa.Utxo[L1]) extends ParseError
        case class TreasuryDatumNotInline(utxo: hydrozoa.Utxo[L1]) extends ParseError
        case class TreasuryDatumDeserializationError(utxo: hydrozoa.Utxo[L1], e: Throwable)
            extends ParseError
        case class TreasuryAddressNotShelley(utxo: hydrozoa.Utxo[L1]) extends ParseError
    }

    object Phase:
        sealed trait Phase
        case object NeedToVote
        case object NeedToTally

}
