package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnPeerPrivate
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.*
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.DisputeRedeemer
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.AwaitingVote
import hydrozoa.rulebased.ledger.l1.state.VoteState.{KzgCommitment, VoteDatum, VoteStatus, given}
import scala.util.{Failure, Success, Try}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AddrKeyHash, Coin, MultiAsset, TransactionInput, Utxo, Value}
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{ScriptSource, ThreeArgumentPlutusScriptWitness}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.{fromData, toData}

type BallotBoxConfig = CardanoNetwork.Section & HeadPeers.Section & FallbackContingency.Section &
    HasTokenNames & OwnPeerPrivate.Section

object BallotBox {

    type ParseConfig = HeadPeers.Section & HasTokenNames

    sealed trait ParseError extends Throwable {}

    object ParseError {
        case class MissingDatum(utxo: Utxo) extends ParseError {
            override def getMessage: String = s"BallotBox UTxO ${utxo.input} has no datum"
        }
        case class DatumNotInline(utxo: Utxo) extends ParseError {
            override def getMessage: String = s"BallotBox UTxO ${utxo.input} datum is not inline"
        }
        case class DatumDeserializationError(utxo: Utxo, cause: Throwable) extends ParseError {
            override def getMessage: String =
                s"Failed to deserialize VoteDatum for UTxO ${utxo.input}: ${cause.getMessage}"
        }
        case class MissingVoteToken(utxo: Utxo) extends ParseError {
            override def getMessage: String =
                s"BallotBox UTxO ${utxo.input} does not carry a Vote token"
        }
        case class InvalidTokenCount(utxo: Utxo, count: Long, msg: String) extends ParseError {
            override def getMessage: String =
                s"BallotBox UTxO ${utxo.input} has invalid Vote token count $count: $msg"
        }
    }

    def parse(utxo: Utxo)(using config: ParseConfig): Either[ParseError, BallotBox[VoteStatus]] =
        for {
            datumOpt <- utxo.output.datumOption.toRight(ParseError.MissingDatum(utxo))
            inline <- datumOpt match {
                case i: Inline => Right(i)
                case _         => Left(ParseError.DatumNotInline(utxo))
            }
            voteDatum <- Try(fromData[VoteDatum](inline.data)) match {
                case Success(d) => Right(d)
                case Failure(e) => Left(ParseError.DatumDeserializationError(utxo, e))
            }
            tokenCount <- utxo.output.value.assets.assets
                .get(config.headMultisigScript.policyId)
                .flatMap(_.get(config.headTokenNames.voteTokenName))
                .toRight(ParseError.MissingVoteToken(utxo))
            voteTokens <- PositiveInt
                .apply(tokenCount.toInt)
                .toRight(
                  ParseError.InvalidTokenCount(
                    utxo,
                    tokenCount.toInt,
                    "BallotBox had less than 1 vote token"
                  )
                )
        } yield BallotBox(
          input = utxo.input,
          ballotBoxOutput = BallotBoxOutput(
            key = voteDatum.key,
            link = voteDatum.link,
            coin = utxo.output.value.coin,
            voteTokens = voteTokens,
            status = voteDatum.voteStatus
          )
        )
}

final case class BallotBox[Status <: VoteStatus](
    input: TransactionInput,
    ballotBoxOutput: BallotBoxOutput[Status]
) {
    def toUtxo(using config: BallotBoxConfig): Utxo =
        Utxo(input, ballotBoxOutput.toOutput)

    /** Verification-key hash this spend is expected to be signed by, for witness-set fee sizing.
      * Inject into the build context via `addExpectedSigners` before finalizing.
      */
    def spendSigners(using config: BallotBoxConfig): Set[AddrKeyHash] =
        Set(config.ownWallet.exportVerificationKey.addrKeyHash)

    def spend(redeemer: DisputeRedeemer)(using config: BallotBoxConfig): Spend =
        Spend(
          this.toUtxo,
          ThreeArgumentPlutusScriptWitness(
            scriptSource = ScriptSource.PlutusScriptAttached,
            redeemer = redeemer.toData,
            datum = DatumInlined
          )
        )
}

extension (unvoted: BallotBox[AwaitingVote]) {

    /** Signer expected for a vote spend, for witness-set fee sizing. Inject into the build context
      * via `addExpectedSigners` before finalizing.
      */
    def votingSigners(using config: BallotBoxConfig): Set[AddrKeyHash] =
        if unvoted.ballotBoxOutput.key == BigInt(0) then
            // Public ballot box: synthetic all-zeros signer for fee estimation; on-chain check is skipped.
            Set(AddrKeyHash(ByteString.fromArray(new Array[Byte](28))))
        else
            Set(
              AddrKeyHash(
                unvoted.ballotBoxOutput.datum.voteStatus.asInstanceOf[AwaitingVote].peer.hash
              )
            )

    def votingSpend(redeemer: DisputeRedeemer)(using config: BallotBoxConfig): Spend =
        Spend(
          unvoted.toUtxo,
          ThreeArgumentPlutusScriptWitness(
            scriptSource = ScriptSource.PlutusScriptAttached,
            redeemer = redeemer.toData,
            datum = DatumInlined
          )
        )
}

// TODO: Coin seems like it must be either the default vote contingency, individual vote contingency, or
// some leftover amount after combining the two (and possibly paying the fee). Can we/should we restrict the type
// any more here?
case class BallotBoxOutput[Status <: VoteStatus](
    key: VoteState.Key,
    link: VoteState.Link,
    coin: Coin,
    voteTokens: PositiveInt,
    status: Status
) {
    val datum: VoteDatum = VoteDatum(key = key, link = link, voteStatus = status)

    def send(using config: BallotBoxConfig): Send = Send(this.toOutput)

    def toOutput(using config: BallotBoxConfig): Babbage =

        Babbage(
          address = HydrozoaBlueprint.mkDisputeAddress(config.network),
          value = Value(
            coin = coin,
            assets = MultiAsset
                .asset(
                  config.headMultisigScript.policyId,
                  config.headTokenNames.voteTokenName,
                  voteTokens.toLong
                )
          ),
          datumOption = Some(Inline(datum.toData)),
          scriptRef = None
        )
}

extension (uncastVote: BallotBoxOutput[AwaitingVote]) {
    def castVote(
        kzgCommitment: KzgCommitment,
        versionMinor: BigInt
    ): BallotBoxOutput[VoteStatus.Voted] =
        uncastVote.copy(status = VoteStatus.Voted(kzgCommitment, versionMinor))

    def abstain: BallotBoxOutput[VoteStatus.Abstain.type] =
        uncastVote.copy(status = VoteStatus.Abstain)

    def voterAddrKeyHash: AddrKeyHash =
        AddrKeyHash(uncastVote.status.peer.hash)
}
