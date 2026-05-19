package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.HydrozoaBlueprint
import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
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
import scalus.cardano.ledger.{AddrKeyHash, Coin, MultiAsset, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{ExpectedSigner, ScriptSource, ThreeArgumentPlutusScriptWitness}
import scalus.uplc.builtin.Data.{fromData, toData}

type VoteOutputConfig = CardanoNetwork.Section & HeadPeers.Section & FallbackContingency.Section &
    HasTokenNames & OwnHeadPeerPrivate.Section

object VoteUtxo {

    type ParseConfig = HeadPeers.Section & HasTokenNames

    sealed trait ParseError extends Throwable {}

    object ParseError {
        case class MissingDatum(utxo: Utxo) extends ParseError {
            override def getMessage: String = s"Vote UTxO ${utxo.input} has no datum"
        }
        case class DatumNotInline(utxo: Utxo) extends ParseError {
            override def getMessage: String = s"Vote UTxO ${utxo.input} datum is not inline"
        }
        case class DatumDeserializationError(utxo: Utxo, cause: Throwable) extends ParseError {
            override def getMessage: String =
                s"Failed to deserialize VoteDatum for UTxO ${utxo.input}: ${cause.getMessage}"
        }
        case class MissingVoteToken(utxo: Utxo) extends ParseError {
            override def getMessage: String = s"Vote UTxO ${utxo.input} does not carry a vote token"
        }
        case class InvalidTokenCount(utxo: Utxo, count: Long, msg: String) extends ParseError {
            override def getMessage: String =
                s"Vote UTxO ${utxo.input} has invalid vote token count $count: $msg"
        }
    }

    def parse(utxo: Utxo)(using config: ParseConfig): Either[ParseError, VoteUtxo[VoteStatus]] =
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
            voteTokens = PositiveInt.unsafeApply(tokenCount.toInt)
        } yield VoteUtxo(
          input = utxo.input,
          voteOutput = VoteOutput(
            key = voteDatum.key,
            link = voteDatum.link,
            coin = utxo.output.value.coin,
            voteTokens = voteTokens,
            status = voteDatum.voteStatus
          )
        )
}

final case class VoteUtxo[Status <: VoteStatus](
    input: TransactionInput,
    voteOutput: VoteOutput[Status]
) {
    def toUtxo(using config: VoteOutputConfig): Utxo =
        Utxo(input, voteOutput.toOutput)

    def spend(redeemer: DisputeRedeemer)(using config: VoteOutputConfig): Spend = {
        val expectedSigner = ExpectedSigner(config.ownHeadWallet.exportVerificationKey.addrKeyHash)
        Spend(
          this.toUtxo,
          ThreeArgumentPlutusScriptWitness(
            scriptSource = ScriptSource.PlutusScriptAttached,
            redeemer = redeemer.toData,
            datum = DatumInlined,
            additionalSigners = Set(expectedSigner)
          )
        )
    }
}

extension (unvoted: VoteUtxo[AwaitingVote]) {

    /** If you're spending in order to vote (rather than tally or resolve), we must have the voter's
      * signature. Otherwise the dispute resolution script will fail.
      */
    def votingSpend(redeemer: DisputeRedeemer)(using config: VoteOutputConfig): Spend = {
        val expectedSigner =
            ExpectedSigner(
              AddrKeyHash(unvoted.voteOutput.datum.voteStatus.asInstanceOf[AwaitingVote].peer.hash)
            )
        Spend(
          unvoted.toUtxo,
          ThreeArgumentPlutusScriptWitness(
            scriptSource = ScriptSource.PlutusScriptAttached,
            redeemer = redeemer.toData,
            datum = DatumInlined,
            additionalSigners = Set(expectedSigner)
          )
        )
    }
}

// TODO: Coin seems like it must be either the default vote contingency, individual vote contingency, or
// some leftover amount after combining the two (and possibly paying the fee). Can we/should we restrict the type
// any more here?
case class VoteOutput[Status <: VoteStatus](
    key: VoteState.Key,
    link: VoteState.Link,
    coin: Coin,
    voteTokens: PositiveInt,
    status: Status
) {
    val datum: VoteDatum = VoteDatum(key = key, link = link, voteStatus = status)

    def send(using config: VoteOutputConfig): Send = Send(this.toOutput)

    def toOutput(using config: VoteOutputConfig): Babbage =

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

extension (uncastVote: VoteOutput[AwaitingVote]) {
    def castVote(kzgCommitment: KzgCommitment, versionMinor: BigInt): VoteOutput[VoteStatus.Voted] =
        uncastVote.copy(status = VoteStatus.Voted(kzgCommitment, versionMinor))

    def voterAddrKeyHash: AddrKeyHash =
        AddrKeyHash(uncastVote.status.peer.hash)
}
