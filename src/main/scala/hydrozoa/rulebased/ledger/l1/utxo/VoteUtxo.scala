package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.config.node.owninfo.OwnHeadPeerPrivate
import hydrozoa.lib.cardano.scalus.VerificationKeyExtra.*
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionValidator.DisputeRedeemer
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.AwaitingVote
import hydrozoa.rulebased.ledger.l1.state.VoteState.{KzgCommitment, VoteDatum, VoteStatus, given}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AddrKeyHash, Coin, MultiAsset, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{ExpectedSigner, ScriptSource, ThreeArgumentPlutusScriptWitness}
import scalus.uplc.builtin.Data.toData

type VoteOutputConfig = CardanoNetwork.Section & HeadPeers.Section & FallbackContingency.Section &
    HasTokenNames & OwnHeadPeerPrivate.Section

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
          address = DisputeResolutionScript.address(config.network),
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
