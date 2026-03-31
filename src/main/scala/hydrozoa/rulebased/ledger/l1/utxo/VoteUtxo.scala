package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.number.PositiveInt
import hydrozoa.multisig.ledger.l1.token.CIP67.HasTokenNames
import hydrozoa.rulebased.ledger.l1.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.l1.state.VoteState
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus.AwaitingVote
import hydrozoa.rulebased.ledger.l1.state.VoteState.{KzgCommitment, VoteDatum, VoteStatus}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AddrKeyHash, Coin, MultiAsset, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.uplc.builtin.Data.toData

type VoteOutputConfig = CardanoNetwork.Section & HeadPeers.Section & FallbackContingency.Section &
    HasTokenNames

final case class VoteUtxo[Status <: VoteStatus](
    input: TransactionInput,
    voteOutput: VoteOutput[Status]
) {
    def toUtxo(using config: VoteOutputConfig): Utxo =
        Utxo(input, voteOutput.toOutput)
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
