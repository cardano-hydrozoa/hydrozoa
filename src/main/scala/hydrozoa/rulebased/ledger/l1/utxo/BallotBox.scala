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
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AddrKeyHash, Coin, MultiAsset, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{ExpectedSigner, ScriptSource, ThreeArgumentPlutusScriptWitness}
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData

type BallotBoxConfig = CardanoNetwork.Section & HeadPeers.Section & FallbackContingency.Section &
    HasTokenNames & OwnPeerPrivate.Section

final case class BallotBox[Status <: VoteStatus](
    input: TransactionInput,
    ballotBoxOutput: BallotBoxOutput[Status]
) {
    def toUtxo(using config: BallotBoxConfig): Utxo =
        Utxo(input, ballotBoxOutput.toOutput)

    def spend(redeemer: DisputeRedeemer)(using config: BallotBoxConfig): Spend = {
        val expectedSigner = ExpectedSigner(config.ownWallet.exportVerificationKey.addrKeyHash)
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

extension (unvoted: BallotBox[AwaitingVote]) {

    def votingSpend(redeemer: DisputeRedeemer)(using config: BallotBoxConfig): Spend = {
        val signers =
            if unvoted.ballotBoxOutput.key == BigInt(0) then
                // Public ballot box: synthetic all-zeros signer for fee estimation; on-chain check is skipped.
                Set(ExpectedSigner(AddrKeyHash(ByteString.fromArray(new Array[Byte](28)))))
            else
                Set(
                  ExpectedSigner(
                    AddrKeyHash(
                      unvoted.ballotBoxOutput.datum.voteStatus.asInstanceOf[AwaitingVote].peer.hash
                    )
                  )
                )
        Spend(
          unvoted.toUtxo,
          ThreeArgumentPlutusScriptWitness(
            scriptSource = ScriptSource.PlutusScriptAttached,
            redeemer = redeemer.toData,
            datum = DatumInlined,
            additionalSigners = signers
          )
        )
    }
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

    def voterAddrKeyHash: AddrKeyHash =
        AddrKeyHash(uncastVote.status.peer.hash)
}
