package hydrozoa.multisig.protocol

import cats.effect.IO
import com.suprnation.actor.ReplyingActorRef
import hydrozoa.multisig.ledger.dapp.utxo.{MultisigRegimeUtxo, ResidualTreasuryUtxo, RolloutUtxo, TreasuryUtxo}

object CardanoBackendProtocol {
    object CardanoBackend {
        type CardanoBackendRef = Ref
        type Ref = ReplyingActorRef[IO, Request, Response]

        type Request = SubmitL1Effects | GetCardanoHeadState

        type Response = GetCardanoHeadStateResp

        /** Submit L1 effects to the Cardano backend. The response from the backend is ignored, so
          * we model this as an async request in the pure model.
          */
        final case class SubmitL1Effects(
        )

        /** Get the head's current utxo state in Cardano. */
        final case class GetCardanoHeadState(
        )

        /** The head's current utxo state in Cardano, provided in response to
          * [[GetCardanoHeadState]].
          */
        final case class GetCardanoHeadStateResp(
            state: CardanoHeadMultisigState
        )

        /** The MS/RB division may need some clarifications here:
          *   - Since we are in `hydrozoa.multisig` package all this is supposed to be used in the
          *     MS regime.
          *   - To distinguish between some states we may need to query RB addresses/states as well,
          *     this is why some RB-related concepts percolates in the state.
          */
        final case class CardanoHeadMultisigState(
            // Treasury state:
            //  - absent, i.e. neither MS nor RB don't exist
            //  - locked in multisig head script along with multisig regime utxo
            //  - residual treasury with all head beacon tokens
            //  - rule-based treasury - TODO: can we get rid of it?
            treasuryState: CardanoTreasuryState,
            // Potentially, there may exist multiple unfinished rollout utxos.
            rolloutUtxos: List[RolloutUtxo]
        )

        trait CardanoTreasuryState

        /** Means: neither MS nor RB don't exist. To interpret this state we need to know whether
          * the finalization tx exists and was submitted to L1:
          *   - if it was, the head lifecycle is over, nothing to do
          *   - otherwise, the initialization didn't get through, the head is in the very beginning
          *     of its lifecycle
          */
        final case class NoTreasury() extends CardanoTreasuryState

        /** Means: the head is in the normal operation, with two utxos being locked at the multisig
          * head script address.
          */
        final case class RegularMultisigTreasury(
            treasuryUtxo: TreasuryUtxo,
            multisigUtxo: MultisigRegimeUtxo
        ) extends CardanoTreasuryState

        /** Means: Finalization tx consumed the treasury and the multisig regime utxo which now
          * waits to be consumed by the deinit tx.
          */
        final case class ResidualMultisigTreasury(
            residualTreasury: ResidualTreasuryUtxo
        ) extends CardanoTreasuryState

        /** Treasury was moved to the rule-based regime script. */
        final case class RuleBasedTreasury() extends CardanoTreasuryState
    }
}
