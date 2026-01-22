package hydrozoa.multisig.protocol.types

import hydrozoa.multisig.ledger.dapp.tx.*

/** [[BlockEffects]] ALWAYS contains UNSIGNED transactions. Compare to [[BlockEffectsSigned]].
  * Informally: "things that potentially need to be sent out to affect the outside world, usually
  * needing to be signed beforehand".
  *
  * NB: There is no notion of UNSIGNED block effects for the initial block, since effects for it
  * come signed from the config (see [[BlockEffectsSigned]])
  *
  * @param blockType
  */
sealed trait BlockEffects {
    def blockType: Block.Type
    def id: Block.Number
}

object BlockEffects {

    final case class Minor(
        override val id: Block.Number,
        header: Block.Header.Minor,
        postDatedRefunds: List[RefundTx.PostDated]
    ) extends BlockEffects {
        override val blockType: Block.Type = Block.Type.Minor
    }

    final case class Major(
        override val id: Block.Number,
        settlement: SettlementTx,
        rollouts: List[RolloutTx],
        fallback: FallbackTx,
        postDatedRefunds: List[RefundTx.PostDated]
    ) extends BlockEffects {
        override val blockType: Block.Type = Block.Type.Major
    }

    final case class Final(
        override val id: Block.Number,
        finalization: FinalizationTx,
        rollouts: List[RolloutTx],
        deinit: Option[DeinitTx]
    ) extends BlockEffects {
        override val blockType: Block.Type = Block.Type.Final
    }
}
