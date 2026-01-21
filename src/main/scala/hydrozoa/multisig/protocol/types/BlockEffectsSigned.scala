package hydrozoa.multisig.protocol.types

import hydrozoa.multisig.ledger.dapp.tx.{DeinitTx, FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx}
import hydrozoa.multisig.protocol.types.AckBlock.HeaderSignature

/** The counterpart to [[BlockEffects]] but with ALL signatures in place along with the signed data.
  * There are two types of signatures involved:
  *   - tx signatures (vkey witnesses)
  *   - header signatures [[HeaderSignature]] which are the same ed25519 signatures over hash of the
  *     onchain representation of [[Header.Minor]], see [[OnchainBlockHeader]]
  *
  * @param blockType
  */
sealed trait BlockEffectsSigned {
    def blockType: Block.Type
    def blockNum: Block.Number
}

object BlockEffectsSigned {

    final case class Initial(
        override val blockNum: Block.Number,
        initialSigned: InitializationTx,
        fallbackSigned: FallbackTx,
    ) extends BlockEffectsSigned {
        override val blockType: Block.Type = Block.Type.Initial
    }

    final case class Minor(
        override val blockNum: Block.Number,
        header: Block.Header.Minor,
        // Verified header signatures
        headerSignatures: Set[HeaderSignature],
        postDatedRefundsSigned: List[RefundTx.PostDated],
    ) extends BlockEffectsSigned {
        override val blockType: Block.Type = Block.Type.Minor
    }

    final case class Major(
        override val blockNum: Block.Number,
        settlementSigned: SettlementTx,
        fallbackSigned: FallbackTx,
        rolloutsSigned: List[RolloutTx],
        postDatedRefundsSigned: List[RefundTx.PostDated],
    ) extends BlockEffectsSigned {
        override val blockType: Block.Type = Block.Type.Major
    }

    final case class Final(
        override val blockNum: Block.Number,
        rolloutsSigned: List[RolloutTx],
        mbDeinitSigned: Option[DeinitTx],
        finalizationSigned: FinalizationTx,
    ) extends BlockEffectsSigned {
        override val blockType: Block.Type = Block.Type.Final
    }

    type Next = Minor | Major | Final
}
