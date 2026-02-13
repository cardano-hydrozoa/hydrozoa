package hydrozoa.multisig.ledger.block

import hydrozoa.multisig.ledger.block.BlockHeader.Minor.HeaderSignature
import hydrozoa.multisig.ledger.dapp.tx.{FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx}

sealed trait Block extends Block.Section

object Block {
    sealed trait Unsigned extends Block, BlockStatus.Unsigned

    object Unsigned {
        final case class Initial(
            override val blockBrief: BlockBrief.Initial,
            override val effects: BlockEffects.Unsigned.Initial,
        ) extends Block.Unsigned,
              BlockType.Initial,
              BlockEffects.Initial.Section {
            override transparent inline def block: Block.Unsigned.Initial = this

            override transparent inline def header: BlockHeader.Initial = blockBrief.header
            override transparent inline def body: BlockBody.Initial.type = blockBrief.body

            override transparent inline def initializationTx: InitializationTx =
                effects.initializationTx
            override transparent inline def fallbackTx: FallbackTx = effects.fallbackTx
        }

        final case class Minor(
            override val blockBrief: BlockBrief.Minor,
            override val effects: BlockEffects.Unsigned.Minor,
        ) extends Block.Unsigned,
              BlockType.Minor,
              BlockEffects.Minor.Section {
            override transparent inline def block: Block.Unsigned.Minor = this

            override transparent inline def header: BlockHeader.Minor = blockBrief.header
            override transparent inline def body: BlockBody.Minor = blockBrief.body

            override transparent inline def headerSerialized: BlockHeader.Minor.Onchain.Serialized =
                effects.headerSerialized
            override transparent inline def postDatedRefundTxs: List[RefundTx.PostDated] =
                effects.postDatedRefundTxs
        }

        final case class Major(
            override val blockBrief: BlockBrief.Major,
            override val effects: BlockEffects.Unsigned.Major,
        ) extends Block.Unsigned,
              BlockType.Major,
              BlockEffects.Major.Section {
            override transparent inline def block: Block.Unsigned.Major = this

            override transparent inline def header: BlockHeader.Major = blockBrief.header
            override transparent inline def body: BlockBody.Major = blockBrief.body

            override transparent inline def settlementTx: SettlementTx = effects.settlementTx
            override transparent inline def rolloutTxs: List[RolloutTx] = effects.rolloutTxs
            override transparent inline def fallbackTx: FallbackTx = effects.fallbackTx
            override transparent inline def postDatedRefundTxs: List[RefundTx.PostDated] =
                effects.postDatedRefundTxs
        }

        final case class Final(
            override val blockBrief: BlockBrief.Final,
            override val effects: BlockEffects.Unsigned.Final,
        ) extends Block.Unsigned,
              BlockType.Final,
              BlockEffects.Final.Section {

            override transparent inline def block: Block.Unsigned.Final = this

            override transparent inline def header: BlockHeader.Final = blockBrief.header
            override transparent inline def body: BlockBody.Final = blockBrief.body

            override transparent inline def finalizationTx: FinalizationTx = effects.finalizationTx
            override transparent inline def rolloutTxs: List[RolloutTx] = effects.rolloutTxs
        }

        type Next = Block.Unsigned & BlockType.Next
        type Intermediate = Block.Unsigned & BlockType.Intermediate

        extension (self: Next)
            transparent inline def blockBriefNext: BlockBrief.Next =
                self.blockBrief.asInstanceOf[BlockBrief.Next]
    }

    sealed trait MultiSigned extends Block, BlockStatus.MultiSigned, Fields.HasFinalizationRequested

    object MultiSigned {
        final case class Initial(
            override val blockBrief: BlockBrief.Initial,
            override val effects: BlockEffects.MultiSigned.Initial,
        ) extends Block.MultiSigned,
              BlockType.Initial,
              BlockEffects.MultiSigned.Initial.Section {
            override transparent inline def block: Block.MultiSigned.Initial = this

            override transparent inline def header: BlockHeader.Initial = blockBrief.header
            override transparent inline def body: BlockBody.Initial.type = blockBrief.body

            override transparent inline def initializationTx: InitializationTx =
                effects.initializationTx
            override transparent inline def fallbackTx: FallbackTx = effects.fallbackTx

            override transparent inline def finalizationRequested: Boolean = false
        }

        final case class Minor(
            override val blockBrief: BlockBrief.Minor,
            override val effects: BlockEffects.MultiSigned.Minor,
            override val finalizationRequested: Boolean,
        ) extends Block.MultiSigned,
              BlockType.Minor,
              BlockEffects.MultiSigned.Minor.Section {
            override transparent inline def block: Block.MultiSigned.Minor = this

            override transparent inline def header: BlockHeader.Minor = blockBrief.header
            override transparent inline def body: BlockBody.Minor = blockBrief.body

            override transparent inline def headerSerialized: BlockHeader.Minor.Onchain.Serialized =
                effects.headerSerialized
            override transparent inline def headerMultiSigned: List[HeaderSignature] =
                effects.headerMultiSigned
            override transparent inline def postDatedRefundTxs: List[RefundTx.PostDated] =
                effects.postDatedRefundTxs
        }

        final case class Major(
            override val blockBrief: BlockBrief.Major,
            override val effects: BlockEffects.MultiSigned.Major,
            override val finalizationRequested: Boolean,
        ) extends Block.MultiSigned,
              BlockType.Major,
              BlockEffects.MultiSigned.Major.Section {
            override transparent inline def block: Block.MultiSigned.Major = this

            override transparent inline def header: BlockHeader.Major = blockBrief.header
            override transparent inline def body: BlockBody.Major = blockBrief.body

            override transparent inline def settlementTx: SettlementTx = effects.settlementTx
            override transparent inline def rolloutTxs: List[RolloutTx] = effects.rolloutTxs
            override transparent inline def fallbackTx: FallbackTx = effects.fallbackTx
            override transparent inline def postDatedRefundTxs: List[RefundTx.PostDated] =
                effects.postDatedRefundTxs
        }

        final case class Final(
            override val blockBrief: BlockBrief.Final,
            override val effects: BlockEffects.MultiSigned.Final,
        ) extends Block.MultiSigned,
              BlockType.Final,
              BlockEffects.MultiSigned.Final.Section {
            override transparent inline def block: Block.MultiSigned.Final = this

            override transparent inline def header: BlockHeader.Final = blockBrief.header
            override transparent inline def body: BlockBody.Final = blockBrief.body

            override transparent inline def finalizationTx: FinalizationTx = effects.finalizationTx
            override transparent inline def rolloutTxs: List[RolloutTx] = effects.rolloutTxs

            override transparent inline def finalizationRequested: Boolean = false
        }

        type Next = Block.MultiSigned & BlockType.Next

        type Intermediate = Block.MultiSigned & BlockType.Intermediate
    }

    object Fields {
        trait HasFinalizationRequested {
            def finalizationRequested: Boolean
        }
    }

    trait Section extends BlockType, BlockBrief.Section, BlockEffects.Section {
        def block: Block
    }
}
