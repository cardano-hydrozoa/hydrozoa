package hydrozoa.multisig.protocol.types

import hydrozoa.multisig.ledger.dapp.tx.*

enum BlockEffects(val blockType: Block.Type) {
    def id: Block.Number

    case Initial (
        override val id: Block.Number,
        override val initialization: InitializationTx
    ) extends BlockEffects(Block.Type.Initial), BlockEffects.Fields.Initial

    case Minor (
        override val id: Block.Number,
        override val immediateRefunds: List[RefundTx.Immediate],
        override val postDatedRefunds: List[RefundTx.PostDated]
    ) extends BlockEffects(Block.Type.Minor), BlockEffects.Fields.Minor

    case Major (
        override val id: Block.Number,
        override val settlement: SettlementTx,
        override val rollouts: List[RolloutTx],
        override val fallback: FallbackTx,
        override val immediateRefunds: List[RefundTx.Immediate],
        override val postDatedRefunds: List[RefundTx.PostDated]
    ) extends BlockEffects(Block.Type.Major), BlockEffects.Fields.Major

    case Final (
        override val id: Block.Number,
        override val finalization: FinalizationTx,
        override val rollouts: List[RolloutTx],
        override val immediateRefunds: List[RefundTx.Immediate]
    ) extends BlockEffects(Block.Type.Final), BlockEffects.Fields.Final

}

object BlockEffects {
    type Next = BlockEffects.Minor | BlockEffects.Major | BlockEffects.Final
    
    object Fields {
        sealed trait Initial extends Initialization

        sealed trait Minor extends Refunds.Immediate, Refunds.PostDated

        sealed trait Major extends Settlement, Rollouts, Fallback, Refunds.Immediate, Refunds.PostDated

        sealed trait Final extends Finalization, Rollouts, Refunds.Immediate

        sealed trait Initialization {
            def initialization: InitializationTx
        }

        sealed trait Settlement {
            def settlement: SettlementTx
        }

        sealed trait Rollouts {
            def rollouts: List[RolloutTx]
        }

        sealed trait Fallback {
            def fallback: FallbackTx
        }

        object Refunds {
            sealed trait Immediate {
                def immediateRefunds: List[RefundTx.Immediate]
            }

            sealed trait PostDated {
                def postDatedRefunds: List[RefundTx.PostDated]
            }
        }

        sealed trait Finalization {
            def finalization: FinalizationTx
        }
    }
}

object BlockEffect {
    type Id = (Block.Number, Int)
    case class Signature (
        id: Id,
        signature: String
    )
}