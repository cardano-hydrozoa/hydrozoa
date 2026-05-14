package hydrozoa.multisig.ledger.block

import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx}
import io.circe.*
import io.circe.generic.semiauto.*

sealed trait BlockEffects extends BlockEffects.Section

object BlockEffects {
    sealed trait Unsigned extends BlockEffects, BlockStatus.Unsigned

    object Unsigned {
        final case class Initial(
            override val initializationTx: InitializationTx,
            override val fallbackTx: FallbackTx,
        ) extends BlockEffects.Unsigned,
              BlockType.Initial,
              BlockEffects.Initial.Section {
            override transparent inline def effects: BlockEffects.Unsigned.Initial = this
        }

        final case class Minor(
            override val headerSerialized: BlockHeader.Minor.Onchain.Serialized,
            override val postDatedRefundTxs: List[RefundTx.PostDated],
        ) extends BlockEffects.Unsigned,
              BlockType.Minor,
              BlockEffects.Minor.Section {
            override transparent inline def effects: BlockEffects.Unsigned.Minor = this
        }

        final case class Major(
            override val settlementTx: SettlementTx,
            override val rolloutTxs: List[RolloutTx],
            override val fallbackTx: FallbackTx,
            override val postDatedRefundTxs: List[RefundTx.PostDated],
        ) extends BlockEffects.Unsigned,
              BlockType.Major,
              BlockEffects.Major.Section {
            override transparent inline def effects: BlockEffects.Unsigned.Major = this
        }

        final case class Final(
            override val finalizationTx: FinalizationTx,
            override val rolloutTxs: List[RolloutTx]
        ) extends BlockEffects.Unsigned,
              BlockType.Final,
              BlockEffects.Final.Section {
            override transparent inline def effects: BlockEffects.Unsigned.Final = this
        }

        type Next = BlockEffects.Unsigned & BlockType.Next
        type Intermediate = BlockEffects.Unsigned & BlockType.Intermediate
    }

    sealed trait HardConfirmed extends BlockEffects, BlockStatus.HardConfirmed

    object HardConfirmed {
        final case class Initial(
            override val initializationTx: InitializationTx,
            override val fallbackTx: FallbackTx,
        ) extends BlockEffects.HardConfirmed,
              BlockType.Initial,
              BlockEffects.HardConfirmed.Initial.Section {
            override transparent inline def effects: BlockEffects.HardConfirmed.Initial = this
        }

        given blockEffectsMultiSignedInitialEncoder: Encoder[BlockEffects.HardConfirmed.Initial] =
            deriveEncoder[BlockEffects.HardConfirmed.Initial]

        final case class Minor(
            override val headerSerialized: BlockHeader.Minor.Onchain.Serialized,
            override val headerMultiSigned: List[BlockHeader.Minor.HeaderSignature],
            override val postDatedRefundTxs: List[RefundTx.PostDated],
        ) extends BlockEffects.HardConfirmed,
              BlockType.Minor,
              BlockEffects.HardConfirmed.Minor.Section {
            override transparent inline def effects: BlockEffects.HardConfirmed.Minor = this
        }

        final case class Major(
            override val settlementTx: SettlementTx,
            override val rolloutTxs: List[RolloutTx],
            override val fallbackTx: FallbackTx,
            override val postDatedRefundTxs: List[RefundTx.PostDated],
        ) extends BlockEffects.HardConfirmed,
              BlockType.Major,
              BlockEffects.HardConfirmed.Major.Section {
            override transparent inline def effects: BlockEffects.HardConfirmed.Major = this
        }

        final case class Final(
            override val finalizationTx: FinalizationTx,
            override val rolloutTxs: List[RolloutTx]
        ) extends BlockEffects.HardConfirmed,
              BlockType.Final,
              BlockEffects.HardConfirmed.Final.Section {
            override transparent inline def effects: BlockEffects.HardConfirmed.Final = this
        }

        type Next = BlockEffects.HardConfirmed & BlockType.Next
        type Intermediate = BlockEffects.HardConfirmed & BlockType.Intermediate

        object Initial {
            type Section = BlockEffects.Initial.Section
        }

        object Minor {
            trait Section extends BlockEffects.Minor.Section, BlockHeader.Minor.MultiSigned.Section
        }

        object Major {
            type Section = BlockEffects.Major.Section
        }

        object Final {
            type Section = BlockEffects.Final.Section
        }
    }

    object Fields {
        trait HasPostDatedRefundTxs {
            def postDatedRefundTxs: List[RefundTx.PostDated]
        }
    }

    import Fields.*

    trait Section extends HasPostDatedRefundTxs {
        def effects: BlockEffects
    }

    object Initial {
        trait Section extends BlockEffects.Section {
            def initializationTx: InitializationTx
            def fallbackTx: FallbackTx

            override transparent inline def postDatedRefundTxs: List[RefundTx.PostDated] = List()
        }
    }

    object Minor {
        trait Section extends BlockEffects.Section, BlockHeader.Minor.Onchain.Serialized.Section
    }

    object Major {
        trait Section extends BlockEffects.Section {
            def settlementTx: SettlementTx
            def rolloutTxs: List[RolloutTx]
            def fallbackTx: FallbackTx
        }
    }

    object Final {
        trait Section extends BlockEffects.Section {
            def finalizationTx: FinalizationTx
            def rolloutTxs: List[RolloutTx]

            override transparent inline def postDatedRefundTxs: List[RefundTx.PostDated] = List()
        }
    }
}
