package hydrozoa.multisig.ledger.block

import hydrozoa.multisig.ledger.dapp.tx.{DeinitTx, FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx}

sealed trait BlockEffects extends BlockEffects.Section

object BlockEffects {
    sealed trait Unsigned extends BlockEffects

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
            override val postDatedRefunds: List[RefundTx.PostDated],
        ) extends BlockEffects.Unsigned,
              BlockType.Minor,
              BlockEffects.Minor.Section {
            override transparent inline def effects: BlockEffects.Unsigned.Minor = this
        }

        final case class Major(
            override val settlementTx: SettlementTx,
            override val rolloutTxs: List[RolloutTx],
            override val fallbackTx: FallbackTx,
            override val postDatedRefunds: List[RefundTx.PostDated],
        ) extends BlockEffects.Unsigned,
              BlockType.Major,
              BlockEffects.Major.Section {
            override transparent inline def effects: BlockEffects.Unsigned.Major = this
        }

        final case class Final(
            override val finalizationTx: FinalizationTx,
            override val rolloutTxs: List[RolloutTx],
            override val mbDeinitTx: Option[DeinitTx]
        ) extends BlockEffects.Unsigned,
              BlockType.Final,
              BlockEffects.Final.Section {
            override transparent inline def effects: BlockEffects.Unsigned.Final = this
        }

        type Next = BlockEffects.Unsigned & BlockType.Next
        type Intermediate = BlockEffects.Unsigned & BlockType.Intermediate
    }

    sealed trait MultiSigned extends BlockEffects

    object MultiSigned {
        final case class Initial(
            override val initializationTx: InitializationTx,
            override val fallbackTx: FallbackTx,
        ) extends BlockEffects.MultiSigned,
              BlockType.Initial,
              BlockEffects.Initial.Section {
            override transparent inline def effects: BlockEffects.MultiSigned.Initial = this
        }

        final case class Minor(
            override val headerSerialized: BlockHeader.Minor.Onchain.Serialized,
            override val headerSignatures: List[BlockHeader.Minor.Signature],
            override val postDatedRefunds: List[RefundTx.PostDated],
        ) extends BlockEffects.MultiSigned,
              BlockType.Minor,
              BlockEffects.Minor.Section,
              BlockHeader.Minor.MultiSigned.Section {
            override transparent inline def effects: BlockEffects.MultiSigned.Minor = this
        }

        final case class Major(
            override val settlementTx: SettlementTx,
            override val rolloutTxs: List[RolloutTx],
            override val fallbackTx: FallbackTx,
            override val postDatedRefunds: List[RefundTx.PostDated],
        ) extends BlockEffects.MultiSigned,
              BlockType.Major,
              BlockEffects.Major.Section {
            override transparent inline def effects: BlockEffects.MultiSigned.Major = this
        }

        final case class Final(
            override val finalizationTx: FinalizationTx,
            override val rolloutTxs: List[RolloutTx],
            override val mbDeinitTx: Option[DeinitTx]
        ) extends BlockEffects.MultiSigned,
              BlockType.Final,
              BlockEffects.Final.Section {
            override transparent inline def effects: BlockEffects.MultiSigned.Final = this
        }

        type Next = BlockEffects.MultiSigned & BlockType.Next
        type Intermediate = BlockEffects.MultiSigned & BlockType.Intermediate
    }

    trait Section {
        def effects: BlockEffects
        def postDatedRefunds: List[RefundTx.PostDated]
    }

    object Initial {
        trait Section extends BlockEffects.Section {
            def initializationTx: InitializationTx
            def fallbackTx: FallbackTx

            override transparent inline def postDatedRefunds: List[RefundTx.PostDated] = List()
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
            def mbDeinitTx: Option[DeinitTx]

            override transparent inline def postDatedRefunds: List[RefundTx.PostDated] = List()
        }
    }
}
