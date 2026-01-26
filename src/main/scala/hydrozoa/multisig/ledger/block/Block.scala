package hydrozoa.multisig.ledger.block

import hydrozoa.multisig.ledger.block.BlockHeader.Minor.Signature
import hydrozoa.multisig.ledger.dapp.tx.{DeinitTx, FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx}

sealed trait Block extends Block.Section

object Block {
    sealed trait Unsigned extends Block

    object Unsigned {
        final case class Initial(
            override val header: BlockHeader.Initial,
            override val effects: BlockEffects.Unsigned.Initial,
        ) extends Block.Unsigned,
              BlockType.Initial,
              BlockEffects.Initial.Section {
            override transparent inline def block: Block.Unsigned.Initial = this

            override transparent inline def body: BlockBody.Initial.type = BlockBody.Initial

            override transparent inline def initializationTx: InitializationTx =
                effects.initializationTx
            override transparent inline def fallbackTx: FallbackTx = effects.fallbackTx
        }

        final case class Minor(
            override val header: BlockHeader.Minor,
            override val body: BlockBody.Minor,
            override val effects: BlockEffects.Unsigned.Minor,
        ) extends Block.Unsigned,
              BlockType.Minor,
              BlockEffects.Minor.Section {
            override transparent inline def block: Block.Unsigned.Minor = this

            override transparent inline def headerSerialized: BlockHeader.Minor.Onchain.Serialized =
                effects.headerSerialized
            override transparent inline def postDatedRefunds: List[RefundTx.PostDated] =
                effects.postDatedRefunds
        }

        final case class Major(
            override val header: BlockHeader.Major,
            override val body: BlockBody.Major,
            override val effects: BlockEffects.Unsigned.Major,
        ) extends Block.Unsigned,
              BlockType.Major,
              BlockEffects.Major.Section {
            override transparent inline def block: Block.Unsigned.Major = this

            override transparent inline def settlementTx: SettlementTx = effects.settlementTx
            override transparent inline def rolloutTxs: List[RolloutTx] = effects.rolloutTxs
            override transparent inline def fallbackTx: FallbackTx = effects.fallbackTx
            override transparent inline def postDatedRefunds: List[RefundTx.PostDated] =
                effects.postDatedRefunds
        }

        final case class Final(
            override val header: BlockHeader.Final,
            override val body: BlockBody.Final,
            override val effects: BlockEffects.Unsigned.Final,
        ) extends Block.Unsigned,
              BlockType.Final,
              BlockEffects.Final.Section {
            override transparent inline def block: Block.Unsigned.Final = this

            override transparent inline def finalizationTx: FinalizationTx = effects.finalizationTx
            override transparent inline def rolloutTxs: List[RolloutTx] = effects.rolloutTxs
            override transparent inline def mbDeinitTx: Option[DeinitTx] = effects.mbDeinitTx
        }

        type Next = Block.Unsigned & BlockType.Next
        type Intermediate = Block.Unsigned & BlockType.Intermediate
    }

    trait MultiSigned extends Block

    object MultiSigned {
        final case class Initial(
            override val header: BlockHeader.Initial,
            override val effects: BlockEffects.MultiSigned.Initial,
        ) extends Block.MultiSigned,
              BlockType.Initial,
              BlockEffects.Initial.Section {
            override transparent inline def block: Block.MultiSigned.Initial = this

            override transparent inline def body: BlockBody.Initial.type = BlockBody.Initial
            override transparent inline def initializationTx: InitializationTx =
                effects.initializationTx
            override transparent inline def fallbackTx: FallbackTx = effects.fallbackTx
        }

        final case class Minor(
            override val header: BlockHeader.Minor,
            override val body: BlockBody.Minor,
            override val effects: BlockEffects.MultiSigned.Minor,
        ) extends Block.MultiSigned,
              BlockType.Minor,
              BlockEffects.Minor.Section,
              BlockHeader.Minor.MultiSigned.Section {
            override transparent inline def block: Block.MultiSigned.Minor = this

            override transparent inline def headerSerialized: BlockHeader.Minor.Onchain.Serialized =
                effects.headerSerialized
            override transparent inline def headerSignatures: List[Signature] =
                effects.headerSignatures
            override transparent inline def postDatedRefunds: List[RefundTx.PostDated] =
                effects.postDatedRefunds
        }

        final case class Major(
            override val header: BlockHeader.Major,
            override val body: BlockBody.Major,
            override val effects: BlockEffects.MultiSigned.Major,
        ) extends Block.MultiSigned,
              BlockType.Major,
              BlockEffects.Major.Section {
            override transparent inline def block: Block.MultiSigned.Major = this

            override transparent inline def settlementTx: SettlementTx = effects.settlementTx
            override transparent inline def rolloutTxs: List[RolloutTx] = effects.rolloutTxs
            override transparent inline def fallbackTx: FallbackTx = effects.fallbackTx
            override transparent inline def postDatedRefunds: List[RefundTx.PostDated] =
                effects.postDatedRefunds
        }

        final case class Final(
            override val header: BlockHeader.Final,
            override val body: BlockBody.Final,
            override val effects: BlockEffects.MultiSigned.Final,
        ) extends Block.MultiSigned,
              BlockType.Final,
              BlockEffects.Final.Section {
            override transparent inline def block: Block.MultiSigned.Final = this

            override transparent inline def finalizationTx: FinalizationTx = effects.finalizationTx
            override transparent inline def rolloutTxs: List[RolloutTx] = effects.rolloutTxs
            override transparent inline def mbDeinitTx: Option[DeinitTx] = effects.mbDeinitTx
        }

        type Next = Block.MultiSigned & BlockType.Next
        type Intermediate = Block.MultiSigned & BlockType.Intermediate
    }

    trait Section extends BlockType, BlockHeader.Section, BlockBody.Section, BlockEffects.Section {
        import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
        import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.KzgCommitment
        import hydrozoa.multisig.protocol.types.LedgerEventId
        import hydrozoa.multisig.protocol.types.LedgerEventId.ValidityFlag

        def block: Block

        override transparent inline def blockNum: BlockNumber = header.blockNum
        override transparent inline def blockVersion: BlockVersion.Full = header.blockVersion
        override transparent inline def startTime: QuantizedInstant = header.startTime
        override transparent inline def endTime: QuantizedInstant = header.endTime
        override transparent inline def kzgCommitment: KzgCommitment = header.kzgCommitment

        override transparent inline def events: List[(LedgerEventId, ValidityFlag)] = body.events
        override transparent inline def depositsAbsorbed: List[LedgerEventId] =
            body.depositsAbsorbed
        override transparent inline def depositsRefunded: List[LedgerEventId] =
            body.depositsRefunded
    }
}
