package hydrozoa.multisig.ledger.block

import hydrozoa.multisig.consensus.ack.{AckBlock, AckId, AckNumber}
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.ledger.block.BlockHeader.Minor.HeaderSignature
import hydrozoa.multisig.ledger.dapp.tx.{DeinitTx, FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx}

sealed trait Block extends Block.Section

object Block {
    sealed trait Unsigned extends Block {
        def acks(wallet: PeerWallet, finalizationRequested: Boolean): List[AckBlock]
    }

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

            override transparent inline def acks(
                wallet: PeerWallet,
                finalizationRequested: Boolean
            ): List[AckBlock] = List()
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

            def ack(wallet: PeerWallet, finalizationRequested: Boolean): AckBlock.Minor =
                AckBlock.Minor(
                  ackId = AckId(wallet, AckNumber.neededToConfirm(header)),
                  blockNum = blockNum,
                  header = wallet.mkMinorHeaderSignature(headerSerialized),
                  postDatedRefundTxs = postDatedRefundTxs.map(_.tx).map(wallet.mkTxSignature),
                  finalizationRequested = finalizationRequested
                )

            override transparent inline def acks(
                wallet: PeerWallet,
                finalizationRequested: Boolean
            ): List[AckBlock.Minor] = List(ack(wallet, finalizationRequested))
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

            def ack1(wallet: PeerWallet, finalizationRequested: Boolean): AckBlock.Major1 =
                AckBlock.Major1(
                  ackId = AckId(wallet, AckNumber.neededToConfirm(header).decrement),
                  blockNum = blockNum,
                  fallbackTx = wallet.mkTxSignature(fallbackTx.tx),
                  rolloutTxs = rolloutTxs.map(_.tx).map(wallet.mkTxSignature),
                  postDatedRefundTxs = postDatedRefundTxs.map(_.tx).map(wallet.mkTxSignature),
                  finalizationRequested = finalizationRequested
                )

            def ack2(wallet: PeerWallet): AckBlock.Major2 =
                AckBlock.Major2(
                  ackId = AckId(wallet, AckNumber.neededToConfirm(header)),
                  blockNum = blockNum,
                  settlementTx = wallet.mkTxSignature(settlementTx.tx)
                )

            override transparent inline def acks(
                wallet: PeerWallet,
                finalizationRequested: Boolean
            ): List[AckBlock & BlockType.Major] =
                List(ack1(wallet, finalizationRequested), ack2(wallet))
        }

        final case class Final(
            override val blockBrief: BlockBrief.Final,
            override val effects: BlockEffects.Unsigned.Final,
        ) extends Block.Unsigned,
              BlockType.Final,
              BlockEffects.Final.Section {
            import scala.annotation.unused

            override transparent inline def block: Block.Unsigned.Final = this

            override transparent inline def header: BlockHeader.Final = blockBrief.header
            override transparent inline def body: BlockBody.Final = blockBrief.body

            override transparent inline def finalizationTx: FinalizationTx = effects.finalizationTx
            override transparent inline def rolloutTxs: List[RolloutTx] = effects.rolloutTxs
            override transparent inline def deinitTx: Option[DeinitTx] = effects.deinitTx

            def ack1(wallet: PeerWallet): AckBlock.Final1 = AckBlock.Final1(
              ackId = AckId(wallet, AckNumber.neededToConfirm(header).decrement),
              blockNum = blockNum,
              rolloutTxs = rolloutTxs.map(_.tx).map(wallet.mkTxSignature),
              deinitTx = deinitTx.map(_.tx).map(wallet.mkTxSignature)
            )

            def ack2(wallet: PeerWallet): AckBlock.Final2 = AckBlock.Final2(
              ackId = AckId(wallet, AckNumber.neededToConfirm(header)),
              blockNum = blockNum,
              finalizationTx = wallet.mkTxSignature(finalizationTx.tx)
            )

            override transparent inline def acks(
                wallet: PeerWallet,
                @unused finalizationRequested: Boolean
            ): List[AckBlock & BlockType.Final] = List(ack1(wallet), ack2(wallet))
        }

        type Next = Block.Unsigned & BlockType.Next
        type Intermediate = Block.Unsigned & BlockType.Intermediate

        extension (self: Next)
            transparent inline def blockBriefNext: BlockBrief.Next =
                self.blockBrief.asInstanceOf[BlockBrief.Next]
    }

    trait MultiSigned extends Block

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
        }

        final case class Minor(
            override val blockBrief: BlockBrief.Minor,
            override val effects: BlockEffects.MultiSigned.Minor,
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
            override transparent inline def deinitTx: Option[DeinitTx] = effects.deinitTx
        }

        type Next = Block.MultiSigned & BlockType.Next

        type Intermediate = Block.MultiSigned & BlockType.Intermediate
    }

    trait Section extends BlockType, BlockBrief.Section, BlockEffects.Section {
        def block: Block
    }
}
