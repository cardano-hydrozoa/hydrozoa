package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.node.operation.multisig.RateLimits
import hydrozoa.multisig.consensus.limiter.LimiterTimestamp
import hydrozoa.multisig.ledger.block.BlockHeader.Minor.HeaderSignature
import hydrozoa.multisig.ledger.l1.tx.{FallbackTx, FinalizationTx, InitializationTx, RefundTx, RolloutTx, SettlementTx}
import io.circe.*
import io.circe.generic.semiauto.*
import java.time.Instant
import scala.concurrent.duration.FiniteDuration

sealed trait Block extends Block.Section

object Block {

    sealed trait Unsigned extends Block, BlockStatus.Unsigned {
        def toContext: Seq[(String, String)] =
            Seq(
              "blockType" -> this.blockTypeString,
              "blockVersion" -> this.blockVersion.toString,
              "blockNumber" -> this.blockNum.toString
            )
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
        type NonFinal = Block.HardConfirmed & BlockType.NonFinal

        extension (self: Next)
            transparent inline def blockBriefNext: BlockBrief.Next =
                self.blockBrief.asInstanceOf[BlockBrief.Next]
    }

    sealed trait HardConfirmed
        extends Block,
          BlockStatus.HardConfirmed,
          Fields.HasFinalizationRequested

    object HardConfirmed {
        final case class Initial(
            override val blockBrief: BlockBrief.Initial,
            override val effects: BlockEffects.HardConfirmed.Initial,
        ) extends Block.HardConfirmed,
              BlockType.Initial,
              BlockEffects.HardConfirmed.Initial.Section {
            override transparent inline def block: Block.HardConfirmed.Initial = this

            override transparent inline def header: BlockHeader.Initial = blockBrief.header
            override transparent inline def body: BlockBody.Initial.type = blockBrief.body

            override transparent inline def initializationTx: InitializationTx =
                effects.initializationTx
            override transparent inline def fallbackTx: FallbackTx = effects.fallbackTx

            override transparent inline def finalizationRequested: Boolean = false
        }

        given blockMultisignedInitialEncoder(using
            CardanoNetwork.Section
        ): Encoder[Block.HardConfirmed.Initial] =
            deriveEncoder[Block.HardConfirmed.Initial]

        final case class Minor(
            override val blockBrief: BlockBrief.Minor,
            override val effects: BlockEffects.HardConfirmed.Minor,
            override val finalizationRequested: Boolean,
        ) extends Block.HardConfirmed,
              BlockType.Minor,
              BlockEffects.HardConfirmed.Minor.Section {
            override transparent inline def block: Block.HardConfirmed.Minor = this

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
            override val effects: BlockEffects.HardConfirmed.Major,
            override val finalizationRequested: Boolean,
        ) extends Block.HardConfirmed,
              BlockType.Major,
              BlockEffects.HardConfirmed.Major.Section {
            override transparent inline def block: Block.HardConfirmed.Major = this

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
            override val effects: BlockEffects.HardConfirmed.Final,
        ) extends Block.HardConfirmed,
              BlockType.Final,
              BlockEffects.HardConfirmed.Final.Section {
            override transparent inline def block: Block.HardConfirmed.Final = this

            override transparent inline def header: BlockHeader.Final = blockBrief.header
            override transparent inline def body: BlockBody.Final = blockBrief.body

            override transparent inline def finalizationTx: FinalizationTx = effects.finalizationTx
            override transparent inline def rolloutTxs: List[RolloutTx] = effects.rolloutTxs

            override transparent inline def finalizationRequested: Boolean = false
        }

        type Next = Block.HardConfirmed & BlockType.Next
        type Intermediate = Block.HardConfirmed & BlockType.Intermediate
        type NonFinal = Block.HardConfirmed & BlockType.NonFinal

        extension (nonFinal: Block.HardConfirmed.NonFinal)
            def headerNonFinal: BlockHeader.NonFinal =
                nonFinal.header.asInstanceOf[BlockHeader.NonFinal]

    }

    /** Result of fast consensus: block brief plus every head peer's soft-ack signature over the
      * brief's [[BlockHeader.Section.signingBytes]]. Carries no L1 effect signatures — those belong
      * to the slow consensus cycle (parked, see
      * [[hydrozoa.multisig.consensus.SlowConsensusActor]]).
      *
      * Not a `Block` (intentionally) because a `Block` is defined as a brief plus its effects, and
      * SoftConfirmed has only the brief.
      */
    sealed trait SoftConfirmed
        extends BlockBrief.Section,
          BlockStatus.SoftConfirmed,
          Fields.HasFinalizationRequested,
          LimiterTimestamp {
        def headerMultiSigned: List[BlockHeader.HeaderSignature]

        override def limiterTimestamp: Instant = blockBrief.endTime.instant

        override def minPeriod(using cfg: RateLimits.Section): FiniteDuration =
            cfg.softBlockMinPeriod
    }

    object SoftConfirmed {
        final case class Minor(
            override val blockBrief: BlockBrief.Minor,
            override val headerMultiSigned: List[BlockHeader.HeaderSignature],
            override val finalizationRequested: Boolean
        ) extends Block.SoftConfirmed,
              BlockType.Minor {
            override transparent inline def header: BlockHeader.Minor = blockBrief.header
            override transparent inline def body: BlockBody.Minor = blockBrief.body
        }

        final case class Major(
            override val blockBrief: BlockBrief.Major,
            override val headerMultiSigned: List[BlockHeader.HeaderSignature],
            override val finalizationRequested: Boolean
        ) extends Block.SoftConfirmed,
              BlockType.Major {
            override transparent inline def header: BlockHeader.Major = blockBrief.header
            override transparent inline def body: BlockBody.Major = blockBrief.body
        }

        final case class Final(
            override val blockBrief: BlockBrief.Final,
            override val headerMultiSigned: List[BlockHeader.HeaderSignature]
        ) extends Block.SoftConfirmed,
              BlockType.Final {
            override transparent inline def header: BlockHeader.Final = blockBrief.header
            override transparent inline def body: BlockBody.Final = blockBrief.body
            override transparent inline def finalizationRequested: Boolean = false
        }

        type Next = Block.SoftConfirmed & BlockType.Next
        type Intermediate = Block.SoftConfirmed & BlockType.Intermediate
        type NonFinal = Block.SoftConfirmed & BlockType.NonFinal

        extension (nonFinal: Block.SoftConfirmed.NonFinal)
            def headerNonFinal: BlockHeader.NonFinal =
                nonFinal.header.asInstanceOf[BlockHeader.NonFinal]
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
