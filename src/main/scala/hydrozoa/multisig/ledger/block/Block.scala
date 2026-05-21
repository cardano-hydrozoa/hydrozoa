package hydrozoa.multisig.ledger.block

/** Namespace for block-shape types still in use on the fast consensus cycle.
  *
  * Only [[Block.SoftConfirmed]] survives the fast/slow split — the [[Block.Unsigned]] /
  * [[Block.HardConfirmed]] hierarchies that used to carry L1 effect transactions have been removed
  * (those effects are now slow-side as `StackEffects.HardConfirmed.Regular`). The genesis
  * initial-block payload lives in [[hydrozoa.config.head.initialization.InitialBlock]] (a
  * [[BlockBrief.Initial]] paired with unsigned init+fallback in [[BlockEffects.Unsigned.Initial]]).
  */
object Block {

    /** Result of fast consensus: block brief plus every head peer's soft-ack signature over the
      * brief's [[BlockHeader.Section.signingBytes]]. Carries no L1 effect signatures — those belong
      * to the slow consensus cycle (see [[hydrozoa.multisig.consensus.SlowConsensusActor]]).
      */
    sealed trait SoftConfirmed
        extends BlockBrief.Section,
          BlockStatus.SoftConfirmed,
          Fields.HasFinalizationRequested {
        def headerMultiSigned: List[BlockHeader.HeaderSignature]
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
}
