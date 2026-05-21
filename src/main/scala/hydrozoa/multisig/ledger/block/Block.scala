package hydrozoa.multisig.ledger.block

import hydrozoa.config.head.network.CardanoNetwork
import io.circe.*
import io.circe.generic.semiauto.*

/** Namespace for block-shape types still in use on the fast consensus cycle.
  *
  * Only [[Block.SoftConfirmed]] (live fast-cycle output) and [[Block.Unsigned.Initial]] (genesis
  * payload carried by [[hydrozoa.config.head.initialization.InitialBlock]]) survive the fast/slow
  * split. The Minor/Major/Final `Unsigned` and the entire `HardConfirmed` hierarchy that used to
  * carry L1 effect transactions were dead — those effects are now slow-side as
  * `StackEffects.HardConfirmed.Regular`.
  */
object Block {

    /** Locally-derived, pre-multisig block view. After the fast/slow split, only the `Initial`
      * variant is alive: the genesis init+fallback pair stored in `HeadConfig`. The slow side's
      * stack-0 hard-ack flow signs them at startup.
      */
    sealed trait Unsigned

    object Unsigned {
        final case class Initial(
            blockBrief: BlockBrief.Initial,
            effects: BlockEffects.Unsigned.Initial
        ) extends Block.Unsigned {
            transparent inline def initializationTx
                : hydrozoa.multisig.ledger.l1.tx.InitializationTx =
                effects.initializationTx
            transparent inline def fallbackTx: hydrozoa.multisig.ledger.l1.tx.FallbackTx =
                effects.fallbackTx
        }

        // Encoder only — `BlockEffects.Unsigned.Initial` has CBOR-hex tx encoders but no
        // decoders; the JSON shape round-trips by re-deriving the unsigned tx pair at decode
        // time in `HeadConfig.headConfigDecoder` from the bootstrap context.
        given (using CardanoNetwork.Section): Encoder[Initial] = deriveEncoder[Initial]
    }

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
