package hydrozoa.multisig.protocol.types

import hydrozoa.multisig.protocol.types.Block.Type

enum AugmentedBlock(val blockType: Block.Type) {
    def block: Block

    def effects: BlockEffects

    case Initial(
        override val block: Block.Initial,
        override val effects: BlockEffects.Initial
    ) extends AugmentedBlock(Type.Initial)

    case Minor(
        override val block: Block.Minor,
        override val effects: BlockEffects.Minor
    ) extends AugmentedBlock(Type.Minor)

    case Major(override val block: Block.Major, override val effects: BlockEffects.Major)
        extends AugmentedBlock(Type.Major)

    case Final(
        override val block: Block.Final,
        override val effects: BlockEffects.Final
    ) extends AugmentedBlock(Type.Final)
}

object AugmentedBlock:
    type Next = Minor | Major | Final
