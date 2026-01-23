package hydrozoa.multisig.protocol.types

import hydrozoa.multisig.protocol.types.Block.Type

enum AugmentedBlock(val blockType: Block.Type) {
    def block: Block

    def effects: BlockEffects

    case Minor(
        override val block: Block.Minor,
        override val effects: BlockEffects.Minor,
    ) extends AugmentedBlock(Type.Minor)

    case Major(
        override val block: Block.Major,
        override val effects: BlockEffects.Major
    ) extends AugmentedBlock(Type.Major)

    case Final(
        override val block: Block.Final,
        override val effects: BlockEffects.Final
    ) extends AugmentedBlock(Type.Final)
}

object AugmentedBlock:
    // TODO: remove, now when we got rid of Initial it's not needed anymore
    type Next = Minor | Major | Final

    extension (self: AugmentedBlock.Next)
        def blockAsNext: Block.Next = self match {
            case Minor(b, _) => b
            case Major(b, _) => b
            case Final(b, _) => b
        }

        def effects: BlockEffects = self match {
            case Minor(_, e) => e
            case Major(_, e) => e
            case Final(_, e) => e
        }
