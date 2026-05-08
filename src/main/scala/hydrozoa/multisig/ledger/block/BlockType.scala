package hydrozoa.multisig.ledger.block

trait BlockType {
    def blockTypeString: String = this match {
        case _: BlockType.Initial => "Initial"
        case _: BlockType.Minor   => "Minor"
        case _: BlockType.Major   => "Major"
        case _: BlockType.Final   => "Final"
        // This should not be reachable, but we can't seal this trait because
        // its extended in another file (BlockBrief.scala)
        case _ => "Unknown"
    }
}

object BlockType {
    trait Initial extends BlockType
    trait Minor extends BlockType
    trait Major extends BlockType
    trait Final extends BlockType

    type Intermediate = BlockType.Minor | BlockType.Major
    type Next = BlockType.Intermediate | BlockType.Final
    type NonFinal = BlockType.Initial | BlockType.Intermediate
}
