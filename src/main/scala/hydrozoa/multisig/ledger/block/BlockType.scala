package hydrozoa.multisig.ledger.block

trait BlockType

object BlockType {
    trait Initial extends BlockType
    trait Minor extends BlockType
    trait Major extends BlockType
    trait Final extends BlockType

    type Intermediate = BlockType.Minor | BlockType.Major
    type Next = BlockType.Intermediate | BlockType.Final
}
