package hydrozoa.multisig.ledger.block

sealed trait BlockStatus

object BlockStatus {
    trait Unsigned extends BlockStatus
    trait MultiSigned extends BlockStatus
}
