package hydrozoa.multisig.ledger.block

sealed trait BlockStatus

object BlockStatus {
    trait Unsigned extends BlockStatus
    trait MultiSigned extends BlockStatus

    /** Marker for blocks soft-confirmed by the fast consensus cycle: every head peer has signed
      * the block's header, but the block's L1 effects have not yet been hard-confirmed. See
      * `consensus/fast-consensus` in the whitepaper.
      */
    trait SoftConfirmed extends BlockStatus
}
