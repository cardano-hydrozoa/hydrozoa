package hydrozoa.l1.multisig.tx

import hydrozoa.L1Tx

// Opaque types for different types of txs used in multisig regime.

object MultisigTxs:

    opaque type DepositTx = L1Tx

    object DepositTx:
        def apply(tx: L1Tx): DepositTx = tx

        extension (tx: DepositTx) {
            def toTx: L1Tx = tx
        }

    opaque type PostDatedRefundTx = L1Tx

    object PostDatedRefundTx:
        def apply(tx: L1Tx): PostDatedRefundTx = tx

        extension (tx: PostDatedRefundTx) {
            def toTx: L1Tx = tx
        }
