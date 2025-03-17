package hydrozoa.l1.multisig.tx

import hydrozoa.{L1, ParticipantSecretKey, Tx, TxKeyWitness}

// Opaque types for different types of txs used in multisig regime.

object MultisigTxs:

    opaque type InitializationTx = Tx[L1]

    object InitializationTx:
        inline def apply(tx: Tx[L1]): InitializationTx = tx

        extension (tx: InitializationTx) {
            def toTxL1: Tx[L1] = tx
        }

    opaque type DepositTx = Tx[L1]

    object DepositTx:
        inline def apply(tx: Tx[L1]): DepositTx = tx

        extension (tx: DepositTx) {
            def toTxL1: Tx[L1] = tx
        }

    opaque type PostDatedRefundTx = Tx[L1]

    object PostDatedRefundTx:
        inline def apply(tx: Tx[L1]): PostDatedRefundTx = tx

        extension (tx: PostDatedRefundTx) {
            def toTxL1: Tx[L1] = tx
        }

    opaque type SettlementTx = Tx[L1]

    object SettlementTx:
        inline def apply(tx: Tx[L1]): SettlementTx = tx

        extension (tx: SettlementTx) {
            def toTx: Tx[L1] = tx
        }

    opaque type FinalizationTx = Tx[L1]

    object FinalizationTx:
        inline def apply(tx: Tx[L1]): FinalizationTx = tx

        extension (tx: FinalizationTx) {
            def toTxL1: Tx[L1] = tx
        }
