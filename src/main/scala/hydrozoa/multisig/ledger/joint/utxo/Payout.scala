package hydrozoa.multisig.ledger.joint.utxo

import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

object Payout {
    object Obligation {
        // Isomorphic to a resolved L2 utxo.
        final case class L2(
            l2Input: TransactionInput,
            output: TransactionOutput.Babbage
        )

        type L1 = L1.L1

        // When processing payout obligations in the L1 dapp ledger, we want to temporarily forget
        // the L2 input while building L1 txs. We can remember it after the txs are built.
        object L1 {
            opaque type L1 = L2

            def apply(x: L2): L1 = x
            
            extension (self: L1) def output: TransactionOutput.Babbage = self.output
        }
    }

    // Only the public [[Utxo.apply]] smart constructor should use this conversion.
    private given Conversion[Obligation.L1, Obligation.L2] = identity

    // Private constructor
    final case class Utxo private (
        l1Input: TransactionInput,
        l2Input: TransactionInput,
        output: TransactionOutput.Babbage
    )

    object Utxo {
        // Public smart constructor
        def apply(l1Input: TransactionInput, payoutObligationL1: Obligation.L1): Utxo =
            val payoutObligation = payoutObligationL1.convert
            import payoutObligation.*
            Utxo(l1Input = l1Input, l2Input = l2Input, output = output)
    }
}
