package hydrozoa.multisig.ledger.joint.obligation

import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

object Genesis {

    /** A genesis obligation created by an L1 deposit transaction.
      */
    final case class Obligation(l1UtxoId: TransactionInput, utxo: TransactionOutput.Babbage) {

        /** Record that the genesis obligation has been discharged by a corresponding utxo that an
          * L2 block has added to the L2 ledger state.
          */
        def discharge(l2UtxoId: TransactionInput): Obligation.Discharged =
            Obligation.Discharged(l2UtxoId, l1UtxoId)
    }

    object Obligation {
        trait Many {
            def genesisObligations: Vector[Genesis.Obligation]
        }

        /** A genesis obligation discharged by a corresponding utxo that an L2 block has added to
          * the L2 ledger state.
          */
        final case class Discharged(
            l1UtxoId: TransactionInput, // (LedgerEventId, OutputIndex)
            l2UtxoId: TransactionInput, // (BlockEffectId, OutputIndex)
        )
    }
}
