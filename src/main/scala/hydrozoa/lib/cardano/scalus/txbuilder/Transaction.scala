package hydrozoa.lib.cardano.scalus.txbuilder

import monocle.Focus.focus
import scalus.cardano.ledger.{TaggedSortedSet, VKeyWitness}

object Transaction {
    import scalus.cardano.ledger.Transaction

    extension (tx: Transaction)
        /** Adds multiple verification key witnesses to a transaction.
          *
          * This function preserves the original CBOR encoding of the transaction body (via the
          * KeepRaw wrapper), modifying only the witness set by adding all provided witnesses.
          *
          * @param tx
          *   The transaction to add the witnesses to
          * @param witnesses
          *   The VKeyWitnesses to add
          * @return
          *   A new Transaction with all witnesses added
          */
        // TODO: Replace with Scalus' [[Transaction.withWitness]] (recently added, so Scalus version bump required).
        // TODO: Use NonEmptyList[VKeyWitness]
        def attachVKeyWitnesses(witnesses: Iterable[VKeyWitness]): Transaction = {
            tx.focus(_.witnessSet.vkeyWitnesses)
                .modify(w => TaggedSortedSet(w.toSet ++ witnesses))
        }
}
