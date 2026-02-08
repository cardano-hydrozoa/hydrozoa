package hydrozoa.multisig.ledger.joint.obligation

import cats.data.NonEmptyVector
import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

object Payout {

    /** A payout obligation created by an L2 transaction that marked one of its utxo outputs as
      * bound for L1.
      */
    final case class Obligation(l2UtxoId: TransactionInput, utxo: TransactionOutput.Babbage) {

        /** Record that the payout obligation has been discharged by an L1 transaction that produced
          * a corresponding payout utxo.
          */
        def discharge(l1UtxoId: TransactionInput): Obligation.Discharged =
            Obligation.Discharged(l1UtxoId, l2UtxoId)
    }

    object Obligation {
        trait Many {
            def payoutObligations: Vector[Payout.Obligation]
        }

        object Many {
            trait Remaining {
                def payoutObligationsRemaining: Vector[Payout.Obligation]
            }

            object Remaining {
                trait NonEmpty {
                    def nePayoutObligationsRemaining: NonEmptyVector[Payout.Obligation]
                }
            }
        }

        /** A payout obligation discharged by an L1 transaction output. */
        final case class Discharged(
            l1UtxoId: TransactionInput, // (BlockEffectId, OutputIndex)
            l2UtxoId: TransactionInput, // (LedgerEventId, OutputIndex)
        )
    }
}
