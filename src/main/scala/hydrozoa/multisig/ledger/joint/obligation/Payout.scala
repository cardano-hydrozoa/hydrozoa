package hydrozoa.multisig.ledger.joint.obligation

import cats.data.NonEmptyVector
import scalus.cardano.ledger.{KeepRaw, TransactionOutput}

object Payout {

    /** A payout obligation created by an L2 transaction that marked one of its utxo outputs as
      * bound for L1.
      */
    final case class Obligation(utxo: KeepRaw[TransactionOutput]) {
        // We use this instead of `Sized`, because `Sized` will re-encode
        def outputSize: Int = utxo.raw.length
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
    }
}
