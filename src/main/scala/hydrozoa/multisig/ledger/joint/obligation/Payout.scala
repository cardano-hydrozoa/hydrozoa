package hydrozoa.multisig.ledger.joint.obligation

import cats.data.NonEmptyVector
import hydrozoa.lib.cardano.petri.boundary.Boundary
import scala.annotation.targetName
import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

object Payout {

    /** A payout obligation created by an L2 transaction that marked one of its utxo outputs as
      * bound for L1.
      */
    final case class Obligation(l2UtxoId: TransactionInput, utxo: TransactionOutput.Babbage)
        extends Boundary.SourceSide[TransactionInput, TransactionOutput.Babbage] {
        override type B = Obligation.Discharged
        override type TargetId = TransactionInput
        override def sourceId: TransactionInput = l2UtxoId
        override def place: TransactionOutput.Babbage = utxo
        override def attachTarget(l1UtxoId: TransactionInput): Obligation.Discharged =
            Obligation.Discharged(l1UtxoId, l2UtxoId, utxo)

        /** Record that the payout obligation has been discharged by an L1 transaction that produced
          * a corresponding payout utxo.
          */
        def discharge(l1UtxoId: TransactionInput): Obligation.Discharged = attachTarget(l1UtxoId)
    }

    object Obligation {
        trait Many {
            def payoutObligations: Vector[Payout.Obligation]
        }

        /** A payout obligation that has not yet been attached to the L2 transaction that will
          * create it.
          */
        type Detached = Detached.DetachedType

        object Detached extends Boundary.Detached[TransactionInput, TransactionOutput.Babbage] {
            override type S = Obligation

            override def apply(x: TransactionOutput.Babbage): Detached.DetachedType = super.apply(x)

            override def attach(
                detached: Obligation.Detached,
                l2UtxoId: TransactionInput
            ): Obligation = Obligation(l2UtxoId, detached.place)

            extension (detached: DetachedType)
                @targetName("attach_m")
                def attach(l2UtxoId: TransactionInput): Obligation =
                    Obligation.Detached.attach(detached, l2UtxoId)

            trait Many {
                def detachedPayoutObligations: Vector[Payout.Obligation.Detached]
            }
        }

        /** A payout obligation masking its source L2 transaction while the L1 transaction that will
          * discharge it is being built.
          */
        type Masked = Masked.MaskedType

        object Masked
            extends Boundary.Masked[Obligation, TransactionInput, TransactionOutput.Babbage] {
            override def apply(obligation: Obligation): Payout.Obligation.Masked =
                super.apply(obligation)

            extension (masked: Masked)
                /** Record that the payout obligation has been discharged by an L1 transaction that
                  * produced a corresponding payout utxo.
                  */
                def discharge(l1UtxoId: TransactionInput): Obligation.Discharged =
                    masked.unmask.discharge(l1UtxoId)

            object Many {
                trait Remaining {
                    def payoutObligationsRemaining: Vector[Payout.Obligation.Masked]
                }

                object Remaining {
                    trait NonEmpty {
                        def payoutObligationsRemaining: NonEmptyVector[Payout.Obligation.Masked]
                    }
                }
            }
        }

        /** A payout obligation discharged by an L1 transaction. */
        final case class Discharged(
            l1UtxoId: TransactionInput,
            l2UtxoId: TransactionInput,
            utxo: TransactionOutput.Babbage
        ) extends Boundary[TransactionInput, TransactionInput, TransactionOutput.Babbage] {
            override def sourceId: TransactionInput = l2UtxoId
            override def targetId: TransactionInput = l1UtxoId
            override def place: TransactionOutput.Babbage = utxo
        }
    }
}
