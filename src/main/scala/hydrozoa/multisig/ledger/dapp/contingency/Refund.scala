package hydrozoa.multisig.ledger.dapp.contingency

import cats.data.NonEmptyVector
import hydrozoa.lib.cardano.petri.boundary.Boundary
import scalus.cardano.ledger.{TransactionInput, TransactionOutput}
import scalus.ledger.api.v3.PosixTime

import scala.annotation.targetName

object Refund {
    final case class Instructions (
        refundStartTime: PosixTime,
        refundUtxo: TransactionOutput.Babbage
    )

    /** A refund contingency created to ensure that an L1 deposit's funds don't get stranded.
      */
    final case class Contingency(
        depositUtxoId: TransactionInput,
        refundInstructions: Refund.Instructions
    ) extends Boundary.SourceSide[TransactionInput, Refund.Instructions] {
        override type B = Contingency.Implemented
        override type TargetId = TransactionInput

        override def sourceId: TransactionInput = depositUtxoId

        override def place: Refund.Instructions = refundInstructions

        override def attachTarget(refundUtxoId: TransactionInput): Contingency.Implemented =
            Contingency.Implemented(refundUtxoId, depositUtxoId, refundInstructions)

        /** Record that the refund contingency has been implemented by an L1 refund transaction that
          * produced a corresponding refund utxo.
          */
        def implement(refundUtxoId: TransactionInput): Contingency.Implemented = attachTarget(
          refundUtxoId
        )
    }

    object Contingency {
        trait Many {
            def refundContingencies: Vector[Refund.Contingency]
        }

        type Detached = Detached.DetachedType

        object Detached extends Boundary.Detached[TransactionInput, Refund.Instructions] {
            override type S = Contingency

            override def apply(x: Refund.Instructions): Detached.DetachedType = super.apply(x)

            override def attach(
                detached: Contingency.Detached,
                depositUtxoId: TransactionInput
            ): Contingency = Contingency(depositUtxoId, detached.place)

            extension (detached: DetachedType)
                @targetName("attach_m")
                def attach(depositUtxoId: TransactionInput): Contingency =
                    Contingency.Detached.attach(detached, depositUtxoId)

            trait Many {
                def detachedRefundContingencies: Vector[Refund.Contingency.Detached]
            }
        }

        type Masked = Masked.MaskedType

        object Masked
            extends Boundary.Masked[Contingency, TransactionInput, Refund.Instructions] {
            override def apply(contingency: Contingency): Refund.Contingency.Masked =
                super.apply(contingency)

            extension (masked: Masked)
                /** Record that the refund contingency has been implemented by an L1 refund
                  * transaction that produced a corresponding refund utxo.
                  */
                def implement(refundUtxoId: TransactionInput): Contingency.Implemented =
                    masked.unmask.implement(refundUtxoId)

            object Many {
                trait Remaining {
                    def refundContingenciesRemaining: Vector[Refund.Contingency.Masked]
                }

                object Remaining {
                    trait NonEmpty {
                        def refundContingenciesRemaining: NonEmptyVector[Refund.Contingency.Masked]
                    }
                }
            }
        }

        /** A refund contingency implemented by an L1 refund transaction that produced a
          * corresponding refund utxo.
          */
        final case class Implemented(
            depositUtxoId: TransactionInput,
            refundUtxoId: TransactionInput,
            refundInstructions: Refund.Instructions
        ) extends Boundary[TransactionInput, TransactionInput, Refund.Instructions] {
            override def sourceId: TransactionInput = depositUtxoId

            override def targetId: TransactionInput = refundUtxoId

            override def place: Refund.Instructions = refundInstructions
        }
    }
}
