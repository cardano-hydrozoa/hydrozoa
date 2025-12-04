package hydrozoa.multisig.ledger.joint.obligation

import cats.data.NonEmptyVector
import hydrozoa.lib.cardano.petri.boundary.Boundary
import scala.annotation.targetName
import scalus.cardano.ledger.{TransactionInput, TransactionOutput}

object Genesis {

    /** A genesis obligation created by an L1 deposit transaction.
      */
    final case class Obligation(l1UtxoId: TransactionInput, utxo: TransactionOutput.Babbage)
        extends Boundary.SourceSide[TransactionInput, TransactionOutput.Babbage] {
        override type B = Obligation.Discharged
        override type TargetId = TransactionInput
        override def sourceId: TransactionInput = l1UtxoId
        override def place: TransactionOutput.Babbage = utxo
        override def attachTarget(l2UtxoId: TransactionInput): Obligation.Discharged =
            Obligation.Discharged(l2UtxoId, l1UtxoId, utxo)

        /** Record that the genesis obligation has been discharged by a corresponding utxo that an
          * L2 block has added to the L2 ledger state.
          */
        def discharge(l2UtxoId: TransactionInput): Obligation.Discharged = attachTarget(l2UtxoId)
    }

    object Obligation {
        trait Many {
            def genesisObligations: Vector[Genesis.Obligation]
        }

        type Detached = Detached.DetachedType

        object Detached extends Boundary.Detached[TransactionInput, TransactionOutput.Babbage] {
            override type S = Obligation

            override def apply(x: TransactionOutput.Babbage): Detached.DetachedType = super.apply(x)

            override def attach(
                detached: Obligation.Detached,
                l1UtxoId: TransactionInput
            ): Obligation = Obligation(l1UtxoId, detached.place)

            extension (detached: DetachedType)
                @targetName("attach_m")
                def attach(l1UtxoId: TransactionInput): Obligation =
                    Obligation.Detached.attach(detached, l1UtxoId)

            trait Many {
                def detachedGenesisObligations: Vector[Genesis.Obligation.Detached]
            }
        }

        type Masked = Masked.MaskedType

        object Masked
            extends Boundary.Masked[Obligation, TransactionInput, TransactionOutput.Babbage] {
            override def apply(obligation: Obligation): Genesis.Obligation.Masked =
                super.apply(obligation)

            extension (masked: Masked)
                /** Record that the genesis obligation has been discharged by a corresponding utxo
                  * that an L2 block has added to the L2 ledger state.
                  */
                def discharge(l2UtxoId: TransactionInput): Obligation.Discharged =
                    masked.unmask.discharge(l2UtxoId)

            object Many {
                trait Remaining {
                    def genesisObligationsRemaining: Vector[Genesis.Obligation.Masked]
                }

                object Remaining {
                    trait NonEmpty {
                        def genesisObligationsRemaining: NonEmptyVector[Genesis.Obligation.Masked]
                    }
                }
            }
        }

        /** A genesis obligation discharged by a corresponding utxo that an L2 block has added to
          * the L2 ledger state.
          */
        final case class Discharged(
            l1UtxoId: TransactionInput,
            l2UtxoId: TransactionInput,
            utxo: TransactionOutput.Babbage
        ) extends Boundary[TransactionInput, TransactionInput, TransactionOutput.Babbage] {
            override def sourceId: TransactionInput = l1UtxoId
            override def targetId: TransactionInput = l2UtxoId
            override def place: TransactionOutput.Babbage = utxo
        }
    }
}
