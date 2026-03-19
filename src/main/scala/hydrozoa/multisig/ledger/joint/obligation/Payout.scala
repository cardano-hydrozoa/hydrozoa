package hydrozoa.multisig.ledger.joint.obligation

import cats.data.NonEmptyVector
import hydrozoa.config.head.network.CardanoNetwork
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.ledger.{Coin, KeepRaw, Sized, TransactionOutput}

object Payout {

    /** A payout obligation created by an L2 transaction that marked one of its utxo outputs as
      * bound for L1.
      */
    final case class Obligation private (utxo: KeepRaw[TransactionOutput]) {
        // We use this instead of `Sized`, because `Sized` will re-encode
        def outputSize: Int = utxo.raw.length
    }

    object Obligation {

        case class MinAdaViolation(utxo: KeepRaw[TransactionOutput], requiredMinAda: Coin)
            extends Throwable {
            override def toString: String =
                s"Transaction output ${utxo.value} did not have required" +
                    s" min ada (${requiredMinAda})" +
                    " to create a payout obligation."
        }

        def apply(
            output: KeepRaw[TransactionOutput],
            network: CardanoNetwork.Section
        ): Either[MinAdaViolation, Obligation] = {
            // FIXME: this does a redundant serialization, since we already have KeepRaw
            val requiredMinCoin = MinCoinSizedTransactionOutput.ensureMinAda(
              Sized(output.value),
              network.cardanoProtocolParams
            )
            val actualCoin = output.value.value.coin
            if actualCoin >= requiredMinCoin
            then Right(new Obligation(output))
            else Left(MinAdaViolation(utxo = output, requiredMinAda = requiredMinCoin))
        }

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
