package hydrozoa.multisig.ledger.l1.txseq

import cats.data.NonEmptyList
import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toEpochQuantizedInstant}
import hydrozoa.multisig.ledger.l1.tx.Tx.Builder.SomeBuildErrorOnly
import hydrozoa.multisig.ledger.l1.tx.{DepositTx, RefundTx, Tx}
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import monocle.syntax.all.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, Utxo, Value}
import scalus.uplc.builtin.{ByteString, Data}

/** Deposit-[post-dated] refund tx sequence contains a deposit and a refund txs see
  * [[DepositRefundTxSeq.Build]] for details.
  * @param depositTx
  * @param refundTx
  *
  * Schema for the sequence building:
  *   - build the deposit tx based on the virtualOutputs and depositFee (see the description down
  *     below)
  *   - build the refund tx, refunding everything but refund tx fee amount
  *
  * Some important security aspects:
  *
  * Deposit operations with post-dated refunds are sensitive to data interception: if an adversary
  * somehow gets to know that somebody is going to deposit to the head, she may try to steal the
  * money by persuading the head to multisign a counterfeited refund tx that sends to her address.
  *
  * The countermeasure is a two-step proof that the depositor is who they say:
  *   1. The depositor has to present the whole deposit tx that hashes to the deposit utxo id
  *   2. The transaction hash should depend on the refund address. Now we use datum in the deposit
  *      utxo.
  *
  * NB The metadata was an alternative to the datum, but we opted against is since in the future
  * we'd like to use CIP-112 guard scripts to eliminate post-dated refunds.
  *
  * There are two possible outcomes for a depositing.
  *
  *   1. Success - a deposit gets absorbed. In that case refund tx is not relevant. The deposit tx
  *      fee is paid from funding utxos. Then:
  *
  * depositValue = sum(virtualOutputs) + depositFee
  *
  * utxosFunding = depositValue + depositTxFee + change
  *
  *   2. Failure - a deposit request was accepted, but the deposit is rejected for some reason -
  *      likely the deposit utxo was created to late. Then the refund tx comes to play, spending the
  *      unlucky deposit utxo that holds depositValue:
  *
  * depositValue = refundTxFee + refundValue
  *
  * To guarantee that a refund can be built, we need to satisfy the condition:
  *
  * depositValue > self.minAda + maximum refundTx fee
  *
  * This can be checked upfront or by trying to build the whole tx sequence.
  *
  * i.e. the depositValue should be big enough that in case of failure it can cover the refund tx
  * fee and minAda storage fee for the refunded utxo (in fact there is a small difference, since
  * datum of the refunded utxo is strictly less than deposit utxo datum which contains additional
  * information, but we can consider them being the same).
  */
final case class DepositRefundTxSeq(
    depositTx: DepositTx,
    refundTx: RefundTx.PostDated
)

object DepositRefundTxSeq {
    export DepositRefundTxSeqOps.{Build, Parse}
}

private object DepositRefundTxSeqOps {

    type Config = CardanoNetwork.Section & HeadPeers.Section & InitialBlock.Section &
        TxTiming.Section

    object Build {
        sealed trait Error extends Throwable {
            override def toString: String = this match {
                case Error.Deposit((err, s)) => s"Deposit tx builder failed: $s, error: $err"
                case Error.Refund((err, s))  => s"Refund tx builder failed: $s, error: $err"
                case Error.TimingIncoherence => "Timing incoherence detected"
                case Error.DepositValueMismatch(depositValue, expectedDepositValue) =>
                    s"Deposit value mismatch, actual: $depositValue, expected: $expectedDepositValue"
            }
        }

        object Error {
            final case class Deposit(e: (SomeBuildErrorOnly, String)) extends Build.Error
            final case class Refund(e: (SomeBuildErrorOnly, String)) extends Build.Error
            case object TimingIncoherence extends Build.Error
            final case class DepositValueMismatch(depositValue: Value, expectedDepositValue: Value)
                extends Build.Error
        }
    }

    /** * @param config
      * @param l2Value
      *   The actual deposit value becomes virtualValue + depositFee
      * @param l2Payload
      *   The L2 payload to pass to the virtual ledger
      * @param depositFee
      *   Deposit fee is an amount that goes to the head's treasury, may equal zero.
      * @param utxosFunding
      *   L1 utxos from which the user wants to fund the virtualOutputs, depositFee and deposit tx
      *   fee.
      * @param changeAddress
      *   Where the change output should go.
      * @param submissionDeadline
      *   The ttl for the deposit tx
      * @param refundAddress
      *   Where the refund should go
      * @param refundDatum
      *   Optional datum to add to the refund utxo.
      */
    final case class Build(config: Config)(
        l2Payload: ByteString,
        l2Value: Value,
        depositFee: Coin,
        utxosFunding: NonEmptyList[Utxo],
        changeAddress: ShelleyAddress,
        requestValidityEndTime: QuantizedInstant,
        refundAddress: ShelleyAddress,
        refundDatum: Option[Data]
    ) {
        def result: Either[Build.Error, DepositRefundTxSeq] = {
            val expectedDepositValue = l2Value + Value(depositFee)

            val submissionDeadline =
                config.txTiming.depositSubmissionEndTime(requestValidityEndTime)

            val refundInstructions = DepositUtxo.Refund.Instructions(
              address = refundAddress,
              datum = refundDatum,
              validityStart = config.txTiming.refundValidityStart(requestValidityEndTime)
            )

            for {

                depositTx <- DepositTx
                    .Build(config)(
                      utxosFunding,
                      l2Payload,
                      l2Value,
                      depositFee,
                      changeAddress,
                      submissionDeadline,
                      refundInstructions
                    )
                    .result
                    .left
                    .map(f => {
                        // println(f)
                        Build.Error.Deposit(f)
                    })

                refundTx <- RefundTx.Build
                    .PostDated(config)(depositTx.depositProduced, refundInstructions)
                    .result
                    .left
                    .map(f => {
                        // println(f)
                        Build.Error.Refund(f)
                    })

                // Run some sanity-checks
                depositUtxoValue = depositTx.depositProduced.value
                _ <- Either
                    .cond(
                      depositUtxoValue == expectedDepositValue,
                      (),
                      Build.Error.DepositValueMismatch(depositUtxoValue, expectedDepositValue)
                    )

                expectedRefundStart = config.txTiming.refundValidityStart(requestValidityEndTime)

                actualRefundStartMatches = expectedRefundStart == refundTx.refundStart
                instructionsRefundStartMatches =
                    expectedRefundStart.toPosixTime == depositTx.depositProduced.datum.refundInstructions.refundStart

                _ <- Either
                    .cond(
                      actualRefundStartMatches && instructionsRefundStartMatches,
                      (),
                      Build.Error.TimingIncoherence // we don't return a DepositRefundTxSeq, because it's not valid
                    )
            } yield DepositRefundTxSeq(depositTx, refundTx)
        }
    }

    object Parse {
        type ParseErrorOr[A] = Either[Error, A]

        enum Error extends Throwable {
            case Deposit(e: DepositTx.Parse.Error)
            case RefundBuildError(e: (SomeBuildErrorOnly, String))
            case CborToTransactionOutputDecodingFailure(wrapped: Throwable)

            override def toString: String = this match {
                case Deposit(e) => s"Deposit parse error: $e"
                case RefundBuildError(e) =>
                    s"Refund build error: ${e._2}"
            }
        }
    }

    /** VirtualOutputs are encoded in CBOR as a list of Babbage outputs. Internally, they are
      * represented as a more restrictive type ([[GenesisObligation]]) that ensure L2 conformance
      */
    final case class Parse(config: Config)(
        depositTxBytes: Tx.Serialized,
        l2Payload: ByteString,
    ) {
        import Parse.*

        def result: ParseErrorOr[DepositRefundTxSeq] = {
            for {
                depositTx <- DepositTx
                    .Parse(config)(txBytes = depositTxBytes, l2Payload = l2Payload)
                    .result
                    .left
                    .map(Parse.Error.Deposit(_))

                depositUtxo = depositTx.depositProduced
                depositValue = depositUtxo.value
                l2Value = depositUtxo.l2Value

                refundInstructions = DepositUtxo.Refund.Instructions(
                  depositUtxo.datum.refundInstructions,
                  config.network,
                  config.slotConfig
                )

                depositFee = depositValue - l2Value

                refundTx <- RefundTx.Build
                    .PostDated(config)(depositUtxo, refundInstructions)
                    .result
                    .left
                    .map(Parse.Error.RefundBuildError(_))

            } yield DepositRefundTxSeq(depositTx, refundTx)
        }
    }

}
