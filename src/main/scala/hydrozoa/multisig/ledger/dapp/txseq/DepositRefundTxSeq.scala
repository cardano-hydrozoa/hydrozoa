package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toEpochQuantizedInstant}
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.SomeBuildErrorOnly
import hydrozoa.multisig.ledger.dapp.tx.{DepositTx, RefundTx, Tx}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.tx.GenesisObligation
import io.bullet.borer.Cbor
import monocle.*
import monocle.syntax.all.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.TransactionWitnessSet.given_Encoder_TransactionWitnessSet
import scalus.cardano.ledger.{Coin, TaggedSortedSet, TransactionOutput, TransactionWitnessSet, Utxo, Value}
import scalus.cardano.txbuilder.keepRawL
import scalus.uplc.builtin.Data

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
    refundTx: RefundTx
)

object DepositRefundTxSeq {
    export DepositRefundTxSeqOps.{Build, Parse}
}

private object DepositRefundTxSeqOps {

    type Config = CardanoNetwork.Section & HeadPeers.Section & InitialBlock.Section &
        TxTiming.Section

    object Build {
        sealed trait Error extends Throwable

        object Error {
            final case class Deposit(e: (SomeBuildErrorOnly, String)) extends Build.Error
            final case class Refund(e: (SomeBuildErrorOnly, String)) extends Build.Error
            case object TimingIncoherence extends Build.Error
            final case class DepositValueMismatch(depositValue: Value, expectedDepositValue: Value)
                extends Build.Error
        }
    }

    /** * @param config
      * @param virtualOutputs
      *   Virtual (l2) outputs to be created (should satisfy l2 utxo requirements).
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
        virtualOutputs: NonEmptyList[GenesisObligation],
        depositFee: Coin,
        utxosFunding: NonEmptyList[Utxo],
        changeAddress: ShelleyAddress,
        submissionDeadline: QuantizedInstant,
        refundAddress: ShelleyAddress,
        refundDatum: Option[Data]
    ) {
        def result: Either[Build.Error, DepositRefundTxSeq] = {
            for {

                virtualValue <- Right(
                  Value.combine(
                    virtualOutputs.toList.map(vo => Value(vo.l2OutputValue))
                  )
                )

                expectedDepositValue = virtualValue + Value(depositFee)

                refundInstructions = DepositUtxo.Refund.Instructions(
                  address = refundAddress,
                  datum = refundDatum,
                  validityStart = config.txTiming.refundValidityStart(submissionDeadline)
                )

                depositTx <- DepositTx
                    .Build(config)(
                      utxosFunding,
                      virtualOutputs,
                      expectedDepositValue,
                      changeAddress,
                      submissionDeadline,
                      refundInstructions
                    )
                    .result
                    .left
                    .map(Build.Error.Deposit(_))

                refundTx <- RefundTx.Build
                    .PostDated(config)(
                      depositValue = virtualValue + Value(depositFee),
                      refundInstructions = refundInstructions
                    )
                    .result
                    .left
                    .map(Build.Error.Refund(_))

                // Run some sanity-checks
                depositUtxoValue = depositTx.depositProduced.value
                _ <- Either
                    .cond(
                      depositUtxoValue == expectedDepositValue,
                      (),
                      Build.Error.DepositValueMismatch(depositUtxoValue, expectedDepositValue)
                    )

                /*
                Obnoxious timing note (sorry):

                The Deposit tx validity end, deposit utxo absorption period, and post-dated refund tx timing are
                all intertwined as follows:

                deposit(i).absorption_start = deposit(i).validity_end + deposit_maturity_duration
                deposit(i).absorption_end = deposit(i).absorption_start + deposit_absorption_duration
                                          = deposit(i).validity_end + deposit_maturity_duration + deposit_absorption_duration

                refund(i).validity_start = deposit(i).absorption_end + silence_duration
                refund(i).validity_end = ∅

                The question becomes: which is authoritative? The refund validity start or the deposit validity end?
                And where do we store this data in our program?

                We have to, unfortunately, store this information in three places, and they have to be coherent.
                When we get a DepositRefundTxSeq, we need the individual transactions to have coherent timing, but we
                _also_ need to store the refund validity start time in the datum of the deposit (for use in the forthcoming
                guard script.)
                 */
                _ <- Either
                    .cond(
                      config.txTiming.refundValidityStart(
                        depositTx.validityEnd
                      ) == refundTx.startTime
                          && refundTx.startTime == depositTx.depositProduced.datum.refundInstructions.validityStart
                              .toEpochQuantizedInstant(config.slotConfig),
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
            case DepositValueMismatch(parsed: Value, expected: Value)
            case Refund(e: RefundTx.Parse.Error)
            case RefundNotPostDated
            case RefundTxMismatch(parsed: RefundTx, expected: RefundTx)
            case ExpectedRefundBuildError(e: (SomeBuildErrorOnly, String))
            case VirtualOutputs(e: Throwable)
            case NoVirtualOutputs
            case NonBabbageVirtualOutput(output: TransactionOutput)
            case VirtualOutputNotShelleyAddress(output: TransactionOutput)
            case VirtualOutputDatumNotInline(output: TransactionOutput)
            case VirtualOutputMultiAssetNotEmpty(output: TransactionOutput)
            case VirtualOutputRefScriptInvalid(output: TransactionOutput)
            case TimingIncoherence
        }
    }

    /** VirtualOutputs are encoded in CBOR as a list of Babbage outputs. Internally, they are
      * represented as a more restrictive type ([[GenesisObligation]]) that ensure L2 conformance
      *
      * @param depositTxBytes
      * @param refundTxBytes
      * @param virtualOutputsBytes
      * @param donationToTreasury
      * @param config
      * @return
      */
    final case class Parse(config: Config)(
        depositTxBytes: Tx.Serialized,
        refundTxBytes: Tx.Serialized,
        virtualOutputsBytes: Array[Byte],
        donationToTreasury: Coin,
    ) {
        import Parse.*

        def result: ParseErrorOr[DepositRefundTxSeq] = {
            for {
                virtualOutputs: NonEmptyList[GenesisObligation] <- for {
                    parsed <- Cbor
                        .decode(virtualOutputsBytes)
                        .to[List[TransactionOutput]]
                        .valueTry
                        .toEither
                        .left
                        .map(Parse.Error.VirtualOutputs(_))
                    nonEmpty <- NonEmptyList.fromList(parsed).toRight(Parse.Error.NoVirtualOutputs)
                    // This whole inner traverse could probably be factored out just to parse a genesis obligation from
                    // a transaction output
                    genesisObligations <- nonEmpty.traverse[ParseErrorOr, GenesisObligation](
                      GenesisObligation.fromTransactionOutput
                    )
                } yield genesisObligations

                virtualValue = Value.combine(
                  virtualOutputs.toList.map(vo => Value(vo.l2OutputValue))
                )

                depositTx <- DepositTx
                    .Parse(config)(txBytes = depositTxBytes, virtualOutputs = virtualOutputs)
                    .result
                    .left
                    .map(Parse.Error.Deposit(_))

                refundTxAny <- RefundTx
                    .Parse(config)(refundTxBytes)
                    .result
                    .left
                    .map(Parse.Error.Refund(_))
                refundTx <- refundTxAny match {
                    case tx: RefundTx.PostDated => Right(tx)
                    case _                      => Left(Parse.Error.RefundNotPostDated)
                }

                depositUtxo = depositTx.depositProduced
                depositValue = depositUtxo.value

                refundInstructions = depositUtxo.datum.refundInstructions
                refundFee = refundTx.tx.body.value.fee
                refundValue = depositValue - Value(refundFee)

                expectedDepositValue = virtualValue + Value(donationToTreasury + refundFee)

                expectedRefundTx <- RefundTx.Build
                    .PostDated(config)(refundValue, ???)
                    .result
                    .left
                    .map(Parse.Error.ExpectedRefundBuildError(_))

                _ <- Either.cond(
                  depositValue == expectedDepositValue,
                  (),
                  Parse.Error.DepositValueMismatch(depositValue, expectedDepositValue)
                )

                _ <- Either.cond(
                  {
                      // The transaction we build to check against will not have signatures, but it will have
                      // the native script witness. Thus, we need to ONLY nullify signatures and not the entire
                      // witness set.
                      val actualRefundTxWithNullifiedVKeyWitnesses = refundTx
                          .focus(_.tx.witnessSetRaw)
                          .andThen(keepRawL())
                          .andThen(Focus[TransactionWitnessSet](_.vkeyWitnesses))
                          .replace(TaggedSortedSet.empty)
                      actualRefundTxWithNullifiedVKeyWitnesses == expectedRefundTx
                  },
                  (),
                  Parse.Error.RefundTxMismatch(refundTx, expectedRefundTx)
                )

                _ <- Either
                    .cond(
                      depositTx.validityEnd + config.txTiming.depositMaturityDuration + config.txTiming.depositAbsorptionDuration
                          + config.txTiming.silenceDuration
                          == refundTx.startTime
                          && refundTx.startTime == depositTx.depositProduced.datum.refundInstructions.validityStart
                              .toEpochQuantizedInstant(config.slotConfig),
                      (),
                      Parse.Error.TimingIncoherence // we don't return a DepositRefundTxSeq, because it's not valid
                    )

            } yield DepositRefundTxSeq(depositTx, refundTx)
        }
    }

}
