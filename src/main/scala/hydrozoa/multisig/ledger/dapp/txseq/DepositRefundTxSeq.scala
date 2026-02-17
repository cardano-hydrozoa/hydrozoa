package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.multisig.timing.TxTiming
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.toEpochQuantizedInstant
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.SomeBuildErrorOnly
import hydrozoa.multisig.ledger.dapp.tx.{DepositTx, RefundTx, Tx}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.tx.GenesisObligation
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, TransactionOutput, TransactionWitnessSet, Utxo, Value}

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

    final case class Build(config: Config)(
        refundInstructions: DepositUtxo.Refund.Instructions,
        refundValue: Value,
        utxosFunding: NonEmptyList[Utxo],
        virtualOutputs: NonEmptyList[GenesisObligation],
        depositFee: Coin,
        changeAddress: ShelleyAddress,
    ) {
        def result: Either[Build.Error, DepositRefundTxSeq] = {
            for {
                partialRefundTx <- RefundTx.Build
                    .PostDated(config)(refundInstructions, refundValue)
                    .partialResult
                    .left
                    .map(Build.Error.Refund(_))

                depositTx <- DepositTx
                    .Build(config)(
                      partialRefundTx,
                      utxosFunding,
                      virtualOutputs,
                      depositFee,
                      changeAddress,
                    )
                    .result
                    .left
                    .map(Build.Error.Deposit(_))

                refundTx <- partialRefundTx
                    .complete(depositTx.depositProduced, config)
                    .left
                    .map(Build.Error.Refund(_))

                virtualValue = Value.combine(
                  virtualOutputs.toList.map(vo => Value(vo.l2OutputValue))
                )

                refundFee = refundTx.tx.body.value.fee

                depositValue = depositTx.depositProduced.l1OutputValue

                expectedDepositValue = virtualValue + Value(depositFee + refundFee)

                _ <- Either
                    .cond(
                      depositValue == expectedDepositValue,
                      (),
                      Build.Error.DepositValueMismatch(depositValue, virtualValue)
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
                      depositTx.validityEnd
                          + config.txTiming.depositMaturityDuration
                          + config.txTiming.depositAbsorptionDuration
                          + config.txTiming.silenceDuration
                          == refundTx.startTime
                          && refundTx.startTime == depositTx.depositProduced.datum.refundInstructions.startTime
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
                depositValue = depositUtxo.l1OutputValue

                refundInstructions = depositUtxo.l1OutputDatum.refundInstructions
                refundFee = refundTx.tx.body.value.fee
                refundValue = depositValue - Value(refundFee)

                expectedDepositValue = virtualValue + Value(donationToTreasury + refundFee)

                expectedRefundTx <- RefundTx.Build
                    .PostDated(config)(refundInstructions, refundValue)
                    .partialResult
                    .flatMap(_.complete(depositUtxo, config))
                    .left
                    .map(Parse.Error.ExpectedRefundBuildError(_))

                _ <- Either.cond(
                  depositValue == expectedDepositValue,
                  (),
                  Parse.Error.DepositValueMismatch(depositValue, expectedDepositValue)
                )

                _ <- Either.cond(
                  refundTx
                      .focus(_.tx.witnessSet)
                      .replace(TransactionWitnessSet.empty) == expectedRefundTx,
                  (),
                  Parse.Error.RefundTxMismatch(refundTx, expectedRefundTx)
                )

                _ <- Either
                    .cond(
                      depositTx.validityEnd + config.txTiming.depositMaturityDuration + config.txTiming.depositAbsorptionDuration
                          + config.txTiming.silenceDuration
                          == refundTx.startTime
                          && refundTx.startTime == depositTx.depositProduced.datum.refundInstructions.startTime
                              .toEpochQuantizedInstant(config.slotConfig),
                      (),
                      Parse.Error.TimingIncoherence // we don't return a DepositRefundTxSeq, because it's not valid
                    )

            } yield DepositRefundTxSeq(depositTx, refundTx)
        }
    }

}
