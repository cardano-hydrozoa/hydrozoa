package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.tx.TxTiming.+
import hydrozoa.multisig.ledger.dapp.tx.{DepositTx, RefundTx, Tx, TxTiming}
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq.ParseError.VirtualOutputRefScriptInvalid
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, TransactionOutput, TransactionWitnessSet, Utxo, Value}
import scalus.cardano.txbuilder.SomeBuildError

final case class DepositRefundTxSeq(
    depositTx: DepositTx,
    refundTx: RefundTx
)

object DepositRefundTxSeq {
    object Builder {
        sealed trait Error extends Throwable

        object Error {
            final case class Deposit(e: (SomeBuildError, String)) extends Builder.Error
            final case class Refund(e: (SomeBuildError, String)) extends Builder.Error
            case object TimingIncoherence extends Builder.Error

            final case class DepositValueMismatch(depositValue: Value, expectedDepositValue: Value)
                extends Builder.Error
        }
    }

    final case class Builder(
        config: Tx.Builder.Config,
        refundInstructions: DepositUtxo.Refund.Instructions,
        refundValue: Value,
        utxosFunding: NonEmptyList[Utxo],
        virtualOutputs: NonEmptyList[GenesisObligation],
        donationToTreasury: Coin,
        changeAddress: ShelleyAddress,
        txTiming: TxTiming
    ) {
        def build: Either[Builder.Error, DepositRefundTxSeq] = for {
            partialRefundTx <- RefundTx.Builder
                .PostDated(config, refundInstructions, refundValue)
                .partialResult
                .left
                .map(Builder.Error.Refund(_))

            depositTx <- DepositTx
                .Builder(
                  config,
                  partialRefundTx,
                  utxosFunding,
                  virtualOutputs,
                  donationToTreasury,
                  changeAddress,
                  txTiming = txTiming
                )
                .build()
                .left
                .map(Builder.Error.Deposit(_))

            refundTx <- partialRefundTx
                .complete(depositTx.depositProduced, config)
                .left
                .map(Builder.Error.Refund(_))

            virtualValue = Value.combine(virtualOutputs.toList.map(vo => Value(vo.l2OutputValue)))

            refundFee = refundTx.tx.body.value.fee

            depositValue = depositTx.depositProduced.l1OutputValue

            expectedDepositValue = virtualValue + Value(donationToTreasury + refundFee)

            _ <- Either
                .cond(
                  depositValue == expectedDepositValue,
                  (),
                  Builder.Error.DepositValueMismatch(depositValue, virtualValue)
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
            // TODO: This assertion also needs to appear in the parser
            _ <- Either
                .cond(
                  depositTx.validityEnd + txTiming.depositMaturityDuration + txTiming.depositAbsorptionDuration + txTiming.silenceDuration
                      == refundTx.startTime
                      && refundTx.startTime.toEpochMilli == depositTx.depositProduced.datum.refundInstructions.startTime.toLong,
                  (),
                  Builder.Error.TimingIncoherence // we don't return a DepositRefundTxSeq, because it's not valid
                )
        } yield DepositRefundTxSeq(depositTx, refundTx)
    }

    sealed trait ParseError extends Throwable

    object ParseError {
        final case class Deposit(e: DepositTx.ParseError) extends ParseError
        final case class DepositValueMismatch(parsed: Value, expected: Value) extends ParseError

        final case class Refund(e: RefundTx.ParseError) extends ParseError
        case object RefundNotPostDated extends ParseError
        final case class RefundTxMismatch(parsed: RefundTx, expected: RefundTx) extends ParseError
        final case class ExpectedRefundBuildError(e: (SomeBuildError, String)) extends ParseError

        final case class VirtualOutputs(e: Throwable) extends ParseError
        case object NoVirtualOutputs extends ParseError
        final case class NonBabbageVirtualOutput(output: TransactionOutput) extends ParseError
        final case class VirtualOutputNotShelleyAddress(output: TransactionOutput)
            extends ParseError
        final case class VirtualOutputDatumNotInline(output: TransactionOutput) extends ParseError
        final case class VirtualOutputMultiAssetNotEmpty(output: TransactionOutput)
            extends ParseError
        final case class VirtualOutputRefScriptInvalid(output: TransactionOutput) extends ParseError
    }

    /** VirtualOutputs are encoded in CBOR as a list of Babbage outputs. Internally, they are
      * represented as a more restrictive type ([[GenesisObligation]]) that ensure L2 conformance
      * @param depositTxBytes
      * @param refundTxBytes
      * @param virtualOutputsBytes
      * @param donationToTreasury
      * @param config
      * @return
      */
    def parse(
        depositTxBytes: Tx.Serialized,
        refundTxBytes: Tx.Serialized,
        virtualOutputsBytes: Array[Byte],
        donationToTreasury: Coin,
        config: Tx.Builder.Config,
        txTiming: TxTiming,
    ): Either[ParseError, DepositRefundTxSeq] = for {
        virtualOutputs: NonEmptyList[GenesisObligation] <- for {
            parsed <- Cbor
                .decode(virtualOutputsBytes)
                .to[List[TransactionOutput]]
                .valueTry
                .toEither
                .left
                .map(ParseError.VirtualOutputs(_))
            nonEmpty <- NonEmptyList.fromList(parsed).toRight(ParseError.NoVirtualOutputs)
            // This whole inner traverse could probably be factored out just to parse a genesis obligation from
            // a transaction output
            genesisObligations <- nonEmpty.traverse(GenesisObligation.fromTransactionOutput)
        } yield genesisObligations

        virtualValue = Value.combine(virtualOutputs.toList.map(vo => Value(vo.l2OutputValue)))

        depositTx <- DepositTx
            .parse(
              txBytes = depositTxBytes,
              config = config,
              virtualOutputs = virtualOutputs,
              txTiming = txTiming
            )
            .left
            .map(ParseError.Deposit(_))

        refundTxAny <- RefundTx.parse(refundTxBytes, config.env).left.map(ParseError.Refund(_))
        refundTx <- refundTxAny match {
            case tx: RefundTx.PostDated => Right(tx)
            case _                      => Left(ParseError.RefundNotPostDated)
        }

        depositUtxo = depositTx.depositProduced
        depositValue = depositUtxo.l1OutputValue

        refundInstructions = depositUtxo.l1OutputDatum.refundInstructions
        refundFee = refundTx.tx.body.value.fee
        refundValue = depositValue - Value(refundFee)

        expectedDepositValue = virtualValue + Value(donationToTreasury + refundFee)

        expectedRefundTx <- RefundTx.Builder
            .PostDated(config, refundInstructions, refundValue)
            .partialResult
            .flatMap(_.complete(depositUtxo, config))
            .left
            .map(ParseError.ExpectedRefundBuildError(_))

        _ <- Either.cond(
          depositValue == expectedDepositValue,
          (),
          ParseError.DepositValueMismatch(depositValue, expectedDepositValue)
        )

        _ <- Either.cond(
          refundTx.focus(_.tx.witnessSet).replace(TransactionWitnessSet.empty) == expectedRefundTx,
          (),
          ParseError.RefundTxMismatch(refundTx, expectedRefundTx)
        )

    } yield DepositRefundTxSeq(depositTx, refundTx)
}
