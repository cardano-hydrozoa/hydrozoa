package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.tx.{DepositTx, RefundTx, Tx}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import io.bullet.borer.Cbor
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{Coin, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.SomeBuildError

final case class DepositRefundTxSeq(
    depositTx: DepositTx,
    refundTx: RefundTx
)

object DepositRefundTxSeq {
    object Builder {
        sealed trait Error extends Throwable

        object Error {
            final case class Deposit(e: (DepositTx.Builder.Error, String)) extends Builder.Error
            final case class Refund(e: (SomeBuildError, String)) extends Builder.Error
        }
    }

    final case class Builder(
        config: Tx.Builder.Config,
        refundInstructions: DepositUtxo.Refund.Instructions,
        refundValue: Value,
        utxosFunding: NonEmptyList[Utxo],
        virtualOutputs: NonEmptyList[TransactionOutput.Babbage],
        donationToTreasury: Coin,
        changeAddress: ShelleyAddress
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
                  changeAddress
                )
                .build()
                .left
                .map(Builder.Error.Deposit(_))

            refundTx <- partialRefundTx
                .complete(depositTx.depositProduced, config)
                .left
                .map(Builder.Error.Refund(_))
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
    }

    def parse(
        depositTxBytes: Tx.Serialized,
        refundTxBytes: Tx.Serialized,
        virtualOutputsBytes: Array[Byte],
        donationToTreasury: Coin,
        config: Tx.Builder.Config
    ): Either[ParseError, DepositRefundTxSeq] = for {
        virtualOutputs <- for {
            parsed <- Cbor
                .decode(virtualOutputsBytes)
                .to[List[TransactionOutput]]
                .valueTry
                .toEither
                .left
                .map(ParseError.VirtualOutputs(_))
            nonEmpty <- NonEmptyList.fromList(parsed).toRight(ParseError.NoVirtualOutputs)
            babbage <- nonEmpty.traverse {
                case o: TransactionOutput.Babbage => Right(o)
                case o: TransactionOutput.Shelley => Left(ParseError.NonBabbageVirtualOutput(o))
            }
        } yield babbage

        virtualValue = Value.combine(virtualOutputs.toList.map(_.value))

        depositTx <- DepositTx
            .parse(depositTxBytes, config, virtualOutputs)
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
          refundTx == expectedRefundTx,
          (),
          ParseError.RefundTxMismatch(refundTx, expectedRefundTx)
        )

    } yield DepositRefundTxSeq(depositTx, refundTx)
}
