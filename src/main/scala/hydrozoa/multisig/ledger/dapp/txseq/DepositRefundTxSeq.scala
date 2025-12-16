package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.tx.{DepositTx, RefundTx, Tx}
import hydrozoa.multisig.ledger.dapp.txseq.DepositRefundTxSeq.ParseError.VirtualOutputRefScriptInvalid
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import hydrozoa.multisig.ledger.virtual.GenesisObligation
import io.bullet.borer.Cbor
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.{Coin, Script, ScriptRef, TransactionOutput, Utxo, Value}
import scalus.cardano.txbuilder.SomeBuildError
import scalus.prelude.Option as SOption

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
        config: Tx.Builder.Config
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
            genesisObligations <- nonEmpty.traverse {
                case o: TransactionOutput.Babbage =>
                    for {
                        shelleyAddress: ShelleyAddress <- o.address match {
                            case sa: ShelleyAddress
                                if sa.delegation == ShelleyDelegationPart.Null =>
                                Right(sa)
                            case _ => Left(ParseError.VirtualOutputNotShelleyAddress(o))
                        }
                        datum <- o.datumOption match {
                            case None            => Right(SOption.None)
                            case Some(i: Inline) => Right(SOption.Some(i.data))
                            case Some(_)         => Left(ParseError.VirtualOutputDatumNotInline(o))
                        }
                        coin <-
                            if o.value.assets.isEmpty
                            then Right(o.value.coin)
                            else Left(ParseError.VirtualOutputMultiAssetNotEmpty(o))
                        refScript: Option[Native | Script.PlutusV3] <- o.scriptRef match {
                            case None                                => Right(None)
                            case Some(ScriptRef(s: Script.PlutusV3)) => Right(Some(s))
                            case Some(ScriptRef(s: Native))          => Right(Some(s))
                            case Some(_) => Left(VirtualOutputRefScriptInvalid(o))
                        }
                    } yield GenesisObligation(
                      l2OutputPaymentAddress = shelleyAddress.payment,
                      l2OutputNetwork = shelleyAddress.network,
                      l2OutputDatum = datum,
                      l2OutputValue = coin,
                      l2OutputRefScript = refScript,
                    )
                case o: TransactionOutput.Shelley => Left(ParseError.NonBabbageVirtualOutput(o))
            }
        } yield genesisObligations

        virtualValue = Value.combine(virtualOutputs.toList.map(vo => Value(vo.l2OutputValue)))

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
