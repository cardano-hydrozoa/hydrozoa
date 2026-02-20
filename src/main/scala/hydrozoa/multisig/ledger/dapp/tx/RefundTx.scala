package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.config.head.initialization.InitialBlock
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.{QuantizedInstant, toQuantizedInstant}
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.explainConst
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import monocle.{Focus, Lens}
import scala.util.{Failure, Success}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.SomeBuildError.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, Send, Spend, ValidityStartSlot}

// TODO: I would prefer to have explicit reference - for which deposit utxo that refund tx relates to.

sealed trait RefundTx {
    def tx: Transaction
    def mStartTime: Option[QuantizedInstant] = this match {
        case self: RefundTx.PostDated => Some(self.startTime)
        case _                        => None
    }
}

object RefundTx {
    export RefundTxOps.{Build, Parse}

    final case class PostDated(override val tx: Transaction, startTime: QuantizedInstant)
        extends RefundTx,
          Tx[PostDated] {
        override val txLens: Lens[PostDated, Transaction] = Focus[PostDated](_.tx)
        override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
    }
}

private object RefundTxOps {
    type Config = CardanoNetwork.Section & HeadPeers.Section & InitialBlock.Section

    object Build {

        final case class PostDated(override val config: Config)(
            override val depositUtxo: DepositUtxo,
            override val refundInstructions: DepositUtxo.Refund.Instructions
        ) extends Build[RefundTx.PostDated] {

            override val stepRefundMetadata =
                ModifyAuxiliaryData(_ =>
                    Some(MD(MD.Refund(headAddress = config.headMultisigAddress)))
                )

            override def result: Either[(SomeBuildError, String), RefundTx.PostDated] =

                val stepSpendDeposit =
                    Spend(depositUtxo.toUtxo, config.headMultisigScript.witnessValue)

                val refundOutput: TransactionOutput = TransactionOutput.Babbage(
                  address = refundInstructions.address,
                  value = depositUtxo.value,
                  datumOption = refundInstructions.mbDatum.map(Inline(_)),
                  scriptRef = None
                )
                val stepSendRefund = Send(refundOutput)
                val stepSetValidity = ValidityStartSlot(
                  refundInstructions.validityStart.toSlot.slot
                )

                val steps =
                    List(stepSpendDeposit, stepSendRefund, stepRefundMetadata, stepSetValidity)

                for {
                    ctx <- TransactionBuilder
                        .build(config.network, steps)
                        .explainConst("adding base refund steps failed")

                    // _ = println(HexUtil.encodeHexString(ctx.transaction.toCbor))

                    finalized <- ctx
                        .finalizeContext(
                          config.cardanoProtocolParams,
                          diffHandler = Change
                              .changeOutputDiffHandler(_, _, config.cardanoProtocolParams, 0),
                          evaluator = config.plutusScriptEvaluatorForTxBuild,
                          validators = Tx.Validators.nonSigningNonValidityChecksValidators
                        )
                        .explainConst("balancing refund tx failed")

                    tx = finalized.transaction
                } yield RefundTx.PostDated(
                  tx,
                  refundInstructions.validityStart
                )
        }
    }

    trait Build[T <: RefundTx] {
        def config: Config
        def depositUtxo: DepositUtxo
        def refundInstructions: DepositUtxo.Refund.Instructions
        //
        def stepRefundMetadata: ModifyAuxiliaryData
        def result: Either[(SomeBuildError, String), T]
    }

    object Parse {
        type ParseErrorOr[A] = Either[Error, A]

        enum Error extends Throwable {
            case MetadataParseError(wrapped: MD.ParseError)
            case TxCborDeserializationFailed(e: Throwable)
            case ValidityStartIsMissing
        }
    }

    final case class Parse(config: Config)(
        txBytes: Tx.Serialized,
    ) {
        import Parse.*
        import Parse.Error.*

        def result: ParseErrorOr[RefundTx] = {

            given OriginalCborByteArray = OriginalCborByteArray(txBytes)
            given ProtocolVersion = config.cardanoProtocolVersion

            io.bullet.borer.Cbor.decode(txBytes).to[Transaction].valueTry match {
                case Success(tx) =>
                    for {

                        _ <- MD.parse(tx) match {
                            case Right(md: Metadata.Refund) => Right(md)
                            case Right(md) =>
                                Left(MetadataParseError(MD.UnexpectedTxType(md, "Refund")))
                            case Left(e) => Left(MetadataParseError(e))
                        }

                        refundTx: RefundTx <- tx.body.value.validityStartSlot match {
                            case Some(startSlot) =>
                                Right(
                                  RefundTx.PostDated(
                                    tx,
                                    Slot(startSlot).toQuantizedInstant(config.slotConfig)
                                  )
                                )
                            case None => Left(ValidityStartIsMissing)
                        }
                    } yield refundTx
                case Failure(e) => Left(TxCborDeserializationFailed(e))
            }
        }
    }
}
