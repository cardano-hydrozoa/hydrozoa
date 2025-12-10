package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.explainConst
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import io.bullet.borer.Encoder
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, platform}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.given
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, Send, Spend}
import scalus.serialization.cbor.Cbor

final case class DepositTx(
    depositProduced: DepositUtxo,
    override val tx: Transaction
) extends Tx

object DepositTx {
    object Builder {
        type Error = SomeBuildError | Error.InsufficientFundingForVirtualOutputs

        object Error {
            final case class InsufficientFundingForVirtualOutputs(diff: Value) extends Throwable
        }

        final case class Result(
            depositTx: DepositTx,
            refundTx: RefundTx.PostDated
        )
    }

    private given Encoder[TransactionOutput.Babbage] =
        summon[Encoder[TransactionOutput]].asInstanceOf[Encoder[TransactionOutput.Babbage]]

    final case class Builder(
        override val config: Tx.Builder.Config,
        partialRefundTx: RefundTx.Builder.PartialResult.PostDated,
        utxosFunding: NonEmptyList[Utxo],
        virtualOutputs: NonEmptyList[TransactionOutput.Babbage],
        changeAddress: ShelleyAddress
    ) extends Tx.Builder {
        def build: Either[(Builder.Error, String), Builder.Result] = {
            import partialRefundTx.builder.refundInstructions

            val depositValue = partialRefundTx.inputValueNeeded

            val virtualOutputsList = virtualOutputs.toList

            val virtualValue = Value.combine(virtualOutputsList.map(_.value))

            val virtualOutputsCbor: Array[Byte] = Cbor.encode(virtualOutputsList)

            val virtualOutputsHash: Hash32 = Hash[Blake2b_256, Any](
              platform.blake2b_256(ByteString.unsafeFromArray(virtualOutputsCbor))
            )

            val stepRefundMetadata =
                ModifyAuxiliaryData(_ =>
                    Some(
                      MD(
                        MD.Deposit(
                          headAddress = config.headAddress,
                          depositUtxoIx = 0, // This builder produces the deposit utxo at index 0
                          virtualOutputsHash = virtualOutputsHash
                        )
                      )
                    )
                )

            val spendUtxosFunding = utxosFunding.toList.map(Spend(_, PubKeyWitness))

            val depositDatum: DepositUtxo.Datum = DepositUtxo.Datum(refundInstructions)

            val rawDepositProduced = TransactionOutput.Babbage(
              address = config.headAddress,
              value = depositValue,
              datumOption = Some(DatumOption.Inline(toData(depositDatum))),
              scriptRef = None
            )

            val sendDeposit = Send(rawDepositProduced)

            val sendChange = Send(
              TransactionOutput.Babbage(
                address = changeAddress,
                value = Value.zero,
                datumOption = None,
                scriptRef = None
              )
            )

            for {
                _ <- Either
                    .cond(
                      (depositValue.coin >= virtualValue.coin) && (depositValue.assets == virtualValue.assets),
                      (),
                      Builder.Error
                          .InsufficientFundingForVirtualOutputs(depositValue - virtualValue)
                    )
                    .explainConst("insufficient funding in deposit utxo for virtual outputs")

                ctx <- TransactionBuilder
                    .build(
                      config.env.network,
                      spendUtxosFunding ++ List(stepRefundMetadata, sendDeposit, sendChange)
                    )
                    .explainConst("building unbalanced deposit tx failed")

                finalized <- ctx
                    .finalizeContext(
                      config.env.protocolParams,
                      diffHandler = new ChangeOutputDiffHandler(
                        config.env.protocolParams,
                        1
                      ).changeOutputDiffHandler,
                      evaluator = config.evaluator,
                      validators = config.validators
                    )
                    .explainConst("balancing deposit tx failed")

                tx = finalized.transaction

                depositProduced = DepositUtxo(
                  TransactionInput(tx.id, 0),
                  config.headAddress,
                  depositDatum,
                  rawDepositProduced.value,
                  virtualOutputs
                )

                depositTx = DepositTx(depositProduced, tx)

                refundTx <- partialRefundTx.complete(depositProduced)
            } yield Builder.Result(depositTx, refundTx)
        }
    }

    // TODO: Restore
    sealed trait ParseError extends Throwable

    // TODO: Restore
    def parse(txBytes: Tx.Serialized): Either[ParseError, DepositTx] = ???
}
