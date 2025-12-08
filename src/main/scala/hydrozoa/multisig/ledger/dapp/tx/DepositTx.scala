package hydrozoa.multisig.ledger.dapp.tx

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.tx.Tx.Builder.{BuildErrorOr, explainConst}
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler
import scalus.cardano.txbuilder.TransactionBuilderStep.{ModifyAuxiliaryData, Send, Spend}

final case class DepositTx(
    depositProduced: DepositUtxo,
    override val tx: Transaction
) extends Tx

object DepositTx {
    // TODO: Restore
    sealed trait ParseError extends Throwable

    object Builder {
        final case class Result(
            depositTx: DepositTx,
            refundTx: RefundTx.PostDated
        )
    }

    final case class Builder(
        override val config: Tx.Builder.Config,
        partialRefundTx: RefundTx.Builder.PartialResult[RefundTx.PostDated],
        utxosFunding: NonEmptyList[Utxo],
        virtualOutputs: NonEmptyList[TransactionOutput.Babbage],
        changeAddress: ShelleyAddress
    ) extends Tx.Builder {
        def build: BuildErrorOr[Builder.Result] = {
            import partialRefundTx.builder.refundInstructions

            // TODO: Add the `virtualOutputs` hash to the deposit tx metadata.
            val stepRefundMetadata =
                ModifyAuxiliaryData(_ => Some(MD(MD.Deposit(headAddress = config.headAddress))))

            val spendUtxosFunding = utxosFunding.toList.map(Spend(_, PubKeyWitness))

            val depositDatum: DepositUtxo.Datum = DepositUtxo.Datum(refundInstructions)
            val rawDepositProduced = TransactionOutput.Babbage(
              address = config.headAddress,
              value = partialRefundTx.inputValueNeeded,
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
                  rawDepositProduced.value
                )

                depositTx = DepositTx(depositProduced, tx)

                refundTx <- partialRefundTx.complete(depositProduced)
            } yield Builder.Result(depositTx, refundTx)
        }
    }

    // TODO: Restore
    def parse(txBytes: Tx.Serialized): Either[ParseError, DepositTx] = ???
}
