package hydrozoa.multisig.ledger.l1.tx

import hydrozoa.config.head.initialization.{InitialBlock, InitializationParameters}
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.peers.HeadPeers
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l1.tx.Metadata as MD
import hydrozoa.multisig.ledger.l1.tx.Tx.Builder.explainConst
import hydrozoa.multisig.ledger.l1.utxo.DepositUtxo
import hydrozoa.multisig.ledger.l2.Destination
import monocle.{Focus, Lens}
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
        case self: RefundTx.PostDated => Some(self.refundStart)
    }
}

object RefundTx {
    export RefundTxOps.Build

    final case class PostDated(
        override val tx: Transaction,
        refundStart: QuantizedInstant,
        refundDestination: Destination,
        requestId: RequestId
    ) extends RefundTx,
          Tx[PostDated] {
        override val txLens: Lens[PostDated, Transaction] = Focus[PostDated](_.tx)
        override val resolvedUtxos: ResolvedUtxos = ResolvedUtxos.empty
    }
}

private object RefundTxOps {
    type Config = CardanoNetwork.Section & HeadPeers.Section & InitialBlock.Section &
        InitializationParameters.Section

    object Build {

        final case class PostDated(override val config: Config)(
            override val depositUtxo: DepositUtxo,
            override val refundInstructions: DepositUtxo.Refund.Instructions,
            override val requestId: RequestId
        ) extends Build[RefundTx.PostDated] {

            override val stepRefundMetadata =
                ModifyAuxiliaryData(_ => Some(MD.Refund().asAuxData(config.headId)))

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
                  refundInstructions.validityStart,
                  Destination(refundInstructions.address, refundInstructions.mbDatum),
                  requestId
                )
        }
    }

    trait Build[T <: RefundTx] {
        def config: Config
        def depositUtxo: DepositUtxo
        def refundInstructions: DepositUtxo.Refund.Instructions
        def requestId: RequestId
        //
        def stepRefundMetadata: ModifyAuxiliaryData
        def result: Either[(SomeBuildError, String), T]
    }

}
