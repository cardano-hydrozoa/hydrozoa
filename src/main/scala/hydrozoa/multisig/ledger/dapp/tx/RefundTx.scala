package hydrozoa.multisig.ledger.dapp.tx

import hydrozoa.*
import hydrozoa.lib.tx.*
import hydrozoa.lib.tx.ScriptSource.NativeScriptValue
import hydrozoa.lib.tx.SomeBuildError.*
import hydrozoa.lib.tx.TransactionBuilderStep.*
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.tx.Metadata as MD
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import scala.language.implicitConversions
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder
import scalus.cardano.ledger.txbuilder.LowLevelTxBuilder.ChangeOutputDiffHandler

sealed trait RefundTx {
    def depositSpent: DepositUtxo
}

object RefundTx {
    final case class Immediate(
        override val depositSpent: DepositUtxo,
        override val tx: Transaction
    ) extends Tx,
          RefundTx

    final case class PostDated(
        override val depositSpent: DepositUtxo,
        override val tx: Transaction
    ) extends Tx,
          RefundTx

    object Immediate {
        def build(): Immediate = ???
    }

    object PostDated {
        case class Recipe(
            depositTx: DepositTx,
            txIx: Int,
            network: Network,
            protocolParams: ProtocolParams,
            evaluator: PlutusScriptEvaluator,
            validators: Seq[Validator],
            headScript: HeadMultisigScript,
            validityStartSlot: Slot
        )

        def build(recipe: Recipe): Either[SomeBuildError, PostDated] = {
            /////////////////////////////////////////////////////////////////////
            // Data extraction

            val deposit = recipe.depositTx.depositProduced
            // NB: Fee is paid from deposit itself
            val depositDatum = deposit._3
            val refundOutput: TransactionOutput =
                TransactionOutput(
                  address = depositDatum.refundAddress.toScalusLedger(network = recipe.network),
                  value = Value(deposit._4),
                  datumOption = depositDatum.refundDatum.asScala.map(Inline(_))
                )

            /////////////////////////////////////////////////////////////////////
            // Step definitions

            val spendDeposit = Spend(
              utxo = TransactionUnspentOutput(recipe.depositTx.depositProduced.toUtxo),
              witness = NativeScriptWitness(
                scriptSource = NativeScriptValue(recipe.headScript.script),
                additionalSigners =
                    recipe.headScript.requiredSigners.toSortedSet.toSet.map(ExpectedSigner(_))
              )
            )

            val createRefund: Send = Send(refundOutput)

            val setValidity = ValidityStartSlot(recipe.validityStartSlot.slot)

            val modifyAuxiliaryData = ModifyAuxiliaryData(_ =>
                Some(
                  MD(
                    MD.L1TxTypes.RefundPostDated,
                    recipe.headScript.address(recipe.network)
                  )
                )
            )

            val steps = List(spendDeposit, createRefund, setValidity, modifyAuxiliaryData)

            //////////////////////////////////////////////////////////////////////
            // Build and finalize

            for {
                unbalanced <- TransactionBuilder
                    .build(recipe.network, steps)
                finalized <- unbalanced
                    .finalizeContext(
                      recipe.protocolParams,
                      diffHandler = new ChangeOutputDiffHandler(
                        recipe.protocolParams,
                        0
                      ).changeOutputDiffHandler,
                      evaluator = recipe.evaluator,
                      validators = recipe.validators
                    )
            } yield PostDated(
              depositSpent = recipe.depositTx.depositProduced,
              tx = finalized.transaction
            )
        }

    }
}
