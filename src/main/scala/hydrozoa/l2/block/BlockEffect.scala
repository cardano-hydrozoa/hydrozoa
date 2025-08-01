package hydrozoa.l2.block

import hydrozoa.UtxoSetL2
import hydrozoa.infra.transitionary.toHydrozoa
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l1.multisig.tx.{FinalizationTx, SettlementTx}
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.node.state.L1BlockEffect

/** This is used in MBT-suite, we can move it to integration subproject.
  */
object BlockEffect:
    def mkL1BlockEffectModel(
        settlementTxBuilder: SettlementTxBuilder,
        finalizationTxBuilder: FinalizationTxBuilder,
        block: Block,
        utxosWithdrawn: UtxoSetL2
    ): L1BlockEffect =
        block.blockHeader.blockType match
            case Minor => Seq.empty
            case Major =>
                // Create settlement tx draft
                val txRecipe = SettlementRecipe(
                  block.blockHeader.versionMajor,
                  block.blockBody.depositsAbsorbed.map(_.toHydrozoa),
                  utxosWithdrawn
                )
                val Right(settlementTxDraft: SettlementTx) =
                    settlementTxBuilder.mkSettlementTxDraft(txRecipe)
                settlementTxDraft
            case Final =>
                // Create finalization tx draft
                val recipe =
                    FinalizationRecipe(block.blockHeader.versionMajor, utxosWithdrawn)
                val Right(finalizationTxDraft: FinalizationTx) =
                    finalizationTxBuilder.buildFinalizationTxDraft(recipe)
                finalizationTxDraft
