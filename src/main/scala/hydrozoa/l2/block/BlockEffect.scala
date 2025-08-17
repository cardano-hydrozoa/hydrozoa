package hydrozoa.l2.block

import hydrozoa.UtxoSetL2
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l1.multisig.tx.{FinalizationTx, SettlementTx}
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.node.state.L1BlockEffect
import hydrozoa.node.state.L1BlockEffect.{
    FinalizationTxEffect,
    MinorBlockL1Effect,
    SettlementTxEffect
}

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
            case Minor => MinorBlockL1Effect(Seq.empty)
            case Major =>
                // Create settlement tx draft
                val txRecipe = SettlementRecipe(
                  block.blockHeader.versionMajor,
                  block.blockBody.depositsAbsorbed,
                  utxosWithdrawn
                )
                val settlementTxDraft: SettlementTx =
                    settlementTxBuilder.mkSettlementTxDraft(txRecipe) match {
                        case Right(stxd) => stxd
                        case Left(e)     => throw new RuntimeException(e)
                    }
                SettlementTxEffect(settlementTxDraft)
            case Final =>
                // Create finalization tx draft
                val recipe =
                    FinalizationRecipe(block.blockHeader.versionMajor, utxosWithdrawn)
                val finalizationTxDraft: FinalizationTx =
                    finalizationTxBuilder.buildFinalizationTxDraft(recipe) match {
                        case Right(ftxd) => ftxd
                        case Left(e)     => throw new RuntimeException(e)
                    }
                FinalizationTxEffect(finalizationTxDraft)
