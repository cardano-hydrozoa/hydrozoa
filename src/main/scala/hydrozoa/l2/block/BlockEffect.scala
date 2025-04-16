package hydrozoa.l2.block

import hydrozoa.Wallet
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l1.multisig.tx.{FinalizationTx, SettlementTx}
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.node.state.L1BlockEffect

object BlockEffect:
//    def mkL1BlockEffect(
//                           settlementTxBuilder: SettlementTxBuilder,
//                           finalizationTxBuilder: FinalizationTxBuilder,
//                           mbOwnPeer: Option[Wallet],
//                           mbNetwork: Option[HeadPeerNetwork],
//                           block: Block,
//                           utxosWithdrawn: UtxosSet
//                       ): L1BlockEffect =
//    block.blockHeader.blockType match
//        case Minor => ()
//        case Major =>
//            // Create settlement tx draft
//            val txRecipe = SettlementRecipe(
//              block.blockHeader.versionMajor,
//              block.blockBody.depositsAbsorbed,
//              utxosWithdrawn
//            )
//            val Right(settlementTxDraft: SettlementTx) =
//                settlementTxBuilder.mkSettlementTxDraft(txRecipe)
//            mbOwnPeer /\ mbNetwork match
//                case Some(ownPeer) -> Some(network) =>
//                    val ownWit: TxKeyWitness = ownPeer.createTxKeyWitness(settlementTxDraft)
//                    // TODO: broadcast ownWit
//
//                    // Confirm block
//                    val acksMajorCombined = network.reqMajor(block, utxosWithdrawn)
//
//                    TxDump.dumpMultisigTx(settlementTxDraft)
//
//                    // L1 effect
//                    val wits = acksMajorCombined.map(_.settlement) + ownWit
//                    val settlementTx = wits.foldLeft(settlementTxDraft)(addWitnessMultisig)
//                    // val serializedTx = serializeTxHex(settlementTx)
//
//                    settlementTx
//                case _ => // used in MBT
//                    settlementTxDraft
//        case Final =>
//            // Create finalization tx draft
//            val recipe =
//                FinalizationRecipe(block.blockHeader.versionMajor, utxosWithdrawn)
//            val Right(finalizationTxDraft: FinalizationTx) =
//                finalizationTxBuilder.buildFinalizationTxDraft(recipe)
//
//            mbOwnPeer /\ mbNetwork match
//                case Some(ownPeer) -> Some(network) =>
//                    val ownWit: TxKeyWitness = ownPeer.createTxKeyWitness(finalizationTxDraft)
//                    // TODO: broadcast ownWit
//
//                    // Confirm block
//                    val acksFinalCombined = network.reqFinal(block, utxosWithdrawn)
//
//                    // L1 effect
//                    val wits = acksFinalCombined.map(_.finalization) + ownWit
//                    val finalizationTx = wits.foldLeft(finalizationTxDraft)(addWitnessMultisig)
//                    // val serializedTx = serializeTxHex(finalizationTx)
//
//                    finalizationTx
//                case _ => // used in MBT
//                    finalizationTxDraft

    def mkL1BlockEffectModel(
        settlementTxBuilder: SettlementTxBuilder,
        finalizationTxBuilder: FinalizationTxBuilder,
        block: Block,
        utxosWithdrawn: UtxosSet
    ): L1BlockEffect =
        block.blockHeader.blockType match
            case Minor => ()
            case Major =>
                // Create settlement tx draft
                val txRecipe = SettlementRecipe(
                  block.blockHeader.versionMajor,
                  block.blockBody.depositsAbsorbed,
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
