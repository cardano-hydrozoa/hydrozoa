package hydrozoa.l2.consensus.network

import hydrozoa.infra.{createTxKeyWitness, genNodeKey}
import hydrozoa.l1.Cardano
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.tx.MultisigTxs.DepositTx
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l2.block.Block
import hydrozoa.l2.ledger.state.UtxosDiff
import hydrozoa.node.server.HeadStateReader
import hydrozoa.{ParticipantVerificationKey, TxAny, TxKeyWitness}

class MockHydrozoaNetwork(
    headStateReader: HeadStateReader,
    initTxBuilder: InitTxBuilder,
    refundTxBuilder: RefundTxBuilder,
    settlementTxBuilder: SettlementTxBuilder,
    finalizationTxBuilder: FinalizationTxBuilder,
    cardano: Cardano,
    theLastVerificationKey: ParticipantVerificationKey // this is the key of the only "real" node
) extends HydrozoaNetwork {

    private val keys1 = genNodeKey()
    private val keys2 = genNodeKey()

    override def participantsKeys(): Set[ParticipantVerificationKey] =
        Set(keys1, keys2).map(_._2)

    override def reqInit(req: ReqInit): Set[TxKeyWitness] = {
        // Head's verification keys
        val vKeys = Set(keys1, keys2).map(_._2) + theLastVerificationKey

        // Native script, head address, and token
        val (headNativeScript, headAddress) = mkHeadNativeScriptAndAddress(vKeys, cardano.network())
        val beaconTokenName = mkBeaconTokenName(req.txId, req.txIx)

        // Recipe to build init initializationTx
        val initTxRecipe = InitTxRecipe(
          headAddress,
          req.txId,
          req.txIx,
          req.amount,
          headNativeScript,
          beaconTokenName
        )

        val Right(initializationTx, _) = initTxBuilder.mkInitializationTxDraft(initTxRecipe)

        val wit1: TxKeyWitness = createTxKeyWitness(initializationTx.toTxL1, keys1._1)
        val wit2: TxKeyWitness = createTxKeyWitness(initializationTx.toTxL1, keys2._1)
        Set(wit1, wit2)
    }

    override def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness] =
        val recipe = PostDatedRefundRecipe(req.depositTx, req.index)
        val Right(tx) = refundTxBuilder.mkPostDatedRefundTxDraft(recipe)

        val wit1: TxKeyWitness = createTxKeyWitness(tx.toTxL1, keys1._1)
        val wit2: TxKeyWitness = createTxKeyWitness(tx.toTxL1, keys2._1)
        Set(wit1, wit2)

    override def reqMinor(block: Block): Set[AckMinor] =
        Set(keys1, keys2).map(_ => AckMinor(block.blockHeader, (), false))

    override def reqMajor(block: Block, utxosWithdrawn: UtxosDiff): Set[AckMajorCombined] =
        // TODO: check block type
        val recipe =
            SettlementRecipe(
              block.blockBody.depositsAbsorbed,
              utxosWithdrawn,
              block.blockHeader.versionMajor
            )
        val Right(settlementTx) = settlementTxBuilder.mkSettlementTxDraft(recipe)

        val wit1: TxKeyWitness = createTxKeyWitness(settlementTx.toTx, keys1._1)
        val wit2: TxKeyWitness = createTxKeyWitness(settlementTx.toTx, keys2._1)
        Set(wit1, wit2).map(w =>
            AckMajorCombined(
              block.blockHeader,
              Set.empty,
              w,
              false
            )
        )

    override def reqFinal(block: Block): Set[AckFinalCombined] =
        // TODO: check block type

        val recipe = FinalizationRecipe(block.blockHeader.versionMajor)

        val Right(finalizationTx) = finalizationTxBuilder.buildFinalizationTxDraft(recipe)

        val wit1: TxKeyWitness = createTxKeyWitness(finalizationTx.toTxL1, keys1._1)
        val wit2: TxKeyWitness = createTxKeyWitness(finalizationTx.toTxL1, keys2._1)
        Set(wit1, wit2).map(w =>
            AckFinalCombined(
              block.blockHeader,
              Set.empty,
              w
            )
        )

}
