package hydrozoa.l2.consensus.network

import hydrozoa.infra.{genNodeKey, createTxKeyWitness}
import hydrozoa.l1.Cardano
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.tx.MultisigTxs.DepositTx
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l2.block.Block
import hydrozoa.node.server.{AwaitingDeposit, HeadStateReader}
import hydrozoa.{L1Tx, ParticipantVerificationKey, TxKeyWitness}

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

        // Recipe to build init tx
        val initTxRecipe = InitTxRecipe(
          headAddress,
          req.txId,
          req.txIx,
          req.amount,
          headNativeScript,
          beaconTokenName
        )

        val Right(tx, _) = initTxBuilder.mkInitializationTxDraft(initTxRecipe)

        val wit1: TxKeyWitness = createTxKeyWitness(tx, keys1._1)
        val wit2: TxKeyWitness = createTxKeyWitness(tx, keys2._1)
        Set(wit1, wit2)
    }

    def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness] =
        val recipe = PostDatedRefundRecipe(DepositTx(req.depositTx), req.index)
        val Right(tx) = refundTxBuilder.mkPostDatedRefundTxDraft(recipe)

        val wit1: TxKeyWitness = createTxKeyWitness(tx.toTx, keys1._1)
        val wit2: TxKeyWitness = createTxKeyWitness(tx.toTx, keys2._1)
        Set(wit1, wit2)

    override def reqMajor(block: Block): Set[AckMajorCombined] =
        // TODO: check block type
        val recipe =
            SettlementRecipe(block.blockBody.depositsAbsorbed, block.blockHeader.versionMajor)
        val Right(tx) = settlementTxBuilder.mkSettlementTxDraft(recipe)

        val wit1: TxKeyWitness = createTxKeyWitness(tx.toTx, keys1._1)
        val wit2: TxKeyWitness = createTxKeyWitness(tx.toTx, keys2._1)
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

        val Right(tx) = finalizationTxBuilder.buildFinalizationTxDraft(recipe)

        val wit1: TxKeyWitness = createTxKeyWitness(tx.toTx, keys1._1)
        val wit2: TxKeyWitness = createTxKeyWitness(tx.toTx, keys2._1)
        Set(wit1, wit2).map(w =>
            AckFinalCombined(
              block.blockHeader,
              Set.empty,
              w
            )
        )
}
