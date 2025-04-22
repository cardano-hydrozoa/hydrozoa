package hydrozoa.l2.consensus.network

import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l2.block.Block
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.node.state.{HeadStateReader, WalletId}
import hydrozoa.{TxKeyWitness, VerificationKeyBytes, Wallet}

class HeadPeerNetworkOneNode(
    reader: HeadStateReader,
    initTxBuilder: InitTxBuilder,
    refundTxBuilder: RefundTxBuilder,
    settlementTxBuilder: SettlementTxBuilder,
    finalizationTxBuilder: FinalizationTxBuilder,
    cardano: CardanoL1,
    ownNode: Wallet,
    otherNodes: Set[Wallet]
) extends HeadPeerNetwork:

    override def reqVerificationKeys(peers: Set[WalletId]): Set[VerificationKeyBytes] =
        requireHeadPeersAreKnown(peers.toSet)
        val headPeersNames = peers.map(_.name)
        otherNodes
            .filter(p => headPeersNames.contains(p.getName))
            .map(p => p.exportVerificationKeyBytes)

    private def requireHeadPeersAreKnown(headPeers: Set[WalletId]): Unit = {
        val headPeersNames = headPeers.map(_.name)
        val knownPeersNames = otherNodes.map(_.getName)
        require(headPeersNames.forall(knownPeersNames.contains), "All peers should be known")
    }

    override def reqInit(headPeers: Set[WalletId], req: ReqInit): Set[TxKeyWitness] = {
        requireHeadPeersAreKnown(headPeers)

        val headPeersNames = headPeers.map(_.name)
        val headOtherPeers = otherNodes.filter(p => headPeersNames.contains(p.getName))

        // All head's verification keys
        val vKeys = (headOtherPeers + ownNode).map(_.exportVerificationKeyBytes)

        // Native script, head address, and token
        val (headNativeScript, headAddress) = mkHeadNativeScriptAndAddress(vKeys, cardano.network)
        val beaconTokenName = mkBeaconTokenName(req.seedOutputRef)

        // Recipe to build init tx
        val initTxRecipe = InitTxRecipe(
          headAddress,
          req.seedOutputRef,
          req.coins,
          headNativeScript,
          beaconTokenName
        )

        val Right(tx, _) = initTxBuilder.mkInitializationTxDraft(initTxRecipe)

        headOtherPeers.map(_.createTxKeyWitness(tx))
    }

    private def getOtherPeersWallets: Set[Wallet] = {
        val otherPeers = reader.multisigRegimeReader(_.headPeers).filterNot(_.name == ownNode.getName)
        requireHeadPeersAreKnown(otherPeers)
        val otherPeersNames = otherPeers.map(_.name)
        val otherPeersWallets = otherNodes.filter(p => otherPeersNames.contains(p.getName))
        otherPeersWallets
    }

    override def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness] =
        val headOtherPeers: Set[Wallet] = getOtherPeersWallets

        val recipe = PostDatedRefundRecipe(req.depositTx, req.index)
        val Right(tx) = refundTxBuilder.mkPostDatedRefundTxDraft(recipe)
        headOtherPeers.map(_.createTxKeyWitness(tx))

    override def reqMinor(block: Block): Set[AckMinor] =
        val headOtherPeers: Set[Wallet] = getOtherPeersWallets
        headOtherPeers.map(_ => AckMinor(block.blockHeader, (), false))

    override def reqMajor(block: Block, utxosWithdrawn: UtxosSet): Set[AckMajorCombined] =
        val headOtherPeers: Set[Wallet] = getOtherPeersWallets

        // TODO: check block type
        val recipe =
            SettlementRecipe(
              block.blockHeader.versionMajor,
              block.blockBody.depositsAbsorbed,
              utxosWithdrawn
            )
        val Right(tx) = settlementTxBuilder.mkSettlementTxDraft(recipe)

        headOtherPeers
            .map(_.createTxKeyWitness(tx))
            .map(witness =>
                AckMajorCombined(
                  block.blockHeader,
                  Set.empty,
                  witness,
                  false
                )
            )

    override def reqFinal(block: Block, utxosWithdrawn: UtxosSet): Set[AckFinalCombined] =
        val headOtherPeers: Set[Wallet] = getOtherPeersWallets

        // TODO: check block type
        val recipe = FinalizationRecipe(block.blockHeader.versionMajor, utxosWithdrawn)

        val Right(tx) = finalizationTxBuilder.buildFinalizationTxDraft(recipe)

        headOtherPeers
            .map(_.createTxKeyWitness(tx))
            .map(witness =>
                AckFinalCombined(
                  block.blockHeader,
                  Set.empty,
                  witness
                )
            )
