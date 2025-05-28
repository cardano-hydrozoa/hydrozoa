package hydrozoa.l2.consensus.network

import hydrozoa.infra.txHash
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.tx.PostDatedRefundTx
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l2.block.Block
import hydrozoa.l2.consensus.ConsensusDispatcher
import hydrozoa.l2.ledger.simple.UtxosSet
import hydrozoa.node.state.{HeadStateReader, WalletId}
import hydrozoa.{TxId, TxKeyWitness, VerificationKeyBytes, Wallet}
import ox.channels.ActorRef

/** TODO: revise, broken
  */
class HeadPeerNetworkOneNode(
    reader: HeadStateReader,
    initTxBuilder: InitTxBuilder,
    refundTxBuilder: RefundTxBuilder,
    settlementTxBuilder: SettlementTxBuilder,
    finalizationTxBuilder: FinalizationTxBuilder,
    cardano: CardanoL1,
    ownNode: Wallet,
    knownPeers: Set[Wallet]
) extends HeadPeerNetwork:

    private def requireHeadPeersAreKnown(headPeers: Set[WalletId]): Unit = {
        val headPeersNames = headPeers.map(_.name)
        val knownPeersNames = knownPeers.map(_.getName)
        require(headPeersNames.forall(knownPeersNames.contains), "All peers should be known")
    }

    override def reqVerificationKeys(): Map[WalletId, VerificationKeyBytes] =
        (knownPeers + ownNode).map(p => (p.getWalletId, p.exportVerificationKeyBytes)).toMap

    override def reqInit(req: ReqInit): TxId = {
        requireHeadPeersAreKnown(req.otherHeadPeers)

        val headPeersNames = (req.otherHeadPeers + req.initiator).map(_.name)
        val headOtherPeers = knownPeers.filter(p => headPeersNames.contains(p.getName))

        // All head's verification keys
        val vKeys = (headOtherPeers + ownNode).map(_.exportVerificationKeyBytes)

        // Native script, head address, and token
        val (headNativeScript, headAddress) = mkHeadNativeScriptAndAddress(vKeys, cardano.network)
        val beaconTokenName = mkBeaconTokenName(req.seedUtxoId)

        // Recipe to build init tx
        val initTxRecipe = InitTxRecipe(
          headAddress,
          req.seedUtxoId,
          req.treasuryCoins,
          headNativeScript,
          beaconTokenName
        )

        val Right(tx, _) = initTxBuilder.mkInitializationTxDraft(initTxRecipe)

        headOtherPeers.map(_.createTxKeyWitness(tx))
        txHash(tx)
    }

    private def getOtherPeersWallets: Set[Wallet] = {
        val otherPeers =
            reader.multisigRegimeReader(_.headPeers).filterNot(_.name == ownNode.getName)
        requireHeadPeersAreKnown(otherPeers)
        val otherPeersNames = otherPeers.map(_.name)
        val otherPeersWallets = knownPeers.filter(p => otherPeersNames.contains(p.getName))
        otherPeersWallets
    }

    override def reqRefundLater(req: ReqRefundLater): PostDatedRefundTx =
        val headOtherPeers: Set[Wallet] = getOtherPeersWallets

        val recipe = PostDatedRefundRecipe(req.depositTx, req.index)
        val Right(tx) = refundTxBuilder.mkPostDatedRefundTxDraft(recipe)
        val wits = headOtherPeers.map(_.createTxKeyWitness(tx))
        ???

    override def reqEventL2(req: ReqEventL2): Unit = ???

    override def reqMinor(req: ReqMinor): Unit =
        val headOtherPeers: Set[Wallet] = getOtherPeersWallets
        headOtherPeers.map(w => AckMinor(w.getWalletId, "", false))
        ???

    override def reqMajor(req: ReqMajor): Unit =
        val headOtherPeers: Set[Wallet] = getOtherPeersWallets

        // TODO: check block type
        val recipe =
            SettlementRecipe(
              req.block.blockHeader.versionMajor,
              req.block.blockBody.depositsAbsorbed,
              ??? // utxosWithdrawn
            )
        val Right(tx) = settlementTxBuilder.mkSettlementTxDraft(recipe)

        headOtherPeers
            .map(_.createTxKeyWitness(tx))
            .map(witness =>
                AckMajorCombined(
                  req.block.blockHeader,
                  Set.empty,
                  witness,
                  false
                )
            )
        ???

    override def reqFinal(req: ReqFinal): Unit =
        val headOtherPeers: Set[Wallet] = getOtherPeersWallets

        val recipe = FinalizationRecipe(req.block.blockHeader.versionMajor, ???) // utxosWithdrawn

        val Right(tx) = finalizationTxBuilder.buildFinalizationTxDraft(recipe)

        headOtherPeers
            .map(_.createTxKeyWitness(tx))
            .map(witness =>
                AckFinalCombined(
                  req.block.blockHeader,
                  Set.empty,
                  witness
                )
            )
        ???

    override def setDispatcher(dispatcherRef: ActorRef[ConsensusDispatcher]): Unit = ???
