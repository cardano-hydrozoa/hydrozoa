package hydrozoa.l2.consensus.network

import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l2.block.Block
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.node.state.{HeadStateReader, PeerInfo}
import hydrozoa.{Peer, PeerPublicKeyBytes, TxKeyWitness}

class HeadPeerNetworkOneNode(
    reader: HeadStateReader,
    initTxBuilder: InitTxBuilder,
    refundTxBuilder: RefundTxBuilder,
    settlementTxBuilder: SettlementTxBuilder,
    finalizationTxBuilder: FinalizationTxBuilder,
    cardano: CardanoL1,
    theNode: Peer,
    knownPeers: Set[Peer]
) extends HeadPeerNetwork:

    override def reqPublicKeys(headPeers: Set[PeerInfo]): Set[PeerPublicKeyBytes] =
        requireHeadPeersAreKnown(headPeers)
        val headPeersNames = headPeers.map(_.name)
        knownPeers
            .filter(p => headPeersNames.contains(p.getName))
            .map(p => p.getPublicKey)

    private def requireHeadPeersAreKnown(headPeers: Set[PeerInfo]): Unit = {
        val headPeersNames = headPeers.map(_.name)
        val knownPeersNames = knownPeers.map(_.getName)
        require(headPeersNames.forall(knownPeersNames.contains))
    }

    override def reqInit(headPeers: Set[PeerInfo], req: ReqInit): Set[TxKeyWitness] = {
        requireHeadPeersAreKnown(headPeers)

        val headPeersNames = headPeers.map(_.name)
        val headOtherPeers = knownPeers.filter(p => headPeersNames.contains(p.getName))
        
        // All head's verification keys
        val vKeys = (headOtherPeers + theNode).map(_.getPublicKey)

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

    private def getHeadPeers = {
        val headPeers = reader.multisigRegimeReader(_.headPeers)
        requireHeadPeersAreKnown(headPeers)
        val headPeersNames = headPeers.map(_.name)
        val headOtherPeers = knownPeers.filter(p => headPeersNames.contains(p.getName))
        headOtherPeers
    }
    
    override def reqRefundLater(req: ReqRefundLater): Set[TxKeyWitness] =
        val headOtherPeers: Set[Peer] = getHeadPeers

        val recipe = PostDatedRefundRecipe(req.depositTx, req.index)
        val Right(tx) = refundTxBuilder.mkPostDatedRefundTxDraft(recipe)
        headOtherPeers.map(_.createTxKeyWitness(tx))

    override def reqMinor(block: Block): Set[AckMinor] =
        val headOtherPeers: Set[Peer] = getHeadPeers
        headOtherPeers.map(_ => AckMinor(block.blockHeader, (), false))

    override def reqMajor(block: Block, utxosWithdrawn: UtxosSet): Set[AckMajorCombined] =
        val headOtherPeers: Set[Peer] = getHeadPeers
        
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
        val headOtherPeers: Set[Peer] = getHeadPeers
        
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
