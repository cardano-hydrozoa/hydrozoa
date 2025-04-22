package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.{addWitnessMultisig, serializeTxHex, txHash}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.tx.SettlementTx
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l2.block.{BlockValidator, ValidationResolution}
import hydrozoa.l2.consensus.network.{AckMajor, AckMajor2, Req, ReqMajor}
import hydrozoa.l2.ledger.state.UtxosSetOpaque
import hydrozoa.l2.ledger.{SimpleGenesis, UtxosSet}
import hydrozoa.node.state.*
import hydrozoa.{TxId, TxKeyWitness, Wallet}
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

private class MajorBlockConfirmationActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet],
    settlementTxBuilder: SettlementTxBuilder,
    cardano: ActorRef[CardanoL1],
    dropMyself: () => Unit
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqMajor
    override type AckType = AckMajor | AckMajor2

    private var utxosActive: UtxosSetOpaque = _
    private var mbGenesis: Option[(TxId, SimpleGenesis)] = _
    private var utxosWithdrawn: UtxosSet = _
    private val acks: mutable.Map[WalletId, AckMajor] = mutable.Map.empty
    private val acks2: mutable.Map[WalletId, AckMajor2] = mutable.Map.empty
    private var finalizeHead: Boolean = false
    private var settlementTxDraft: SettlementTx = _
    private var ownAck2: Option[AckMajor2] = None

    private def tryMakeAck2(): Option[AckType] =
        log.debug(s"tryMakeAck2 - acks: ${acks.keySet}")
        val (headPeers, isFinalizationRequested) =
            stateActor.ask(_.head.openPhase(open => (open.headPeers, open.isFinalizationRequested)))
        log.debug(s"headPeers: $headPeers")
        if ownAck2.isEmpty && acks.keySet == headPeers then
            // TODO: how do we check that all acks are valid?
            // Create settlement tx draft
            val txRecipe = SettlementRecipe(
              req.block.blockHeader.versionMajor,
              req.block.blockBody.depositsAbsorbed,
              utxosWithdrawn
            )
            val Right(settlementTxDraft: SettlementTx) =
                settlementTxBuilder.mkSettlementTxDraft(txRecipe)
            val serializedTx = serializeTxHex(settlementTxDraft)
            log.info(s"Settlement tx for block ${req.block.blockHeader.blockNum} is $serializedTx")
            // TxDump.dumpMultisigTx(settlementTxDraft)
            val (me, settlementTxKeyWitness) =
                walletActor.ask(w => (w.getWalletId, w.createTxKeyWitness(settlementTxDraft)))
            val ownAck2 = AckMajor2(me, settlementTxKeyWitness, isFinalizationRequested)
            this.settlementTxDraft = settlementTxDraft
            this.ownAck2 = Some(ownAck2)
            deliverAck2(ownAck2)
            Some(ownAck2)
        else None

    private def tryMakeResult(): Unit =
        log.debug("tryMakeResult")
        val headPeers = stateActor.ask(_.head.openPhase(_.headPeers))
        if (acks2.keySet == headPeers) then
            // Create effects
            // L1 effect
            val wits = acks2.map(_._2.settlement)
            val settlementTx = wits.foldLeft(settlementTxDraft)(addWitnessMultisig)
            // val serializedTx = serializeTxHex(settlementTx)
            val l1Effect: L1BlockEffect = settlementTx
            val l2Effect: L2BlockEffect = utxosActive
            // Block record and state update by block application
            // TODO: L1PostDatedBlockEffect
            val record = BlockRecord(req.block, l1Effect, (), l2Effect)
            stateActor.tell(nodeState =>
                nodeState.head.openPhase(s =>
                    s.applyBlockRecord(record)
                    // Dump state
                    nodeState.head.dumpState()
                )
            )
            log.info(s"Submitting settlement tx: ${txHash(settlementTx)}")
            cardano.tell(_.submit(settlementTx))
            if (finalizeHead) stateActor.tell(_.head.openPhase(_.switchToFinalizingPhase()))
            // TODO: the absence of this line is a good test!
            resultChannel.send(())
            dropMyself()

    override def deliver(ack: AckType): Option[AckType] =
        log.debug(s"deliver ack: $ack")
        ack match
            case ack: AckMajor =>
                acks.put(ack.peer, ack)
                ()
            case ack2: AckMajor2 =>
                deliverAck2(ack2)
        val mbAck2 = tryMakeAck2()
        tryMakeResult()
        log.info(s"exiting deliver, mbAck2: $mbAck2")
        mbAck2

    private def deliverAck2(ack2: AckMajor2): Unit = {
        acks2.put(ack2.peer, ack2)
        if ack2.nextBlockFinal then this.finalizeHead = true
    }

    override def init(req: ReqType): Seq[AckType] =
        log.trace(s"init req: $req")

        // Block validation (the leader can skip validation for its own block).
        val (utxosActive, mbGenesis, utxosWithdrawn) =
            if stateActor.ask(_.head.openPhase(_.isBlockLeader))
            then
                val ownBlock = stateActor.ask(_.head.openPhase(_.pendingOwnBlock))
                (ownBlock.utxosActive, ownBlock.mbGenesis, ownBlock.utxosWithdrawn)
            else
                val (prevHeader, stateL2Cloned, poolEventsL2, depositUtxos) =
                    stateActor.ask(
                      _.head.openPhase(openHead =>
                          (
                            openHead.l2Tip.blockHeader,
                            openHead.stateL2.blockProduction,
                            openHead.immutablePoolEventsL2,
                            openHead.peekDeposits
                          )
                      )
                    )
                val resolution = BlockValidator.validateBlock(
                  req.block,
                  prevHeader,
                  stateL2Cloned,
                  poolEventsL2,
                  depositUtxos,
                  false
                )
                resolution match
                    case ValidationResolution.Valid(utxosActive, mbGenesis, utxosWithdrawn) =>
                        log.info(s"Major block ${req.block.blockHeader.blockNum} is valid.")
                        (utxosActive, mbGenesis, utxosWithdrawn)
                    case resolution =>
                        throw RuntimeException(s"Block validation failed: $resolution")

        // Update local state
        this.req = req
        this.utxosActive = utxosActive
        this.mbGenesis = mbGenesis
        this.utxosWithdrawn = utxosWithdrawn

        // Prepare own acknowledgement
        val (me) = walletActor.ask(w => (w.getWalletId))
        // TODO: postDatedTransaction
        val ownAck = AckMajor(me, Seq.empty, TxKeyWitness(Array.empty, Array.empty))
        log.debug(s"Own AckMajor: $ownAck")
        val mbAck2 = deliver(ownAck)
        log.debug(s"Own AckMajor2: $mbAck2")

        Seq(ownAck) ++ mbAck2.toList

    private val resultChannel: Channel[Unit] = Channel.buffered(1)

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end MajorBlockConfirmationActor
