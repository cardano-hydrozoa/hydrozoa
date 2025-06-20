package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.{addWitnessMultisig, serializeTxHex, txHash}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.tx.FinalizationTx
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l2.block.{BlockValidator, ValidationResolution}
import hydrozoa.l2.consensus.network.{AckFinal, AckFinal2, Req, ReqFinal}
import hydrozoa.node.state.*
import hydrozoa.{TaggedUtxoSet, UtxoSet, UtxoSetL2, Wallet}
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

private class FinalBlockConfirmationActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet],
    finalizationTxBuilder: FinalizationTxBuilder,
    cardano: ActorRef[CardanoL1],
    dropMyself: () => Unit
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqFinal
    override type AckType = AckFinal | AckFinal2

    private var utxosWithdrawn: UtxoSetL2 = _
    private val acks: mutable.Map[WalletId, AckFinal] = mutable.Map.empty
    private val acks2: mutable.Map[WalletId, AckFinal2] = mutable.Map.empty
    private var finalizationTxDraft: FinalizationTx = _
    private var ownAck2: Option[AckFinal2] = None

    private def tryMakeAck2(): Option[AckType] =
        log.debug(s"tryMakeAck2 - acks: ${acks.keySet}")
        val headPeers = stateActor.ask(_.head.finalizingPhase(_.headPeers))
        log.debug(s"headPeers: $headPeers")
        if (req != null && ownAck2.isEmpty && acks.keySet == headPeers)
            // TODO: how do we check that all acks are valid?
            // Create finalization tx draft
            val recipe =
                FinalizationRecipe(req.block.blockHeader.versionMajor, utxosWithdrawn)
            val Right(finalizationTxDraft: FinalizationTx) =
                finalizationTxBuilder.buildFinalizationTxDraft(recipe)
            val serializedTx = serializeTxHex(finalizationTxDraft)
            log.info(
              s"Finalization tx for final block ${req.block.blockHeader.blockNum} is $serializedTx"
            )
            // TxDump.dumpMultisigTx(settlementTxDraft)
            val (me, settlementTxKeyWitness) =
                walletActor.ask(w => (w.getWalletId, w.createTxKeyWitness(finalizationTxDraft)))
            val ownAck2 = AckFinal2(me, settlementTxKeyWitness)
            this.finalizationTxDraft = finalizationTxDraft
            this.ownAck2 = Some(ownAck2)
            deliverAck2(ownAck2)
            Some(ownAck2)
        else None

    private def tryMakeResult(): Unit =
        log.debug("tryMakeResult")
        val headPeers = stateActor.ask(_.head.finalizingPhase(_.headPeers))
        if (req != null && acks2.keySet == headPeers) then
            // Create effects
            // L1 effect
            val wits = acks2.map(_._2.finalization)
            val finalizationTx = wits.foldLeft(finalizationTxDraft)(addWitnessMultisig)
            // val serializedTx = serializeTxHex(finalizationTx)
            val l1Effect: L1BlockEffect = finalizationTx
            val l2Effect: L2BlockEffect = None
            // Block record and state update by block application
            val record = BlockRecord(req.block, l1Effect, None, l2Effect)
            // Close the head
            stateActor.tell(nodeState =>
                nodeState.head.finalizingPhase(s => s.finalizeHead(record))
            )
            // Submit finalization tx
            log.info(s"Submitting finalization tx: ${txHash(finalizationTx)}")
            cardano.tell(_.submit(finalizationTx))
            // TODO: the absence of this line is a good test!
            resultChannel.send(())
            dropMyself()

    override def deliver(ack: AckType): Option[AckType] =
        log.debug(s"deliver ack: $ack")
        ack match
            case ack: AckFinal =>
                acks.put(ack.peer, ack)
                ()
            case ack2: AckFinal2 =>
                deliverAck2(ack2)
        val mbAck2 = tryMakeAck2()
        tryMakeResult()
        log.info(s"exiting deliver, mbAck2: $mbAck2")
        mbAck2

    private def deliverAck2(ack2: AckFinal2): Unit =
        acks2.put(ack2.peer, ack2)

    override def init(req: ReqType): Seq[AckType] =
        log.trace(s"init req: $req")

        val utxosWithdrawn =
            if stateActor.ask(_.head.finalizingPhase(_.isBlockLeader))
            then
                val ownBlock = stateActor.ask(_.head.finalizingPhase(_.pendingOwnBlock))
                ownBlock.utxosWithdrawn
            else
                val (prevHeader, stateL2Cloned) =
                    stateActor.ask(
                      _.head.finalizingPhase(head =>
                          (
                            head.l2Tip.blockHeader,
                            head.stateL2.cloneForBlockProducer(),
                          )
                      )
                    )
                val resolution = BlockValidator.validateBlock(
                  req.block,
                  prevHeader,
                  stateL2Cloned,
                  Seq.empty,
                  TaggedUtxoSet.apply(),
                  true
                )
                resolution match
                    case ValidationResolution.Valid(_, _, utxosWithdrawn) =>
                        log.info(s"Final block ${req.block.blockHeader.blockNum} is valid.")
                        utxosWithdrawn
                    case resolution =>
                        throw RuntimeException(s"Final block validation failed: $resolution")

        // Update local state
        this.req = req
        this.utxosWithdrawn = utxosWithdrawn

        // Prepare own acknowledgement
        val (me) = walletActor.ask(w => (w.getWalletId))
        val ownAck = AckFinal(me, Seq.empty)
        log.debug(s"Own AckFinal: $ownAck")
        val mbAck2 = deliver(ownAck)
        log.debug(s"Own AckFinal2: $mbAck2")

        Seq(ownAck) ++ mbAck2.toList

    private val resultChannel: Channel[Unit] = Channel.buffered(1)

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end FinalBlockConfirmationActor
