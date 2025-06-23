package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.{Ed25519SignatureHex, Wallet}
import hydrozoa.infra.encodeHex
import hydrozoa.l2.block.{BlockValidator, ValidationResolution, mkBlockHeaderSignatureMessage}
import hydrozoa.l2.consensus.network.*
import hydrozoa.l2.ledger.HydrozoaL2Ledger
import hydrozoa.node.state.*
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

private class MinorBlockConfirmationActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet],
    dropMyself: () => Unit
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqMinor
    override type AckType = AckMinor

    private var utxosActive: HydrozoaL2Ledger.LedgerUtxoSetOpaque = _
    private val acks: mutable.Map[WalletId, AckMinor] = mutable.Map.empty
    private var finalizeHead: Boolean = false

    private def tryMakeResult(): Unit =
        log.trace("tryMakeResult")
        val headPeers = stateActor.ask(_.head.openPhase(_.headPeers))
        if (req != null && acks.keySet == headPeers)
            // Create effects
            // TODO: Should become a resolution vote at some point
            val l1Effect: L1BlockEffect = ()
            // TODO: May be absent
            val l2Effect: L2BlockEffect = Some(utxosActive)
            // Block record and state update by block application
            val record = BlockRecord(req.block, l1Effect, None, l2Effect)
            stateActor.tell(nodeState =>
                nodeState.head.openPhase(s =>
                    s.applyBlockRecord(record)
                    // Dump state
                    nodeState.head.dumpState()
                )
            )
            // Move head into finalization phase if finalizeHead flag was received
            if (finalizeHead) stateActor.tell(_.head.openPhase(_.switchToFinalizingPhase()))
            // TODO: the absence of this line is a good test!
            resultChannel.send(())
            dropMyself()

    override def deliver(ack: AckType): Option[AckType] =
        log.trace(s"deliver ack: $ack")
        acks.put(ack.peer, ack)
        if ack.nextBlockFinal then this.finalizeHead = true
        tryMakeResult()
        None

    override def init(req: ReqType): Seq[AckType] =
        log.trace(s"init req: $req")

        // Block validation (the leader can skip validation since its own block).
        val (utxosActive, _, _, isNextBlockFinal) =
            if stateActor.ask(_.head.openPhase(_.isBlockLeader))
            then
                val (ownBlock, isFinalizationRequested) = stateActor.ask(
                  _.head.openPhase(open => (open.pendingOwnBlock, open.isNextBlockFinal))
                )
                (
                  ownBlock.utxosActive,
                  ownBlock.mbGenesis,
                  ownBlock.utxosWithdrawn,
                  isFinalizationRequested
                )
            else
                val (
                  prevHeader,
                  stateL2Cloned,
                  poolEventsL2,
                  depositUtxos,
                  isFinalizationRequested
                ) =
                    stateActor.ask(
                      _.head.openPhase(open =>
                          (
                            open.l2Tip.blockHeader,
                            open.stateL2.cloneForBlockProducer(),
                            open.immutablePoolEventsL2,
                            open.peekDeposits,
                            open.isNextBlockFinal
                          )
                      )
                    )
                val resolution = BlockValidator.validateBlock(
                  req.block,
                  prevHeader,
                  stateL2Cloned,
                  poolEventsL2,
                  depositUtxos, // FIXME: do we need it for a minor block?
                  false // minor is not a final
                )
                resolution match
                    case ValidationResolution.Valid(utxosActive, mbGenesis, utxosWithdrawn) =>
                        (utxosActive, mbGenesis, utxosWithdrawn, isFinalizationRequested)
                    case resolution =>
                        throw RuntimeException(s"Minor block validation failed: $resolution")

        // Update local state
        this.req = req
        this.utxosActive = utxosActive

        // Prepare own acknowledgement
        val msg = mkBlockHeaderSignatureMessage(req.block.blockHeader)

        // Sign block header
        val (me, signature) =
            walletActor.ask(w => (w.getWalletId, w.createEd25519Signature(msg)))
        val signatureHex = Ed25519SignatureHex(encodeHex(signature.signature))

        val ownAck: AckType = AckMinor(me, signatureHex, isNextBlockFinal)
        deliver(ownAck)
        Seq(ownAck)

    private val resultChannel: Channel[Unit] = Channel.buffered(1)

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end MinorBlockConfirmationActor
