package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.Wallet
import hydrozoa.l2.block.{BlockValidator, ValidationResolution}
import hydrozoa.l2.consensus.network.*
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.l2.ledger.state.UtxosSetOpaque
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

    private var utxosActive: UtxosSetOpaque = _
    private val acks: mutable.Map[WalletId, AckMinor] = mutable.Map.empty
    private var finalizeHead: Boolean = false

    private def tryMakeResult(): Unit =
        log.trace("tryMakeResult")
        val headPeers = stateActor.ask(_.head.openPhase(_.headPeers))
        if (req != null && acks.keySet == headPeers)
            // Create effects
            // TODO: Should become a resolution vote at some point
            val l1Effect: L1BlockEffect = ()
            val l2Effect: L2BlockEffect = utxosActive
            // Block record and state update by block application
            val record = BlockRecord(req.block, l1Effect, (), l2Effect)
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
        val (utxosActive, _, _, isFinalizationRequested) =
            if stateActor.ask(_.head.openPhase(_.isBlockLeader))
            then
                val (ownBlock, isFinalizationRequested) = stateActor.ask(
                  _.head.openPhase(open => (open.pendingOwnBlock, open.isFinalizationRequested))
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
                            open.stateL2.blockProduction,
                            open.immutablePoolEventsL2,
                            open.peekDeposits,
                            open.isFinalizationRequested
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
        val (me, signature) =
            walletActor.ask(w => (w.getWalletId, "signature_stub"))

        // FIXME: how do we decide whether we want to wrap up the head?
        // Answer: User API should provide a method for that, so with the next
        // acknowledgment the node can indicate they want to finalize the head.
        val ownAck: AckType = AckMinor(me, signature, isFinalizationRequested)
        deliver(ownAck)
        Seq(ownAck)

    private val resultChannel: Channel[Unit] = Channel.buffered(1)

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end MinorBlockConfirmationActor
