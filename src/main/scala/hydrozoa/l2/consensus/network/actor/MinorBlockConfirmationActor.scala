package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.l2.block.{BlockValidator, ValidationResolution}
import hydrozoa.l2.consensus.network.*
import hydrozoa.l2.ledger.UtxosSet
import hydrozoa.l2.ledger.state.UtxosSetOpaque
import hydrozoa.node.state.*
import hydrozoa.{VerificationKeyBytes, Wallet}
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

private class MinorBlockConfirmationActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet]
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
        if acks.keySet == headPeers
        then
            // Create effects
            // TODO: Should become a resolution vote at some point
            val l1Effect: L1BlockEffect = ()
            val l2Effect: L2BlockEffect = utxosActive
            // Block record and state update by block application
            val record = BlockRecord(req.block, l1Effect, (), l2Effect)
            stateActor.tell(_.head.openPhase(s => s.applyBlockRecord(record)))
            // Move head into finalization phase if finalizeHead flag was received
            if (finalizeHead) stateActor.tell(_.head.openPhase(_.finalizeHead()))
            // Dump state
            // dumpState()

    override def deliver(ack: AckType): Option[AckType] =
        log.trace(s"deliver ack: $ack")
        acks.put(ack.peer, ack)
        if ack.nextBlockFinal then this.finalizeHead = true
        tryMakeResult()
        None

    override def init(req: ReqType): AckType =
        log.trace(s"init req: $req")
        this.req = req
        // The leader can skip validation since its own block.
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
                    case ValidationResolution.Valid(utxosActive, mbGenesis, utxosWithdrawn) => (utxosActive, mbGenesis, utxosWithdrawn)
                    case _ => throw RuntimeException("Block validation filed.")

        val (me, signature) =
            walletActor.ask(w => (w.getWalletId, "signature_stub"))
        // FIXME: how do we decide whether we want to wrap up the head?
        // Answer: User API should provide a method for that, so with the next
        // acknowledgment the node can indicate they want to finalize the head.
        val ownAck: AckType = AckMinor(me, signature, false)
        deliver(ownAck)
        ownAck

    private val resultChannel: Channel[Map[WalletId, VerificationKeyBytes]] = Channel.rendezvous
//    private def resultChannel(using req: ReqType): Channel[req.resultType] = Channel.rendezvous

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end MinorBlockConfirmationActor
