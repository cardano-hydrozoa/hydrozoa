package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.{Piper, addWitness, serializeTxHex, txHash}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.{InitTx, PostDatedRefundTx, toL1Tx}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.consensus.network.*
import hydrozoa.node.server.TxDump
import hydrozoa.node.state.{InitializingHeadParams, NodeState, WalletId}
import hydrozoa.*
import hydrozoa.l1.multisig.state.DepositUtxo
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

private class RefundLaterActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet],
    refundTxBuilder: RefundTxBuilder,
    dropMyself: () => Unit
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqRefundLater
    override type AckType = AckRefundLater

    private var txDraft: PostDatedRefundTx = _
    private val acks: mutable.Map[WalletId, TxKeyWitness] = mutable.Map.empty

    override def init(req: ReqType): Seq[AckType] =
        log.trace(s"Init req: $req")

        val Right(txDraft) =
            refundTxBuilder.mkPostDatedRefundTxDraft(
              PostDatedRefundRecipe(req.depositTx, req.index)
            )
        log.info("Post-dated refund tx hash: " + txHash(txDraft))

        // TxDump.dumpMultisigTx(refundTxDraft)

        val (me, ownWit) = walletActor.ask(w => (w.getWalletId, w.createTxKeyWitness(txDraft)))
        val ownAck = AckRefundLater(me, ownWit)

        this.req = req
        this.txDraft = txDraft
        deliver(ownAck)
        Seq(ownAck)

    override def deliver(ack: AckType): Option[AckType] =
        log.trace(s"Deliver ack: $ack")
        acks.put(ack.peer, ack.signature)
        tryMakeResult()
        None

    private def tryMakeResult(): Unit =
        log.trace("tryMakeResult")
        val headPeers = stateActor.ask(_.head.openPhase(_.headPeers))
        if (req != null && acks.keySet == headPeers)
            // All wits are here, we can sign and save post-dated
            // refund transaction for future's use.
            val refundTx = acks.values.foldLeft(txDraft)(addWitness)
            val serializedTx = serializeTxHex(refundTx)
            log.info("Post-dated refund refundTx: " + serializedTx)

            val depositUtxoId = UtxoIdL1.apply(txHash(req.depositTx), req.index)
            stateActor.tell(_.head.openPhase(_.enqueueDeposit(depositUtxoId, refundTx)))
            resultChannel.send(refundTx)
            dropMyself()

    private val resultChannel: Channel[PostDatedRefundTx] = Channel.buffered(1)

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end RefundLaterActor
