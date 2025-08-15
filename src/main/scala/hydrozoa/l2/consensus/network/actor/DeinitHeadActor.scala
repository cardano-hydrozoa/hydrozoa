package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.transitionary.{toIArray, toScalus}
import hydrozoa.infra.{addWitness, serializeTxHex}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScript}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.{DeinitTx, InitTx, toL1Tx}
import hydrozoa.l1.rulebased.tx.deinit.{DeinitTxBuilder, DeinitTxRecipe}
import hydrozoa.l1.rulebased.tx.fallback.{FallbackTxBuilder, FallbackTxRecipe}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.consensus.network.*
import hydrozoa.node.server.TxDump
import hydrozoa.node.state.{InitializingHeadParams, NodeState, WalletId}
import io.bullet.borer.Cbor
import ox.channels.{ActorRef, Channel, Source}
import scalus.builtin.{ByteString, given}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.TransactionOutput.Shelley
import scalus.cardano.ledger.{TransactionHash, VKeyWitness}

import scala.collection.mutable

private class DeinitHeadActor(
    // TODO: remove
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet],
    cardanoActor: ActorRef[CardanoL1],
    dropMyself: () => Unit
) extends ConsensusActor:

    override type ReqType = ReqDeinit
    override type AckType = AckDeinit
    private val log = Logger(getClass)
    private val acks: mutable.Map[WalletId, VKeyWitness] = mutable.Map.empty
    private val resultChannel: Channel[TransactionHash] = Channel.buffered(1)
    private var ownAck: AckDeinit = _
    private var txDraft: DeinitTx = _

    override def init(req: ReqType): Seq[AckType] =
        log.info(s"Initializing deinit actor: $req")

        val txDraft = req.deinitTx

        log.info("Deinit tx draft: " + serializeTxHex(txDraft))
        log.info("Deinit tx draft hash: " + txDraft.id)

        val (me, ownWit) = walletActor.ask(w => (w.getWalletId, w.createTxKeyWitness(txDraft)))
        val ownAck: AckType = AckDeinit(me, ownWit)

        this.req = req
        this.ownAck = ownAck
        this.txDraft = txDraft

        deliver(ownAck)
        Seq(ownAck)

    override def deliver(ack: AckType): Option[AckType] =
        log.info(s"Deliver ack: $ack")
        acks.put(ack.peer, ack.signature)
        tryMakeResult()
        None

    private def tryMakeResult(): Unit =
        log.info("tryMakeResult")

        // Initially the request may be absent (if an ack comes first)
        if (req != null)

            // TODO: this hangs, I suppose because we call deinit from the state, though it is strange.
            // stateActor.ask(_.head.openPhase(open => (open.headPeers)))
            val headPeers = req.headPeers;

            if acks.keySet == headPeers
            then
                // All wits are here, we can sign and submit
                val deinitTx = acks.values.foldLeft(txDraft)(addWitness)
                val serializedTx = serializeTxHex(deinitTx)
                log.info("Deinit tx: " + serializedTx)

                // TODO: submission should be carried on by a separate thread
                cardanoActor.ask(_.submit(toL1Tx(deinitTx))) match
                    case Right(txHash) =>
                        // TODO: deinit the head
                        resultChannel.send(txHash)
                        dropMyself()

                    case Left(err) =>
                        val msg = s"Can't submit deinit tx: $err"
                        log.error(msg)
                        // FIXME: what should go next here?
                        throw RuntimeException(msg)

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end DeinitHeadActor
