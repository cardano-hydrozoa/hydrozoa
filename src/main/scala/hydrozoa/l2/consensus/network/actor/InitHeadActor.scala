package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.{addWitness, serializeTxHex, txHash}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.{InitTx, toL1Tx}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.consensus.network.*
import hydrozoa.node.server.TxDump
import hydrozoa.node.state.{InitializingHeadParams, NodeState, WalletId}
import hydrozoa.*
import ox.channels.{ActorRef, Channel, Source}

import scala.collection.mutable

private class InitHeadActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet],
    cardanoActor: ActorRef[CardanoL1],
    initTxBuilder: InitTxBuilder,
    dropMyself: () => Unit
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqInit
    override type AckType = AckInit

    private var ownAck: AckInit = _

    private var txDraft: InitTx = _
    private var headNativeScript: NativeScript = _
    private var headAddress: AddressBechL1 = _
    private var beaconTokenName: TokenName = _
    private var seedAddress: AddressBechL1 = _
    private val acks: mutable.Map[WalletId, TxKeyWitness] = mutable.Map.empty

    override def init(req: ReqType): Seq[AckType] =
        log.trace(s"Init req: $req")

        val headPeers = req.otherHeadPeers + req.initiator
        val Some(headVKeys) = stateActor.ask(_.getVerificationKeys(headPeers))
        val (headNativeScript, headAddress) =
            mkHeadNativeScriptAndAddress(headVKeys, cardanoActor.ask(_.network))
        val beaconTokenName = mkBeaconTokenName(req.seedUtxoId)

        log.info(s"Head's address: $headAddress, beacon token name: $beaconTokenName")

        val initTxRecipe = InitTxRecipe(
          headAddress,
          req.seedUtxoId,
          req.treasuryCoins,
          headNativeScript,
          beaconTokenName
        )

        log.info(s"initTxRecipe: $initTxRecipe")

        // Builds and balance initialization tx
        val Right(txDraft, seedAddress) = initTxBuilder.mkInitializationTxDraft(initTxRecipe)

        log.info("Init tx draft hash: " + txHash(txDraft))

        val (me, ownWit) = walletActor.ask(w => (w.getWalletId, w.createTxKeyWitness(txDraft)))
        val ownAck: AckType = AckInit(me, ownWit)

        this.req = req
        this.ownAck = ownAck
        this.txDraft = txDraft
        this.headNativeScript = headNativeScript
        this.headAddress = headAddress
        this.beaconTokenName = beaconTokenName
        this.seedAddress = seedAddress
        deliver(ownAck)
        Seq(ownAck)

    override def deliver(ack: AckType): Option[AckType] =
        log.trace(s"Deliver ack: $ack")
        acks.put(ack.peer, ack.signature)
        tryMakeResult()
        None

    private def tryMakeResult(): Unit =
        log.trace("tryMakeResult")
        val headPeers = req.otherHeadPeers + req.initiator
        if acks.keySet == headPeers
        then
            // All wits are here, we can sign and submit
            val initTx = acks.values.foldLeft(txDraft)(addWitness)
            val serializedTx = serializeTxHex(initTx)
            log.info("Initialization tx: " + serializedTx)

            cardanoActor.ask(_.submit(toL1Tx(initTx))) match
                case Right(txHash) =>
                    // Put the head into Initializing phase
                    val headPeersVKs = stateActor.ask(_.getVerificationKeyMap(headPeers))
                    val params = InitializingHeadParams(
                      ownAck.peer,
                      headPeersVKs,
                      HeadParams.default,
                      headNativeScript,
                      headAddress,
                      beaconTokenName,
                      seedAddress,
                      initTx,
                      System.currentTimeMillis()
                    )
                    stateActor.tell(_.tryInitializeHead(params))
                    TxDump.dumpInitTx(initTx)
                    resultChannel.send(txHash)
                    dropMyself()

                case Left(err) =>
                    val msg = s"Can't submit init tx: $err"
                    log.error(msg)
                    // FIXME: what should go next here?
                    throw RuntimeException(msg)

    private val resultChannel: Channel[TxId] = Channel.buffered(1)

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end InitHeadActor
