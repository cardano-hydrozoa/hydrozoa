package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.transitionary.toIArray
import hydrozoa.infra.{addWitness, serializeTxHex, txHash}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptScalus}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.{InitTx, toL1Tx}
import hydrozoa.l1.rulebased.tx.fallback.{FallbackTxBuilder, FallbackTxRecipe}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.consensus.network.*
import hydrozoa.node.server.TxDump
import hydrozoa.node.state.{InitializingHeadParams, NodeState, WalletId}
import io.bullet.borer.Cbor
import ox.channels.{ActorRef, Channel, Source}
import scalus.builtin.{ByteString, given}
import scalus.cardano.ledger.Script.Native

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
    private var headNativeScript: Native = _
    private var headMintingPolicy: CurrencySymbol = _
    private var headAddress: AddressBechL1 = _
    private var beaconTokenName: TokenName = _
    private var seedAddress: AddressBechL1 = _
    private val acks: mutable.Map[WalletId, TxKeyWitness] = mutable.Map.empty

    override def init(req: ReqType): Seq[AckType] =
        log.trace(s"Init req: $req")

        val headPeers = req.otherHeadPeers + req.initiator
        val Some(headVKeys) = stateActor.ask(_.getVerificationKeys(headPeers))

        // log.info(s"Head's address: $headAddress, beacon token name: $beaconTokenName")

        val initTxRecipe = InitTxRecipe(
          network = cardanoActor.ask(_.network),
          seedUtxo = req.seedUtxoId,
          coins = req.treasuryCoins,
          peers = headVKeys
        )

        log.info(s"initTxRecipe: $initTxRecipe")

        // Builds and balance initialization tx
        val Right(txDraft, seedAddress) = initTxBuilder.mkInitializationTxDraft(initTxRecipe)
//
//        log.info("Init tx draft: " + serializeTxHex(txDraft))
//        log.info("Init tx draft hash: " + txHash(txDraft))

        val (me, ownWit) = walletActor.ask(w =>
            (w.getWalletId, w.createTxKeyWitness(txDraft))
        )
        val ownAck: AckType = AckInit(me, ownWit)

        this.req = req
        this.ownAck = ownAck
        this.txDraft = txDraft
        this.headNativeScript = headNativeScript
        this.headMintingPolicy = CurrencySymbol(headNativeScript.scriptHash.toIArray)
        this.headAddress = headAddress
        this.beaconTokenName = mkBeaconTokenName(req.seedUtxoId)
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

        // Initially the request may be absent (if an ack comes first)
        if (req != null)
            val headPeers = req.otherHeadPeers + req.initiator
            if acks.keySet == headPeers
            then
                // All wits are here, we can sign and submit
                val initTx = acks.values.foldLeft(txDraft)(addWitness)
                val serializedTx = serializeTxHex(initTx)
                log.info("Initialization tx: " + serializedTx)

                // TODO: submission should be carried on by a separate thread
                cardanoActor.ask(_.submit(toL1Tx(initTx))) match
                    case Right(txHash) =>
                        // Put the head into Initializing phase
                        val (headPeersVKs, autonomousBlocks) =
                            stateActor.ask(s =>
                                (s.getVerificationKeyMap(headPeers), s.autonomousBlockProduction)
                            )
                        val params = InitializingHeadParams(
                          ownAck.peer,
                          headPeersVKs,
                          HeadParams.default,
                          headNativeScript,
                          headMintingPolicy,
                          headAddress,
                          beaconTokenName,
                          seedAddress,
                          initTx,
                          System.currentTimeMillis(),
                          autonomousBlocks
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
