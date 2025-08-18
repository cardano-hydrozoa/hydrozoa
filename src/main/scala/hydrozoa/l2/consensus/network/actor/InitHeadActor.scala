package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{addWitness, serializeTxHex}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScript}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.{InitTx, MultisigTx}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.consensus.network.*
import hydrozoa.node.server.TxDump
import hydrozoa.node.state.{InitializingHeadParams, NodeState, WalletId}
import ox.channels.{ActorRef, Channel, Source}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.Script.Native
import scalus.cardano.ledger.{AssetName, PolicyId, TransactionHash, VKeyWitness}

import scala.collection.mutable

private class InitHeadActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet],
    cardanoActor: ActorRef[CardanoL1],
    initTxBuilder: InitTxBuilder,
    dropMyself: () => Unit
) extends ConsensusActor:

    override type ReqType = ReqInit
    override type AckType = AckInit
    private val log = Logger(getClass)
    private val acks: mutable.Map[WalletId, VKeyWitness] = mutable.Map.empty
    private val resultChannel: Channel[TransactionHash] = Channel.buffered(1)
    private var ownAck: AckInit = _
    private var txDraft: InitTx = _
    private var headNativeScript: Native = _
    private var headMintingPolicy: PolicyId = _
    private var headAddress: AddressL1 = _
    private var beaconTokenName: AssetName = _
    private var seedAddress: AddressL1 = _

    override def init(req: ReqType): Seq[AckType] =
        log.trace(s"Init req: $req")

        val headPeers = req.otherHeadPeers + req.initiator
        val headVKeys = stateActor.ask(_.getVerificationKeys(headPeers)) match {
            case Some(keys) => keys
            case _ =>
                throw RuntimeException(
                  "State actor could not provide verification keys for head peers"
                )
        }
        val headNativeScript = mkHeadNativeScript(headVKeys)

        val headAddress: AddressL1 = Address[L1](
          ShelleyAddress(
            network = cardanoActor.ask(_.network),
            payment = ShelleyPaymentPart.Script(headNativeScript.scriptHash),
            delegation = Null
          )
        )

        log.info(s"Head's address: $headAddress, beacon token name: $beaconTokenName")

        val initTxRecipe = InitTxRecipe(
          network = cardanoActor.ask(_.network),
          seedUtxo = req.seedUtxoId,
          coins = req.treasuryCoins,
          peers = headVKeys
        )

        log.info(s"initTxRecipe: $initTxRecipe")

        // Builds and balance initialization tx
        val (txDraft, seedAddress) = initTxBuilder.mkInitializationTxDraft(initTxRecipe) match {
            case Right(res) => res
            case Left(e) =>
                throw new RuntimeException(
                  s"Init head actor could not produce initialization tx draft. Error from builder: ${e}"
                )
        }

        log.info("Init tx draft: " + serializeTxHex(txDraft))
        log.info("Init tx draft hash: " + txDraft.untagged.id)

        val (me, ownWit) = walletActor.ask(w => (w.getWalletId, w.createTxKeyWitness(txDraft)))
        val ownAck: AckType = AckInit(me, ownWit)

        this.req = req
        this.ownAck = ownAck
        this.txDraft = txDraft
        this.headNativeScript = headNativeScript
        this.headMintingPolicy = headNativeScript.scriptHash
        this.headAddress = headAddress
        this.beaconTokenName = mkBeaconTokenName(req.seedUtxoId)
        this.seedAddress = seedAddress

        @annotation.unused
        val _ = deliver(ownAck)
        Seq(ownAck)

    override def deliver(ack: AckType): Option[AckType] =
        log.trace(s"Deliver ack: $ack")
        @annotation.unused
        val _ = acks.put(ack.peer, ack.signature)
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
                val initTx = acks.values.foldLeft(txDraft.untagged)(addWitness)
                val serializedTx = serializeTxHex(initTx)
                log.info("Initialization tx: " + serializedTx)

                // TODO: submission should be carried on by a separate thread
                cardanoActor.ask(_.submit(initTx)) match
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
                          MultisigTx(initTx),
                          System.currentTimeMillis(),
                          autonomousBlocks
                        )
                        stateActor.tell(_.tryInitializeHead(params))
                        TxDump.dumpInitTx(MultisigTx(initTx))
                        resultChannel.send(txHash)
                        dropMyself()

                    case Left(err) =>
                        val msg = s"Can't submit init tx: $err"
                        log.error(msg)
                        // FIXME: what should go next here?
                        throw RuntimeException(msg)

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end InitHeadActor
