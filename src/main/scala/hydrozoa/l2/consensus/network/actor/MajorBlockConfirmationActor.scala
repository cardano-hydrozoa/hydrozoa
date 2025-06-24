package hydrozoa.l2.consensus.network.actor

import com.typesafe.scalalogging.Logger
import hydrozoa.infra.{addWitnessMultisig, serializeTxHex, txHash}
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.tx.SettlementTx
import hydrozoa.l1.multisig.tx.fallback.{FallbackTxBuilder, FallbackTxRecipe}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l2.block.{BlockValidator, ValidationResolution}
import hydrozoa.l2.consensus.network.{AckMajor, AckMajor2, Req, ReqMajor}
import hydrozoa.l2.ledger.simple.SimpleL2Ledger
import hydrozoa.l2.ledger.{HydrozoaL2Ledger, L2Genesis}
import hydrozoa.node.state.*
import hydrozoa.*
import hydrozoa.l1.rulebased.onchain.DisputeResolutionScript
import ox.channels.{ActorRef, Channel, Source}
import ox.resilience.{RetryConfig, retryEither}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt

private class MajorBlockConfirmationActor(
    stateActor: ActorRef[NodeState],
    walletActor: ActorRef[Wallet],
    settlementTxBuilder: SettlementTxBuilder,
    fallbackTxBuilder: FallbackTxBuilder,
    dropMyself: () => Unit
) extends ConsensusActor:

    private val log = Logger(getClass)

    override type ReqType = ReqMajor
    override type AckType = AckMajor | AckMajor2

    private var utxosActive: HydrozoaL2Ledger.LedgerUtxoSetOpaque = _
    private var mbGenesis: Option[(TxId, L2Genesis)] = _
    private var utxosWithdrawn: UtxoSetL2 = _
    private val acks: mutable.Map[WalletId, AckMajor] = mutable.Map.empty
    private val acks2: mutable.Map[WalletId, AckMajor2] = mutable.Map.empty
    private var finalizeHead: Boolean = false
    private var settlementTxDraft: SettlementTx = _
    private var fallbackTxDraft: TxL1 = _
    private var ownAck2: Option[AckMajor2] = None

    private def tryMakeAck2(): Option[AckType] =
        log.debug(s"tryMakeAck2 - acks: ${acks.keySet}")

        val (headPeers, isNextBlockFinal) =
            stateActor.ask(_.head.openPhase(open => (open.headPeers, open.isNextBlockFinal)))
        log.debug(s"headPeers: $headPeers")
        if req != null && ownAck2.isEmpty && acks.keySet == headPeers
        then
            // TODO: how do we check that all acks are valid?
            val (me, settlementTxKeyWitness) =
                walletActor.ask(w => (w.getWalletId, w.createTxKeyWitness(settlementTxDraft)))
            val ownAck2 = AckMajor2(me, settlementTxKeyWitness, isNextBlockFinal)

            this.ownAck2 = Some(ownAck2)
            deliverAck2(ownAck2)
            Some(ownAck2)
        else None

    private def tryMakeResult(): Unit =
        log.debug("tryMakeResult")
        val headPeers = stateActor.ask(_.head.openPhase(_.headPeers))

        if (req != null && acks.keySet == headPeers && acks2.keySet == headPeers) then
            // L1 effect
            val wits = acks2.map(_._2.settlement)
            val settlementTx = wits.foldLeft(this.settlementTxDraft)(addWitnessMultisig)
            // val serializedTx = serializeTxHex(settlementTx)
            val l1Effect: L1BlockEffect = settlementTx

            // L1 post-dated fallback effect
            val witsFallback = acks.map(_._2.postDatedTransition)
            val fallbackTx = witsFallback.foldLeft(this.fallbackTxDraft)(addWitnessMultisig)
            log.info("Fallback signed tx: " + serializeTxHex(fallbackTx))

            // L2 effect
            val l2Effect = Some(utxosActive)

            // Block record and state update by block application
            val record = BlockRecord(req.block, l1Effect, Some(fallbackTx), l2Effect)
            log.info(s"Major block record is: $record")
            stateActor.tell(nodeState =>
                nodeState.head.openPhase(s =>
                    s.applyBlockRecord(record, mbGenesis)
                    // Dump state
                    nodeState.head.dumpState()
                )
            )

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
                def tryValidate = {
                    val (prevHeader, stateL2Cloned, poolEventsL2, depositUtxos) =
                        stateActor.ask(
                          _.head.openPhase(openHead =>
                              (
                                openHead.l2Tip.blockHeader,
                                openHead.stateL2.cloneForBlockProducer(),
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
                            Right(utxosActive, mbGenesis, utxosWithdrawn)
                        case r @ ValidationResolution.NotYetKnownDeposits(depositsUnknown) =>
                            log.warn(
                              s"Validation failed with NotYetKnownDeposits: $depositsUnknown"
                            )
                            Left(r)
                        // Fail fast on other errors for now
                        case resolution =>
                            throw RuntimeException(s"Block validation failed: $resolution")
                }

                retryEither(RetryConfig.delay(10, 1.second))(tryValidate) match
                    case Right(ret) => ret
                    case Left(err)  => throw RuntimeException(err.toString)

        // Update local state
        this.req = req
        this.utxosActive = utxosActive
        this.mbGenesis = mbGenesis
        this.utxosWithdrawn = utxosWithdrawn

        // Prepare own ack1
        val myself = walletActor.ask(w => w.getWalletId)

        // Create settlement tx draft
        val txRecipe = SettlementRecipe(
          req.block.blockHeader.versionMajor,
          req.block.blockBody.depositsAbsorbed,
          utxosWithdrawn
        )
        log.info(s"Settlement tx recipe: $txRecipe")
        val Right(settlementTxDraft: SettlementTx) =
            settlementTxBuilder.mkSettlementTxDraft(txRecipe)
        val serializedTx = serializeTxHex(settlementTxDraft)
        log.info(s"Settlement tx for block ${req.block.blockHeader.blockNum} is $serializedTx")
        this.settlementTxDraft = settlementTxDraft
        // TxDump.dumpMultisigTx(settlementTxDraft)

        // Create the corresponding post-dated fallback tx
        val (peersKeys, headAddress, headNativeScript, headMintingPolicy) =
            stateActor.ask(state =>
                state.head.openPhase { openHead =>
                    val peersKeys = state.getVerificationKeys(openHead.headPeers).get
                    val (headAddress, headNativeScript, headMintingPolicy) =
                        (
                          openHead.headBechAddress,
                          openHead.headNativeScript,
                          openHead.headMintingPolicy
                        )
                    (peersKeys, headAddress, headNativeScript, headMintingPolicy)
                }
            )

        val fallbackTxRecipe = FallbackTxRecipe(
          multisigTx = settlementTxDraft,
          // FIXME: script
          treasuryScript = AddressBech[L1](
            "addr_test1qr79wm0n5fucskn6f58u2qph9k4pm9hjd3nkx4pwe54ds4gh2vpy4h4r0sf5ah4mdrwqe7hdtfcqn6pstlslakxsengsgyx75q"
          ),
          disputeScript = DisputeResolutionScript.entAddress(networkL1static),
          votingDuration = 1024,
          // Sorting
          peers = peersKeys.toList,
          headAddressBech32 = headAddress,
          headNativeScript = headNativeScript,
          headMintingPolicy = headMintingPolicy
        )

        log.error(s"FallbackTxRecipe= $fallbackTxRecipe")

        val Right(fallbackTxDraft) = fallbackTxBuilder.buildFallbackTxDraft(fallbackTxRecipe)

        log.info("Fallback tx draft: " + serializeTxHex(fallbackTxDraft))
        log.info("Fallback tx draft hash: " + txHash(fallbackTxDraft))

        this.fallbackTxDraft = fallbackTxDraft

        val (me, fallbackTxKeyWitness) =
            walletActor.ask(w => (w.getWalletId, w.createTxKeyWitness(fallbackTxDraft)))

        // TODO: rollouts
        val ownAck = AckMajor(myself, Seq.empty, fallbackTxKeyWitness)
        log.debug(s"Own AckMajor: $ownAck")

        val mbAck2 = deliver(ownAck)
        log.debug(s"Own AckMajor2: $mbAck2")

        Seq(ownAck) ++ mbAck2.toList

    private val resultChannel: Channel[Unit] = Channel.buffered(1)

    override def result(using req: Req): Source[req.resultType] =
        resultChannel.asInstanceOf[Source[req.resultType]]

end MajorBlockConfirmationActor
