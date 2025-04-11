package hydrozoa.node.server

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.event.MultisigL1EventSource
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.*
import hydrozoa.l1.multisig.tx.deposit.{DepositTxBuilder, DepositTxRecipe}
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l2.block.*
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.consensus.network.*
import hydrozoa.l2.ledger.state.UtxosSetOpaque
import hydrozoa.l2.ledger.{AdaSimpleLedger, SimpleGenesis, UtxosSet}
import hydrozoa.node.TestPeer
import hydrozoa.node.rest.SubmitRequestL2
import hydrozoa.node.rest.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.server.DepositError
import hydrozoa.node.state.*
import scalus.prelude.Maybe

class Node(
    nodeState: NodeState,
    ownPeer: Wallet,
    network: HeadPeerNetwork,
    cardano: CardanoL1,
    initTxBuilder: InitTxBuilder,
    depositTxBuilder: DepositTxBuilder,
    refundTxBuilder: RefundTxBuilder,
    settlementTxBuilder: SettlementTxBuilder,
    finalizationTxBuilder: FinalizationTxBuilder
):

    private val log = Logger(getClass)

    // FIXME: protect
    def nodeStateReader = nodeState

    // FIXME: find the proper place for it
    private var multisigL1EventManager: Option[MultisigL1EventSource] = None

    def initializeHead(
        otherHeadPeers: Set[WalletId],
        treasuryAda: Long,
        txId: TxId,
        txIx: TxIx
    ): Either[InitializationError, TxId] =
        assert(otherHeadPeers.nonEmpty, "Solo node mode is not supported yet.")
        log.info(s"Init the head with seed ${txId.hash}#${txIx.ix}, amount $treasuryAda ADA")

        // Request verification keys from known peers
        val knownVKeys = network.reqVerificationKeys()
        log.info(s"knownVKeys: $knownVKeys")

        // ReqInit
        val seedOutput = UtxoIdL1(txId, txIx)
        val treasuryCoins = treasuryAda * 1_000_000
        val reqInit = ReqInit(ownPeer.getWalletId, otherHeadPeers, seedOutput, treasuryCoins)

        network.reqInit(reqInit)

        Left("all is good (supposedly)")

    end initializeHead

    def deposit(r: DepositRequest): Either[DepositError, DepositResponse] = {

        log.info(s"Deposit request: $r")

        /*

        How deadline relates to other consensus parameters:

         * `depositMarginMaturity` (s) - mature time
         * `minimalDepositWindow` (s) - minimal window
         * `depositMarginExpiry` (s) - potential race prevention

                `depositMarginMaturity`         `minimalDepositWindow`        `depositMarginExpiry`
        ----*|============================|*******************************|==========================|-------

             ^ we are here                ^ now settlement tx can pick up                            ^ closest
               `latestBlockTime`            the deposit                                                deadline

              - no enough confirmations     - mature enough                 - to close to deadline
                can be rolled back with     - far enough from deadline        may lead to races with
                higher probability                                            post-dated refund tx

         So basic check for requested deadline is:

          deadline > latestBlockTime + depositMarginMaturity + minimalDepositWindow + depositMarginExpiry

         */

        // TODO: Check deadline
//        val (maturity, window, expiry) = nodeState.head.openPhase(_.depositTimingParams)
//        val latestBlockTime = cardano.lastBlockTime
//        val minimalDeadline = latestBlockTime + maturity + window + expiry
//
//        val deadline = r.deadline.getOrElse(minimalDeadline)
//        if (deadline < minimalDeadline)
//            return Left(
//              s"Deadline ($deadline) should be later than $minimalDeadline = latestBlockTime ($latestBlockTime) +" +
//                  s" depositMarginMaturity ($maturity) +" +
//                  s" minimalDepositWindow ($window) + depositMarginExpiry ($expiry)"
//            )

        // Make the datum and the recipe
        // TODO: should we check that datum is sound?
        val depositDatum = DepositDatum(
          decodeBech32AddressL2(r.address),
          Maybe.fromOption(r.datum.map(datumByteString)),
          BigInt.apply(0), // deadline,
          decodeBech32AddressL1(r.refundAddress),
          Maybe.fromOption(r.datum.map(datumByteString))
        )

        val depositTxRecipe = DepositTxRecipe(UtxoIdL1(r.txId, r.txIx), depositDatum)

        // Build a deposit transaction draft as a courtesy of Hydrozoa (no signature)
        val Right(depositTxDraft, index) = depositTxBuilder.buildDepositTxDraft(depositTxRecipe)
        val depositTxHash = txHash(depositTxDraft)

        val serializedTx = serializeTxHex(depositTxDraft)
        log.info(s"Deposit tx: $serializedTx")
        log.info(s"Deposit tx hash: $depositTxHash, deposit output index: $index")

        TxDump.dumpMultisigTx(depositTxDraft)

        val Right(refundTxDraft) =
            refundTxBuilder.mkPostDatedRefundTxDraft(
              PostDatedRefundRecipe(depositTxDraft, index)
            )

        TxDump.dumpMultisigTx(refundTxDraft)

        // Own signature
        val ownWit: TxKeyWitness = ownPeer.createTxKeyWitness(refundTxDraft)

        // ReqRefundLater
        // TODO: Add a comment to explain how it's guaranteed that
        //  a deposit cannot be stolen by malicious peers
        val peersWits: Set[TxKeyWitness] =
            network.reqRefundLater(ReqRefundLater(depositTxDraft, index))
        // TODO: broadcast ownWit

        val wits = peersWits + ownWit

        val refundTx = wits.foldLeft(refundTxDraft)(addWitness)
        val serializedRefundTx = serializeTxHex(refundTx)
        log.info(s"Refund tx: $serializedRefundTx")

        // TODO temporarily we submit the deposit tx here
        val Right(depositTxId) =
            cardano.submit(
              addWitness(depositTxDraft, ownPeer.createTxKeyWitness(depositTxDraft)).toL1Tx
            ) // TODO: add the combined function
        log.info(s"Deposit tx submitted: $depositTxId")

//        // Emulate L1 deposit event
//        multisigL1EventManager.map(
//          _.handleDepositTx(toL1Tx(depositTxDraft), depositTxHash)
//        )

        // TODO: store the post-dated refund in the store along with the deposit id

        println(nodeState.head.openPhase(_.stateL1))

        Right(DepositResponse(refundTx, UtxoIdL1(depositTxHash, index)))
    }

    def submitL1(hex: String): Either[String, TxId] =
        cardano.submit(deserializeTxHex[L1](hex))

    def submitL2(req: SubmitRequestL2): Either[String, TxId] =
        nodeState.head.openPhase { s =>
            val ledger = s.stateL2

            val event = req match
                case Transaction(tx) =>
                    AdaSimpleLedger.mkTransactionEvent(tx)
                case Withdrawal(wd) =>
                    AdaSimpleLedger.mkWithdrawalEvent(wd)

            s.poolEventL2(event)
            Right(event.getEventId)
        }

    /** Manually triggers next block creation procedure.
      * @param nextBlockFinal
      * @return
      */
    def handleNextBlock(
        nextBlockFinal: Boolean
    ): Either[String, (BlockRecord, UtxosSet, UtxosSet)] =

        def nextBlockInOpen(
            nextBlockFinal: Boolean
        ): Option[(Block, UtxosSetOpaque, UtxosSet, UtxosSet, Option[(TxId, SimpleGenesis)])] =
            nodeState.head.openPhase { s =>

                println(
                  ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> handleNextBlock.nextBlockInOpen"
                )
                println("-----------------------   POOL    --------------------------------------")
                println(s.immutablePoolEventsL2)
                println("-----------------------   L1 State --------------------------------------")
                println(s.stateL1)
                println

                val maybeNewBlock = createBlock(
                  s.stateL2.blockProduction,
                  s.immutablePoolEventsL2,
                  s.peekDeposits,
                  s.l2Tip.blockHeader,
                  timeCurrent,
                  false
                )

                maybeNewBlock
            }

        def nextBlockInFinal()
            : (Block, UtxosSetOpaque, UtxosSet, UtxosSet, Option[(TxId, SimpleGenesis)]) =
            nodeState.head.finalizingPhase { s =>

                println(
                  ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> handleNextBlock.nextBlockInFinal"
                )
                println("-----------------------   L1 State --------------------------------------")
                println(s.stateL1)
                println

                val finalBlock = createBlock(
                  s.stateL2.blockProduction,
                  Seq.empty,
                  UtxoSet(Map.empty),
                  s.l2Tip.blockHeader,
                  timeCurrent,
                  true
                )
                finalBlock.get
            }

        val (maybeNewBlock, finalizeHead) = nodeState.head.currentPhase match
            case HeadPhase.Open       => (nextBlockInOpen(nextBlockFinal), nextBlockFinal)
            case HeadPhase.Finalizing => (Some(nextBlockInFinal()), false)
            case phase =>
                log.error(s"A block can't be produced in phase: $phase")
                (None, false)

        maybeNewBlock match
            case Some(block, utxosActive, utxosAdded, utxosWithdrawn, mbGenesis) =>
                // Create effects
                val l1Effect =
                    mkL1BlockEffect(
                      settlementTxBuilder,
                      finalizationTxBuilder,
                      Some(ownPeer),
                      Some(network),
                      block,
                      utxosWithdrawn
                    )
                val l2Effect = block.blockHeader.blockType match
                    case Minor => utxosActive
                    case Major => utxosActive /\ mbGenesis
                    case Final => ()

                // Record and block application
                val record = BlockRecord(block, l1Effect, (), l2Effect)
                applyBlock(record)
                // Move head into finalization phase if finalizeHead flag is received
                if (finalizeHead) nodeState.head.openPhase(_.finalizeHead())
                // Dump state
                dumpState()
                // Result
                Right((record, utxosAdded, utxosWithdrawn))
            case None => Left("Block can't be produced at the moment.")

    private def dumpState(): Unit =
        nodeState.head.currentPhase match
            case HeadPhase.Open =>
                println(
                  "-----------------------   L1 State --------------------------------------"
                )
                println(nodeState.head.openPhase(_.stateL1))
                println
                println(
                  "-----------------------   POOL    ---------------------------------------"
                )
                println(nodeState.head.openPhase(_.immutablePoolEventsL2))
                println
                println(
                  "-----------------------   L2 State   ------------------------------------"
                )
                println(nodeState.head.openPhase(_.stateL2.getUtxosActive))
                println
                println(
                  "------------------------  BLOCKS   --------------------------------------"
                )
                println(nodeState.head.openPhase(_.immutableBlocksConfirmedL2))
                println
                println(
                  "------------------------  EVENTS   --------------------------------------"
                )
                println(nodeState.head.openPhase(_.immutableEventsConfirmedL2))

            case HeadPhase.Finalizing =>
                println(
                  "-----------------------   L1 State --------------------------------------"
                )
                println(nodeState.head.finalizingPhase(_.stateL1))
                println(
                  "-----------------------   L2 State   ------------------------------------"
                )
                println(nodeState.head.finalizingPhase(_.stateL2.getUtxosActive))
            // println
            // println(
            //    "------------------------  BLOCKS   --------------------------------------"
            // )
            // println(nodeState.head.finalizingPhase(_.immutableBlocksConfirmedL2))
            case HeadPhase.Finalized => println("Node is finalized.")

    private def applyBlock(
        blockRecord: BlockRecord
    ): Unit =
        val block = blockRecord.block
        block.blockHeader.blockType match
            case Minor =>
                // No L1 effects so far in multisig mode for Minor blocks
                // L2 effect
                val l2BlockEffect_ = blockRecord.l2Effect.asInstanceOf[MinorBlockL2Effect]
                nodeState.head.openPhase { s =>
                    s.addBlock(blockRecord)
                    s.stateL2.replaceUtxosActive(l2BlockEffect_)
                    val body = block.blockBody
                    s.confirmMempoolEvents(
                      block.blockHeader.blockNum,
                      body.eventsValid,
                      None,
                      body.eventsInvalid
                    )
                }

            case Major =>
                // L1 effect
                val settlementTx = toL1Tx(
                  blockRecord.l1Effect.asInstanceOf[SettlementTx]
                ) // FIXME: casting
                val Right(settlementTxId) = cardano.submit(settlementTx)
                log.info(s"Settlement tx submitted: $settlementTxId")

//                // Emulate L1 event
//                // TODO: I don't think we have to wait L1 event in reality
//                //  instead we need to update the treasury right upon submitting.
//                //  Another concern - probably we have to do it in one atomic change
//                //  along with the L2 effect. Otherwise the next settlement transaction
//                //  may use the old treasury.
//                multisigL1EventManager.map(
//                  _.handleSettlementTx(settlementTx, settlementTxId)
//                )

                // L2 effect
                val l2BlockEffect_ = blockRecord.l2Effect.asInstanceOf[MajorBlockL2Effect]
                nodeState.head.openPhase { s =>
                    s.addBlock(blockRecord)
                    s.stateL2.replaceUtxosActive(l2BlockEffect_._1)
                    val body = block.blockBody
                    s.confirmMempoolEvents(
                      block.blockHeader.blockNum,
                      body.eventsValid,
                      l2BlockEffect_._2,
                      body.eventsInvalid
                    )

                    // FIXME: we should it clean up immediately, so the next block won't pick it up again
                    //  Solution: keep L1 state in accordance to ledger, filter out deposits absorbed
                    //  They can be known from L1 major block effects (currently not implemented).
                    s.removeAbsorbedDeposits(body.depositsAbsorbed)
                }

            case Final =>
                // L1 effect
                val finalizationTx = toL1Tx(
                  blockRecord.l1Effect.asInstanceOf[FinalizationTx]
                ) // FIXME: casting
                val Right(finalizationTxId) = cardano.submit(finalizationTx)
                log.info(s"Finalizationtx submitted: $finalizationTxId")

//                // Emulate L1 event
//                // TODO: temporarily handle the event, again, we don't want to wait
//                multisigL1EventManager.foreach(
//                  _.handleFinalizationTx(
//                    finalizationTx,
//                    finalizationTxId
//                  )
//                )

                // L2 effect
                nodeState.head.finalizingPhase { s =>
                    s.stateL2.flush
                    s.confirmValidMempoolEvents(
                      block.blockHeader.blockNum,
                      block.blockBody.eventsValid
                    )
                    s.closeHead(blockRecord)
                }

end Node

def mkL1BlockEffect(
    settlementTxBuilder: SettlementTxBuilder,
    finalizationTxBuilder: FinalizationTxBuilder,
    mbOwnPeer: Option[Wallet],
    mbNetwork: Option[HeadPeerNetwork],
    block: Block,
    utxosWithdrawn: UtxosSet
): L1BlockEffect =
    block.blockHeader.blockType match
        case Minor => ()
        case Major =>
            // Create settlement tx draft
            val txRecipe = SettlementRecipe(
              block.blockHeader.versionMajor,
              block.blockBody.depositsAbsorbed,
              utxosWithdrawn
            )
            val Right(settlementTxDraft: SettlementTx) =
                settlementTxBuilder.mkSettlementTxDraft(txRecipe)
            mbOwnPeer /\ mbNetwork match
                case Some(ownPeer) -> Some(network) =>
                    val ownWit: TxKeyWitness = ownPeer.createTxKeyWitness(settlementTxDraft)
                    // TODO: broadcast ownWit

                    // Confirm block
                    val acksMajorCombined = network.reqMajor(block, utxosWithdrawn)

                    TxDump.dumpMultisigTx(settlementTxDraft)

                    // L1 effect
                    val wits = acksMajorCombined.map(_.settlement) + ownWit
                    val settlementTx = wits.foldLeft(settlementTxDraft)(addWitness)
                    // val serializedTx = serializeTxHex(settlementTx)

                    settlementTx
                case _ => // used in MBT
                    settlementTxDraft
        case Final =>
            // Create finalization tx draft
            val recipe =
                FinalizationRecipe(block.blockHeader.versionMajor, utxosWithdrawn)
            val Right(finalizationTxDraft: FinalizationTx) =
                finalizationTxBuilder.buildFinalizationTxDraft(recipe)

            mbOwnPeer /\ mbNetwork match
                case Some(ownPeer) -> Some(network) =>
                    val ownWit: TxKeyWitness = ownPeer.createTxKeyWitness(finalizationTxDraft)
                    // TODO: broadcast ownWit

                    // Confirm block
                    val acksFinalCombined = network.reqFinal(block, utxosWithdrawn)

                    // L1 effect
                    val wits = acksFinalCombined.map(_.finalization) + ownWit
                    val finalizationTx = wits.foldLeft(finalizationTxDraft)(addWitness)
                    // val serializedTx = serializeTxHex(finalizationTx)

                    finalizationTx
                case _ => // used in MBT
                    finalizationTxDraft
