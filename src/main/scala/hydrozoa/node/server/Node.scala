package hydrozoa.node.server

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.Cardano
import hydrozoa.l1.event.MultisigL1EventManager
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.deposit.{DepositTxBuilder, DepositTxRecipe}
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l1.multisig.tx.{DepositTx, FinalizationTx, MultisigTx, SettlementTx}
import hydrozoa.l1.wallet.Wallet
import hydrozoa.l2.block.*
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.consensus.network.*
import hydrozoa.l2.ledger.state.{UtxosSetOpaque, UtxosSetOpaqueMutable}
import hydrozoa.l2.ledger.{AdaSimpleLedger, SimpleGenesis, UtxosDiff}
import hydrozoa.node.api.SubmitRequestL2
import hydrozoa.node.api.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.server.DepositError
import scalus.prelude.Maybe

class Node(
    state: NodeStateManager,
    ownKeys: (ParticipantSecretKey, ParticipantVerificationKey),
    network: HeadPeerNetwork,
    cardano: Cardano,
    wallet: Wallet,
    initTxBuilder: InitTxBuilder,
    depositTxBuilder: DepositTxBuilder,
    refundTxBuilder: RefundTxBuilder,
    settlementTxBuilder: SettlementTxBuilder,
    finalizationTxBuilder: FinalizationTxBuilder,
    log: Logger
):

    // FIXME: find the proper place for it
    private var multisigL1EventManager: Option[MultisigL1EventManager] = None

    def initializeHead(ada: Long, txId: TxId, txIx: TxIx): Either[InitializeError, TxId] = {

        state.asAbsent { _ =>
            log.info(s"Init the head with seed ${txId.hash}#${txIx.ix}, amount $ada ADA")

            // Make a recipe to build init tx

            // All nodes' verification keys
            val vKeys = network.participantsKeys() + ownKeys._2
            // Native script, head address, and token
            val seedOutput = OutputRefL1(txId, txIx)
            val (headNativeScript, headAddress) =
                mkHeadNativeScriptAndAddress(vKeys, cardano.network())
            val beaconTokenName = mkBeaconTokenName(seedOutput)
            val treasuryCoins = ada * 1_000_000
            val initTxRecipe = InitTxRecipe(
              headAddress,
              seedOutput,
              treasuryCoins,
              headNativeScript,
              beaconTokenName
            )

            // Builds and balance initialization tx
            val (txDraft, seedAddress) = initTxBuilder.mkInitializationTxDraft(initTxRecipe) match
                case Right(v, seedAddress) => (v, seedAddress)
                case Left(err)             => return Left(err)

            val ownWit: TxKeyWitness = createTxKeyWitness(txDraft, ownKeys._1)

            val peersWits: Set[TxKeyWitness] = network.reqInit(ReqInit(seedOutput, treasuryCoins))
            // TODO: broadcast ownWit

            // TODO: this is temporal, in real world we need to give the tx to the initiator to be signed
            val userWit = wallet.createTxKeyWitness(txDraft)

            // All wits are here, we can sign and submit
            val wits = peersWits + ownWit + userWit

            val initTx = wits.foldLeft(txDraft)(addWitness)
            val serializedTx = serializeTxHex(initTx)
            log.info("Init tx: " + serializedTx)

            cardano.submit(MultisigTx.toL1Tx(initTx)) match
                case Right(txHash) =>
                    // Put the head into multisig regime state
                    log.info(
                      s"Head was initialized at address: $headAddress, token name: $beaconTokenName"
                    )
                    // initialize new multisig event manager
                    multisigL1EventManager = Some(
                      MultisigL1EventManager(
                        HeadParams.default,
                        headNativeScript,
                        beaconTokenName,
                        AddressBechL1(headAddress),
                        state,
                        log
                      )
                    )

                    // Emulate L1 init event
                    multisigL1EventManager.foreach(
                      _.handleInitTx(MultisigTx.toL1Tx(initTx), seedAddress)
                    )

                    TxDump.dumpInitTx(initTx)
                    println(state.asOpen(_.stateL1))
                    Right(txHash)
                case Left(err) => Left(err)
        }
    }

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

        // Check deadline
        val (maturity, window, expiry) = state.asOpen(_.depositTimingParams)
        val latestBlockTime = cardano.lastBlockTime
        val minimalDeadline = latestBlockTime + maturity + window + expiry

        val deadline = r.deadline.getOrElse(minimalDeadline)
        if (deadline < minimalDeadline)
            return Left(
              s"Deadline ($deadline) should be later than $minimalDeadline = latestBlockTime ($latestBlockTime) +" +
                  s" depositMarginMaturity ($maturity) +" +
                  s" minimalDepositWindow ($window) + depositMarginExpiry ($expiry)"
            )

        // Make the datum and the recipe
        // TODO: should we check that datum is sound?
        val depositDatum = DepositDatum(
          decodeBech32AddressL2(r.address),
          Maybe.fromOption(r.datum.map(datumByteString)),
          deadline,
          decodeBech32AddressL1(r.refundAddress),
          Maybe.fromOption(r.datum.map(datumByteString))
        )

        val depositTxRecipe = DepositTxRecipe(OutputRefL1(r.txId, r.txIx), depositDatum)

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
        val ownWit: TxKeyWitness = createTxKeyWitness(refundTxDraft, ownKeys._1)

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
              MultisigTx.toL1Tx(
                addWitness(depositTxDraft, wallet.createTxKeyWitness(depositTxDraft))
              )
            ) // TODO: add the combined function
        log.info(s"Deposit tx submitted: $depositTxId")

        // Emulate L1 deposit event
        multisigL1EventManager.map(
          _.handleDepositTx(MultisigTx.toL1Tx(depositTxDraft), depositTxHash)
        )

        // TODO: store the post-dated refund in the store along with the deposit id

        println(state.asOpen(_.stateL1))

        Right(DepositResponse(refundTx, OutputRefL1(depositTxHash, index)))
    }

    def submitL1(hex: String): Either[String, TxId] =
        cardano.submit(deserializeTxHex(hex))

    def submitL2(req: SubmitRequestL2): Either[String, TxId] =
        state.asOpen { s =>
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
    def handleNextBlock(nextBlockFinal: Boolean): Either[String, (Block, UtxosDiff, UtxosDiff)] =
        state.asOpen { s =>

            println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> handleNextBlock")

            println("-----------------------   POOL    --------------------------------------")
            println(state.asOpen(_.immutablePoolEventsL2))
            println("-----------------------   L1 State --------------------------------------")
            println(state.asOpen(_.stateL1))
            println

            // FIXME: collect these values atomically
            // (d) Let utxosActive be a mutable variable initialized to stateL2.utxosActive
            // for now (and probably this is legit) we use utxosActive within L2 ledger
            // var utxosActive: UTxOs = state.asOpen(_.utxosActive)
            // TODO: do we need to clone the ledger for block creation?
            val stateL2 = state.asOpen(_.stateL2)
            val poolEvents = state.asOpen(_.immutablePoolEventsL2)
            val finalizing = state.asOpen(_.finalizing)
            val deposits = state.asOpen(_.peekDeposits)
            val prevHeader = state.asOpen(_.l2Tip.blockHeader)

            // TODO: separate creation and handling
            val newBlock = createBlock(
              stateL2.blockProduction,
              poolEvents,
              deposits,
              prevHeader,
              timeCurrent,
              finalizing
            )

            // Set finalization flag after producing the block
            if (nextBlockFinal) s.setFinalizing()

            newBlock match
                // FIXME utxosAdded is not used I think
                case Some(block, utxosActive, utxosAdded, utxosWithdrawn, mbGenesis) =>
                    block.blockHeader.blockType match
                        case Minor =>
                            // TODO: produce and broadcast own signature
                            val acksMinor = network.reqMinor(block)
                            // Immediate L2 effect
                            applyAnyBlockL2Effect(block, utxosActive, mbGenesis)
                            // No L1 effects so far in multisig mode for Minor blocks
                            dumpState()

                        case Major =>
                            // Create settlement tx draft
                            val txRecipe = SettlementRecipe(
                              block.blockHeader.versionMajor,
                              block.blockBody.depositsAbsorbed,
                              utxosWithdrawn
                            )
                            val Right(settlementTxDraft: SettlementTx) =
                                settlementTxBuilder.mkSettlementTxDraft(txRecipe)
                            val ownWit: TxKeyWitness =
                                createTxKeyWitness(settlementTxDraft, ownKeys._1)
                            // TODO: broadcast ownWit
                            // Confirm block
                            val acksMajorCombined = network.reqMajor(block, utxosWithdrawn)

                            TxDump.dumpMultisigTx(settlementTxDraft)

                            // Immediate L2 effect
                            applyAnyBlockL2Effect(block, utxosActive, mbGenesis)

                            // L1 effect
                            val wits: Set[TxKeyWitness] =
                                acksMajorCombined.map(_.settlement) + ownWit
                            val settlementTx = wits.foldLeft(settlementTxDraft)(addWitness)
                            val serializedTx = serializeTxHex(settlementTx)
                            log.info(s"Settlement tx: $serializedTx")

                            // Submit settlement tx
                            val Right(settlementTxId) =
                                cardano.submit(MultisigTx.toL1Tx(settlementTx))
                            log.info(s"Settlement tx submitted: $settlementTxId")

                            // FIXME: Dump augmented virtual tx to combined L1/L2 diagram
                            // TxDump.dumpTx(augmentWithVirtualInputs(settlementTx, utxosWithdrawn.map(_._1)).asL2)

                            // Emulate L1 event
                            // TODO: I don't think we have to wait L1 event in reality
                            //  instead we need to update the treasury right upon submitting.
                            //  Another concern - probably we have to do it in one atomic change
                            //  along with the L2 effect. Otherwise the next settlement transaction
                            //  may use the old treasury.

                            multisigL1EventManager.map(
                              _.handleSettlementTx(MultisigTx.toL1Tx(settlementTx), settlementTxId)
                            )

                            dumpState()

                        case Final =>
                            // Create finalization tx draft
                            val recipe =
                                FinalizationRecipe(block.blockHeader.versionMajor, utxosWithdrawn)
                            val Right(finalizationTxDraft: FinalizationTx) =
                                finalizationTxBuilder.buildFinalizationTxDraft(recipe)
                            val ownWit: TxKeyWitness =
                                createTxKeyWitness(finalizationTxDraft, ownKeys._1)
                            // TODO: broadcast ownWit

                            // Confirm block
                            val acksFinalCombined = network.reqFinal(block, utxosWithdrawn)

                            // Immediate L2 effect
                            applyAnyBlockL2Effect(block, utxosActive, mbGenesis)

                            // L1 effect
                            val wits: Set[TxKeyWitness] =
                                acksFinalCombined.map(_.finalization) + ownWit
                            val finalizationTx = wits.foldLeft(finalizationTxDraft)(addWitness)
                            val serializedTx = serializeTxHex(finalizationTx)

                            log.info(s"Finalization tx: $serializedTx")
                            TxDump.dumpMultisigTx(finalizationTx)

                            val Right(finalizationTxId) =
                                cardano.submit(MultisigTx.toL1Tx(finalizationTx))
                            log.info(s"Finalization tx submitted: $finalizationTxId")

                            // FIXME: Dump augmented virtual tx to combined L1/L2 diagram
                            // TxDump.dumpTx(augmentWithVirtualInputs(settlementTx, utxosWithdrawn.map(_._1)).asL2)

                            // Emulate L1 event
                            // TODO: temporary: handle the event, again, we don't want to wait
                            multisigL1EventManager.map(
                              _.handleFinalizationTx(
                                MultisigTx.toL1Tx(finalizationTx),
                                finalizationTxId
                              )
                            )

                    Right((block, utxosAdded, utxosWithdrawn))

                case None => Left("Block can't be produced at the moment.")
        }

    private def dumpState(): Unit = {
        println(
          "-----------------------   L1 State --------------------------------------"
        )
        println(state.asOpen(_.stateL1))
        println
        println(
          "-----------------------   POOL    ---------------------------------------"
        )
        println(state.asOpen(_.immutablePoolEventsL2))
        println
        println(
          "-----------------------   L2 State   ------------------------------------"
        )
        println(state.asOpen(_.stateL2.activeState))
        println
        println(
          "------------------------  BLOCKS   --------------------------------------"
        )
        println(state.asOpen(_.immutableBlocksConfirmedL2))
        println
        println(
          "------------------------  EVENTS   --------------------------------------"
        )
        println(state.asOpen(_.immutableEventsConfirmedL2))
    }

    private def applyAnyBlockL2Effect(
        block: Block,
        utxosActive: UtxosSetOpaque,
        mbGenesis: Option[(TxId, SimpleGenesis)]
    ): Unit =
        state.asOpen { s =>
            s.stateL2.updateUtxosActive(utxosActive)
            val body = block.blockBody
            s.addBlock(block)
            s.confirmMempoolEvents(
              block.blockHeader.blockNum,
              body.eventsValid,
              mbGenesis,
              body.eventsInvalid
            )

            val depositsAbsorbed = body.depositsAbsorbed

            // FIXME: we should it clean up immediately, so the next block won't pick it up again
            //  Solution: keep L1 state in accordance to ledger, filter out deposits absorbed
            //  They can be known from L1 major block effects (currently not implemented).
            s.removeAbsorbedDeposits(depositsAbsorbed)

        }
