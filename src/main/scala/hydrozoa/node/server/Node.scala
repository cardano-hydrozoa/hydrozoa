package hydrozoa.node.server

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.Cardano
import hydrozoa.l1.event.MultisigEventManager
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.MultisigTxs.{DepositTx, FinalizationTx, SettlementTx}
import hydrozoa.l1.multisig.tx.deposit.{DepositTxBuilder, DepositTxRecipe}
import hydrozoa.l1.multisig.tx.finalization.{FinalizationRecipe, FinalizationTxBuilder}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l1.wallet.Wallet
import hydrozoa.l2.block.{Block, finalDummyBlock, majorDummyBlock}
import hydrozoa.l2.consensus.HeadParams
import hydrozoa.l2.consensus.network.*
import hydrozoa.node.server.DepositError
import scalus.prelude.Maybe

class Node(
    headStateManager: HeadStateManager,
    ownKeys: (ParticipantSecretKey, ParticipantVerificationKey),
    network: HydrozoaNetwork,
    cardano: Cardano,
    wallet: Wallet,
    initTxBuilder: InitTxBuilder,
    depositTxBuilder: DepositTxBuilder,
    refundTxBuilder: RefundTxBuilder,
    settlementTxBuilder: SettlementTxBuilder,
    finalizationTxBuilder: FinalizationTxBuilder,
    log: Logger
):

    var multisigEventManager: Option[MultisigEventManager] = None
    val txDump: os.Path = os.pwd / "txs.out"

    def initializeHead(amount: Long, txId: TxId, txIx: TxIx): Either[InitializeError, TxId] = {
        log.info(s"Init the head with seed ${txId.hash}#${txIx.ix}, amount $amount ADA")

        // TODO: check head/node status

        // Make a recipe to build init tx

        // Head's verification keys
        val vKeys = network.participantsKeys() + ownKeys._2

        // Native script, head address, and token
        val (headNativeScript, headAddress) = mkHeadNativeScriptAndAddress(vKeys, cardano.network())
        val beaconTokenName = mkBeaconTokenName(txId, txIx)

        val initTxRecipe = InitTxRecipe(
          headAddress,
          txId,
          txIx,
          amount,
          headNativeScript,
          beaconTokenName
        )

        // Builds and balance Cardano tx
        val (txDraft, seedAddress) = initTxBuilder.mkInitializationTxDraft(initTxRecipe) match
            case Right(v, seedAddress) => (v, seedAddress)
            case Left(err)             => return Left(err)

        val ownWit: TxKeyWitness = signTx(txDraft, ownKeys._1)

        val peersWits: Set[TxKeyWitness] = network.reqInit(ReqInit(txId, txIx, amount))
        // TODO: broadcast ownWit

        // TODO: this is temporal, in real world we need to give the tx to the initiator to be signed
        val userWit = wallet.sign(txDraft)

        // All wits are here, we can sign and submit
        val wits = peersWits + ownWit + userWit

        val initTx: L1Tx = wits.foldLeft(txDraft)(addWitness)

//        log.whenInfoEnabled {
        val serializedTx = serializeTxHex(initTx)
        log.info("Init tx: " + serializedTx)
        // TODO: factor out from the main code
        os.remove(txDump)
        os.write(txDump, serializedTx)
//        }

        val ret = cardano.submit(initTx)

        ret match
            case Right(_) =>
                // Put the head into multisig regime state
                log.info(
                  s"Head was initialized at address: $headAddress, token name: $beaconTokenName"
                )
                // initialize new multisig event manager
                multisigEventManager = Some(
                  MultisigEventManager(
                    HeadParams.default,
                    headNativeScript,
                    beaconTokenName,
                    AddressBechL1(headAddress),
                    headStateManager,
                    log
                  )
                )

                // Emulate l1 init event
                multisigEventManager.map(_.handleInitTx(initTx, seedAddress))

        ret
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
        val Some(maturity, window, expiry) = headStateManager.depositTimingParams
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
        // TODO: should we check that datums are sound?
        val depositDatum = DepositDatum(
          decodeBech32AddressL2(r.address),
          Maybe.fromOption(r.datum.map(datumByteString)),
          deadline,
          decodeBech32AddressL1(r.refundAddress),
          Maybe.fromOption(r.datum.map(datumByteString))
        )

        val depositTxRecipe = DepositTxRecipe((r.txId, r.txIx), depositDatum)

        // Build a deposit transaction draft as a courtesy of Hydrozoa (no signature)
        val Right(depositTx, index) = depositTxBuilder.buildDepositTxDraft(depositTxRecipe)
        val depositTxHash = txHash(depositTx)

        val serializedTx = serializeTxHex(depositTx)
        log.info(s"Deposit tx: $serializedTx")
        log.info(s"Deposit tx hash: $depositTxHash, deposit output index: $index")
        os.write.append(txDump, "\n" + serializedTx)

        val Right(refundTxDraft) =
            refundTxBuilder.mkPostDatedRefundTxDraft(
              PostDatedRefundRecipe(DepositTx(depositTx), index)
            )

        // Own signature
        val ownWit: TxKeyWitness = signTx(refundTxDraft.toTx, ownKeys._1)

        // ReqRefundLater
        // TODO: Add a comment to explain how it's guaranteed that
        //  a deposit cannot be stolen by malicious peers
        val peersWits: Set[TxKeyWitness] = network.reqRefundLater(ReqRefundLater(depositTx, index))
        // TODO: broadcast ownWit

        val wits = peersWits + ownWit

        val refundTx: L1Tx = wits.foldLeft(refundTxDraft.toTx)(addWitness)
        val serializedRefundTx = serializeTxHex(refundTx)
        log.info(s"Refund tx: $serializedRefundTx")
        os.write.append(txDump, "\n" + serializedRefundTx)

        // TODO temporarily we submit the deposit tx here
        val Right(depositTxId) =
            cardano.submit(
              addWitness(depositTx, wallet.sign(depositTx))
            ) // TODO: add the combined function
        log.info(s"Deposit tx submitted: $depositTxId")

        // Emulate L1 deposit event
        multisigEventManager.map(_.handleDepositTx(depositTx, depositTxHash))

        // TODO: store the post-dated refund in the store along with the deposit id

        Right(DepositResponse(refundTx, (depositTxHash, index)))
    }

    def submit(hex: String): Either[String, TxId] =
        cardano.submit(deserializeTxHex(hex))

    def handleNextMajorBlock(nextBlockFinal: Boolean): Either[String, String] =
        if nextBlockFinal then produceFinalBlock else produceMajorBlock

    def produceMajorBlock: Either[String, String] =

        val awaitingDeposits: Set[AwaitingDeposit] = headStateManager.peekDeposits

        log.info(s"Awaiting deposits: $awaitingDeposits")

        val nextMajorVersion: Int = headStateManager.currentMajorVersion + 1

        val block: Block = majorDummyBlock(nextMajorVersion, awaitingDeposits)

        val txRecipe = SettlementRecipe(awaitingDeposits, nextMajorVersion)

        val Right(settlementTxDraft: SettlementTx) =
            settlementTxBuilder.mkSettlementTxDraft(txRecipe)

        val ownWit: TxKeyWitness = signTx(settlementTxDraft.toTx, ownKeys._1)

        val ackMajorCombined: Set[AckMajorCombined] = network.reqMajor(block)

        val wits: Set[TxKeyWitness] = ackMajorCombined.map(_.settlement) + ownWit

        val settlementTx: L1Tx = wits.foldLeft(settlementTxDraft.toTx)(addWitness)

        val serializedTx = serializeTxHex(settlementTx)
        log.info(s"Settlement tx: $serializedTx")
        os.write.append(txDump, "\n" + serializedTx)

        val Right(settlementTxId) = cardano.submit(settlementTx)

        log.info(s"Settlement tx submitted: $settlementTxId")

        multisigEventManager.map(_.handleSettlementTx(settlementTx, settlementTxId))

        Right(serializedTx)

    def produceFinalBlock: Either[String, String] =

        // Block
        val nextMajorVersion: Int = headStateManager.currentMajorVersion + 1
        val block: Block = finalDummyBlock(nextMajorVersion)

        // Tx
        val recipe = FinalizationRecipe(nextMajorVersion)

        val Right(finalizationTxDraft: FinalizationTx) =
            finalizationTxBuilder.buildFinalizationTxDraft(recipe)

        // Consensus
        val ownWit: TxKeyWitness = signTx(finalizationTxDraft.toTx, ownKeys._1)
        val ackFinalCombined: Set[AckFinalCombined] = network.reqFinal(block)
        // TODO: broadcast ownWit

        // Sign and submit
        val wits: Set[TxKeyWitness] = ackFinalCombined.map(_.finalization) + ownWit
        val finalizationTx: L1Tx = wits.foldLeft(finalizationTxDraft.toTx)(addWitness)
        val serializedTx = serializeTxHex(finalizationTx)
        log.info(s"Finalization tx: $serializedTx")
        os.write.append(txDump, "\n" + serializedTx)

        val Right(finalizationTxId) = cardano.submit(finalizationTx)
        log.info(s"Finalization tx submitted: $finalizationTxId")

        // TODO: temporary: handle the event
        multisigEventManager.map(_.handleFinalizationTx(finalizationTx, finalizationTxId))

        Right(serializedTx)
