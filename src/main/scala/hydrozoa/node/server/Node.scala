package hydrozoa.node.server

import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.Cardano
import hydrozoa.l1.event.MultisigEventManager
import hydrozoa.l1.multisig.onchain.{mkBeaconTokenName, mkHeadNativeScriptAndAddress}
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.MultisigTxs.{DepositTx, SettlementTx}
import hydrozoa.l1.multisig.tx.deposit.{DepositTxBuilder, DepositTxRecipe}
import hydrozoa.l1.multisig.tx.initialization.{InitTxBuilder, InitTxRecipe}
import hydrozoa.l1.multisig.tx.refund.{PostDatedRefundRecipe, RefundTxBuilder}
import hydrozoa.l1.multisig.tx.settlement.{SettlementRecipe, SettlementTxBuilder}
import hydrozoa.l1.wallet.Wallet
import hydrozoa.l2.block.{Block, majorDummyBlock}
import hydrozoa.l2.consensus.network.{AckMajorCombined, HydrozoaNetwork, ReqInit, ReqRefundLater}
import hydrozoa.l2.consensus.{HeadParams}
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
    log: Logger
):

    var multisigEventManager: Option[MultisigEventManager] = None

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
        val txDraft = initTxBuilder.mkInitDraft(initTxRecipe) match
            case Right(v)  => v
            case Left(err) => return Left(err)

        val ownWit: TxKeyWitness = signTx(txDraft, ownKeys._1)

        val peersWits: Set[TxKeyWitness] = network.reqInit(ReqInit(txId, txIx, amount))
        // TODO: broadcast ownWit

        // TODO: this is temporal, in real world we need to give the tx to the initiator to be signed
        val userWit = wallet.sign(txDraft)

        // All wits are here, we can sign and submit
        val wits = peersWits + ownWit + userWit

        val initTx: L1Tx = wits.foldLeft(txDraft)(addWitness)

        log.whenInfoEnabled {
            log.info("Init tx: " + Transaction.deserialize(initTx.bytes).serializeToHex())
        }

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
                    AddressBechL1(headAddress),
                    headStateManager,
                    log
                  )
                )

                // FIXME: wait for the tx?
                // Emulate l1 init event
                multisigEventManager.map(_.handleInitTx(initTx))

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
        val Some(maturity, window, expiry) = headStateManager.depositTimingParams()
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
        val Right(depositTx, index) = depositTxBuilder.mkDepositTx(depositTxRecipe)
        val depositTxHash = txHash(depositTx)

        log.info(s"Deposit tx: ${serializeTxHex(depositTx)}")
        log.info(s"Deposit tx hash: $depositTxHash, deposit output index: $index")

        val Right(refundTxDraft) =
            refundTxBuilder.mkPostDatedRefund(
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
        log.info(s"Refund tx: ${serializeTxHex(refundTx)}")

        // TODO temporarily we submit the deposit tx here
        val Right(depositTxId) =
            cardano.submit(
              addWitness(depositTx, wallet.sign(depositTx))
            ) // TODO: add the combined function
        log.info(s"Deposit tx submitted: $depositTxId")

        // FIXME: wait for the tx?
        // Emulate L1 deposit event
        multisigEventManager.map(_.handleDepositTx(depositTx, depositTxHash))

        // TODO: store the post-dated refund in the store along with the deposit id

        Right(DepositResponse(refundTx, (depositTxHash, index)))
    }

    def submit(hex: String): Either[String, TxId] =
        cardano.submit(deserializeTxHex(hex))

    def produceMajorBlock(): Either[String, String] =
        val awaitingDeposits: Set[AwaitingDeposit] = headStateManager.peekDeposits

        log.info(s"Awaiting deposits: $awaitingDeposits")

        val nextMajorVersion: Int = headStateManager.currentMajorVersion + 1

        val block: Block = majorDummyBlock(nextMajorVersion, awaitingDeposits)

        val txRecipe = SettlementRecipe(awaitingDeposits, nextMajorVersion)

        val Right(settlementTxDraft: SettlementTx) = settlementTxBuilder.mkSettlement(txRecipe)

        val ownWit: TxKeyWitness = signTx(settlementTxDraft.toTx, ownKeys._1)

        val ackMajorCombined: Set[AckMajorCombined] = network.reqMajor(block)

        val wits: Set[TxKeyWitness] = ackMajorCombined.map(_.settlement) + ownWit

        val settlementTx: L1Tx = wits.foldLeft(settlementTxDraft.toTx)(addWitness)

        log.info(s"Settlement tx: ${serializeTxHex(settlementTx)}")

        val Right(settlementTxId) = cardano.submit(settlementTx)

        log.info(s"Settlement tx submitted: $settlementTxId")

        multisigEventManager.map(_.handleSettlementTx(settlementTx, settlementTxId))

        Right(serializeTxHex(settlementTx))
