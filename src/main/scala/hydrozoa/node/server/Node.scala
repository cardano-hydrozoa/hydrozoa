package hydrozoa.node.server

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.*
import hydrozoa.l1.multisig.tx.deposit.{DepositTxBuilder, DepositTxRecipe}
import hydrozoa.l2.consensus.network.*
import hydrozoa.l2.ledger.{AdaSimpleLedger, UtxosSet}
import hydrozoa.node.rest.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.rest.{StateL2Response, SubmitRequestL2}
import hydrozoa.node.server.DepositError
import hydrozoa.node.state.*
import hydrozoa.node.state.HeadPhase.Open
import ox.channels.ActorRef
import scalus.prelude.Maybe

class Node:

    private val log = Logger(getClass)

    var network: ActorRef[HeadPeerNetwork] = _
    var nodeState: ActorRef[NodeState] = _
    var cardano: ActorRef[CardanoL1] = _
    var wallet: ActorRef[Wallet] = _
    var depositTxBuilder: ActorRef[DepositTxBuilder] = _

    // FIXME: protect, currently used in tests
    def nodeStateReader = nodeState

    def initializeHead(
        // otherHeadPeers: Set[WalletId],
        treasuryAda: Long,
        txId: TxId,
        txIx: TxIx
    ): Either[InitializationError, TxId] =
        // FIXME: for now others are all known peers minus own peer
        val otherHeadPeers =
            nodeState.ask(_.getKnownPeers).filterNot(p => p == wallet.ask(_.getWalletId))
        assert(otherHeadPeers.nonEmpty, "Solo node mode is not supported yet.")
        log.info(s"Init the head with seed ${txId.hash}#${txIx.ix}, amount $treasuryAda ADA")

        // Request verification keys from known peers
        val knownVKeys = network.ask(_.reqVerificationKeys())
        log.info(s"knownVKeys: $knownVKeys")

        // ReqInit
        val seedOutput = UtxoIdL1(txId, txIx)
        val treasuryCoins = treasuryAda * 1_000_000
        val reqInit = ReqInit(wallet.ask(_.getWalletId), otherHeadPeers, seedOutput, treasuryCoins)
        val initTxId = network.ask(_.reqInit(reqInit))

        Right(initTxId)
    end initializeHead

    def deposit(r: DepositRequest): Either[DepositError, DepositResponse] =

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

        val depositTxRecipe =
            DepositTxRecipe(UtxoIdL1(r.txId, r.txIx), r.depositAmount, depositDatum)

        // Build a deposit transaction draft as a courtesy of Hydrozoa (no signature)
        val Right(depositTxDraft, index) =
            depositTxBuilder.ask(_.buildDepositTxDraft(depositTxRecipe))
        val depositTxHash = txHash(depositTxDraft)

        val serializedTx = serializeTxHex(depositTxDraft)
        log.info(s"Deposit tx: $serializedTx")
        log.info(s"Deposit tx hash: $depositTxHash, deposit output index: $index")

        // FIXME: in fact it's not a multisig tx, we have to revise tx dumping
        // TxDump.dumpMultisigTx(depositTxDraft)

        val req = ReqRefundLater(depositTxDraft, index)
        val refundTx = network.ask(_.reqRefundLater(req))
        val serializedRefundTx = serializeTxHex(refundTx)
        log.info(s"Refund tx: $serializedRefundTx")

        // TODO: temporarily we submit the deposit tx here on the node that handles the request
        // TODO: shall we add a combined function for signing?
        val Right(depositTxId) =
            cardano.ask(
              _.submit(
                addWitness(depositTxDraft, wallet.ask(_.createTxKeyWitness(depositTxDraft)))
              )
            )

        log.info(s"Deposit tx submitted: $depositTxId")
        Right(DepositResponse(refundTx, UtxoIdL1(depositTxHash, index)))
    end deposit

    def submitL1(hex: String): Either[String, TxId] =
        cardano.ask(_.submit(deserializeTxHex[L1](hex)))

    def submitL2(req: SubmitRequestL2): Either[String, TxId] =
        val event = req match
            case Transaction(tx) =>
                AdaSimpleLedger.mkTransactionEvent(tx)
            case Withdrawal(wd) =>
                AdaSimpleLedger.mkWithdrawalEvent(wd)

        network.tell(_.reqEventL2(ReqEventL2(event)))
        Right(event.getEventId)
    end submitL2

    /** Manually triggers next block creation procedure. This was used for model-based testing, and
      * we will need to get it back.
      * @param nextBlockFinal
      * @return
      */
    def handleNextBlock(
        nextBlockFinal: Boolean
    ): Either[String, (BlockRecord, UtxosSet, UtxosSet)] =
        ???

    def stateL2(): StateL2Response =
        nodeState.ask(_.mbInitializedOn) match // FIXME: slight abuse
            case None => List.empty
            case Some(_) =>
                val currentPhase = nodeState.ask(s => s.reader.currentPhase)
                currentPhase match
                    case Open => nodeState.ask(_.head.openPhase(_.stateL2.getState)).toList
                    case _    => List.empty
    end stateL2

    def tryFinalize(): Either[String, String] =
        nodeState.ask(_.mbInitializedOn) match // FIXME: slight abuse
            case None => Left("No head was found.")
            case Some(_) =>
                nodeState.ask(_.head.currentPhase) match
                    case Open =>
                        nodeState.tell(_.head.openPhase(_.requestFinalization()))
                        log.info("Head finalization was successfully requested")
                        Right("Head finalization request succeeded.")
                    case other =>
                        Left(s"Head is in the wrong phase: $other")
end Node
