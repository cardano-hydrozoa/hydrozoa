package hydrozoa.node.server

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.*
import hydrozoa.l1.multisig.tx.deposit.{DepositTxBuilder, DepositTxRecipe}
import hydrozoa.l2.block.Block
import hydrozoa.l2.consensus.network.*
import hydrozoa.l2.ledger.{L2Genesis, mkTransactionEvent, mkWithdrawalEvent}
import hydrozoa.node.rest.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.rest.{StateL2Response, SubmitRequestL2}
import hydrozoa.node.server.DepositError
import hydrozoa.node.state.*
import hydrozoa.node.state.HeadPhase.{Finalizing, Open}
import ox.channels.ActorRef
import ox.resilience.{RetryConfig, retry, retryEither}
import scalus.prelude.Option as SOption
import scalus.prelude.Option.asScalus

import scala.concurrent.duration.DurationInt
import scala.util.Try

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
        otherHeadPeers: Set[WalletId],
        treasuryAda: Long,
        txId: TxId,
        txIx: TxIx
    ): Either[InitializationError, TxId] =

        log.info(s"Init the head with seed ${txId.hash}#${txIx.ix}, amount $treasuryAda ADA")

        // TODO: explicit type for errors
        Try({
            // Check number of peers
            if (otherHeadPeers.isEmpty)
                val msg = "Solo node mode is not supported yet"
                log.error(msg)
                throw IllegalArgumentException(msg)

            // Check others are others indeed
            if (otherHeadPeers.contains(wallet.ask(_.getWalletId)))
                val msg = "Other head peers should NOT contain own peer"
                log.error(msg)
                throw IllegalArgumentException(msg)

            // TODO: we don't need keys of peers which are not going to form a head
            // Request verification keys from known peers
            val knownVKeys = network.ask(_.reqVerificationKeys())
            log.info(s"knownVKeys: $knownVKeys")

            // ReqInit
            val seedOutput = UtxoIdL1(txId, txIx)
            // TODO: unify - somewhere we ask for Ada, somewhere for Lovelace
            val treasuryCoins = treasuryAda * 1_000_000
            val reqInit =
                ReqInit(wallet.ask(_.getWalletId), otherHeadPeers, seedOutput, treasuryCoins)
            val initTxId = network.ask(_.reqInit(reqInit))
            initTxId
        }).toEither.left.map(_.getMessage)

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
          (r.datum.map(datumByteString)).asScalus,
          BigInt.apply(0), // deadline,
          decodeBech32AddressL1(r.refundAddress),
          (r.datum.map(datumByteString)).asScalus
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

    def awaitTxL1(txId: TxId): Option[TxL1] = cardano.ask(_.awaitTx(txId))

    def submitL2(req: SubmitRequestL2): Either[String, TxId] =
        val event = req match
            case Transaction(tx) => mkTransactionEvent(tx)
            case Withdrawal(wd)  => mkWithdrawalEvent(wd)

        network.tell(_.reqEventL2(ReqEventL2(event)))
        Right(event.getEventId)
    end submitL2

    /** Tries to make a block, and if it succeeds, tries to wait until consensus on the block is
      * done and effects are ready. Returns all that so it can be checked against a model.
      *
      * NB: This is used for testing only.
      *
      * NB: Not exposed within API.
      *
      * NB: requires autonomousBlockProduction = false
      * @param nextBlockFinal
      *   whether the next block should be final
      * @return
      */
    def produceNextBlockLockstep(
        nextBlockFinal: Boolean,
        quitConsensusImmediately: Boolean = false
    ): Either[String, (BlockRecord, Option[(TxId, L2Genesis)])] =

        assert(
          !nodeState.ask(_.autonomousBlockProduction),
          "Autonomous block production should be turned off to use this function"
        )

        log.info(
          s"Calling tryProduceBlock in lockstep, nextBlockFinal=$nextBlockFinal, quitConsensusImmediately=$quitConsensusImmediately..."
        )
        val errorOrBlock = nodeState.ask(_.head.currentPhase) match
            case Open =>
                nodeState.ask(
                  _.head.openPhase(
                    _.tryProduceBlock(nextBlockFinal, true, quitConsensusImmediately)
                  )
                ) match
                    case Left(err)    => Left(err)
                    case Right(block) => Right(block)
            case Finalizing =>
                nodeState.ask(_.head.finalizingPhase(_.tryProduceFinalBlock(true))) match
                    case Left(err)    => Left(err)
                    case Right(block) => Right(block)
            case other => Left(s"Node should be in Open or Finalizing pase, but it's in $other")

        errorOrBlock match
            case Left(err) => Left(err)
            case Right(block) =>
                val effects = retryEither(RetryConfig.delay(100, 100.millis)) {
                    nodeState
                        .ask(_.head.getBlockRecord(block))
                        .toRight(
                          s"Effects for block ${block.blockHeader.blockNum} have not bee found after 10 secs of waiting"
                        )
                }

                effects match
                    case Left(err)                     => Left(err)
                    case Right(blockRecord, mbGenesis) => Right(blockRecord, mbGenesis)

    def stateL2(): StateL2Response =
        nodeState.ask(_.mbInitializedOn) match // FIXME: slight abuse
            case None => List.empty
            case Some(_) =>
                val currentPhase = nodeState.ask(s => s.reader.currentPhase)
                currentPhase match
                    case Open =>
                        nodeState
                            .ask(_.head.openPhase(_.stateL2.getState))
                            .utxoMap
                            .toList
                            .map((utxoId, output) => utxoId -> OutputNoTokens.apply(output))
                    case _ => List.empty
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
