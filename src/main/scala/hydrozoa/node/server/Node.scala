package hydrozoa.node.server

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.*
import hydrozoa.l1.CardanoL1
import hydrozoa.l1.multisig.state.DepositDatum
import hydrozoa.l1.multisig.tx.*
import hydrozoa.l1.multisig.tx.deposit.{DepositTxBuilder, DepositTxRecipe, ttlMargin}
import hydrozoa.l2.block.Block
import hydrozoa.l2.consensus.network.*
import hydrozoa.l2.ledger.L2EventGenesis
import hydrozoa.node.rest.SubmitRequestL2.{Transaction, Withdrawal}
import hydrozoa.node.rest.{StateL2Response, SubmitRequestL2}
import hydrozoa.node.server.DepositError
import hydrozoa.node.state.*
import hydrozoa.node.state.HeadPhase.{Finalizing, Open}
import ox.channels.ActorRef
import ox.resilience.{RetryConfig, retryEither}
import scalus.cardano.ledger.{LedgerToPlutusTranslation, Slot, TransactionHash}
import scalus.prelude.asScalus

import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions
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
        txId: TransactionHash,
        txIx: TxIx
    ): Either[InitializationError, TransactionHash] =

        log.info(s"Init the head with seed ${txId.toHex}#${txIx.untagged}, amount $treasuryAda ADA")

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
            val seedOutput = UtxoId[L1](txId, txIx)
            // TODO: unify - somewhere we ask for Ada, somewhere for Lovelace
            val treasuryCoins = treasuryAda * 1_000_000
            val reqInit =
                ReqInit(wallet.ask(_.getWalletId), otherHeadPeers, seedOutput, treasuryCoins)
            val initTxId = network.ask(_.reqInit(reqInit))
            initTxId
        }).toEither.left.map(_.getMessage)

    end initializeHead

    /** Handle a combined deposit request, i.e. build a deposit tx for the user and the post-dated
      * refund. Arguably should be split up into two separate things.
      *
      * @param r
      *   the user's deposit request
      * @return
      *   An error or a pair (post-dates refund tx, deposit utxo id)
      */
    def deposit(r: DepositRequest): Either[DepositError, DepositResponse] =

        /*

        How deposit timing works
        ------------------------

        Deposit eligibility
        -------------------

        Hydrozoa has to be able to determine whether a deposit is eligible for absorption or not.

        The main source of data for deposit timings for a particular deposit is the deposit tx itself:

        1. The _validity upper bound_ is mandatory and should be a bounded slot value.
           This value is converted to POSIX time using genesis/slot configuration.
           If not set, a deposit is not going to be absorbed.
           The use of the tx's validity interval and not the block's time protects from possible
           volatility of that information in the case of forks.

        2. The deposit output should have a datum of DepositDatum type.
           Its `deadline` field should contain POSIX time that represents the time by which
           a user wants to have the deposit absorbed into L2 ot refunded back to L1.

        These two things form a window we call `depositIntervalOuter`:

        depositIntervalOuter :=
              [ depositTx.upperBound |> toTime
              , deposit.datum.deadline
              )

        Then two parameters from Hydrozoa consensus parameters known as _deposit margins_ are used to
        narrow this window by cutting off its boundaries:

         * `depositMarginMaturity` (seconds, used for the lower bound)
                A newly created deposit should be aged during this period before being absorbed.
                This is done to lower the likelihood of a deposit being rolled back.

         * `depositMarginExpiry` (seconds, used for the upper bound)
                Controls a safe distance between two potential txs that can spend a deposit utxo to prevent contention:
                    - settlement tx that absorbs a deposit
                    - post-dated refund that returns a deposit

        Or more formally:

        depositMargins :=
          [ + depositMarginMaturity
          , - depositMarginExpiry
          )

        This way we calculate `depositIntervalInner` that defines the _actual window_:

        depositIntervalInner  := depositIntervalOuter + depositMargins

        Or visually:

             |----------------------------------- depositIntervalOuter -----------------------------------|
             ^ depositTx.upperBound                                                                       ^ deposit.datum.deadline

        -----|==============================|*******************************|=============================|-------

             |---`depositMarginMaturity` ---|---- `depositIntervalInner` ---|--- `depositMarginExpiry` ---|

        Deposit is eligible for absorption if the time of a major block which tries to absorb a deposit is in the
        `depositIntervalInner` interval.


        Post-dated refund tx
        --------------------

        Post-dated refund txs should get `deadline` as their lower validity interval.

        Building deposit txs
        --------------------

        If a depositor uses Hydrozoa to build a deposit transaction, we ask her to specify `deadline` that goes to the
        datum of deposit utxo. Hydrozoa node determines the upper bound of the validity interval for deposit tx automatically
        based on the current time and `hydrozoa.l1.multisig.tx.deposit.ttlMargin` constant/parameter.

        If the user sets the `deadline` duration too low and the head fails to absorb the deposit within the window,
        ...this is not implemented -> the deposit will be refunded immediately (if L2 consensus still holds when this is determined)
        ... this is implemented -> or via the post-dated refund tx.

        The user only loses time and tx fees. Of course, the frontend UI could also have additional guardrails of its own
        to protect the user's time and tx fees, but the hydrozoa node doesn't need to know or care about these guardrails.

         */

        log.info(s"Deposit request: $r")

        // We don't check deadline correctness.

        // FIXME: now we don't need that
        // TODO: Check deadline
//        val (maturity, expiry) = nodeState.ask(_.head.openPhase(_.depositTimingParams))
//        val latestBlockTime = cardano.ask(_.lastBlockTime)
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
        val depositDatum = DepositDatum(
          LedgerToPlutusTranslation.getAddress(r.address),
          r.datum.asScalus,
          r.deadline,
          LedgerToPlutusTranslation.getAddress(r.refundAddress),
          r.datum.asScalus
        )

        // the upper bound AKA ttl for deposit tx
        val ttl = Slot(cardano.ask(_.lastBlockSlot).slot + ttlMargin)

        val depositTxRecipe =
            DepositTxRecipe(UtxoId[L1](r.txId, r.txIx), r.depositAmount, depositDatum, ttl)

        // Build a deposit transaction draft as a courtesy of Hydrozoa (no signature)
        val Right(depositTxDraft, index) =
            depositTxBuilder.ask(_.buildDepositTxDraft(depositTxRecipe))
        val depositTxHash = depositTxDraft.id

        val serializedTx = serializeTxHex(depositTxDraft)
        log.info(s"Deposit tx: $serializedTx")
        log.info(s"Deposit tx hash: $depositTxHash, deposit output index: $index")

        // FIXME: in fact it's not a multisig tx, we have to revise tx dumping
        // TxDump.dumpMultisigTx(depositTxDraft)

        val req = ReqRefundLater(depositTxDraft, TxIx(index))
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
        Right(DepositResponse(refundTx, UtxoId[L1](depositTxHash, index)))
    end deposit

    def submitL1(hex: String): Either[String, TransactionHash] =
        cardano.ask(_.submit(deserializeTxHex[L1](hex)))

    def awaitTxL1(txId: TransactionHash): Option[TxL1] = cardano.ask(_.awaitTx(txId))

    def submitL2(req: SubmitRequestL2): Either[String, TransactionHash] =
        req match
            case Transaction(tx) =>
                network.tell(_.reqEventL2(ReqEventL2(tx)))
                Right((tx.getEventId))

            case Withdrawal(wd) =>
                network.tell(_.reqEventL2(ReqEventL2(wd)))
                Right((wd.getEventId))

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
        nextBlockFinal: Boolean
    ): Either[String, (BlockRecord, Option[(TransactionHash, L2EventGenesis)])] =
        assert(
          !nodeState.ask(_.autonomousBlockProduction),
          "Autonomous block production should be turned off to use this function"
        )

        log.info(
          s"Calling tryProduceBlock in lockstep, nextBlockFinal=$nextBlockFinal..."
        )

        val errorOrBlock = nodeState.ask(_.head.currentPhase) match
            case Open =>
                nodeState.ask(
                  _.head.openPhase(
                    _.tryProduceBlock(nextBlockFinal, true)
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
                val effects = retryEither(RetryConfig.delay(30, 100.millis)) {
                    log.info("Trying to obtain block results...")
                    nodeState
                        .ask(_.head.getBlockRecord(block))
                        .toRight(
                          s"Effects for block ${block.blockHeader.blockNum} have not been found after 30 secs of waiting"
                        )
                }

                effects match
                    case Left(err)                     => Left(err)
                    case Right(blockRecord, mbGenesis) => Right(blockRecord, mbGenesis)

    def runDispute() =
        log.warn("Running test dispute routine...")
        nodeState.ask(_.head.openPhase(_.runTestDispute()))

    def stateL2(): StateL2Response =
        nodeState.ask(_.mbInitializedOn) match // FIXME: slight abuse
            case None => List.empty
            case Some(_) =>
                val currentPhase = nodeState.ask(s => s.reader.currentPhase)
                currentPhase match
                    case Open => {
                        val stateL2 = nodeState
                            .ask(s => s.head.openPhase(os => os.stateL2))
                            .toList
                        stateL2
                            .map((utxoId, output) => utxoId -> OutputNoTokens(output))

                    }
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
