package hydrozoa.l2.block

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.encodeHex
import hydrozoa.l1.multisig.state.{DepositDatum, DepositUtxos}
import hydrozoa.l2.block.*
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.consensus.network.{HeadPeerNetwork, ReqFinal, ReqMajor, ReqMinor}
import hydrozoa.l2.ledger.*
import hydrozoa.l2.ledger.L2EventLabel.{
    L2EventGenesisLabel,
    L2EventTransactionLabel,
    L2EventWithdrawalLabel
}
import ox.channels.ActorRef
import ox.sleep
import scalus.cardano.ledger.TransactionHash
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.{Context, State}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.language.{implicitConversions, strictEquality}

class BlockProducer:

    private val log = Logger(getClass)

    private var networkRef: ActorRef[HeadPeerNetwork] = _

    def setNetworkRef(networkRef: ActorRef[HeadPeerNetwork]): Unit =
        this.networkRef = networkRef

    def produceBlock(
        /** l2Ledger contains the (immutable) context and the initial state prior to block
          * production
          */
        l2Ledger: (Context, State),
        poolEvents: Seq[L2Event],
        depositsPending: DepositUtxos,
        prevHeader: BlockHeader,
        timeCreation: PosixTime,
        maturityMargin: BigInt,
        expiryMargin: BigInt,
        finalizing: Boolean
    ): Either[
      String,
      (Block, UtxoSetL2, UtxoSetL2, UtxoSetL2, Option[(TransactionHash, L2EventGenesis)])
    ] =

        // TODO: move to the block producer?
        val poolEventsSorted = poolEvents.sortBy(_.getEventId)

        log.info(s"Pool events for block production: ${poolEvents.map(_.getEventId)}")
        log.info(
          s"Pool events for block production (sorted): ${poolEventsSorted.map(_.getEventId)}"
        )

        BlockProducer.createBlock(
          l2Ledger,
          poolEventsSorted,
          depositsPending,
          prevHeader,
          timeCreation,
          maturityMargin,
          expiryMargin,
          finalizing
        ) match
            case Some(some @ (block, _, _, _, _)) =>
                log.info(s"A new block was produced: $block")
                // FIXME: this is needed now so we can see deposits on all nodes for sure
                sleep(1.second)
                log.info(s"Starting consensus on block ${block.blockHeader.blockNum}")
                block.blockHeader.blockType match
                    case Minor => networkRef.tell(_.reqMinor(ReqMinor(block)))
                    case Major => networkRef.tell(_.reqMajor(ReqMajor(block)))
                    case Final => networkRef.tell(_.reqFinal(ReqFinal(block)))
                Right(some)
            case None =>
                val msg =
                    s"Block production procedure was unable to create a block number ${prevHeader.blockNum + 1}"
                log.warn(msg)
                Left(msg)

object BlockProducer:

    private val log = Logger(getClass)

    /** "Pure" function that produces an L2 block along with sets of added and withdrawn utxos.
      *
      * @param l2Ledger
      *   L2 ledger for block building
      * @param poolEvents
      *   pooled events
      * @param depositsPending
      *   deposits that can be absorbed in the block
      * @param prevHeader
      *   previsus block's header
      * @param timeCreation
      *   timestamp for the block
      * @param maturityMargin
      *   consensus parameter that defines the maturity margin for deposits
      * @param expiryMargin
      *   consensus parameter that defines the expiry margin for deposits
      * @param finalizing
      *   finalization flag
      * @return
      *   Immutable block, active UtxoSet, set of utxos added, set of utxos withdrawn and optional
      *   genesis event. Returns None if a block can't be produced at the moment, i.e. no event in
      *   the pool, no deposits to absorb, and multisig regime keep-alive is not yet needed.
      */
    def createBlock(
        l2Ledger: (Context, State),
        poolEvents: Seq[L2Event],
        depositsPending: DepositUtxos,
        prevHeader: BlockHeader,
        timeCreation: PosixTime,
        maturityMargin: BigInt,
        expiryMargin: BigInt,
        finalizing: Boolean
    ): Option[
      (Block, UtxoSet[L2], UtxoSet[L2], UtxoSet[L2], Option[(TransactionHash, L2EventGenesis)])
    ] =

        // 1. Initialize the variables and arguments.
        // (a) Let block be a mutable variable initialized to an empty BlockL2
        // instead on block we use mutable parts and finalize the block
        // at the end using the block builder
        val txValid, wdValid: mutable.Set[TransactionHash] = mutable.Set.empty
        val eventsInvalid: mutable.Set[(TransactionHash, L2EventLabel)] = mutable.Set.empty
        var depositsAbsorbed: Seq[UtxoIdL1] = Seq.empty

        // (c) Let previousMajorBlock be the latest major block in blocksConfirmedL2
        // val previousMajorBlock = state.asOpen(_.l2LastMajor)

        // FIXME: seems we can remove `utxosAdded` if we have `Option[(TxId, SimpleGenesis)]`
        // (e) Let utxosAdded be a mutable variable initialized to an empty UtxoSetL2
        // (f) Let utxosWithdrawn be a mutable variable initialized to an empty UtxoSetL2
        type UtxosDiffMutable = mutable.Set[(UtxoIdL2, OutputL2)]
        val utxosAdded, utxosWithdrawn: UtxosDiffMutable = mutable.Set()

        // We use a mutable state
        var state: State = l2Ledger._2
        // 3. For each non-genesis L2 event...
        poolEvents.foreach {
            case tx: L2EventTransaction =>
                HydrozoaL2Mutator.transit(l2Ledger._1, state, tx) match
                    case Right(newState) =>
                        txValid.add(tx.getEventId)
                        state = newState
                    case Left(err) =>
                        log.debug(s"Transaction can't be applied to STSL2: ${err}")
                        eventsInvalid.add(tx.getEventId, L2EventTransactionLabel)
            case wd: L2EventWithdrawal =>
                HydrozoaL2Mutator.transit(l2Ledger._1, state, wd) match
                    case Right(newState) =>
                        wdValid.add(wd.getEventId)
                        val utxosDiff: Set[(UtxoIdL2, OutputL2)] =
                            wd.transaction.body.value.inputs.foldLeft(Set.empty)((set, input) =>
                                set +
                                    // N.B.: partial here, but we assume that all of our L2 UTxOs are Babbage.
                                    ((
                                      UtxoIdL2(input),
                                      Output[L2](state.utxo(input).asInstanceOf[Babbage])
                                    ))
                            )
                        utxosWithdrawn.addAll(utxosDiff)
                        state = newState
                    case Left(err) =>
                        log.debug(s"Withdrawal can't be applied to STSL2: $err")
                        eventsInvalid.add(wd.getEventId, L2EventWithdrawalLabel)
        }

        // 4. If finalizing is False...
        val mbGenesis: Option[(TransactionHash, L2EventGenesis)] = if !finalizing then
            // Check pending deposits timing
            val depositsEligible: DepositUtxos = TaggedUtxoSet.apply(
              UtxoSet[L1](
                depositsPending.untagged.filter((_, depositUtxo) =>
                    depositUtxo.mbInlineDatumAs[DepositDatum] match
                        case None =>
                            log.warn("Deposit UTxO with no (correct) datum found. Ignoring.")
                            false
                        case Some(datum) =>
                            // TODO:
                            val depositTxTtl = 0
                            val depositDeadline = datum.deadline
                            // Eligibility condition
                            timeCreation > (depositTxTtl + maturityMargin)
                            && timeCreation < (depositDeadline - expiryMargin)
                )
              )
            )

            if depositsEligible.untagged.isEmpty then None
            else
                val depositsSorted = depositsEligible.untagged.toList
                    .sortWith((a, b) => a._1._1.toHex.compareTo(b._1._1.toHex) < 0)
                    .toSeq
                val genesis: L2EventGenesis = L2EventGenesis.apply(depositsSorted)
                val genesisHash = genesis.getEventId

                HydrozoaL2Mutator.transit(l2Ledger._1, state, genesis) match {
                    case Left(err) =>
                        log.debug(s"Genesis can't be applied to STSL2: ${err}")
                        eventsInvalid.add(genesis.getEventId, L2EventGenesisLabel)
                        None
                    case Right(newState) =>
                        state = newState
                        depositsAbsorbed = depositsSorted.map(_._1)
                        utxosAdded.addAll(genesis.resolvedL2UTxOs)
                        Some(genesisHash, genesis)

                }
        else None

        // 5. If finalizing is True...
        if (finalizing)
            utxosWithdrawn.addAll(state.utxo.unsafeAsL2)

        // 6. Set block.blockType...
        val multisigRegimeKeepAlive = false // TODO: implement

        // No block if it's empty and keep-alive is not needed.
        if (
          poolEvents.isEmpty
          && depositsAbsorbed.isEmpty
          && !finalizing
          && !multisigRegimeKeepAlive
        )
            return None

        // Build the block
        val blockBuilder = BlockBuilder()
            .timeCreation(timeCreation)
            .blockNum(prevHeader.blockNum + 1)
            .utxosActive(encodeHex(getUtxosActiveCommitment(state.utxo)))
            .apply(b => eventsInvalid.foldLeft(b)((b, e) => b.withInvalidEvent(e._1, e._2)))
            .apply(b => txValid.foldLeft(b)((b, txId) => b.withTransaction(txId)))

        def withdrawalsValid[A <: TBlockMajor, B <: TCheck, C <: TCheck](b: BlockBuilder[A, B, C]) =
            wdValid.foldLeft(b)((b, e) => b.withWithdrawal(e))

        val block =
            if finalizing then
                blockBuilder.finalBlock
                    .versionMajor(prevHeader.versionMajor + 1)
                    .apply(withdrawalsValid)
                    .build
            else if utxosAdded.nonEmpty || utxosWithdrawn.nonEmpty || multisigRegimeKeepAlive
            then
                blockBuilder.majorBlock
                    .versionMajor(prevHeader.versionMajor + 1)
                    .apply(withdrawalsValid)
                    .apply(b => depositsAbsorbed.foldLeft(b)((b, d) => b.withDeposit(d)))
                    .build
            else
                blockBuilder
                    .versionMajor(prevHeader.versionMajor)
                    .versionMinor(prevHeader.versionMinor + 1)
                    .build

        Some(
          block,
          UtxoSet[L2](state.utxo.unsafeAsL2),
          UtxoSet[L2](utxosAdded.toMap),
          UtxoSet[L2](utxosWithdrawn.toMap),
          mbGenesis
        )
