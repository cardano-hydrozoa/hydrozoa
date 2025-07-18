package hydrozoa.l2.block

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.infra.{Piper, encodeHex}
import hydrozoa.l1.multisig.state.DepositUtxos
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.consensus.network.{HeadPeerNetwork, ReqFinal, ReqMajor, ReqMinor}
import hydrozoa.l2.ledger.*
import hydrozoa.l2.ledger.L2EventLabel.{L2EventTransactionLabel, L2EventWithdrawalLabel}
import SimpleL2Ledger.SimpleL2LedgerClass
import ox.channels.ActorRef
import ox.sleep
import scalus.ledger.api.v3

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.language.strictEquality

class BlockProducer:

    private val log = Logger(getClass)

    private var networkRef: ActorRef[HeadPeerNetwork] = _

    def setNetworkRef(networkRef: ActorRef[HeadPeerNetwork]): Unit =
        this.networkRef = networkRef

    def produceBlock(
        l2Ledger: SimpleL2LedgerClass,
        poolEvents: Seq[L2Event],
        depositsPending: DepositUtxos,
        prevHeader: BlockHeader,
        timeCreation: PosixTime,
        finalizing: Boolean
    ): Either[
      String,
      (Block, Map[v3.TxOutRef, v3.TxOut], UtxoSetL2, UtxoSetL2, Option[(TxId, L2Genesis)])
    ] =

        // TODO: move to the block producer?
        val poolEventsSorted = poolEvents.sortBy(_.getEventId.hash)

        log.info(s"Pool events for block production: ${poolEvents.map(_.getEventId.hash)}")
        log.info(
          s"Pool events for block production (sorted): ${poolEventsSorted.map(_.getEventId.hash)}"
        )

        BlockProducer.createBlock(
          l2Ledger,
          poolEventsSorted,
          depositsPending,
          prevHeader,
          timeCreation,
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
      *   timestamp fot the block
      * @param finalizing
      *   finalization flag
      * @return
      *   Immutable block, set of utxos added, set of utxos withdrawn and optional genesis event.
      *   Returns None if a block can't be produced at the moment, i.e. no event in the pool, no
      *   deposits to absorb, and multisig regime keep-alive is not yet needed.
      */
    def createBlock(
        l2Ledger: SimpleL2LedgerClass,
        poolEvents: Seq[L2Event],
        depositsPending: DepositUtxos,
        prevHeader: BlockHeader,
        timeCreation: PosixTime,
        finalizing: Boolean
    ): Option[
      (Block, Map[v3.TxOutRef, v3.TxOut], UtxoSetL2, UtxoSetL2, Option[(TxId, L2Genesis)])
    ] =

        // 1. Initialize the variables and arguments.
        // (a) Let block be a mutable variable initialized to an empty BlockL2
        // instead on block we use mutable parts and finalize the block
        // at the end using the block builder
        val txValid, wdValid: mutable.Set[TxId] = mutable.Set.empty
        val eventsInvalid: mutable.Set[(TxId, L2EventLabel)] = mutable.Set.empty
        var depositsAbsorbed: Seq[UtxoId[L1]] = Seq.empty

        // (c) Let previousMajorBlock be the latest major block in blocksConfirmedL2
        // val previousMajorBlock = state.asOpen(_.l2LastMajor)

        // FIXME: seems we can remove `utxosAdded` if we have `Option[(TxId, SimpleGenesis)]`
        // (e) Let utxosAdded be a mutable variable initialized to an empty UtxoSetL2
        // (f) Let utxosWithdrawn be a mutable variable initialized to an empty UtxoSetL2
        type UtxosDiffMutable = mutable.Set[(UtxoIdL2, Output[L2])]
        val utxosAdded, utxosWithdrawn: UtxosDiffMutable = mutable.Set()

        // 3. For each non-genesis L2 event...
        poolEvents.foreach {
            case tx: L2EventTransaction =>
                l2Ledger.toLedgerTransaction(tx.transaction) |> l2Ledger.submit match
                    case Right(txId, _) => txValid.add(txId)
                    case Left(txId, err) =>
                        log.debug(s"Transaction can't be submitted: $err")
                        eventsInvalid.add(txId, L2EventTransactionLabel)
            case wd: L2EventWithdrawal =>
                l2Ledger.toLedgerTransaction(wd.withdrawal) |> l2Ledger.submit match
                    case Right(txId, (_, utxosDiff)) =>
                        wdValid.add(txId)
                        utxosWithdrawn.addAll(utxosDiff.utxoMap)
                    case Left(txId, err) =>
                        log.debug(s"Withdrawal can't be submitted: $err")
                        eventsInvalid.add(txId, L2EventWithdrawalLabel)
        }

        // 4. If finalizing is False...
        val mbGenesis = if !finalizing then
            // TODO: check deposits timing
            val depositsEligible: DepositUtxos =
                TaggedUtxoSet.apply(depositsPending.unTag.utxoMap.filter(_ => true))
            if depositsEligible.unTag.utxoMap.isEmpty then None
            else
                val depositsSorted = depositsEligible.unTag.utxoMap.toList.sortWith((a, b) =>
                    a._1._1.hash.compareTo(b._1._1.hash) < 0
                )
                val genesis: L2Genesis = L2Genesis.apply(depositsSorted)
                val genesisHash = calculateGenesisHash(genesis)
                val genesisUtxos = mkGenesisOutputs(genesis, genesisHash)
                l2Ledger.addGenesisUtxos(genesisUtxos)
                utxosAdded.addAll(genesisUtxos.utxoMap.toSet)
                depositsAbsorbed = depositsSorted.map(_._1)
                Some(genesisHash, genesis)
        else None

        // 5. If finalizing is True...
        if (finalizing)
            utxosWithdrawn.addAll(l2Ledger.flushAndGetState.utxoMap)

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
            .utxosActive(encodeHex(l2Ledger.getUtxosActiveCommitment))
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
          l2Ledger.getUtxosActive,
          UtxoSet[L2](utxosAdded.toMap),
          UtxoSet[L2](utxosWithdrawn.toMap),
          mbGenesis
        )
