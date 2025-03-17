package hydrozoa.l2.block

import hydrozoa.*
import hydrozoa.l1.multisig.state.{DepositTag, DepositUtxos}
import hydrozoa.l2.block.MempoolEventTypeL2.{MempoolTransaction, MempoolWithdrawal}
import hydrozoa.l2.event.{L2NonGenesisEvent, L2TransactionEvent, L2WithdrawalEvent}
import hydrozoa.l2.ledger.*
import hydrozoa.l2.ledger.state.{Utxos, UtxosDiff, UtxosDiffMutable}
import hydrozoa.node.server.TxDump

import scala.collection.mutable

/** "Pure" function that produces an L2 block along with sets of added and withdrawn utxos.
  *
  * @param stateL2
  *   clone of the L2 ledger FIXME: what's the best way to do that?
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
    stateL2: AdaSimpleLedger[TBlockProduction],
    poolEvents: Seq[L2NonGenesisEvent],
    depositsPending: DepositUtxos,
    prevHeader: BlockHeader,
    timeCreation: PosixTime,
    finalizing: Boolean
): Option[(Block, Utxos, UtxosDiff, UtxosDiff, Option[(TxId, SimpleGenesis)])] =

    // 1. Initialize the variables and arguments.
    // (a) Let block be a mutable variable initialized to an empty BlockL2
    // instead on block we use mutable parts and finalize the block
    // at the end using the block builder
    val txValid, wdValid: mutable.Set[TxId] = mutable.Set.empty
    val eventsInvalid: mutable.Set[(TxId, MempoolEventTypeL2)] = mutable.Set.empty
    var depositsAbsorbed: Set[OutputRef[L1]] = Set.empty

    // (c) Let previousMajorBlock be the latest major block in blocksConfirmedL2
    // val previousMajorBlock = state.asOpen(_.l2LastMajor)

    // (e) Let utxosAdded be a mutable variable initialized to an empty UtxoSetL2
    // (f) Let utxosWithdrawn be a mutable variable initialized to an empty UtxoSetL2
    val utxosAdded, utxosWithdrawn: UtxosDiffMutable = mutable.Set()

    // 3. For each non-genesis L2 event...
    poolEvents.foreach {
        case tx: L2TransactionEvent =>
            stateL2.submit(mkL2T(tx.simpleTransaction)) match
                case Right(txId, mbCardanoTx, _) =>
                    // FIXME: move out
                    mbCardanoTx.foreach(tx => TxDump.dumpTx(tx))
                    txValid.add(txId)
                case Left(txId, _err) => eventsInvalid.add(txId, MempoolTransaction)
        case wd: L2WithdrawalEvent =>
            stateL2.submit(mkL2W(wd.simpleWithdrawal)) match
                case Right(txId, mbCardanoTx, utxosDiff) =>
                    wdValid.add(txId)
                    utxosWithdrawn.addAll(utxosDiff)
                // FIXME: move out
                // TODO: it's not necessary now, settlement txs do the job
                // mbCardanoTx.foreach(tx => os.write.append(txDump, "\n" + serializeTxHex(tx)))
                case Left(txId, _err) =>
                    eventsInvalid.add(txId, MempoolWithdrawal)
    }

    // 4. If finalizing is False...
    val mbGenesis = if !finalizing then
        // TODO: check deposits timing
        val eligibleDeposits: DepositUtxos =
            UtxoSet[L1, DepositTag](depositsPending.map.filter(_ => true))
        if eligibleDeposits.map.isEmpty then None
        else
            val genesis: SimpleGenesis = SimpleGenesis.apply(eligibleDeposits)
            stateL2.submit(mkL2G(genesis)) match
                case Right(txId, mbCardanoTx, utxos) =>
                    utxosAdded.addAll(utxos)
                    // output refs only
                    depositsAbsorbed = eligibleDeposits.map.keySet.toSet
                    // FIXME: move out
                    mbCardanoTx.foreach(tx => TxDump.dumpTx(tx))
                    Some(txId, genesis)
                case Left(_, _) => ??? // unreachable
    else None

    // 5. If finalizing is True...
    if (finalizing)
        utxosWithdrawn.addAll(stateL2.flush)

    // 6. Set block.blockType...
    val multisigRegimeKeepAlive = false // TODO: implement

    // No block if it's empty and keep-alive is not needed.
    if (poolEvents.isEmpty && depositsAbsorbed.isEmpty && !multisigRegimeKeepAlive)
        return None

    // Build the block
    val blockBuilder = BlockBuilder()
        .timeCreation(timeCreation)
        .blockNum(prevHeader.blockNum + 1)
        .utxosActive(RH32UtxoSetL2.dummy) // TODO: calculate Merkle root hash
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

    Some(block, stateL2.activeState, utxosAdded.toSet, utxosWithdrawn.toSet, mbGenesis)
