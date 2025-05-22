package hydrozoa.l2.block

import com.typesafe.scalalogging.Logger
import hydrozoa.*
import hydrozoa.l1.multisig.state.{DepositTag, DepositUtxos}
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.consensus.network.{HeadPeerNetwork, ReqFinal, ReqMajor, ReqMinor}
import hydrozoa.l2.ledger.*
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel.{
    TransactionL2EventLabel,
    WithdrawalL2EventLabel
}
import hydrozoa.l2.ledger.state.UtxosSetOpaque
import ox.channels.ActorRef
import ox.sleep

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.language.strictEquality

// TODO: unify in terms of abstract ledger and types

class BlockProducer:

    private val log = Logger(getClass)

    private var networkRef: ActorRef[HeadPeerNetwork] = _

    def setNetworkRef(networkRef: ActorRef[HeadPeerNetwork]): Unit =
        this.networkRef = networkRef

    def produceBlock(
        stateL2: AdaSimpleLedger[TBlockProduction],
        poolEvents: Seq[NonGenesisL2],
        depositsPending: DepositUtxos,
        prevHeader: BlockHeader,
        timeCreation: PosixTime,
        finalizing: Boolean
    ): Either[String, (Block, UtxosSetOpaque, UtxosSet, UtxosSet, Option[(TxId, SimpleGenesis)])] =

        val sortedPoolEvents = poolEvents
            .sortWith((e1, e2) => e1.getEventId.hash.compareTo(e2.getEventId.hash) == -1 )

        log.info(s"Pool events for block production: $poolEvents")
        log.info(s"Pool events for block production (sorted): $sortedPoolEvents")

        createBlock(
          stateL2,
          sortedPoolEvents,
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
                val msg = "Block production procedure was unable to create a block"
                log.warn(msg)
                Left(msg)

/** "Pure" function that produces an L2 block along with sets of added and withdrawn utxos.
  *
  * @param stateL2
  *   cloned L2 ledger for block building
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
    poolEvents: Seq[NonGenesisL2],
    depositsPending: DepositUtxos,
    prevHeader: BlockHeader,
    timeCreation: PosixTime,
    finalizing: Boolean
): Option[(Block, UtxosSetOpaque, UtxosSet, UtxosSet, Option[(TxId, SimpleGenesis)])] =

    // 1. Initialize the variables and arguments.
    // (a) Let block be a mutable variable initialized to an empty BlockL2
    // instead on block we use mutable parts and finalize the block
    // at the end using the block builder
    val txValid, wdValid: mutable.Set[TxId] = mutable.Set.empty
    val eventsInvalid: mutable.Set[(TxId, NonGenesisL2EventLabel)] = mutable.Set.empty
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
        case tx: TransactionL2 =>
            stateL2.submit(tx) match
                case Right(txId, _)   => txValid.add(txId)
                case Left(txId, _err) => eventsInvalid.add(txId, TransactionL2EventLabel)
        case wd: WithdrawalL2 =>
            stateL2.submit(wd) match
                case Right(txId, utxosDiff) =>
                    wdValid.add(txId)
                    utxosWithdrawn.addAll(utxosDiff)
                case Left(txId, _err) =>
                    eventsInvalid.add(txId, WithdrawalL2EventLabel)
    }

    // 4. If finalizing is False...
    val mbGenesis = if !finalizing then
        // TODO: check deposits timing
        val eligibleDeposits: DepositUtxos =
            UtxoSet[L1, DepositTag](depositsPending.map.filter(_ => true))
        if eligibleDeposits.map.isEmpty then None
        else
            val genesis: SimpleGenesis = SimpleGenesis.apply(eligibleDeposits)
            stateL2.submit(AdaSimpleLedger.mkGenesisEvent(genesis)) match
                case Right(txId, utxos) =>
                    utxosAdded.addAll(utxos)
                    depositsAbsorbed = eligibleDeposits.map.keySet.toList.sortWith((a, b) =>
                        a._1.hash.compareTo(b._1.hash) < 0
                    )
                    Some(txId, genesis)
                case Left(_, _) => ??? // unreachable, submit for deposits always succeeds
    else None

    // 5. If finalizing is True...
    if (finalizing)
        utxosWithdrawn.addAll(stateL2.flush)

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
//        .utxosActive(RH32UtxoSetL2.dummy) // TODO: calculate Merkle root hash
        .utxosActive(42) // TODO: calculate Merkle root hash
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

    Some(block, stateL2.getUtxosActive, utxosAdded.toSet, utxosWithdrawn.toSet, mbGenesis)
