package hydrozoa.l2.block

import hydrozoa.*
import hydrozoa.infra.CryptoHash.H32
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.block.EventType.{Genesis, Transaction, Withdrawal}
import hydrozoa.l2.block.MempoolEventTypeL2.{MempoolTransaction, MempoolWithdrawal}
import hydrozoa.l2.event.{L2Event, L2Genesis, L2Transaction_, L2Withdrawal_}
import hydrozoa.l2.ledger.state.Utxos

import scala.collection.mutable

case class Block(
    blockHeader: BlockHeader,
    blockBody: BlockBody
)

val zeroBlock =
    Block(BlockHeader(0, Major, timeCurrent, 0, 0, RH32UtxoSetL2.dummy), BlockBody.empty)

case class BlockHeader(
    blockNum: Int,
    blockType: BlockTypeL2,
    timeCreation: PosixTime,
    versionMajor: Int,
    versionMinor: Int,
    utxosActive: RH32UtxoSetL2
)

enum BlockTypeL2:
    case Minor
    case Major
    case Final

case class BlockBody(
    eventsValid: Seq[(TxId, MempoolEventTypeL2)],
    eventsInvalid: Seq[(TxId, MempoolEventTypeL2)],
    depositsAbsorbed: Seq[OutputRef[L1]]
)

object BlockBody:
    def empty: BlockBody = BlockBody(Seq.empty, Seq.empty, Seq.empty)

enum EventType:
    case Genesis
    case Transaction
    case Withdrawal

opaque type RH32UtxoSetL2 = H32[UtxoSetL2]

object RH32UtxoSetL2:
    def dummy: RH32UtxoSetL2 = H32.hash(IArray())

type UtxoSetL2 = Utxos

sealed trait TBlockType
sealed trait TBlockMinor extends TBlockType
sealed trait TBlockMajor extends TBlockMinor
sealed trait TBlockFinal extends TBlockMajor

sealed trait TCheck
sealed trait TNone extends TCheck
sealed trait TSet extends TCheck with TNone

/*
Missing checks:
 - minor block should contain at least one confirmed tx
 */

case class SafeBlockBuilder[
    BlockType <: TBlockType,
    BlockNum <: TCheck,
    VersionMajor <: TCheck
] private (
    blockType: BlockTypeL2 = Minor,
    blockNum: Int = 0,
    timeCreation: PosixTime = timeCurrent,
    versionMajor: Int = 0,
    versionMinor: Int = 0,
    eventsValid: Set[(TxId, MempoolEventTypeL2)] = Set.empty,
    eventsInvalid: Set[(TxId, MempoolEventTypeL2)] = Set.empty,
    depositsAbsorbed: Set[OutputRef[L1]] = Set.empty,
    utxosActive: RH32UtxoSetL2 = RH32UtxoSetL2.dummy
) {
    def majorBlock(implicit
        ev: BlockType =:= TBlockMinor
    ): SafeBlockBuilder[TBlockMajor, BlockNum, VersionMajor] =
        copy(blockType = Major, versionMinor = 0)

    def finalBlock(implicit
        ev: BlockType =:= TBlockMinor
    ): SafeBlockBuilder[TBlockFinal, BlockNum, VersionMajor] =
        copy(blockType = Final, versionMinor = 0)

    def blockNum(blockNum: Int)(implicit
        ev: BlockNum =:= TNone
    ): SafeBlockBuilder[BlockType, TSet, VersionMajor] =
        copy(blockNum = blockNum)

    def timeCreation(timeCreation: PosixTime): SafeBlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(timeCreation = timeCreation)

    def versionMajor(versionMajor: Int)(implicit
        ev: VersionMajor =:= TNone
    ): SafeBlockBuilder[BlockType, BlockNum, TSet] =
        copy(versionMajor = versionMajor)

    def versionMinor(versionMinor: Int)(implicit
        ev: BlockType =:= TBlockMinor
    ): SafeBlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(versionMinor = versionMinor)

    def withTransaction(txId: TxId): SafeBlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(eventsValid = eventsValid.+((txId, MempoolTransaction)))

    def withWithdrawal(txId: TxId)(implicit
        ev: BlockType <:< TBlockMajor
    ): SafeBlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(eventsValid = eventsValid.+((txId, MempoolWithdrawal)))

    def withInvalidEvent(
        txId: TxId,
        eventType: MempoolEventTypeL2
    ): SafeBlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(eventsValid = eventsValid.+((txId, eventType)))

    def withDeposits(ds: Set[OutputRef[L1]])(implicit
        ev: BlockType =:= TBlockMajor
    ): SafeBlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(depositsAbsorbed = depositsAbsorbed.++(ds))

    def build(implicit
        blockNumEv: BlockNum =:= TSet,
        versionMajorEv: VersionMajor =:= TSet
    ): Block =
        Block(
          BlockHeader(
            blockNum,
            blockType,
            timeCreation,
            versionMajor,
            versionMinor,
            utxosActive
          ),
          BlockBody(this.eventsValid.toSeq, this.eventsInvalid.toSeq, this.depositsAbsorbed.toSeq)
        )
}

object SafeBlockBuilder {
    def apply(): SafeBlockBuilder[TBlockMinor, TNone, TNone] =
        SafeBlockBuilder[TBlockMinor, TNone, TNone]()
}

enum MempoolEventTypeL2:
    case MempoolTransaction
    case MempoolWithdrawal

//sealed trait MempoolEventTypeL2
//sealed trait MempoolTransaction extends MempoolEventTypeL2
//sealed trait MempoolWithdrawal extends MempoolEventTypeL2

class BlockBuilder:

    private var timeCreation: Option[PosixTime] = None
    private val eventsValid: mutable.Buffer[(TxId, EventType)] = mutable.Buffer()
    private val eventsInvalid: mutable.Buffer[(TxId, EventType)] = mutable.Buffer()
    private var depositsAbsorbed: Set[OutputRef[L1]] = Set.empty
    private var blockType: Option[BlockTypeL2] = None
    private var blockNum: Int = 0
    private var utxosActive: RH32UtxoSetL2 = RH32UtxoSetL2.dummy
    private var versionMajor, versionMinor: Int = 0

    def withTimeCreation(timeCurrent: PosixTime) =
        this.timeCreation = Some(timeCurrent)
        this

    def withConfirmedEvent(txId: TxId, l2Event: L2Event) =
        eventsValid.appendAll(mutable.Buffer((txId, eventTypeTag(l2Event)))) // FIXME:
        this

    def withInvalidEvent(txId: TxId, l2Event: L2Event) =
        eventsInvalid.appendAll(mutable.Buffer((txId, eventTypeTag(l2Event)))) // FIXME:
        this

    def withDeposits(ds: Set[OutputRef[L1]]) =
        this.depositsAbsorbed = ds
        this

    def withBlockType(ty: BlockTypeL2) =
        this.blockType = Some(ty)
        this

    def withBlockNum(num: Int) =
        this.blockNum = num
        this

    def withUtxosActive(hash: RH32UtxoSetL2) =
        this.utxosActive = hash
        this

    def withPreviousVersions(major: Int, minor: Int) =
        blockType match
            case Some(Minor) =>
                versionMajor = major
                versionMinor = minor + 1
            case Some(_) =>
                versionMajor = major + 1
                versionMinor = 0
            case _ => throw IllegalStateException("block type is not known")

    def build: Block =
//        Block(
//          BlockHeader(
//            blockNum,
//            blockType.get,
//            timeCreation.get,
//            versionMajor,
//            versionMinor,
//            utxosActive
//          ),
//          BlockBody(this.eventsValid.toSeq, this.eventsInvalid.toSeq, this.depositsAbsorbed.toSeq)
//        )
        ???

def eventTypeTag(e: L2Event): EventType = e match
    case _: L2Genesis      => Genesis
    case _: L2Transaction_ => Transaction
    case _: L2Withdrawal_  => Withdrawal
