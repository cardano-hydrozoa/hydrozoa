package hydrozoa.l2.block

import hydrozoa.*
import hydrozoa.infra.CryptoHash.H32
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.block.MempoolEventTypeL2.{MempoolTransaction, MempoolWithdrawal}
import hydrozoa.l2.event.{L2Event, L2GenesisEvent, L2TransactionEvent, L2WithdrawalEvent}
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

/** We don't add genesis events to blocks, since they can't be invalid and because they can be
  * calculated from `depositsAbsorbed`.
  */
enum MempoolEventTypeL2:
    case MempoolTransaction
    case MempoolWithdrawal

type UtxoSetL2 = Utxos

opaque type RH32UtxoSetL2 = H32[UtxoSetL2]

object RH32UtxoSetL2:
    def dummy: RH32UtxoSetL2 = H32.hash(IArray()) // TODO: implement

/*
Block builder. Missing checks:
    - minor block should contain at least one confirmed tx
    - utxoActive should be set (but it's not always true)
    - minor should have versionMinor > 0
    - major/final should have versionMajor > 0

    TODO: add bulk with*
 */

sealed trait TBlockType
sealed trait TBlockMinor extends TBlockType
sealed trait TBlockMajor extends TBlockMinor
sealed trait TBlockFinal extends TBlockMajor

sealed trait TCheck
sealed trait TNone extends TCheck
sealed trait TSet extends TCheck with TNone

case class BlockBuilder[
    BlockType <: TBlockType,
    BlockNum <: TCheck,
    VersionMajor <: TCheck
] private (
    blockType: BlockTypeL2 = Minor,
    blockNum: Int = 0,
    timeCreation: PosixTime = timeCurrent,
    versionMajor: Int = 0,
    versionMinor: Int = 0,
    // FIXME: add type tags
    eventsValid: Set[(TxId, MempoolEventTypeL2)] = Set.empty,
    eventsInvalid: Set[(TxId, MempoolEventTypeL2)] = Set.empty,
    depositsAbsorbed: Set[OutputRef[L1]] = Set.empty,
    utxosActive: RH32UtxoSetL2 = RH32UtxoSetL2.dummy
) {
    def majorBlock(implicit
        ev: BlockType =:= TBlockMinor
    ): BlockBuilder[TBlockMajor, BlockNum, VersionMajor] =
        copy(blockType = Major, versionMinor = 0)

    def finalBlock(implicit
        ev: BlockType =:= TBlockMinor
    ): BlockBuilder[TBlockFinal, BlockNum, VersionMajor] =
        copy(blockType = Final, versionMinor = 0)

    def blockNum(blockNum: Int)(implicit
        ev: BlockNum =:= TNone
    ): BlockBuilder[BlockType, TSet, VersionMajor] =
        copy(blockNum = blockNum)

    def timeCreation(timeCreation: PosixTime): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(timeCreation = timeCreation)

    def versionMajor(versionMajor: Int)(implicit
        ev: VersionMajor =:= TNone
    ): BlockBuilder[BlockType, BlockNum, TSet] =
        copy(versionMajor = versionMajor)

    def versionMinor(versionMinor: Int)(implicit
        ev: BlockType =:= TBlockMinor
    ): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(versionMinor = versionMinor)

    def withTransaction(txId: TxId): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(eventsValid = eventsValid.+((txId, MempoolTransaction)))

    def withWithdrawal(txId: TxId)(implicit
        ev: BlockType <:< TBlockMajor
    ): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(eventsValid = eventsValid.+((txId, MempoolWithdrawal)))

    def withInvalidEvent(
        txId: TxId,
        eventType: MempoolEventTypeL2
    ): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(eventsInvalid = eventsInvalid.+((txId, eventType)))

    def withDeposit(d: OutputRef[L1])(implicit
        ev: BlockType =:= TBlockMajor
    ): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(depositsAbsorbed = depositsAbsorbed.+(d))

    def utxosActive(utxosActive: RH32UtxoSetL2): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(utxosActive = utxosActive)

    def apply(
        foo: BlockBuilder[BlockType, BlockNum, VersionMajor] => BlockBuilder[
          BlockType,
          BlockNum,
          VersionMajor
        ]
    ): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        foo(this)

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

object BlockBuilder {
    def apply(): BlockBuilder[TBlockMinor, TNone, TNone] =
        BlockBuilder[TBlockMinor, TNone, TNone]()
}
