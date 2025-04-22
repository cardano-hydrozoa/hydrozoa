package hydrozoa.l2.block

import hydrozoa.*
import hydrozoa.l2.block.BlockTypeL2.{Final, Major, Minor}
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel
import hydrozoa.l2.ledger.event.NonGenesisL2EventLabel.{
    TransactionL2EventLabel,
    WithdrawalL2EventLabel
}
import hydrozoa.l2.merkle.RH32UtxoSetL2

case class Block(
    blockHeader: BlockHeader,
    blockBody: BlockBody
)

val zeroBlock =
    Block(BlockHeader(0, Major, timeCurrent, 0, 0, 42), BlockBody.empty)

case class BlockHeader(
    blockNum: Int,
    blockType: BlockTypeL2,
    timeCreation: PosixTime,
    versionMajor: Int,
    versionMinor: Int,
    utxosActive: Int // RH32UtxoSetL2
)

enum BlockTypeL2 derives CanEqual:
    case Minor
    case Major
    case Final

case class BlockBody(
    eventsValid: Seq[(TxId, NonGenesisL2EventLabel)],
    eventsInvalid: Seq[(TxId, NonGenesisL2EventLabel)],
    depositsAbsorbed: Seq[UtxoId[L1]]
)

object BlockBody:
    def empty: BlockBody = BlockBody(Seq.empty, Seq.empty, Seq.empty)

// FIXME: should come form ledger/node

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
    eventsValid: Set[(TxId, NonGenesisL2EventLabel)] = Set.empty, // TODO: are sets ok?
    eventsInvalid: Set[(TxId, NonGenesisL2EventLabel)] = Set.empty,
    depositsAbsorbed: Seq[UtxoId[L1]] = Seq.empty,
    // utxosActive: RH32UtxoSetL2 = RH32UtxoSetL2.dummy
    utxosActive: Int = 42
) {
    def majorBlock(using
        ev: BlockType =:= TBlockMinor
    ): BlockBuilder[TBlockMajor, BlockNum, VersionMajor] =
        copy(blockType = Major, versionMinor = 0)

    def finalBlock(using
        ev: BlockType =:= TBlockMinor
    ): BlockBuilder[TBlockFinal, BlockNum, VersionMajor] =
        copy(blockType = Final, versionMinor = 0)

    def blockNum(blockNum: Int)(using
        ev: BlockNum =:= TNone
    ): BlockBuilder[BlockType, TSet, VersionMajor] =
        copy(blockNum = blockNum)

    def timeCreation(timeCreation: PosixTime): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(timeCreation = timeCreation)

    def versionMajor(versionMajor: Int)(using
        ev: VersionMajor =:= TNone
    ): BlockBuilder[BlockType, BlockNum, TSet] =
        copy(versionMajor = versionMajor)

    def versionMinor(versionMinor: Int)(using
        ev: BlockType =:= TBlockMinor
    ): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(versionMinor = versionMinor)

    def withTransaction(txId: TxId): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(eventsValid = eventsValid.+((txId, TransactionL2EventLabel)))

    def withWithdrawal(txId: TxId)(using
        ev: BlockType <:< TBlockMajor
    ): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(eventsValid = eventsValid.+((txId, WithdrawalL2EventLabel)))

    def withInvalidEvent(
        txId: TxId,
        eventType: NonGenesisL2EventLabel
    ): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(eventsInvalid = eventsInvalid.+((txId, eventType)))

    def withDeposit(d: UtxoId[L1])(using
        ev: BlockType =:= TBlockMajor
    ): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(depositsAbsorbed = depositsAbsorbed ++ Seq(d))

//    def utxosActive(utxosActive: RH32UtxoSetL2): BlockBuilder[BlockType, BlockNum, VersionMajor] =
//        copy(utxosActive = utxosActive)

    def utxosActive(utxosActive: Int): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        copy(utxosActive = utxosActive)

    def apply(
        foo: BlockBuilder[BlockType, BlockNum, VersionMajor] => BlockBuilder[
          BlockType,
          BlockNum,
          VersionMajor
        ]
    ): BlockBuilder[BlockType, BlockNum, VersionMajor] =
        foo(this)

    def build(using
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
          BlockBody(this.eventsValid.toSeq, this.eventsInvalid.toSeq, this.depositsAbsorbed)
        )
}

object BlockBuilder {
    def apply(): BlockBuilder[TBlockMinor, TNone, TNone] =
        BlockBuilder[TBlockMinor, TNone, TNone]()
}
