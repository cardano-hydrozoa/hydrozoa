package hydrozoa.l2.block

import hydrozoa.infra.CryptoHash.H32

case class Block(
    blockHeader: BlockHeader,
    blockBody: Unit
)

case class BlockHeader(
    blockNum: Int,
    blockType: BlockTypeL2,
    timeCreation: BigInt,
    versionMajor: Int,
    versionMinor: Int,
    utxosActive: RH32UtxoSetL2
)

enum BlockTypeL2:
    case Minor
    case Major
    case Final

opaque type RH32UtxoSetL2 = H32
