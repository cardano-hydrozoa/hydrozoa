package hydrozoa.l2.block

import hydrozoa.infra.CryptoHash.H32
import hydrozoa.node.server.AwaitingDeposit

case class Block(
    blockHeader: BlockHeader,
    blockBody: BlockBody
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

case class BlockBody(depositsAbsorbed: Set[AwaitingDeposit])

opaque type RH32UtxoSetL2 = H32[UtxoSetL2]
type UtxoSetL2 = Unit 

def majorDummyBlock(major: Int, depositsAbsorbed: Set[AwaitingDeposit]): Block =
    Block(
      BlockHeader(0, BlockTypeL2.Major, 0, major, 0, H32.hash(IArray())),
      BlockBody(depositsAbsorbed)
    )

def finalDummyBlock(major: Int): Block =
    Block(
      BlockHeader(0, BlockTypeL2.Final, 0, major, 0, H32.hash(IArray())),
      BlockBody(Set.empty)
    )
