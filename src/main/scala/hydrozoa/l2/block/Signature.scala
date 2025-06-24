package hydrozoa.l2.block

import hydrozoa.l1.rulebased.onchain.DisputeResolutionValidator.{BlockHeader as SBlockHeader, given}
import hydrozoa.l2.block.BlockHeader as HBlockHeader
import scalus.builtin.Builtins.serialiseData
import scalus.builtin.ByteString
import scalus.builtin.ToData.toData

def mkOnchainBlockHeader(bh: HBlockHeader) = {
    // Convert block header into onchain representation
    val blockHeader: SBlockHeader = SBlockHeader(
      BigInt(bh.blockNum),
      bh.blockType,
      bh.timeCreation,
      BigInt(bh.versionMajor),
      BigInt(bh.versionMinor),
      ByteString.fromHex(bh.utxosActive)
    )
    blockHeader
}

def mkBlockHeaderSignatureMessage(bh: HBlockHeader): IArray[Byte] = {
    val blockHeader: SBlockHeader = mkOnchainBlockHeader(bh)
    // Serialize it to data and get bytes
    val msg = serialiseData(blockHeader.toData)
    IArray.from(msg.bytes)
}
