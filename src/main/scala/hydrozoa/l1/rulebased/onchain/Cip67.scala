package hydrozoa.l1.rulebased.onchain

import com.bloxbean.cardano.client.cip.cip67.CIP67AssetNameUtil.labelToPrefix
import hydrozoa.infra.CryptoHash.H28
import hydrozoa.infra.{encodeHex, Piper}
import hydrozoa.{TokenName, UtxoIdL1}

// Hydrozoa's head dispute fungible tokens prefix (3477 spells “DISP” on a phone dial pad)
// TODO: Use IArray?
val disputeTokenPrefix: Array[Byte] = labelToPrefix(3477)

/** @return
  *   treasury beacon token name
  */
def mkVoteTokenName(treasuryUtxoId: UtxoIdL1): TokenName =
    val name = H28.hash_[UtxoIdL1](
      (treasuryUtxoId.txId.hash.getBytes.toList ++ BigInt(
        treasuryUtxoId.outputIx.ix
      ).toByteArray.toList).toArray
    )
//    ("0x" + encodeHex(IArray.from(disputeTokenPrefix ++ name.bytes)))
//        |> TokenName.apply
    encodeHex(IArray.from(disputeTokenPrefix ++ name.bytes))
        |> TokenName.apply
