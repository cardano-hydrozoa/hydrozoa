package hydrozoa.l1.rulebased.onchain

import com.bloxbean.cardano.client.cip.cip67.CIP67AssetNameUtil.labelToPrefix
import hydrozoa.infra.CryptoHash.H28
import hydrozoa.infra.{Piper, encodeHex}
import hydrozoa.UtxoIdL1
import scalus.builtin.ByteString
import scalus.cardano.ledger.AssetName

// Hydrozoa's head dispute fungible tokens prefix (3477 spells “DISP” on a phone dial pad)
// TODO: Use IArray?
val disputeTokenPrefix: Array[Byte] = labelToPrefix(3477)

/** @return
  *   treasury beacon token name
  */
def mkVoteTokenName(treasuryUtxoId: UtxoIdL1): AssetName =
    val name = H28.hash_[UtxoIdL1](
      (treasuryUtxoId.transactionId.bytes.toList ++ BigInt(
        treasuryUtxoId.index
      ).toByteArray.toList).toArray
    )
    AssetName(ByteString.fromHex(encodeHex(IArray.from(disputeTokenPrefix ++ name.bytes))))
