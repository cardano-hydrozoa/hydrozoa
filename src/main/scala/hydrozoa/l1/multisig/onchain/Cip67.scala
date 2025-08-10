package hydrozoa.l1.multisig.onchain

import com.bloxbean.cardano.client.cip.cip67.CIP67AssetNameUtil.labelToPrefix
import hydrozoa.infra.CryptoHash.H28
import hydrozoa.infra.{Piper, encodeHex}
import hydrozoa.UtxoIdL1
import scalus.builtin.ByteString
import scalus.cardano.ledger.AssetName

//  Hydrozoa's head beacon tokens prefix (4937 spells “HYDR” on dial pad)
// TODO: Use IArray?
val treasuryBeaconPrefix: Array[Byte] = labelToPrefix(4937)

// TODO: move to another module
/** @return
  *   treasury beacon token name
  */
def mkBeaconTokenName(seedUtxoId: UtxoIdL1): AssetName =
    val name = H28.hash_[UtxoIdL1](
      (seedUtxoId.transactionId.bytes.toList ++ BigInt(
        seedUtxoId.index
      ).toByteArray.toList).toArray
    )
    // FIXME: move the prefix somewhere else
    (encodeHex(IArray.from(treasuryBeaconPrefix ++ name.bytes)))
        |> (s => AssetName(ByteString.fromHex(s)))
