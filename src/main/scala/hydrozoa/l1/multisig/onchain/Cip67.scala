package hydrozoa.l1.multisig.onchain

import com.bloxbean.cardano.client.cip.cip67.CIP67AssetNameUtil.labelToPrefix
import hydrozoa.infra.CryptoHash.H28
import hydrozoa.infra.{encodeHex, Piper}
import hydrozoa.{TokenName, UtxoIdL1}

//  Hydrozoa's head beacon tokens prefix (4937 spells “HYDR” on dial pad)
// TODO: Use IArray?
val treasuryBeaconPrefix: Array[Byte] = labelToPrefix(4937)

// TODO: move to another module
/** @return
  *   treasury beacon token name
  */
def mkBeaconTokenName(seedUtxoId: UtxoIdL1): TokenName =
    val name = H28.hash_[UtxoIdL1](
      (seedUtxoId.txId.hash.getBytes.toList ++ BigInt(
        seedUtxoId.outputIx.ix
      ).toByteArray.toList).toArray
    )
    // FIXME: move the prefix somewhere else
    ("0x" + encodeHex(IArray.from(treasuryBeaconPrefix ++ name.bytes)))
        |> TokenName.apply
