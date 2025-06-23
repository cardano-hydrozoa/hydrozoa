package hydrozoa.l1.multisig.onchain

import com.bloxbean.cardano.client.cip.cip67.CIP67AssetNameUtil.labelToPrefix

//  Hydrozoa's head beacon tokens prefix (4937 spells “HYDR” on dial pad)
// TODO: Use IArray?
val treasuryBeaconPrefix: Array[Byte] = labelToPrefix(4937)
