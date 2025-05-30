package hydrozoa.l1.rulebased.onchain

import com.bloxbean.cardano.client.cip.cip67.CIP67AssetNameUtil.labelToPrefix

//  Hydrozoa's head dispute fungible tokens prefix (3477 spells “DISP” on a phone dial pad)
val disputeTokenPrefix: Array[Byte] = labelToPrefix(3477)
