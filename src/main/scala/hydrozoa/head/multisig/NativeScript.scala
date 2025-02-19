package hydrozoa.head.multisig

import com.bloxbean.cardano.client.cip.cip67.CIP67AssetNameUtil.labelToPrefix
import com.bloxbean.cardano.client.crypto.Blake2bUtil.{blake2bHash224, blake2bHash256}
import com.bloxbean.cardano.client.crypto.VerificationKey
import com.bloxbean.cardano.client.transaction.spec.script.{NativeScript, ScriptAll, ScriptPubkey}

//  Hydrozoa's head beacon tokens prefix (4937 spells “HYDR” on dial pad)
val assetNamePrefix: Array[Byte] = labelToPrefix(4937)

/**
 * @param vKeys set of participants' verificarion keys
 * @return
 * headNativeScript :: Set PubKeyHash → Timelock
 * headNativeScript ≔ AllOf.map Signature
 * FIXME: vKeys should contain at least one key
 * FIXME: use ParticipantVerificationKey type
 */
def mkHeadNativeScript(vKeys: Set[VerificationKey]): NativeScript = {
  vKeys
    .map(ScriptPubkey.create)
    .foldLeft(ScriptAll())((s: ScriptAll, k: ScriptPubkey) => s.addScript(k))
}
