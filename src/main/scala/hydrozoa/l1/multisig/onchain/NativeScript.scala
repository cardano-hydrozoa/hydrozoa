package hydrozoa.l1.multisig.onchain

import com.bloxbean.cardano.client.address.AddressProvider.getEntAddress
import com.bloxbean.cardano.client.cip.cip67.CIP67AssetNameUtil.labelToPrefix
import com.bloxbean.cardano.client.common.model.Network
import com.bloxbean.cardano.client.crypto.VerificationKey
import com.bloxbean.cardano.client.transaction.spec.script.{ScriptAll, ScriptPubkey}
import com.bloxbean.cardano.client.util.HexUtil.encodeHexString
import hydrozoa.infra.CryptoHash.*
import hydrozoa.{
    ParticipantVerificationKey,
    TxId,
    TxIx,
    NativeScript as HNativeScript,
    Network as HNetwork
}

//  Hydrozoa's head beacon tokens prefix (4937 spells “HYDR” on dial pad)
val assetNamePrefix: Array[Byte] = labelToPrefix(4937)

/** @param vKeys
  *   set of participants' verificarion keys
  * @return
  *   headNativeScript :: Set PubKeyHash → Timelock headNativeScript ≔ AllOf.map Signature FIXME:
  *   vKeys should contain at least one key FIXME: use ParticipantVerificationKey type
  */
def mkHeadNativeScriptAndAddress(
    vKeys: Set[ParticipantVerificationKey],
    network: HNetwork
): (HNativeScript, String) = {
    val script = vKeys
        // TODO: compose vs multiple map?
        .map(_.bytes)
        .map(VerificationKey.create)
        .map(ScriptPubkey.create)
        .foldLeft(ScriptAll())((s: ScriptAll, k: ScriptPubkey) => s.addScript(k))
    val nw = Network(network.networkId, network.protocolMagic)
    val address = getEntAddress(script, nw).toBech32
    (HNativeScript(script.scriptRefBytes), address)
}

/** @return
  *   treasury beacon token name
  */
def mkBeaconTokenName(txId: TxId, txIx: TxIx): String =
    val name = H28.hash_(
      (txId.hash.getBytes.toList ++ BigInt(txIx.ix).toByteArray.toList).toArray
    )
    encodeHexString(assetNamePrefix ++ name.bytes, true)
