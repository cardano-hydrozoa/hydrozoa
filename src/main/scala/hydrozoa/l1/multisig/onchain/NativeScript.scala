package hydrozoa.l1.multisig.onchain

import com.bloxbean.cardano.client.address.AddressProvider.getEntAddress
import com.bloxbean.cardano.client.common.model.Network
import com.bloxbean.cardano.client.crypto.VerificationKey
import com.bloxbean.cardano.client.transaction.spec.script.{ScriptAll, ScriptPubkey}
import com.bloxbean.cardano.client.util.HexUtil.encodeHexString
import hydrozoa.infra.CryptoHash.*
import hydrozoa.{
    UtxoIdL1,
    PeerPublicKeyBytes,
    NativeScript as HNativeScript,
    Network as HNetwork
}

/** Creates `AllOf` native script from peer nodes' keys.
  *
  * {{{
  *  headNativeScript :: Set PubKeyHash → Timelock
  *  headNativeScript ≔ AllOf.map Signature
  * }}}
  *
  * @param vKeys
  *   set of participants' verification keys, should contain at least one key
  * @return
  *   pair of serilized native script and corresponding bech32 address
  */
def mkHeadNativeScriptAndAddress(
                                    vKeys: Set[PeerPublicKeyBytes],
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
def mkBeaconTokenName(seedOutput: UtxoIdL1): String =
    val name = H28.hash_[UtxoIdL1](
      (seedOutput.txId.hash.getBytes.toList ++ BigInt(
        seedOutput.outputIx.ix
      ).toByteArray.toList).toArray
    )
    encodeHexString(treasuryBeaconPrefix ++ name.bytes, true)
