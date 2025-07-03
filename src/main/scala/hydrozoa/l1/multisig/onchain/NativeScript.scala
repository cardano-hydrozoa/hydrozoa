package hydrozoa.l1.multisig.onchain

import com.bloxbean.cardano.client.address.AddressProvider.getEntAddress
import com.bloxbean.cardano.client.common.model.Network
import com.bloxbean.cardano.client.crypto.KeyGenUtil.getKeyHash
import com.bloxbean.cardano.client.crypto.VerificationKey
import com.bloxbean.cardano.client.transaction.spec.script.{ScriptAll, ScriptPubkey}
import com.bloxbean.cardano.client.util.HexUtil.encodeHexString
import hydrozoa.infra.CryptoHash.*
import hydrozoa.infra.{CryptoHash, PSStyleAssoc, Piper, encodeHex, toBB}
import hydrozoa.{AddressBech, AddressBechL1, L1, TokenName, UtxoIdL1, VerificationKeyBytes, CurrencySymbol as HCurrencySymbol, NativeScript as HNativeScript, Network as HNetwork}

/** Creates `AllOf` native script from peer nodes' keys.
  *
  * {{{
  *  headNativeScript :: Set PubKeyHash → Timelock
  *  headNativeScript ≔ AllOf.map Signature
  * }}}
  *
  * Always sorts vKeys in alphabetical order of their hashes since this changes the script.
  *
  * @param vKeys
  *   set of participants' verification keys, should contain at least one key
  * @return
  *   serialized native script, currency symbol, and corresponding bech32 address
  */
def mkHeadNativeScriptAndAddress(
    vKeys: Set[VerificationKeyBytes],
    network: HNetwork
): (HNativeScript, HCurrencySymbol, AddressBechL1) =

    // Script
    val script = vKeys.toList
        .map(vkb => (getKeyHash(vkb.bytes), VerificationKey.create(vkb.bytes)))
        .sortWith((a, b) => a._1.compareTo(b._1) < 0)
        .map((_, vk) => ScriptPubkey.create(vk))
        .foldLeft(ScriptAll())((s: ScriptAll, k: ScriptPubkey) => s.addScript(k))

    // Script body
    val scriptRefBytes = script.scriptRefBytes

    // Currency symbol
    val currencySymbol = IArray.from(script.getScriptHash) |> HCurrencySymbol.apply

    // Address
    val address = getEntAddress(script, network.toBB).toBech32

    (HNativeScript(scriptRefBytes), currencySymbol, AddressBech[L1](address))
