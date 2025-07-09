package hydrozoa.l1.multisig.onchain

import com.bloxbean.cardano.client.address.AddressProvider.getEntAddress
import com.bloxbean.cardano.client.common.model.Network
import com.bloxbean.cardano.client.crypto.KeyGenUtil.getKeyHash
import com.bloxbean.cardano.client.crypto.VerificationKey
import com.bloxbean.cardano.client.transaction.spec.script.{ScriptAll, ScriptPubkey}
import com.bloxbean.cardano.client.util.HexUtil.encodeHexString
import hydrozoa.infra.CryptoHash.*
import hydrozoa.infra.{CryptoHash, PSStyleAssoc, Piper, encodeHex, toBB}
import hydrozoa.{
    AddressBech,
    AddressBechL1,
    L1,
    TokenName,
    UtxoIdL1,
    VerificationKeyBytes,
    CurrencySymbol as HCurrencySymbol,
    NativeScript as HNativeScript,
    Network as HNetwork
}
import scalus.builtin.ByteString
import scalus.cardano.ledger.{AddrKeyHash, Hash, Script}
import scalus.ledger.api.Timelock.{AllOf, Signature}

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
  *   serialized native script
  */
def mkHeadNativeScript(
    vKeys: Set[VerificationKeyBytes],
): Script.Native =
    Script.Native(
      AllOf(
        vKeys
            .map(key => AddrKeyHash(ByteString.fromArray(key.bytes)))
            .toIndexedSeq
            .sorted(using Ordering[AddrKeyHash])
            .map(Signature(_))
      )
    )

