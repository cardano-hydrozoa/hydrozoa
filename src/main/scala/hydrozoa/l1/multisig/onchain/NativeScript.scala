package hydrozoa.l1.multisig.onchain

import hydrozoa.VerificationKeyBytes
import hydrozoa.infra.verKeyHash
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
            .map(key => AddrKeyHash(ByteString.fromHex(key.verKeyHash)))
            .toIndexedSeq
            .sorted(using Ordering[AddrKeyHash])
            .map(Signature(_))
      )
    )
