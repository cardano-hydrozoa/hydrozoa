package hydrozoa.multisig.ledger.l1.real.script.multisig

import hydrozoa.VerificationKeyBytes
import scalus.cardano.ledger.{AddrKeyHash, Hash, Script}
import scalus.ledger.api.Timelock.{AllOf, Signature}

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
  *   serialized native script
  */
def mkHeadNativeScript(
    vKeys: Set[VerificationKeyBytes]
): Script.Native =
    Script.Native(
      AllOf(
        vKeys
            .map(key => key.verKeyHash)
            .toIndexedSeq
            .sorted(using Ordering[AddrKeyHash])
            .map(Signature(_))
      )
    )
