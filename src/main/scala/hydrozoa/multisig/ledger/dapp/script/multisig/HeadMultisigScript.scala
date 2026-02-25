package hydrozoa.multisig.ledger.dapp.script.multisig

import hydrozoa.config.head.peers.HeadPeers
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Timelock.{AllOf, Signature}
import scalus.cardano.txbuilder.ScriptSource.{NativeScriptAttached, NativeScriptValue}
import scalus.cardano.txbuilder.{ExpectedSigner, NativeScriptWitness}
import scalus.uplc.builtin.Builtins.blake2b_224

case class HeadMultisigScript(private val script0: Script.Native) {
    val script: Script.Native = script0
    def mkAddress(network: Network = Mainnet): ShelleyAddress =
        ShelleyAddress(
          network = network,
          payment = ShelleyPaymentPart.Script(script.scriptHash),
          delegation = Null
        )
    val policyId: PolicyId = script.scriptHash

    /** NOTE: because this is [[Set]], the traversal order WILL NOT be the same as
      * [[headPeers.headPeerVKeys]]
      */
    val requiredSigners: Set[ExpectedSigner] = {
        script.script
            .asInstanceOf[Timelock.AllOf]
            .scripts
            .map(_.asInstanceOf[Signature].keyHash)
            .toSet
            .map(ExpectedSigner(_))
    }
    val numSigners: Int = requiredSigners.toSeq.size

    def checkSigners(signers: Set[ExpectedSigner]): Boolean = signers == requiredSigners

    // use when the multisig witness utxo id not available
    val witnessValue: NativeScriptWitness = NativeScriptWitness(
      scriptSource = NativeScriptValue(script),
      additionalSigners = requiredSigners
    )

    // use when referencing the multisig witness utxo
    // or after [[witnessValue]] has been used within a tx
    val witnessAttached: NativeScriptWitness = NativeScriptWitness(
      scriptSource = NativeScriptAttached,
      additionalSigners = requiredSigners
    )

}

object HeadMultisigScript:
    def apply(headPeers: HeadPeers): HeadMultisigScript =
        HeadMultisigScript(
          Script.Native(
            AllOf(
              headPeers.headPeerVKeys
                  .map(vkey => AddrKeyHash(blake2b_224(vkey)))
                  .toList
                  .toIndexedSeq
                  .map(Signature(_))
            )
          )
        )
