package hydrozoa.multisig.ledger.dapp.script.multisig

import hydrozoa.config.head.peers.HeadPeers
import scala.collection.SortedSet
import scalus.builtin.Builtins.blake2b_224
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.Timelock.{AllOf, Signature}
import scalus.cardano.ledger.{Hash, *}
import scalus.cardano.txbuilder.ScriptSource.{NativeScriptAttached, NativeScriptValue}
import scalus.cardano.txbuilder.{ExpectedSigner, NativeScriptWitness}

case class HeadMultisigScript(private val script0: Script.Native) {
    val script: Script.Native = script0
    def mkAddress(network: Network = Mainnet): ShelleyAddress =
        ShelleyAddress(
          network = network,
          payment = ShelleyPaymentPart.Script(script.scriptHash),
          delegation = Null
        )
    val policyId: PolicyId = script.scriptHash

    // TODO: Make NonEmptySortedSet? This is basically what the implementation would look like
    val requiredSigners: Set[ExpectedSigner] = {
        val sortedKeys = script.script
            .asInstanceOf[Timelock.AllOf]
            .scripts
            .map(_.asInstanceOf[Signature].keyHash)
            .sorted(using Hash.Ordering)
        SortedSet
            .from(
              sortedKeys.map(pkh => ExpectedSigner(pkh))
              // TODO: make an order instance upstream
            )(using (x: ExpectedSigner, y: ExpectedSigner) => Hash.Ordering.compare(x.hash, y.hash))
            .toSet
    }
    val numSigners: Int = requiredSigners.toSeq.size

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
                  .sorted(using Ordering[AddrKeyHash])
                  .map(Signature(_))
            )
          )
        )
