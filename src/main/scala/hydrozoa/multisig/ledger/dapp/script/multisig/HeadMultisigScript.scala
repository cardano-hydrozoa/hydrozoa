package hydrozoa.multisig.ledger.dapp.script.multisig

import cats.*
import cats.data.*
import hydrozoa.VerificationKeyBytes
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.Timelock.{AllOf, Signature}
import scalus.cardano.ledger.{Hash, *}
import scalus.cardano.txbuilder.ScriptSource.NativeScriptAttached
import scalus.cardano.txbuilder.{ExpectedSigner, NativeScriptWitness}

import scala.collection.SortedSet

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
        val sortedKeys = script
            .script
            .asInstanceOf[Timelock.AllOf]
            .scripts
            .map(_.asInstanceOf[Signature].keyHash)
            .sorted(using Hash.Ordering)
        SortedSet.from(
            sortedKeys.map(pkh => ExpectedSigner(pkh))
            // TODO: make an order instance upstream
        )(using (x: ExpectedSigner, y: ExpectedSigner) => Hash.Ordering.compare(x.hash, y.hash)).toSet
    }
    val numSigners: Int = requiredSigners.toSeq.size

    val witness: NativeScriptWitness = NativeScriptWitness(
      scriptSource = NativeScriptAttached,
        additionalSigners = requiredSigners.toSet
    )

}

object HeadMultisigScript:
    def apply(vKeys: NonEmptyList[VerificationKeyBytes]): HeadMultisigScript =
        HeadMultisigScript(
          Script.Native(
            AllOf(
              vKeys
                  .map(key => key.verKeyHash)
                  .toList
                  .toIndexedSeq
                  .sorted(using Ordering[AddrKeyHash])
                  .map(Signature(_))
            )
          )
        )
