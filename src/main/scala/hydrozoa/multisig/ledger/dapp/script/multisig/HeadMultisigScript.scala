package hydrozoa.multisig.ledger.dapp.script.multisig

import cats.*
import cats.data.*
import hydrozoa.VerificationKeyBytes
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Timelock.{AllOf, Signature}
import scalus.cardano.ledger.txbuilder.ScriptSource.NativeScriptAttached
import scalus.cardano.ledger.txbuilder.{ExpectedSigner, NativeScriptWitness}

case class HeadMultisigScript(private val script0: Script.Native) {
    val script: Script.Native = script0
    def mkAddress(network: Network = Mainnet): ShelleyAddress =
        ShelleyAddress(
          network = network,
          payment = ShelleyPaymentPart.Script(script.scriptHash),
          delegation = Null
        )
    val policyId: PolicyId = script.scriptHash
    val requiredSigners: Set[ExpectedSigner] =
        Set.from(
          script.script
              .asInstanceOf[Timelock.AllOf]
              .scripts
              .map(keyHash => ExpectedSigner(keyHash.asInstanceOf[Signature].keyHash))
        )
    val numSigners: Int = requiredSigners.toSeq.size

    val witness: NativeScriptWitness = NativeScriptWitness(
      scriptSource = NativeScriptAttached,
      additionalSigners = requiredSigners
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
