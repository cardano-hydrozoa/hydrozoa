package hydrozoa.multisig.ledger.dapp.script.multisig

import cats.*
import cats.data.*
import hydrozoa.VerificationKeyBytes
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.ledger.api
import scalus.ledger.api.Timelock.{AllOf, Signature}

case class HeadMultisigScript(private val script0: Script.Native) {
    val script: Script.Native = script0
    def address(network: Network = Mainnet): ShelleyAddress =
        ShelleyAddress(
          network = network,
          payment = ShelleyPaymentPart.Script(script.scriptHash),
          delegation = Null
        )
    val policyId: PolicyId = script.scriptHash
    val requiredSigners: TaggedOrderedSet[AddrKeyHash] =
        TaggedOrderedSet.from(
          script
              .asInstanceOf[api.Timelock.AllOf]
              .scripts
              .map(_.asInstanceOf[Signature].keyHash)
        )
    val numSigners: Int = requiredSigners.toSeq.size
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
