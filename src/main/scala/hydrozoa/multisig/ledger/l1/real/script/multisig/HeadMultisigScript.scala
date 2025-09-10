package hydrozoa.multisig.ledger.l1.real.script.multisig

import cats.*
import cats.data.*
import hydrozoa.VerificationKeyBytes
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.{AddrKeyHash, Hash, PolicyId, Script}
import scalus.ledger.api
import scalus.ledger.api.Timelock.{AllOf, Signature}

object HeadMultisigScript:
    opaque type HeadMultisigScript = Script.Native
    given Conversion[HeadMultisigScript, Script.Native] = identity

    extension (s: HeadMultisigScript)
        def script: Script.Native = s
        def address(network: Network = Mainnet): ShelleyAddress =
            ShelleyAddress(
              network = network,
              payment = ShelleyPaymentPart.Script(script.scriptHash),
              delegation = Null
            )
        def policyId: PolicyId = script.scriptHash
        def requiredSigners: Set[AddrKeyHash] =
            s.script
                .asInstanceOf[api.Timelock.AllOf]
                .scripts
                .map(_.asInstanceOf[Signature].keyHash)
                .toSet

    def apply(vKeys: NonEmptyList[VerificationKeyBytes]): HeadMultisigScript =
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

type HeadMultisigScript = HeadMultisigScript.HeadMultisigScript
