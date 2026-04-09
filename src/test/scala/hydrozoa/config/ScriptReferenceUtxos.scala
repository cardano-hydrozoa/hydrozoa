package hydrozoa.config

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.rulebased.ledger.l1.script.plutus.{DisputeResolutionScript, RuleBasedTreasuryScript}
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AddrKeyHash, Script, ScriptRef, TransactionInput, Utxo, Value}

type ScriptReferenceUtxosGen = CardanoNetwork.Section => Gen[ScriptReferenceUtxos]

// This is for the happy path right now, feel free to expand the parameters
def generateScriptReferenceUtxos(network: CardanoNetwork.Section): Gen[ScriptReferenceUtxos] = for {
    treasuryId <- Arbitrary.arbitrary[TransactionInput]
    disputeId <- Arbitrary.arbitrary[TransactionInput]
    address <- Arbitrary
        .arbitrary[AddrKeyHash]
        .map(akh =>
            ShelleyAddress(
              network = network.network,
              payment = Key(akh),
              delegation = Null
            )
        )

    mkUtxo = (id: TransactionInput, script: Script) =>
        Utxo(id, Babbage(address, Value.ada(10), None, Some(ScriptRef(script))))

    Right(treasury) =
        ScriptReferenceUtxos.TreasuryScriptUtxo(
          network,
          // TODO: use Blueprint
          mkUtxo(treasuryId, RuleBasedTreasuryScript.compiledPlutusV3Script)
        )

    Right(dispute) =
        ScriptReferenceUtxos.DisputeScriptUtxo(
          network,
          // TODO: use Blueprint
          mkUtxo(disputeId, DisputeResolutionScript.compiledPlutusV3Script)
        )

} yield ScriptReferenceUtxos(
  treasury,
  dispute
)
