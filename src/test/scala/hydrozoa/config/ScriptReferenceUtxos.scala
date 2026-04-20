package hydrozoa.config

import cats.*
import cats.data.*
import org.scalacheck.Arbitrary
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AddrKeyHash, Script, ScriptRef, TransactionInput, Utxo, Value}
import test.{GenWithTestPeers, given}

// This is for the happy path right now, feel free to expand the parameters
def generateScriptReferenceUtxos: GenWithTestPeers[ScriptReferenceUtxos] =
    for {
        testPeers <- ReaderT.ask
        network = testPeers.cardanoNetwork.network
        treasuryId <- ReaderT.liftF(Arbitrary.arbitrary[TransactionInput])
        disputeId <- ReaderT.liftF(Arbitrary.arbitrary[TransactionInput])
        address <- ReaderT.liftF(
          Arbitrary
              .arbitrary[AddrKeyHash]
              .map(akh =>
                  ShelleyAddress(
                    network = network,
                    payment = Key(akh),
                    delegation = Null
                  )
              )
        )

        mkUtxo = (id: TransactionInput, script: Script) =>
            Utxo(id, Babbage(address, Value.ada(10), None, Some(ScriptRef(script))))

        Right(treasury) =
            ScriptReferenceUtxos.TreasuryScriptUtxo(
              testPeers,
              mkUtxo(treasuryId, HydrozoaBlueprint.treasuryScript)
            ): @unchecked

        Right(dispute) =
            ScriptReferenceUtxos.DisputeScriptUtxo(
              testPeers,
              mkUtxo(disputeId, HydrozoaBlueprint.disputeScript)
            ): @unchecked

    } yield ScriptReferenceUtxos(
      treasury,
      dispute
    )
