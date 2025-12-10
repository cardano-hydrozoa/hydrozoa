package hydrozoa.multisig.ledger.virtual

import cats.data.NonEmptyList
import cats.syntax.all.*
import hydrozoa.*
import hydrozoa.multisig.ledger.dapp.utxo.DepositUtxo
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{Hash as _, *}
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.prelude.Option as SOption

// A sum type for ledger events
sealed trait L2Event

// TODO: Run L2 conformance during parsing?
final case class L2EventTransaction(transaction: Transaction) extends L2Event {
    def volume: Long = transaction.body.value.outputs.map(sto => sto.value.value.coin.value).sum
}

final case class L2EventGenesis(
    genesisObligations: NonEmptyList[GenesisObligation],
    // blake2b_256(treasuryTokenName.bytestring ++ nextBlockVersion)
    // TODO: Type this better? It shouldn't really be a TransactionHash, because it's
    // preimage is not a [[Transaction]]
    genesisId: TransactionHash
) extends L2Event {
    val asUtxos: Utxos = {
        Map.from(
          genesisObligations.toList.zipWithIndex.map(x =>
              (TransactionInput(genesisId, x._2), x._1.toBabbage)
          )
        )
    }
}

/** A genesis obligation is the boundary between the L1 and L2 ledgers. It contains the well-formed
  * fields of L2-conformant UTxOs.
  */
case class GenesisObligation private (
    l2OutputPaymentAddress: ShelleyPaymentPart,
    l2OutputNetwork: Network,
    l2OutputDatum: SOption[Data],
    l2OutputValue: Coin,
    l2OutputRefScript: Option[Script.Native | Script.PlutusV3]
) {
    def toBabbage: Babbage =
        Babbage(
          address = ShelleyAddress(
            network = l2OutputNetwork,
            payment = l2OutputPaymentAddress,
            delegation = Null
          ),
          value = Value(l2OutputValue),
          datumOption = Some(Inline(toData(l2OutputDatum))),
          scriptRef = l2OutputRefScript.map(ScriptRef(_))
        )
}

object GenesisObligation {
    def fromDepositUtxo(d: DepositUtxo, network: Network): GenesisObligation = {
        import d.datum.*
        val l2PP: ShelleyPaymentPart = address match {
            case PubKeyCredential(hash) =>
                ShelleyPaymentPart.keyHash(AddrKeyHash.fromArray(hash.hash.bytes))
            case ScriptCredential(hash) =>
                ShelleyPaymentPart.scriptHash(ScriptHash.fromArray(hash.bytes))
        }

        new GenesisObligation(
          l2OutputPaymentAddress = l2PP,
          l2OutputNetwork = network,
          l2OutputDatum = datum,
          l2OutputValue = d._4,
          l2OutputRefScript = d._5
        )
    }
}
