package hydrozoa.multisig.ledger.dapp.utxo

import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.{KzgCommitment, kzgCommitment}
import scala.util.Try
import scalus.*
import scalus.builtin.Data.{FromData, ToData, fromData, toData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.{AssetName, TransactionInput, TransactionOutput, Utxo, Utxos, Value}

// TODO: Make opaque
final case class MultisigTreasuryUtxo(
    treasuryTokenName: AssetName,
    utxoId: TransactionInput,
    address: ShelleyAddress,
    datum: MultisigTreasuryUtxo.Datum,
    value: Value
) {
    def asUtxo: Utxo =
        Utxo(
          utxoId,
          TransactionOutput.apply(
            address = address,
            value = value,
            datumOption = Some(Inline(datum.toData))
          )
        )

    def kzgCommitment: KzgCommitment = ByteString.fromArray(datum.commit.bytes)
}

object MultisigTreasuryUtxo {

    /** If SomeTx extends TreasuryUtxo.Spent it means that tx is spending it. */
    trait Spent {
        def treasurySpent: MultisigTreasuryUtxo
    }

    /** If SomeTx extends TreasuryUtxo.Produced it means that tx is producing it. */
    trait Produced {
        def treasuryProduced: MultisigTreasuryUtxo
    }

    /** If SomeTx extends TreasuryUtxo.MbProduced it means that tx produced it optionally. */
    trait MbProduced {
        final def mbTreasuryProduced: Option[MultisigTreasuryUtxo] = this match
            case produced: (this.type & Produced) => Some(produced.treasuryProduced)
            case _                                => None
    }

    /** If some args extend this, it means that args contain it. */
    trait ToSpend {
        def treasuryToSpend: MultisigTreasuryUtxo
    }

    final case class Datum(
        commit: KzgCommitment,
        versionMajor: BigInt
    ) derives FromData,
          ToData

    def mkInitMultisigTreasuryDatum(initialL2Utxos: Utxos): Datum =
        Datum(
          initialL2Utxos.kzgCommitment,
          BigInt(BlockVersion.Major(0).toLong)
        )

    // TODO: Make into Either?
    def fromUtxo(utxo: Utxo): Option[MultisigTreasuryUtxo] = {
        val t = for {
            // Utxo has to be at a shelley address
            shelleyAddress <- Try(utxo.output.address.asInstanceOf[ShelleyAddress])

            // Treasury token name has to be the only asset in the value of the UTxO that is at the policy ID corresponding
            // to the script hash
            hnsScriptHash <- Try(shelleyAddress._2.asInstanceOf[ShelleyPaymentPart.Script].hash)
            treasuryTokenName <- Try(utxo.output.value.assets.assets(hnsScriptHash).keys.head)

            // Datum has to be inline and deserializable from Data
            inline <- Try(utxo.output.datumOption.get.asInstanceOf[Inline].data)
            datum: MultisigTreasuryUtxo.Datum <- Try(fromData[MultisigTreasuryUtxo.Datum](inline))
        } yield MultisigTreasuryUtxo(
          treasuryTokenName = treasuryTokenName,
          utxoId = utxo.input,
          address = shelleyAddress,
          datum = datum,
          value = utxo.output.value
        )
        t.toOption
    }
}
