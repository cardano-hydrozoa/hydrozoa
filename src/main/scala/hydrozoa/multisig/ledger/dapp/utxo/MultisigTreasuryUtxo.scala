package hydrozoa.multisig.ledger.dapp.utxo

import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment.{KzgCommitment, kzgCommitment}
import scalus.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, Coin, TransactionInput, TransactionOutput, Utxo, Utxos, Value}
import scalus.uplc.builtin.Data.{FromData, ToData, toData}
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}

// TODO: Make opaque

/** @param treasuryTokenName
  * @param utxoId
  * @param address
  * @param datum
  * @param value
  *   Contains equity (used to pay fees for settlement, rollout, and fallback transactions) and
  *   liabilities (use to "cover" the L2 in the evacuation map)
  * @param equity
  */
final case class MultisigTreasuryUtxo(
    treasuryTokenName: AssetName,
    utxoId: TransactionInput,
    address: ShelleyAddress,
    datum: MultisigTreasuryUtxo.Datum,
    value: Value,
    equity: Equity
) {
    def asUtxo: Utxo =
        Utxo(
          utxoId,
          TransactionOutput.apply(
            address = address,
            value = value,
            inlineDatum = datum.toData
          )
        )

    def kzgCommitment: KzgCommitment = ByteString.fromArray(datum.commit.bytes)
}

/** Equity must always be positive
  * @param coin
  */
final case class Equity private (coin: Coin)

object Equity {
    def apply(amount: Coin): Option[Equity] =
        if amount.value >= 0L
        then Some(new Equity(amount))
        else None
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

}
