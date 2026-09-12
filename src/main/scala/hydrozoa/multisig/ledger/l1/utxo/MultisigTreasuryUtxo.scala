package hydrozoa.multisig.ledger.l1.utxo

import hydrozoa.multisig.ledger.block.BlockVersion
import hydrozoa.multisig.ledger.commitment.KzgCommitment.KzgCommitment
import hydrozoa.multisig.ledger.joint.EvacuationMap
import hydrozoa.multisig.ledger.l2.L2StateHash
import scalus.*
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, Coin, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.uplc.builtin.Data.{FromData, ToData, toData}
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}

/** @param value
  *   Contains equity and liabilities (use to "cover" the L2 in the evacuation map)
  * @param equity
  *   Is the excess treasury value above what is needed to back the liabilities. Equity is used to
  *   pay fees for settlement and rollout.
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

    /** Every field moves with the head: `commit` with the evacuation map, `versionMajor` with each
      * settlement, `l2StateHash` with the L2 state behind them. The head's configuration digest is
      * not here — it is immutable for the head's life, so it rides the multisig regime utxo's datum
      * ([[MultisigRegimeUtxo.Datum.headParamsHash]]), which is written once and never rewritten.
      *
      * @param l2StateHash
      *   the L2 ledger's digest of the state this datum's block leaves behind
      *   ([[hydrozoa.multisig.ledger.l2.L2StateHash]]). Where `commit` commits to the evacuation
      *   map — the L1-compatible *projection* of that state — this commits to the state itself, so
      *   two peers that agree on every evacuable payout and still diverged in the ledger that
      *   produced them do not both get their settlement signed. The datum is N-of-N multisigned by
      *   the hard-ack flow and lands on L1, which makes settlement the head's strongest state
      *   anchor and as sparse as its major cadence (`docs/spec/l2-state-certificate.md`).
      *
      * No validator reads this datum — the multisig treasury sits under a native script — so the
      * enforcement is entirely off-chain, in the peers' rebuild-before-signing.
      */
    final case class Datum(
        commit: KzgCommitment,
        versionMajor: BigInt,
        l2StateHash: ByteString
    ) derives FromData,
          ToData

    def mkInitMultisigTreasuryDatum(
        initialEvacuationMap: EvacuationMap,
        initialL2StateHash: L2StateHash
    ): Datum =
        Datum(
          initialEvacuationMap.kzgCommitment,
          BigInt(BlockVersion.Major(0).toLong),
          initialL2StateHash.byteString
        )

}
