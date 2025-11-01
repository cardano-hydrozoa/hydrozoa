package hydrozoa.multisig.ledger.dapp.utxo

import scalus.*
import scalus.builtin.Data.{FromData, ToData, toData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.{AssetName, TransactionInput, TransactionOutput, Value}
import scalus.cardano.txbuilder.TransactionUnspentOutput

// TODO: Make opaque
final case class TreasuryUtxo(
    headTokenName: AssetName,
    // TODO: rename to utxoId
    txId: TransactionInput,
    address: ShelleyAddress,
    datum: TreasuryUtxo.Datum,
    value: Value
) {
    val asUtxo: TransactionUnspentOutput =
        TransactionUnspentOutput(
          txId,
          TransactionOutput.apply(
            address = address,
            value = value,
            datumOption = Some(Inline(datum.toData))
          )
        )
}

object TreasuryUtxo {

    /** If SomeTx extends TreasuryUtxo.Spent it means that tx is spending it. */
    trait Spent {
        def treasurySpent: TreasuryUtxo
    }

    /** If SomeTx extends TreasuryUtxo.Produced it means that tx is producing it. */
    trait Produced {
        def treasuryProduced: TreasuryUtxo
    }

    /** If SomeTx extends TreasuryUtxo.MbProduced it means that tx produced it optionally. */
    trait MbProduced {
        final def mbTreasuryProduced: Option[TreasuryUtxo] = this match
            case produced: (this.type & Produced) => Some(produced.treasuryProduced)
            case _                                => None
    }

    /** If some args extend this, it means that args contain it. */
    trait ToSpend {
        def treasuryToSpend: TreasuryUtxo
    }

    final case class Datum(
        commit: KzgCommit,
        versionMajor: VersionMajor,
        paramsHash: H32
    ) derives FromData,
          ToData

    /** This is all placeholder stuff: */

    private type H32 = ByteString

    private type KzgCommit = ByteString

    private type VersionMajor = BigInt

    // TODO: implement hashing for params
    // TODO: implement root hash
    def mkInitMultisigTreasuryDatum: Datum =
        mkMultisigTreasuryDatum(0, ByteString.empty)

    // TODO: implement hashing for params
    // TODO: implement root hash
    def mkMultisigTreasuryDatum(major: Int, _params: H32): Datum =
        Datum(
          ByteString.empty,
          BigInt(major),
          ByteString.empty
        )

//    def fromUtxo(utxo : (TransactionInput, TransactionOutput)) : Option[TreasuryUtxo] = {
//        val tuxo = for {
//           datum <- Try(fromData[TreasuryUtxo.Datum](utxo._2.asInstanceOf[Babbage].datumOption.get.asInstanceOf[Inline].data))
//           va
//        } yield ???
//
//        tuxo match {
//            case Success(v) => Some(v)
//            case Failure(e) => None
//        }
//    }
}
