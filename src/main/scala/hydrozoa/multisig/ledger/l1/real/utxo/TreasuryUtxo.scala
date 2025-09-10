package hydrozoa.multisig.ledger.l1.real.utxo

import scalus.*
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.ledger.{AssetName, TransactionInput, TransactionOutput}

final case class TreasuryUtxo(headTokenName: AssetName, utxo: (TransactionInput, TransactionOutput))

object TreasuryUtxo {
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

}
