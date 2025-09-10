package hydrozoa.multisig.ledger.l1.real.utxo

import hydrozoa.{L1, Utxo}
import scalus.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.cardano.ledger.AssetName

final case class TreasuryUtxo(headTokenName: AssetName, utxo: Utxo[L1])

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
