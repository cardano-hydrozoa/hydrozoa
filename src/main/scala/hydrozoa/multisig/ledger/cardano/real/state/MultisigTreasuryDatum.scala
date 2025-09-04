package hydrozoa.multisig.ledger.cardano.real.state

import scalus.*
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{ByteString, Data, FromData, ToData}

case class MultisigTreasuryDatum  (
        commit: KzgCommit,
        versionMajor: VersionMajor,
        paramsHash: H32
    ) derives FromData, ToData

private type H32 = ByteString

private type KzgCommit = ByteString

private type VersionMajor = BigInt
