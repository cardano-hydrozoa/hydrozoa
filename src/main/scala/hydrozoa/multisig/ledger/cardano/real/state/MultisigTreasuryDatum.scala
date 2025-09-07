package hydrozoa.multisig.ledger.cardano.real.state

import scalus._
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.ToData

case class MultisigTreasuryDatum  (
        commit: KzgCommit,
        versionMajor: VersionMajor,
        paramsHash: H32
    ) derives FromData, ToData

private type H32 = ByteString

private type KzgCommit = ByteString

private type VersionMajor = BigInt
