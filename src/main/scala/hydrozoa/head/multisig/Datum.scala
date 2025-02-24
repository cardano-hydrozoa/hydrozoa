package hydrozoa.head.multisig

import scalus.*
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.FromDataInstances.given
import scalus.builtin.ToDataInstances.given
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.ledger.api.v1.{Address, PosixTime}
import scalus.prelude.Maybe

// MultisigTreasuryDatum

case class MultisigTreasuryDatum(
    utxosActive: ByteString,
    versionMajor: BigInt,
    params: ByteString
)

type L2ConsensusParamsH32 = ByteString

// FIXME: implement hashing for L2ConsensusParamsH32
def mkInitMultisigTreasuryDatum(_params: L2ConsensusParamsH32): MultisigTreasuryDatum =
    MultisigTreasuryDatum(
      ByteString.empty,
      BigInt(0),
      ByteString.empty
    )

given FromData[MultisigTreasuryDatum] = FromData.deriveCaseClass[MultisigTreasuryDatum]
given ToData[MultisigTreasuryDatum] = ToData.deriveCaseClass[MultisigTreasuryDatum](0)

// DepositDatum

case class DepositDatum(
    address: Address,
    datum: Maybe[ByteString],
    deadline: PosixTime,
    refundAddress: Address,
    refundDatum: Maybe[ByteString]
)

given FromData[DepositDatum] = FromData.deriveCaseClass[DepositDatum]
given ToData[DepositDatum] = ToData.deriveCaseClass[DepositDatum](0)
