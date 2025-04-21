package hydrozoa.l1.multisig.state

import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.OutputL1
import scalus.*
import scalus.bloxbean.Interop
import scalus.builtin.Data.{FromData, ToData, fromData}
import scalus.builtin.FromDataInstances.given
import scalus.builtin.ToDataInstances.given
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.ledger.api.v1.{Address, PosixTime}
import scalus.prelude.Maybe

import scala.util.Try

// MultisigTreasuryDatum

case class MultisigTreasuryDatum(
    utxosActive: ByteString,
    versionMajor: BigInt,
    params: ByteString
)

type L2ConsensusParamsH32 = ByteString

// TODO: implement hashing for L2ConsensusParamsH32
// TODO: implement root hash
def mkInitMultisigTreasuryDatum: MultisigTreasuryDatum =
    mkMultisigTreasuryDatum(0, ByteString.empty)

// TODO: implement hashing for L2ConsensusParamsH32
// TODO: implement root hash
def mkMultisigTreasuryDatum(major: Int, _params: L2ConsensusParamsH32): MultisigTreasuryDatum =
    MultisigTreasuryDatum(
      ByteString.empty,
      BigInt(major),
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

def depositDatum(output: OutputL1): Option[DepositDatum] =
    for
        datumHex <- output.mbInlineDatum
        datum <- Try(
          fromData[DepositDatum](
            Interop.toScalusData(
              PlutusData.deserialize(HexUtil.decodeHexString(datumHex))
            )
          )
        ).toOption
    yield datum
