package hydrozoa.l1.multisig.state

import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.OutputL1
import scalus.*
import scalus.bloxbean.Interop
import scalus.builtin.Data.{FromData, ToData, fromData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v1.{Address, PosixTime}
import scalus.prelude.Option

import scala.util.Try

// MultisigTreasuryDatum

case class MultisigTreasuryDatum(
    utxosActive: ByteString,
    versionMajor: BigInt,
    params: L2ConsensusParamsH32
) derives FromData,
      ToData

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

// DepositDatum

case class DepositDatum(
    address: Address,
    datum: Option[ByteString],
    deadline: PosixTime,
    refundAddress: Address,
    refundDatum: Option[ByteString]
) derives FromData,
      ToData

import scala.Option as OptionS

def depositDatum(output: OutputL1): OptionS[DepositDatum] =
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
