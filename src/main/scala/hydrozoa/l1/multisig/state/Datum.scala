package hydrozoa.l1.multisig.state

import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.OutputL1
import scalus.*
import scalus.bloxbean.Interop
import scalus.builtin.Data.{FromData, ToData, fromData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.ledger.DatumOption
import scalus.cardano.ledger.DatumOption.Inline
import scalus.ledger.api.v1.{Address, PosixTime}
import scalus.prelude.Option

import scala.language.implicitConversions
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
// FIXME: I'd _like_ to use hydrozoa.Address[L], but then the ToData deriving doesn't work, despite defining an
// instance in the Address[L] object.
case class DepositDatum(
    address: Address,
    /** Represents an optional inline datum */
    datum: Option[Data],
    deadline: PosixTime,
    refundAddress: Address,
    /** Represents an optional inline datum */
    refundDatum: Option[Data]
) derives FromData,
      ToData

import scala.Option as OptionS

def depositDatum(output: OutputL1): OptionS[DepositDatum] =
    for
        datumOption <- output.datumOption
        datum <- Try(
          fromData[DepositDatum](datumOption.asInstanceOf[Inline].data)
        ).toOption
    yield datum
