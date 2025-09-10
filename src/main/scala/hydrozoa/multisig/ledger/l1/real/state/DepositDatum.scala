package hydrozoa.multisig.ledger.l1.real.state

import hydrozoa.OutputL1
import scalus.*
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData, fromData}
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.cardano.ledger.DatumOption.Inline
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v1.PosixTime
import scalus.prelude.Option

import scala.util.Try

case class DepositDatum(
    address: Address,
    datum: Option[Data],
    deadline: PosixTime,
    refundAddress: Address,
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