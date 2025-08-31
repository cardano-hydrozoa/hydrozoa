package hydrozoa.multisig.cardano.state

import scalus.*
import scalus.builtin.Data.{FromData, ToData}
import scalus.builtin.{Data, FromData, ToData}
import scalus.ledger.api.v1.{Address, PosixTime}
import scalus.prelude.Option

case class DepositDatum(
       address: Address,
       datum: Option[Data],
       deadline: PosixTime,
       refundAddress: Address,
       refundDatum: Option[Data]
   ) derives FromData, ToData
