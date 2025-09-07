package hydrozoa.multisig.ledger.cardano.real.state

import scalus._
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v1.PosixTime
import scalus.prelude.Option

case class DepositDatum(
       address: Address,
       datum: Option[Data],
       deadline: PosixTime,
       refundAddress: Address,
       refundDatum: Option[Data]
   ) derives FromData, ToData
