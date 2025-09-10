package hydrozoa.multisig.ledger.l1.utxo

import hydrozoa.{L1, Utxo}
import scalus.*
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData, fromData}
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.{TransactionInput, TransactionOutput}
import scalus.ledger.api.v3.Address
import scalus.ledger.api.v1.PosixTime
import scalus.prelude.Option

import scala.Option as OptionS
import scala.util.Try

final case class DepositUtxo(utxo: (TransactionInput, TransactionOutput), datum: DepositUtxo.Datum)

object DepositUtxo {

    /** This is all placeholder stuff: */

    case class Datum(
        address: Address,
        datum: Option[Data],
        deadline: PosixTime,
        refundAddress: Address,
        refundDatum: Option[Data]
    ) derives FromData,
          ToData
}
