package hydrozoa.lib.cardano.scalus.ledger.txbuilder

import monocle.syntax.all.*
import scalus.cardano.ledger.{Coin, ProtocolParams, Sized, TransactionOutput}
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput

import scala.annotation.tailrec

object TxBuilder {}

/** Recursively calculate the minAda for UTxO.
  *
  * @param candidateOutput
  *   The initial output
  * @param params
  *   Protocol params (for minAda calculation)
  * @param update
  *   A function that takes the calculated minAda for the [[candidateOutput]] and modifies the
  *   output to calculate the new minAda. By default, it is [[replaceAdaUpdate]]
  * @return
  *   An output that has the [[update]] function applied to it until the minAda condition is
  *   satisfied for the UTxO
  */
@tailrec
def setMinAda(
    candidateOutput: Babbage,
    params: ProtocolParams,
    update: (Coin, Babbage) => Babbage = replaceAdaUpdate
): Babbage = {
    val minAda = MinCoinSizedTransactionOutput(Sized(candidateOutput), params)
//    println(s"Current candidate output value: ${candidateOutput.value.coin};" +
//        s" minAda required for current candidate output: $minAda; " +
//        s" size of current candidate output: ${Sized(candidateOutput.asInstanceOf[TransactionOutput]).size}")
    if minAda <= candidateOutput.value.coin
    then candidateOutput
    else setMinAda(update(minAda, candidateOutput), params, update)
}

/** An update function for use with calcMinAda. It replaces the output's coin with the given coin.
  * @param coin
  * @param to
  * @return
  */
def replaceAdaUpdate(coin: Coin, to: Babbage): Babbage =
    to.focus(_.value.coin).replace(coin)
