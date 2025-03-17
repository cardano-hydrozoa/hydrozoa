package hydrozoa.l2.ledger

import hydrozoa.l2.ledger.event.AnyL2Event
import hydrozoa.TxL2

/** @tparam U
  *   type for representing utxo set
  * @tparam E
  *   type for events
  * @tparam H
  *   type for event hash
  * @tparam V
  *   type for event verifier
  */
trait L2Ledger[U, G, T, W, UD, E <: AnyL2Event[G, T, W, UD], H, -V]:
    def activeState: U
    // FIXME: add error to the left
    def submit[E1 <: E](event: E1): Either[(H, String), (H, Option[TxL2], event.UtxosDiff)]
    // FIXME: naming: currently only calculates the hash
    // FIXME L1Tx
    def evaluate[E1 <: E](event: E1): Either[String, (H, Option[TxL2])]
    def isEmpty: Boolean
    def flush: U
    def updateUtxosActive(activeState: U): Unit

/** @tparam E
  *   type for events
  */
trait Verifier[-E]:
    def isValid(event: E): Boolean
