package hydrozoa.l2.ledger

import hydrozoa.l2.ledger.event.AnyL2Event
import hydrozoa.{TxL2, TxL2Virtual}

/** @tparam U
  *   type for representing utxo set
  * @tparam E
  *   type for events
  * @tparam H
  *   type for event hash
  * @tparam V
  *   type for event verifier
  */
trait L2Ledger[U, G, T, W, UD, E <: AnyL2Event[H, G, T, W, UD], H, -V]:

    def activeState: U

    /** Submits an event, returning its hash and dependent type for UTxO diff.
      * @param event
      * @tparam SomeEvent
      * @return
      */
    def submit[SomeEvent <: E](event: SomeEvent): Either[(H, String), (H, event.UtxosDiff)]

    def isEmpty: Boolean
    def flush: U
    def updateUtxosActive(activeState: U): Unit

/** @tparam E
  *   type for events
  */
trait Verifier[-E]:
    def isValid(event: E): Boolean
